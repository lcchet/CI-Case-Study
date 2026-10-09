library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# read data
claims_raw <- read_excel("Claims Data.xlsx")

# helper function to parse dates
parse_mixed_date <- function(x) {
  if (inherits(x, c("POSIXt", "Date"))) {
    return(as.Date(x))
  }
  is_num <- !is.na(suppressWarnings(as.numeric(as.character(x))))
  out <- as.Date(rep(NA, length(x)))
  out[is_num] <- as.Date(as.numeric(as.character(x[is_num])), origin = "1899-12-30")
  out[!is_num] <- as.Date(parse_date_time(x[!is_num], orders = c("dmy", "ymd HMS", "ymd", "mdy"), quiet = TRUE))
  return(out)
}

# clean up claims data
claims_cleaned <- claims_raw %>%
  distinct(`Claim Number`, .keep_all = TRUE) %>%
  mutate(
    DOB_clean = parse_mixed_date(`Date of Birth`),
    Diagnosis_clean = parse_mixed_date(`Date of Diagnosis`),
    Age_at_Claim = as.period(interval(DOB_clean, Diagnosis_clean))$year
  ) %>%
  # remove invalid dates outside 2000-2025 or diagnosis before birth
  filter(
    !is.na(Diagnosis_clean),
    Diagnosis_clean >= as.Date("2000-01-01"),
    Diagnosis_clean <= as.Date("2025-12-31"),
    is.na(Age_at_Claim) | Age_at_Claim >= 0
  ) %>%
  # standardize categorical fields
  mutate(
    Gender_clean = str_trim(Gender),
    Gender_clean = case_when(
      Gender_clean %in% c("F", "f", "Female", "female") ~ "F",
      Gender_clean %in% c("M", "m", "Male", "male", "MALE", "2") ~ "M",
      TRUE ~ "Unknown"
    ),
    
    # fix missing product names using plan code
    Product_Name_clean = str_trim(`Product Name`),
    Product_Name_clean = case_when(
      Product_Name_clean %in% c("(unknown)", "", "NA") & str_trim(`Plan Code (Dread Disease)`) == "VC20" ~ "VitalCare Critical Illness Plan",
      Product_Name_clean %in% c("(unknown)", "", "NA") & str_trim(`Plan Code (Dread Disease)`) == "LS30" ~ "LifeSecure Critical Illness Rider",
      TRUE ~ Product_Name_clean
    ),
    
    # fix mismatched plan codes
    Plan_Code_clean = case_when(
      Product_Name_clean == "VitalCare Critical Illness Plan"   ~ "VC20",
      Product_Name_clean == "LifeSecure Critical Illness Rider" ~ "LS30",
      TRUE ~ str_trim(`Plan Code (Dread Disease)`)
    ),
    
    Claim_Type_clean = str_trim(`Claim Type`),
    Claim_Type_clean = case_when(
      str_detect(str_to_lower(Claim_Type_clean), "early") ~ "Early-stage",
      str_detect(str_to_lower(Claim_Type_clean), "major") ~ "Major",
      TRUE ~ "Unknown"
    ),
    
    Claim_Status_clean = str_trim(`Claim Status`),
    Claim_Status_clean = case_when(
      is.na(Claim_Status_clean) ~ "Unknown",
      Claim_Status_clean %in% c("Paid", "PAID") ~ "Paid",
      Claim_Status_clean %in% c("Declined", "DECLINED") ~ "Declined",
      Claim_Status_clean %in% c("Pending", "PENDING") ~ "Pending",
      TRUE ~ str_to_title(Claim_Status_clean)
    ),
    
    Cause_clean = str_trim(`Claim Cause / Condition`),
    Cause_clean = case_when(
      Cause_clean %in% c("Common Cold", "Fracture") ~ "Non-CI Excluded",
      Cause_clean %in% c("999", "Unknown", "")      ~ "Unknown",
      TRUE ~ Cause_clean
    )
  ) %>%
  # clean claim amounts
  mutate(
    Amount_is_text = str_detect(str_to_upper(as.character(`Claim Amount`)), "PENDING"),
    Claim_Amount_num = as.numeric(gsub("[^0-9.-]", "", as.character(`Claim Amount`))),
    Claim_Amount_num = abs(Claim_Amount_num),
    Claim_Amount_clean = case_when(
      Amount_is_text ~ NA_real_,
      Claim_Amount_num <= 0 ~ NA_real_,
      Claim_Amount_num >= 1e8 ~ NA_real_,
      TRUE ~ Claim_Amount_num
    )
  ) %>%
  select(
    `Claim Number`,
    `Policy Number`,
    `Plan Code (Dread Disease)` = Plan_Code_clean,
    `Product Name`              = Product_Name_clean,
    Gender                      = Gender_clean,
    `Date of Birth`             = DOB_clean,
    `Date of Diagnosis`         = Diagnosis_clean,
    `Age at Claim`              = Age_at_Claim,
    `Claim Cause / Condition`   = Cause_clean,
    `Claim Type`                = Claim_Type_clean,
    `Claim Status`              = Claim_Status_clean,
    `Claim Amount`              = Claim_Amount_clean
  ) %>%
  filter(
    `Claim Status` == "Paid",
    `Claim Cause / Condition` != "Non-CI Excluded"
  )

# join with policy data to impute missing amounts and apply claim limits
policy_clean <- read.csv("Policy_Data_Cleaned.csv", check.names = FALSE)

claims_final <- claims_cleaned %>%
  left_join(
    policy_clean %>% select(`Policy Number`, `Sum Assured`, `Policy Issue Date`), 
    by = "Policy Number"
  ) %>%
  mutate(
    `Policy Issue Date` = as.Date(`Policy Issue Date`),
    Years_In_Force = floor(as.numeric(difftime(`Date of Diagnosis`, `Policy Issue Date`, units = "days")) / 365.25),
    Years_In_Force = ifelse(Years_In_Force < 0 | is.na(Years_In_Force), 0, Years_In_Force),
    # calculate booster (+5% per year up to 50% for VitalCare)
    Boosted_SA = case_when(
      `Product Name` == "VitalCare Critical Illness Plan" ~ `Sum Assured` * (1 + 0.05 * pmin(Years_In_Force, 10)),
      TRUE ~ `Sum Assured`
    ),
    # fill in pending claim amounts
    `Claim Amount` = case_when(
      is.na(`Claim Amount`) & `Claim Type` == "Early-stage" & `Product Name` == "VitalCare Critical Illness Plan" ~ pmin(Boosted_SA * 0.25, 50000),
      is.na(`Claim Amount`) & `Claim Type` == "Major" ~ Boosted_SA,
      TRUE ~ `Claim Amount`
    )
  ) %>%
  arrange(`Policy Number`, `Date of Diagnosis`) %>%
  group_by(`Policy Number`) %>%
  mutate(
    Claim_Sequence = row_number(),
    Cumulative_Paid = cumsum(`Claim Amount`)
  ) %>%
  ungroup() %>%
  filter(
    # filter out multiple claims for LifeSecure and payouts over 300% for VitalCare
    !(`Product Name` == "LifeSecure Critical Illness Rider" & Claim_Sequence > 1),
    !(`Product Name` == "VitalCare Critical Illness Plan" & (Cumulative_Paid - `Claim Amount`) >= (Boosted_SA * 3))
  ) %>%
  select(-`Sum Assured`, -`Policy Issue Date`, -Years_In_Force, -Boosted_SA, -Claim_Sequence, -Cumulative_Paid)

# quick checks
cat("Original rows:", nrow(claims_raw), "\nCleaned rows:", nrow(claims_final), "\n")
table(claims_final$`Product Name`, claims_final$`Plan Code (Dread Disease)`, useNA = "always")
table(claims_final$`Claim Cause / Condition`, useNA = "always")
table(claims_final$`Claim Status`, useNA = "always")
table(claims_final$`Claim Type`, useNA = "always")
summary(claims_final %>% select(`Age at Claim`, `Claim Amount`))

options(scipen = 999)

# export
write.csv(claims_final, "Claims_Data_Cleaned.csv", row.names = FALSE)