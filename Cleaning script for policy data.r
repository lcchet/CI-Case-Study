library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# load raw data
policy_raw <- read_excel("Policy Data.xlsx")

# helper to parse mixed excel dates/strings
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

# clean and standardize policy data
policy_cleaned <- policy_raw %>%
  distinct(`Policy Number`, .keep_all = TRUE) %>%
  
  # dates and age checks
  mutate(
    DOB_clean    = parse_mixed_date(`Date of Birth`),
    Issue_clean  = parse_mixed_date(`Policy Issue Date`),
    Status_clean = parse_mixed_date(`Status Date`),
    Calc_Age     = as.period(interval(DOB_clean, Issue_clean))$year,
    
    # trust recorded Entry Age if Calc_Age looks off/juvenile
    Entry_Age_clean = case_when(
      !is.na(Calc_Age) & Calc_Age >= 18 & abs(Calc_Age - `Entry Age`) <= 1 ~ Calc_Age,
      `Entry Age` >= 18 ~ as.numeric(`Entry Age`),
      TRUE ~ NA_real_
    ),
    
    # re-align bad DOBs based on Issue Date and Entry Age
    DOB_realigned = if_else(
      is.na(Calc_Age) | Calc_Age < 18 | Calc_Age < 0,
      Issue_clean - years(as.numeric(`Entry Age`)),
      DOB_clean
    )
  ) %>%
  # keep valid issue dates between 2000 and 2025
  filter(
    !is.na(Issue_clean),
    Issue_clean >= as.Date("2000-01-01"),
    Issue_clean <= as.Date("2025-12-31"),
    !is.na(Entry_Age_clean)
  ) %>%
  
  # clean categorical columns
  mutate(
    Gender_clean = str_trim(Gender),
    Gender_clean = case_when(
      Gender_clean %in% c("F", "f", "Female", "female") ~ "F",
      Gender_clean %in% c("M", "m", "Male", "male", "MALE", "2") ~ "M",
      TRUE ~ "Unknown"
    ),
    
    Smoker_clean = str_trim(`Smoker Status`),
    Smoker_clean = case_when(
      Smoker_clean %in% c("0", "N", "NS", "Non-smoker", "Non smoker", "non-smoker") ~ "Non-Smoker",
      Smoker_clean %in% c("1", "S", "Y", "Smoker") ~ "Smoker",
      TRUE ~ "Unknown"
    ),
    
    Occ_Class_clean = suppressWarnings(as.integer(str_trim(`Occupation Class`))),
    Occ_Class_clean = if_else(Occ_Class_clean %in% 1:4, Occ_Class_clean, NA_integer_),
    
    Education_clean = str_to_title(str_trim(`Education Level`)),
    Education_clean = case_when(
      str_detect(Education_clean, "Bach|Bsc|Degree") ~ "Bachelor",
      str_detect(Education_clean, "Mast|Msc")        ~ "Master",
      str_detect(Education_clean, "Phd|Doctor")      ~ "Doctorate",
      str_detect(Education_clean, "Dip")             ~ "Diploma",
      str_detect(Education_clean, "Sec|High|O-Level") ~ "Secondary",
      TRUE ~ "Other/Unknown"
    ),
    
    Marital_clean = str_to_title(str_trim(`Marital Status`)),
    Marital_clean = case_when(
      Marital_clean %in% c("Maried", "Married", "M") ~ "Married",
      Marital_clean %in% c("Single", "S")            ~ "Single",
      Marital_clean %in% c("Divorce", "Divorced")    ~ "Divorced",
      Marital_clean == "Widowed"                     ~ "Widowed",
      TRUE ~ "Unknown"
    )
  ) %>%
  
  # clean sum assured and remove extreme outliers/placeholders
  mutate(
    Sum_Assured_num = as.numeric(gsub("[^0-9.-]", "", `Sum Assured`)),
    Sum_Assured_num = abs(Sum_Assured_num),
    Sum_Assured_clean = case_when(
      Sum_Assured_num < 10000 ~ NA_real_,
      Sum_Assured_num >= 1e8  ~ NA_real_,
      TRUE                    ~ Sum_Assured_num
    )
  ) %>%
  
  # fix height (meters to cm) and weight (grams to kg)
  mutate(
    Height_clean = as.numeric(`Height (cm)`),
    Height_clean = case_when(
      Height_clean > 0 & Height_clean < 2.5 ~ Height_clean * 100,
      Height_clean %in% c(16, 17)           ~ Height_clean * 10,
      Height_clean <= 50 | Height_clean >= 250 ~ NA_real_,
      TRUE ~ Height_clean
    ),
    
    Weight_clean = as.numeric(gsub("[^0-9.-]", "", as.character(`Weight (kg)`))),
    Weight_clean = abs(Weight_clean),
    Weight_clean = case_when(
      Weight_clean > 1000                      ~ Weight_clean / 1000,
      Weight_clean <= 20 | Weight_clean >= 300 ~ NA_real_,
      TRUE ~ Weight_clean
    )
  ) %>%
  
  # select final columns
  select(
    `Policy Number`,
    `Plan Code (Basic Plan)`,
    `Plan Code (Dread Disease)`,
    `Product Name`,
    Gender              = Gender_clean,
    `Date of Birth`     = DOB_realigned,
    `Policy Issue Date` = Issue_clean,
    `Entry Age`         = Entry_Age_clean,
    `Smoker Status`     = Smoker_clean,
    `Marital Status`    = Marital_clean,
    `Education Level`   = Education_clean,
    `Occupation Class`  = Occ_Class_clean,
    `Height (cm)`       = Height_clean,
    `Weight (kg)`       = Weight_clean,
    `Annual Income`,
    `Sum Assured`       = Sum_Assured_clean,
    `Annual Premium`,
    `Premium Frequency`,
    Region,
    `Policy Status`,
    `Status Date`       = Status_clean
  )

# sanity checks
cat("Original rows:", nrow(policy_raw), "\nCleaned rows:", nrow(policy_cleaned), "\n")
table(policy_cleaned$Gender, useNA = "always")   
table(policy_cleaned$`Smoker Status`, useNA = "always")
summary(policy_cleaned %>% select(`Height (cm)`, `Weight (kg)`, `Sum Assured`, `Entry Age`))

options(scipen = 999)

# export cleaned dataset
write.csv(policy_cleaned, "Policy_Data_Cleaned.csv", row.names = FALSE)