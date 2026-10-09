library(tidyverse)
library(lubridate)

# load data
claims_raw <- read_csv("Claims_Data_Cleaned.csv")
policy_raw <- read_csv("Policy_Data_Cleaned.csv")

# format dates
claims <- claims_raw %>%
  mutate(across(c(`Date of Birth`, `Date of Diagnosis`), as.Date))

policy <- policy_raw %>%
  mutate(across(c(`Date of Birth`, `Policy Issue Date`, `Status Date`), as.Date))

# join policy info and update demographics
claims_step1 <- claims %>%
  inner_join(
    policy %>% select(
      `Policy Number`,
      `Gender_policy` = `Gender`,
      `DOB_policy` = `Date of Birth`,
      `Policy Issue Date`,
      `Policy Status`,
      `Status Date`,
      `Sum Assured`,
      `Plan Code DD_policy` = `Plan Code (Dread Disease)`
    ),
    by = "Policy Number"
  ) %>%
  mutate(
    Gender = `Gender_policy`,
    `Date of Birth` = `DOB_policy`,
    `Age at Claim` = floor(as.numeric(difftime(`Date of Diagnosis`, `DOB_policy`, units = "days")) / 365.25)
  )

# apply business rules and exclusions
claims_filtered <- claims_step1 %>%
  filter(
    # VC20 death rule: diagnosis must be at least 14 days before death
    !(
      `Plan Code DD_policy` == "VC20" & 
      tolower(`Policy Status`) %in% c("died", "death") & 
      as.numeric(difftime(`Status Date`, `Date of Diagnosis`, units = "days")) < 14
    ),
    # age limit
    !(
      `Plan Code DD_policy` == "VC20" & 
      `Age at Claim` >= 75
    ),
    # 90-day waiting period
    as.numeric(difftime(`Date of Diagnosis`, `Policy Issue Date`, units = "days")) > 90,
    # diagnosis cannot be after policy lapsed
    !(
      tolower(`Policy Status`) == "lapsed" & 
      `Date of Diagnosis` > `Status Date`
    ),
    # LifeSecure (LS30) rider does NOT cover Early-stage CI
    !(
      `Plan Code DD_policy` == "LS30" & 
      `Claim Type` == "Early-stage"
    )
  )

# calculate max payout cap and adjust claim amounts
claims_final <- claims_filtered %>%
  mutate(
    Completed_Years = floor(as.numeric(difftime(`Date of Diagnosis`, `Policy Issue Date`, units = "days")) / 365.25),
    Max_Allowed_Claim = case_when(
      `Plan Code DD_policy` == "VC20" ~ `Sum Assured` * (1 + pmin(0.50, pmax(0, Completed_Years * 0.05))),
      TRUE                            ~ `Sum Assured`
    ),
    `Claim Amount` = pmin(`Claim Amount`, Max_Allowed_Claim)
  ) %>%
  select(
    `Claim Number`,
    `Policy Number`,
    `Plan Code (Dread Disease)`,
    `Product Name`,
    `Gender`,
    `Date of Birth`,
    `Date of Diagnosis`,
    `Age at Claim`,
    `Claim Cause / Condition`,
    `Claim Type`,
    `Claim Status`,
    `Claim Amount`
  )

# save output
write_csv(claims_final, "Claims_Data_Final.csv")