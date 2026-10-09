library(tidyverse)
library(lubridate)

# load datasets
policy <- read_csv("Policy_Data_Cleaned.csv")
claims_reconciled <- read_csv("Claims_Data_Final.csv")

# calculate exposure time in years
exposure_summary <- policy %>%
  mutate(
    Issue_Date = as.Date(`Policy Issue Date`),
    Status_Date = as.Date(`Status Date`),
    Exposure_Years = as.numeric(difftime(Status_Date, Issue_Date, units = "days")) / 365.25
  ) %>%
  filter(Exposure_Years > 0) %>% # remove negative or zero errors
  group_by(`Product Name`) %>%
  summarise(Total_Exposure_Years = sum(Exposure_Years, na.rm = TRUE))

# get claim counts and merge to calculate rates
incidence_rates <- claims_reconciled %>%
  group_by(`Product Name`) %>%
  summarise(Claim_Count = n()) %>%
  right_join(exposure_summary, by = "Product Name") %>%
  mutate(
    Claim_Count = replace_na(Claim_Count, 0), # handle 0 claims
    Incidence_Rate_per_1000 = (Claim_Count / Total_Exposure_Years) * 1000
  ) %>%
  arrange(desc(Incidence_Rate_per_1000))

print(incidence_rates)

# save output
write_csv(incidence_rates, "Incidence Rates.csv")