# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "openxlsx", 'conflicted', "survey", "fredr")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
invisible(sapply(packages, install_if_missing))

# Load the packages
library(tidyverse)
library(openxlsx)
library(conflicted)
library(survey)
library(fredr)

# Prefer dplyr over all other packages
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), base::as.numeric())

# Setting file paths and environment variables ----

# Set the FRED API Key
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Set the file path to the project-level file that you would like to create a household level version for
ahs_data_project_level_file_path <- "S:/BuildingProducts/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_2015_to_2023_project_level.xlsx"

# Set the file path for the location you would like to output a household level version to
output_file_path <- "S:/BuildingProducts/AHS Historical Data/Cleaned Household Level AHS Files/ahs_data_household_level_2015_to_2023.xlsx"

# Set the year to adjust spending to (using CPI-U)
inflation_adj_year <- 2023

# Reading in inflation data ---- 

# Read in annual CPI data for 1993-2023, and select the date/value
inflation <- fredr(series_id = 'CPIAUCSL', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2023-12-01')) %>%
  select(date, value) %>%
  mutate(date = year(date))

# Store the current CPI value in cpi_for_adjustment
cpi_for_adjustment <- inflation %>%
  filter(date == inflation_adj_year) %>%
  select(value)
cpi_for_adjustment <- cpi_for_adjustment$value

# Adjust the CPI to read as multipliers to 2023 dollar amounts (i.e. 1 for 2023, ~1.04 for 2022, etc.)
inflation <- inflation %>%
  mutate(rate = cpi_for_adjustment/value) %>%
  rename(year = date, `CPI (FRED ID: CPIAUCSL` = value)

# Reading in raw data ---- 

# Read in the AHS data
ahs_data_raw <- read.xlsx(ahs_data_project_level_file_path)

# Preliminary data analysis ----

# Create a recoded JOBTYPE variable (source: https://www.census.gov/data-tools/demo/codebook/ahs/ahsdict.html?s_year=2023%20National&s_availability=PUF&s_keyword=jobtype )
ahs_data <- ahs_data_raw %>%
  mutate(JOBTYPE_recode = case_when(
    JOBTYPE == '01' ~ 'Earthquake damage required extensive repairs to home',
    JOBTYPE == '02' ~ 'Tornado or hurricane damage required extensive repairs to home',
    JOBTYPE == '03' ~ 'Landslide damage required extensive repairs to home',
    JOBTYPE == '04' ~ 'Fire damage required extensive repairs to home',
    JOBTYPE == '05' ~ 'Flood damage required extensive repairs to home',
    JOBTYPE == '06' ~ 'Other natural disaster damage required extensive repairs to home',
    
    JOBTYPE == '07'  ~ 'Added bedroom onto home',
    JOBTYPE == '08'  ~ 'Added bathroom onto home',
    JOBTYPE == '09'  ~ 'Added recreation room onto home',
    JOBTYPE == '10' ~ 'Added kitchen onto home',
    JOBTYPE == '11' ~ 'Added other inside room onto home',
    JOBTYPE == '12' ~ 'Bathroom remodeled',
    JOBTYPE == '13' ~ 'Kitchen remodeled',
    JOBTYPE == '14' ~ 'Attached garage or carport added to home',
    JOBTYPE == '15' ~ 'Porch, deck, patio, or terrace added to home',
    JOBTYPE == '16' ~ 'Added or replaced roof over entire home',
    JOBTYPE == '17' ~ 'Added or replaced siding on home',
    JOBTYPE == '18' ~ 'Added or replaced doors or windows in home',
    JOBTYPE == '19' ~ 'Added or replaced chimney, stairs or other exterior addition',
    JOBTYPE == '20' ~ 'Added or replaced insulation in home',
    JOBTYPE == '21' ~ 'Added or replaced internal water pipes in home',
    JOBTYPE == '22' ~ 'Added or replaced plumbing fixtures in home',
    JOBTYPE == '23' ~ 'Added or replaced electrical wiring, fuse boxes, or breaker switches in home',
    JOBTYPE == '24' ~ 'Added or replaced security system in home',
    JOBTYPE == '25' ~ 'Added or replaced carpeting, flooring, paneling, or ceiling tiles',
    JOBTYPE == '26' ~ 'Added or replaced central air conditioning',
    JOBTYPE == '27' ~ 'Added or replaced built-in heating equipment',
    JOBTYPE == '28' ~ 'Added or replaced septic tank',
    JOBTYPE == '29' ~ 'Added or replaced water heater',
    JOBTYPE == '30' ~ 'Added or replaced built-in dishwasher or garbage disposal',
    JOBTYPE == '31' ~ 'Other major improvements or repairs inside home (up to three could be reported)',
    JOBTYPE == '32' ~ 'Added or replaced driveways or walkways',
    JOBTYPE == '33' ~ 'Added or replaced fencing or walls',
    JOBTYPE == '34' ~ 'Added or replaced swimming pool, tennis court, or other recreational structure',
    JOBTYPE == '35' ~ 'Added or replaced shed, detached garage, or other building',
    JOBTYPE == '36' ~ 'Added or replaced landscaping or sprinkler system',
    JOBTYPE == '37' ~ 'Other major improvements or repairs to lot or yard (up to three could be reported)',
    TRUE           ~ as.character(JOBTYPE)  # keeps original value for unmatched cases
  ))

ahs_data <- ahs_data %>%
  # Join the inflation data
  left_join(inflation, by = c('AHSYEAR' = 'year'))

ahs_data <- ahs_data %>%
  # create jobcost_real, an inflation adjusted version of JOBCOST
  mutate(jobcost_real = if_else(JOBCOST >= 0, JOBCOST * rate, NA_real_),
         HINCP_real = if_else(HINCP == -1, HINCP * rate, HINCP),
         MARKETVAL_real = if_else(MARKETVAL >= 0, MARKETVAL * rate, MARKETVAL),
         TOTBALAMT_real = if_else(TOTBALAMT >= 0, TOTBALAMT * rate, TOTBALAMT),
         TOTHCAMT_real = if_else(TOTHCAMT >= 0, TOTHCAMT * rate, TOTHCAMT),
         REMODAMT_real = if_else(REMODAMT >= 0, REMODAMT * rate, REMODAMT),
         MAINTAMT_real = if_else(MAINTAMT >= 0, MAINTAMT * rate, MAINTAMT),
         MORTAMT_real = if_else(MORTAMT == -1, MORTAMT * rate, MORTAMT)) %>%
  # Amend the JOBDIY variable to read 'Pro' or 'DIY'
  mutate(JOBDIY = if_else(JOBDIY == 'Not DIY', 'Pro', JOBDIY)) %>%
  mutate(HHAGE_binned = case_when(
    HHAGE <= 29 ~ 'under_30',
    HHAGE >= 30 & HHAGE <= 39 ~ '30s',
    HHAGE >= 40 & HHAGE <= 49 ~ '40s',
    HHAGE >= 50 & HHAGE <= 59 ~ '50s',
    HHAGE >= 60 & HHAGE <= 69 ~ '60s',
    HHAGE >= 70 & HHAGE <= 79 ~ '70s',
    HHAGE >= 80 ~ '80_plus',
    TRUE ~ NA
  )) %>%
  # Drop the inflation rate
  select(-c(rate, `CPI (FRED ID: CPIAUCSL`))

# Generate a dataframe of household-level demographics 
household_demographics <- ahs_data %>%
  # Group by all identifying variables
  group_by(AHSYEAR, CONTROL, DIVISION, CSA, BLD, CONDO, HOA, TENURE, YRBUILT, HOWBUY, VACANCY, VACANCY_RECLASS, VACINHER, VACINVEST,UNITSIZE, LOTSIZE, UNITFLOORS, TOTROOMS, BATHROOMS, BEDROOMS, 
           HHAGE, HHAGE_binned, HHRACE, HHSPAN, HHMOVE, DISHH, NUMCARE, NUMERRND, NUMHEAR, NUMMEMRY, NUMSEE, NUMWALK,
           MARKETVAL, MARKETVAL_real, HINCP, HINCP_real, TOTBALAMT, TOTBALAMT_real, MORTAMT, MORTAMT_real, TOTHCAMT, TOTHCAMT_real, REMODAMT, REMODAMT_real, MAINTAMT, MAINTAMT_real, 
           HMRACCESS, HMRENEFF, HMRSALE) %>%
  # And take the average WEIGHT (this file is project-level, so one household may represent 2+ rows in the file)
  summarize(WEIGHT = mean(WEIGHT)) %>%
  # Ungroup
  ungroup() %>%
  # Reorder the variables
  select(AHSYEAR, CONTROL, WEIGHT, everything())

ahs_data <- ahs_data %>%
  # filter for homeowners with a JOBCOST > 0 
  #filter(TENURE == 'Owned/Bought' & JOBCOST > 0 & JOBDIY != '-1') %>%
  filter(TENURE == 'Owned/Bought') %>%
  # Find the weighted JOBCOST
  mutate(jobcost_weighted = if_else(JOBCOST >= 0, jobcost_real * WEIGHT, NA_real_)) %>%
  # Create job_type_diy, a string containing the DIY status of the project (JOBDIY), and the recoded job description (jobtype_recode)
  mutate(job_type_diy = if_else(JOBDIY == -1, NA, paste0(JOBDIY, " - ", JOBTYPE_recode))) %>%
  # Reorder the variables
  select(CONTROL, WEIGHT, AHSYEAR, JOBFUNDS, job_type_diy, jobcost_real, jobcost_weighted)

ahs_data_final <- ahs_data %>%
  # Group by household (CONTROL), year, and the job description/DIY status string...
  group_by(CONTROL, AHSYEAR, job_type_diy) %>%
  # to find the total spending (real and real/weighted)
  summarize(jobcost_real = sum(jobcost_real, na.rm = T),
            jobcost_weighted = sum(jobcost_weighted, na.rm = T)) %>%
  ungroup() %>%
  # and arrange by the job description/DIY status string
  arrange(job_type_diy)


ahs_data_final <- ahs_data_final %>%
  mutate(jobcost_real = if_else(is.na(job_type_diy), 1, jobcost_real),
         jobcost_weighted = if_else(is.na(job_type_diy), 1, jobcost_weighted)) %>%
  mutate(job_type_diy = if_else(is.na(job_type_diy), 'No Projects', job_type_diy))

# Create weighted / unweighted versions ----

# Pivot the fata to a wide format, taking values from the weighted/inflation adjusted JOBCOST variable thgat was created in the previous section, and arrange by AHS Survey Year
ahs_data_final_weighted <- ahs_data_final %>%
  pivot_wider(names_from = job_type_diy, values_from = jobcost_weighted, id_cols = c(CONTROL, AHSYEAR)) %>%
  arrange(AHSYEAR)

# Join the household demographic information back to the weighted household level file, by CONTROL (a unique household identifier) and AHSYEAR, then reorder variables
ahs_data_final_weighted <- ahs_data_final_weighted %>%
  left_join(household_demographics, by = c('CONTROL', 'AHSYEAR')) %>%
  select(CONTROL, AHSYEAR, WEIGHT:HMRSALE, everything())

# Create Unweighted Household Spend and Weighted Household Spend by summing up all project-level spending variables, then reorder variables
ahs_data_final_weighted <- ahs_data_final_weighted %>%
  mutate(`Unweighted Household Spend` = rowSums(select(., starts_with('DIY'),  starts_with('Pro')), na.rm = TRUE),
         `Weighted Household Spend` = `Unweighted Household Spend` * WEIGHT) %>%
  select(CONTROL:HMRSALE, `Unweighted Household Spend`, `Weighted Household Spend`, everything())

# Pivot the fata to a wide format, taking values from the unweighted/inflation adjusted JOBCOST variable that was created in the previous section, and arrange by AHS Survey Year
ahs_data_final_unweighted <- ahs_data_final %>%
  pivot_wider(names_from = job_type_diy, values_from = jobcost_real, id_cols = c(CONTROL, AHSYEAR)) %>%
  arrange(AHSYEAR)

# Join the household demographic information back to the unweighted household level file, by CONTROL (a unique household identifier) and AHSYEAR, then reorder variables
ahs_data_final_unweighted <- ahs_data_final_unweighted %>%
  left_join(household_demographics, by = c('CONTROL', 'AHSYEAR')) %>%
  select(CONTROL, AHSYEAR, WEIGHT:HMRSALE, everything())

# Create Unweighted Household Spend and Weighted Household Spend by summing up all project-level spending variables, then reorder variables
ahs_data_final_unweighted <- ahs_data_final_unweighted %>%
  mutate(`Unweighted Household Spend` = rowSums(select(., starts_with('DIY'),  starts_with('Pro')), na.rm = TRUE),
         `Weighted Household Spend` = `Unweighted Household Spend` * WEIGHT) %>%
  select(CONTROL:HMRSALE, `Unweighted Household Spend`, `Weighted Household Spend`, everything())

# Output data -----

dataset_list <- list('Weighted' = ahs_data_final_weighted,
                     'Unweighted' = ahs_data_final_unweighted,
                     'Inflation Assumptions' = inflation)

write.xlsx(dataset_list, output_file_path)
