# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "utils", "openxlsx", "eply", 'plyr', 'conflicted')

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
library(utils)
library(openxlsx)
library(eply)
library(plyr)
library(conflicted)

# Prefer dplyr over all other packages
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::arrange, base::as.numeric())

# Setting file paths / reading in raw data ----

# This line must be run independently of all other lines, and will ask for the user to enter the file path to 1997-2023 data in the console below!
data_1997_2023_file_path <- "S:/BuildingProducts/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_1997_to_2023_project_level.xlsx"
output_file_path <- "S:/BuildingProducts/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_1995_to_2023_project_level.xlsx"

data_1995_file_path <- "S:/BuildingProducts/AHS Historical Data/Raw AHS Files/1995/ahs1995n.csv"

# Read in the 1995 data, 1995 is in a wide format
data_1995 <- read.csv(data_1995_file_path)

# Calculate the total number of Owner-Occupied households
households <- data_1995 %>%
  distinct(CONTROL, .keep_all = TRUE) %>%
  filter(TENURE %in% c("'1'")) %>%
  summarize(households = sum(WEIGHT))
households <- round(households$households)

# Read in variable lists ---- 

# Read in the Variable Matching sheet for Household variables
household_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                                sheet = 'Household')

# Filter for household variables relevant to 2023 (All non-NA values within the 'variable' column)
household_variables <- household_variables %>%
  filter(Year == 1995 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2023
household_variables <- household_variables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
household_variables <- household_variables[!household_variables %in% c("DEGREE", "MOVE(1 - 15)", "RAC")]


# Read in the Variable Matching sheet for Project variables
project_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                              sheet = 'Project')

# Filter for project variables relevant to 2023 (All non-NA values within the 'variable' column)
project_variables <- project_variables %>%
  filter(Year == 1995 & !is.na(Variable)) 

# Create a vector storing the relevant project variables for 2023
project_variables <- project_variables$Variable
# Remove 'RAY(1 - 16)' from project_variables as it is not accessible as of July, 2023
project_variables <- project_variables[!project_variables %in% c("RAY(1 - 16)")]

# Prep 1995 data ----

# 1995 data is in a wide format, and features 16 unique columns (for each of the RAS, RAH, & RAD variables) for individual jobs. 
## i.e. each household contains columns 'RAS1', 'RAS2', 'RAS3', etc...'RAH1', 'RAH2', 'RAH3', etc...'RAD1', 'RAD2', 'RAD3', etc.
# Search for any columns that start with 'RAS', store them in 'selected_columns'
selected_columns <- grep("^RAS", names(data_1995), value = TRUE)
# Search for any columns that start with 'RAH', store them in 'selected_columns2'
selected_columns2 <- grep("^RAH", names(data_1995), value = TRUE)
# Search for any columns that start with 'RAD', store them in 'selected_columns3'
selected_columns3 <- grep("^RAD", names(data_1995), value = TRUE)

# Create variables_1995, storing all relevant 1995 variables (those within the Variable Matching Sheet + all RAS/RAH/RAD columns)
variables_1995 <- c(household_variables, selected_columns, selected_columns2, selected_columns3)

# Remove the selected_columns datasets, these are all in Variable_1995 now
rm(selected_columns2, selected_columns3)

# Create 'data_1995_selected', selecting the variables of interest for 1995 Data
data_1995_selected <- data_1995[,variables_1995]
# Replace any "' '" entries (i.e. blank quotes) to NAs
data_1995_selected <- replace(data_1995_selected[,c(1:ncol(data_1995_selected))], data_1995_selected[,c(1:ncol(data_1995_selected))] == "' '", NA)

# Pivot the Selected Dataset to a long format for the RAS (JOBTYPE) variable
## Each Houshold has 16 'RAS' field, thus 16 rows for each Household will be created. Only RAS entries that are not NA will be relevant!
# long_format_temp <- data_1995_selected %>%
#   pivot_longer(cols = c(18:29), names_to = 'RASCount', values_to = 'RAS')
long_format_temp <- data_1995_selected %>%
  pivot_longer(cols = starts_with("RAS"), names_to = 'RASCount', values_to = 'RAS')

# Filter for RAS entries that are not NA...Non NA entries represent jobs
long_format_temp <- long_format_temp %>%
  filter(!is.na(RAS))

# Pivot the dataset to a long format again, now using the 'RAH' (JOBDIY) variable. Only RAH entries that are not NA will be relevant!
long_format_temp <- long_format_temp %>%
  pivot_longer(cols = starts_with("RAH"), names_to = 'RAHCount', values_to = 'RAH')

# Filter for RAH entries that are not NA...Non NA entries represent jobs
long_format_temp <- long_format_temp %>%
  filter(!is.na(RAH))

# Amend the 'RASCount' variable to a numeric value
long_format_temp <- long_format_temp %>%
  mutate(RASCount = case_when(RASCount == 'RAS1' ~ 1,
                              RASCount == 'RAS2' ~ 2,
                              RASCount == 'RAS3' ~ 3,
                              RASCount == 'RAS4' ~ 4,
                              RASCount == 'RAS5' ~ 5,
                              RASCount == 'RAS6' ~ 6,
                              RASCount == 'RAS7' ~ 7,
                              RASCount == 'RAS8' ~ 8,
                              RASCount == 'RAS9' ~ 9,
                              RASCount == 'RAS10' ~ 10,
                              RASCount == 'RAS11' ~ 11,
                              RASCount == 'RAS12' ~ 12,
                              RASCount == 'RAS13' ~ 13,
                              RASCount == 'RAS14' ~ 14,
                              RASCount == 'RAS15' ~ 15,
                              RASCount == 'RAS16' ~ 16,
                              is.na(RASCount) ~ NA))

# Amend the 'RAHCount' variable to a numeric value
long_format_temp <- long_format_temp %>%         
         mutate(RAHCount = case_when(RAHCount == 'RAH1' ~ 1,
                              RAHCount == 'RAH2' ~ 2,
                              RAHCount == 'RAH3' ~ 3,
                              RAHCount == 'RAH4' ~ 4,
                              RAHCount == 'RAH5' ~ 5,
                              RAHCount == 'RAH6' ~ 6,
                              RAHCount == 'RAH7' ~ 7,
                              RAHCount == 'RAH8' ~ 8,
                              RAHCount == 'RAH9' ~ 9,
                              RAHCount == 'RAH10' ~ 10,
                              RAHCount == 'RAH11' ~ 11,
                              RAHCount == 'RAH12' ~ 12,
                              RAHCount == 'RAH13' ~ 13,
                              RAHCount == 'RAH14' ~ 14,
                              RAHCount == 'RAH15' ~ 15,
                              RAHCount == 'RAH16' ~ 16,
                              is.na(RAHCount) ~ NA))

# Filter the long dataset for only rows in which RASCount matches RAHCount. This assures that jobs receive the correct 'DIY' variables (if present)
long_format_temp <- long_format_temp %>%
  filter(RAHCount == RASCount)

# Pivot the dataset to a long format again, now using the 'RAD' (JOBCOST) variable
long_format_temp <- long_format_temp %>%
  pivot_longer(cols = starts_with("RAD"), names_to = 'RADCount', values_to = 'RAD')

# Amend the 'RADCount' variable to a numeric value
long_format_temp <- long_format_temp %>%         
  mutate(RADCount = case_when(RADCount == 'RAD1' ~ 1,
                              RADCount == 'RAD2' ~ 2,
                              RADCount == 'RAD3' ~ 3,
                              RADCount == 'RAD4' ~ 4,
                              RADCount == 'RAD5' ~ 5,
                              RADCount == 'RAD6' ~ 6,
                              RADCount == 'RAD7' ~ 7,
                              RADCount == 'RAD8' ~ 8,
                              RADCount == 'RAD9' ~ 9,
                              RADCount == 'RAD10' ~ 10,
                              RADCount == 'RAD11' ~ 11,
                              RADCount == 'RAD12' ~ 12,
                              RADCount == 'RAD13' ~ 13,
                              RADCount == 'RAD14' ~ 14,
                              RADCount == 'RAD15' ~ 15,
                              RADCount == 'RAD16' ~ 16,
                              is.na(RADCount) ~ NA))

# Filter the long dataset for only rows in which RASCount matches RADCount. This assures that jobs receive the correct 'JOBCOST' variable
data_1995 <- long_format_temp %>%
  # Assure that we are linking jobs to the correct cost
  filter(RASCount == RADCount) %>%
  # Drop the 'Count' variables...Those were only needed for within-household job-matching
  select(-c(RASCount, RADCount, RAHCount))

# Find the number of 'doers'
doers <- as.numeric(length(unique(data_1995$CONTROL)))

# Clean the yearly data ----

# Custom function for creating Job Cateogory classes
reclassify <- function(data) {
  
  data <- data %>%
    
    # Unquote all variables
    mutate_all(.funs = unquote) %>%
    
    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(RAS == '01' ~ 'DisRepairs',
                                   RAS %in% c('02', '03', '04', '05', '06', '07', '08', '09', '10', '35', '36', '73') ~ 'RoomAdd',
                                   RAS %in% c('16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '71') ~ 'Bathroom',
                                   RAS %in% c('26', '27', '28', '29', '30', '31', '32', '33', '34', '72') ~ 'Kitchen',
                                   RAS %in% c('11', '12', '13', '14', '67') ~ 'OutsideAtt',
                                   RAS %in% c('15', '37', '38', '39', '45', '46') ~ 'Exterior',
                                   RAS %in% c('49', '50', '51', '52', '53', '54', '55', '56', '64') ~ 'Interior',
                                   RAS %in% c('40', '41', '42', '43', '44', '47', '48', '57', '58', '59', '61', '62', '63', '74') ~ 'Systems',
                                   RAS %in% c('60', '65', '66', '68', '69', '70') ~ 'LotYardOther')) %>%
    
    # Create a 'jobtype_recode' column for each job type
    mutate(jobtype_recode = case_when(RAS == '01' ~ 'disaster',
                                      RAS %in% c('03', '05', '06', '09', '10', '35', '36') ~ 'room_addition',
                                      RAS %in% c('02', '04', '07', '08') ~ 'room_addition_k&b',
                                      RAS %in% c('16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34') ~ 'k&b',
                                      RAS %in% c('11', '14', '65', '69') ~ 'garage_driveway_shed',
                                      RAS %in% c('12', '13', '67') ~ 'decking_porch',
                                      RAS == '37' ~ 'roofing',
                                      RAS %in% c('38', '39') ~ 'siding',
                                      RAS %in% c('45', '46') ~ 'windows_doors',
                                      RAS == '15' ~ 'other_exterior_improvements',
                                      RAS %in% c('49', '50') ~ 'insulation',
                                      RAS %in% c('40', '41', '60') ~ 'water_pipes_septic',
                                      RAS %in% c('47', '48') ~ 'plumbing_fixtures',
                                      RAS %in% c('42', '43', '44') ~ 'electrical_security',
                                      RAS %in% c('51', '52', '53', '54', '55', '56')  ~ 'flooring',
                                      RAS %in% c('57', '58', '59') ~ 'hvac',
                                      RAS == '61' ~ 'water_heaters',
                                      RAS %in% c('62', '63') ~ 'dishwashers',
                                      RAS == '64' ~ 'other_interior_improvements',
                                      RAS %in% c('66', '68', '70') ~ 'other_outdoor_improvements')) %>%
    
    
    # Create a 'CSA' variable
    mutate(CSA = case_when(SMSA == "0520" ~ 'ATL',
                           SMSA == "0720" ~ 'BAL',
                           SMSA == "1000" ~ 'BIR',
                           SMSA == "1120" ~ 'BOS',
                           SMSA %in% c("1600", "9991") ~ 'CHI',
                           SMSA == "1640" ~ 'CIN',
                           SMSA == "1680" ~ 'CLE',
                           SMSA == "1920" ~ 'DAL',
                           SMSA == "2080" ~ 'DEN',
                           SMSA == "2160" ~ 'DET',
                           SMSA == "3360" ~ 'HOU',
                           SMSA == "3760" ~ 'KC',
                           SMSA == "4120" ~ 'LV',
                           SMSA == "4480" ~ 'LA',
                           SMSA == "4920" ~ 'MEM',
                           SMSA == "5000" ~ 'MIA',
                           SMSA == "5080" ~ 'MIL',
                           SMSA == "5120" ~ 'MIN',
                           SMSA == "5560" ~ 'NO',
                           SMSA %in% c("5600", "5640","9992", "9993") ~ 'NYC',
                           SMSA == "5880" ~ 'OKC',
                           SMSA == "6160" ~ 'PHI',
                           SMSA == "6200" ~ 'PHX',
                           SMSA == "6280" ~ 'PIT',
                           #SMSA == "38900" ~ 'POR',
                           SMSA == "6640" ~ 'RAL',
                           #SMSA == "40060" ~ 'RIC',
                           SMSA == "6780" ~ 'RIV',
                           SMSA == "6840" ~ 'ROC',
                           SMSA == "7240" ~ 'SA',
                           SMSA == "7360" ~ 'SF',
                           SMSA == "7400" ~ 'SJ',
                           SMSA == "7600" ~ 'SEA',
                           SMSA == "8280" ~ 'TAM',
                           SMSA == "8840" ~ 'DC',
                           SMSA == "9999" ~ 'Not in Metro')) %>%
    
    # Edit the CSA variable to read 'Others' for those in 2011-2013 but not in 2015-2023
    mutate(CSA = ifelse(is.na(CSA), 'Others', CSA)) %>%
    
    # Amend the ZINC2 variable (-6 here is NA)
    # ZINC2 = HINCP (2023)
    mutate(ZINC2 = ifelse(ZINC2 == '-6', NA, ZINC2)) %>%
    
    #ZSMHC does not need to be amended...No NA or NR values
    
    # Amend the CSTMNT variable (-6 here is NA, -9 & . here are 'Not Reported')
    # CSTMNT = MAINTAMT (2023)
    mutate(CSTMNT = case_when(CSTMNT == '-6' ~ NA,
                              CSTMNT %in% c('-9', '.') ~ 'NR',
                              !CSTMNT %in% c('-6', '-9', '.') ~ CSTMNT)) %>%
    
    # Amend the VALUE variable (-6 here is NA, -9 here is 'Not Reported')
    # VALUE = MARKETVAL (2023)
    mutate(VALUE = case_when(VALUE == '-6' ~ NA,
                             VALUE == '-9' ~ 'NR',
                             !VALUE %in% c('-6', '-9') ~ VALUE)) %>%
    
    # Amend the TENURE variable (-6 here is NA)
    mutate(TENURE = case_when(TENURE == '1' ~ 'Owned/Bought',
                              TENURE == '2' ~ 'Rented',
                              TENURE == '3' ~ 'Occupied Without Rent',
                              TENURE == '-6' ~ NA)) %>%
    
    # Amend the UNITSF variable (-9 here is 'Not Reported')
    # UNITSF = UNITSIZE (2023)
    mutate(UNITSF = as.numeric(UNITSF),
           UNITSF = case_when(UNITSF == 9998 ~ 'NR',
                              UNITSF < 500 ~ '< 500 sq. ft.',
                              UNITSF >= 500 & UNITSF < 750 ~ '500 - 749 sq. ft.',
                              UNITSF >= 750 & UNITSF < 1000 ~ '750 - 999 sq. ft.',
                              UNITSF >= 1000 & UNITSF < 1500 ~ '1,000 - 1,499 sq. ft.',
                              UNITSF >= 1500 & UNITSF < 2000 ~ '1,500 - 1,999 sq. ft.',
                              UNITSF >= 2000 & UNITSF < 2500 ~ '2,000 - 2,499 sq. ft.',
                              UNITSF >= 2500 & UNITSF < 3000 ~ '2,500 - 2,999 sq. ft.',
                              UNITSF >= 3000 & UNITSF < 4000 ~ '3,000 - 3,999 sq. ft.',
                              UNITSF >= 4000 ~ '>= 4,000 sq. ft.')) %>%
    
    # Amend the LOT variable (-6 here is NA)
    # LOT = LOTSIZE (2023)
    ## LOT is reported in SF through 2013. LOTSIZE is reported in buckets of acre-based size. 1 acre = 43,650 SF.
    mutate(LOT = as.numeric(LOT),
           LOT = case_when(LOT == 999998 ~ 'NR',
                           LOT/43560 < .125 ~ '< 1/8 acre',
                           LOT/43560 >= .125 & LOT/43560 < .25 ~ '1/8 - 1/4 acre',
                           LOT/43560 >= .25 & LOT/43560 < .5 ~ '1/4 - 1/2 acre.',
                           LOT/43560 >= .5 & LOT/43560 < 1 ~ '1/2 - 1 acre',
                           LOT/43560 >= 1 & LOT/43560 < 5 ~ '1 - 5 acres',
                           LOT/43560 >= 5 & LOT/43560 < 10 ~ '5 - 10 acres',
                           LOT/43560 >= 10 ~ '>= 10 acres')) %>%

    # Amend the NUNIT2 variable
    # NUNIT2 = BLD (2023)
    mutate(NUNIT2 = case_when(NUNIT2 == '1' ~ 'SF Detached',
                              NUNIT2 == '2' ~ 'SF Attached',
                              NUNIT2 == '3' ~ '2+ Apartments',
                              NUNIT2 %in% c('4', '5') ~ 'Manufactured/Mobile')) %>%
    
    # Amend the ISTATUS variable
    # ISTATUS = INTSTATUS (2023)
    mutate(ISTATUS = case_when(ISTATUS == '1' ~ 'Occupied',
                               ISTATUS == '2' ~ 'URE',
                               ISTATUS == '3' ~ 'Vacant')) %>%
    
    # Create the VACANCY_RECLASS variable, grouping into 'Year Round Vacant', 'Seasonal', & 'Occupied' (-6 here is NA)
    mutate(VACANCY_RECLASS = case_when(VACANCY %in% c('01', '02', '03', '04', '05') ~ 'Year Round Vacant',
                                       VACANCY %in% c('06', '08', '09', '10', '11') ~ 'Seasonal',
                                       VACANCY == '-6' & ISTATUS == 'Occupied' ~ 'Occupied',
                                       VACANCY == '-6' & ISTATUS != 'Occupied' ~ NA)) %>%
    
    # Amend the VACANCY variable (-6 here is NA)
    mutate(VACANCY = case_when(VACANCY %in% c('01', '04') ~ 'For Rent or Rented',
                               VACANCY == '02' ~ 'Rent or Sale',
                               VACANCY %in% c('03', '05') ~ 'For Sale or Sold',
                               VACANCY == '06' ~ 'Occasional Use',
                               VACANCY == '07' ~ 'Other',
                               VACANCY == '08' ~ 'Seasonal, Summer Only',
                               VACANCY == '09' ~ 'Seasonal, Winter Only',
                               VACANCY == '10' ~ 'Seasonal, Other',
                               VACANCY == '11' ~ 'Migratory',
                               VACANCY == '-6' ~ NA)) %>%
    
    # Amend the RAH variable (-6 here is NA, -9 here is 'Not Reported')
    # RAH = JOBDIY (2023)
    mutate(RAH = case_when(RAH == '1' ~ 'DIY',
                           RAH == '2' ~ 'Not DIY',
                           RAH == '-9' ~ 'NR',
                           RAH == '-6' ~ NA)) %>%
    
    # Amend the RAD variable (99999 & -6 here are NA, 99998 & -9 here are 'Not Reported')
    # RAD = JOBCOST (2023)
    mutate(RAD = case_when(RAD %in% c('99999', '-6') ~ NA,
                         RAD %in%  c('99998', '-9') ~ 'NR',
                         !RAD %in% c('-6', '-9', '99999', '99998') ~ RAD))  %>%
    
    # Rename variables to match 2015-2023
    rename(BLD = NUNIT2, HINCP = ZINC2, UNITSIZE = UNITSF, LOTSIZE = LOT, INTSTATUS = ISTATUS, 
           MAINTAMT = CSTMNT, TOTHCAMT = ZSMHC, MARKETVAL = VALUE, OMB13CBSA = SMSA, YRBUILT = BUILT, 
           JOBTYPE = RAS, JOBDIY = RAH, JOBCOST = RAD)
}

# Read in the 1997-2023 data and store as data_1997_2023
data_1997_2023 <- read.xlsx(data_1997_2023_file_path)

# Finalize the 1995 data by using the custom function above
data_1995 <- reclassify(data_1995) %>%
  mutate(AHSYEAR = 1995)

# Function to replace 'NR' with -1 in a vector
replace_non_response_values_1 <- function(x) {
  ifelse(x == 'NR', -1, x)
}

# Function to apply the replacement to all columns in a data frame
replace_non_response_values_2 <- function(data) {
  data %>% mutate(across(everything(), replace_non_response_values_1))
}

# Apply ReplaceNR_Final() to the 1995 data, this will replace any 'NR' values with -1
data_1995 <- replace_non_response_values_2(data_1995)

data_1995 <- data_1995 %>%
  # Convert numeric columns to numeric values
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         #HHAGE = as.numeric(HHAGE),
         #HHMOVE = as.numeric(HHMOVE),
         #REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST),
         # Added the following on 12.28, make sure they join with old columns
         MAINTAMT = as.numeric(MAINTAMT),
         #TOTBALAMT = as.numeric(TOTBALAMT),
         TOTHCAMT = as.numeric(TOTHCAMT))

#Read in column names from 1997-2023 cleaned data
col_order_check <- names(data_1997_2023)
#Specify the column order for 1995
col_order <- col_order_check[col_order_check %in% names(data_1995)]
#Find columns missing from 1995 though present in 2015-2023
missing_cols <- col_order_check[!col_order_check %in% col_order]

# Reorder 1995 data to match the column order in 2015-2023
data_1995 <- data_1995[col_order]

# Bind the 1995 data to the 1997-2023 data. rbind.fill() fills all columns from 1995 that are present in 1997-2023 (binding them as new rows)
# 1995 data will have NA values for all of the 'MissingCols'
data_1995_2023 <- data_1997_2023 %>%
  rbind.fill(data_1995) 

# Order the 1995-2023 data by year
data_1995_2023 <- data_1995_2023[order(data_1995_2023$AHSYEAR), ]

# Amend the BLD variable to match the classes in 2001-2023...1995-1999 BLD data uses 'Manufactured/Mobile' rather than 'Mobile' (as used in 2001-2023)
data_1995_2023 <- data_1995_2023 %>%
  mutate(BLD = case_when(BLD == 'Manufactured/Mobile' ~ 'Mobile',
                         BLD != 'Manufactured/Mobile' ~ BLD))

# Create a 'LogicCheck' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998' 
# These are all typical NA classifications for AHS variables, if LogicCheck is empty after running the following line, you are good to go! 
# If it is not empty, check the rows for potentially missed NA values.
logic_check <- data_1995_2023 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998')))

# Output data ----

# Output the dataset to the specified file path
write.xlsx(data_1995_2023, output_file_path)
