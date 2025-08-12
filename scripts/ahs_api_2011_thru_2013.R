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
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), base::as.numeric())

# Setting file paths / reading in raw data ----

# Set the file path to 2015-2021 data
data_2015_2023_file_path <- "C:/Users/ianwe/Downloads/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_2015_to_2023_project_level.xlsx"
output_file_path <- "C:/Users/ianwe/Downloads/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_2011_to_2023_project_level.xlsx"

# Set the file path to the folder that contains all yearly AHS data folders you are interested in
folder_location <- "C:/Users/ianwe/Downloads/AHS Historical Data/Raw AHS Files/11_13"
folder_location

# Before running any further code, make sure the file folders are located in the directory printed in the console below
# Each year should have its own folder (i.e. '1997', '1999', etc.)

# Define the 'files_to_pull'. If needing to include additional files, paste them at the end of 'files_to_pull' as follows:
## Example for all files: files_to_pull <- c('household.csv', 'project.csv', 'mortgage.csv', 'people.csv)
## Make sure to uncomment portions of the for-in loop below to read in mortgage/person data!
files_to_pull <- c('household.csv', 'project.csv')

# Define the years of survey coverage for files that will be read in. 1995 is omitted here given it does not have a project/household file
years <- as.character(c(2011, 2013))

# Create a vector of folder locations for each survey year, this vector should contain file paths to the Yearly Unzipped File Folders (one for each year)
year_folders <- paste0(folder_location, '/', years)

# For each folder year
for (i in 1:length(year_folders)) {

  # Assign the variable 'household_{YEAR}' for each household csv
  assign(paste0("household_", years[i]),
         value = read.csv(paste0(year_folders[i], '/' , files_to_pull[1])))

  # Assign the variable 'project_{YEAR}' for each household csv
  assign(paste0("project_", years[i]),
         value = read.csv(paste0(year_folders[i], '/' , files_to_pull[2])))

  # # Assign the variable 'mortgage_{YEAR}' for each mortgage csv
  # assign(paste0("mortgage_", years[i]),
  #        value = read.csv(paste0(year_folders[i], '/' , files_to_pull[3])))
  #
  # # Assign the variable 'person_{YEAR}' for each person csv
  # assign(paste0("person_", years[i]),
  #        value = read.csv(paste0(year_folders[i], '/' , files_to_pull[4])))

}

# Rename the TAXINC variable to TAXCRD to match 2011
household_2013 <- household_2013 %>%
  rename(TAXCRD = TAXINC)

# Remove unnecessary objects
rm(year_folders, folder_location, files_to_pull, i)

# Read in variable lists ---- 

# Read in the variable matching sheet for household variables
household_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                                sheet = 'Household')

# Filter for household variables relevant to 2011-2013 (All non-NA values within the 'variable' column)
household_variables <- household_variables %>%
  filter(Year == 2011 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2021
household_variables <- household_variables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
household_variables <- household_variables[household_variables != "DEGREE"]

# Read in the variable matching sheet for project variables
project_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                              sheet = 'Project')

# Filter for project variables relevant to 2021 (All non-NA values within the 'variable' column)
project_variables <- project_variables %>%
  filter(Year == 2011 & !is.na(Variable))

# Create a vector storing the relevant project variables for 2021
project_variables <- project_variables$Variable

# Join Household and Project data ----

# Create a list of all 2011-2013 household datasets
household_list <- list(household_2011, household_2013)
# Create a list of all 2011-2013 project datasets
project_list <- list(project_2011, project_2013)

# Function to select columns and retain names
select_columns <- function(data, columns) {
  selected_data <- data[, columns, drop = FALSE]
  return(selected_data)
}

# Apply the function to each dataset
selected_datasets <- lapply(project_list, select_columns, columns = project_variables)

# For each folder year
for (i in 1:length(years)) {
  # Assign the variable 'project_{YEAR}' for each amended 'project' dataset
  assign(paste0("project_", years[i]),
         value = as.data.frame(selected_datasets[i]))
}

# Remove 'selected_datasets', this variable will be used again below
rm(selected_datasets)

# Select the appropriate columns from the housing data
selected_datasets <- lapply(household_list, select_columns, columns = household_variables)

# For each folder year
for (i in 1:length(years)) {
  # Assign the variable 'household_{YEAR}' for each amended 'household' dataset
  assign(paste0("household_", years[i]),
         value = as.data.frame(selected_datasets[i]))
}

# Finalize the 2011-2013 data by joining amended household/project datasets
data_2011 <- household_2011 %>%
  left_join(project_2011, by = 'CONTROL')
data_2013 <- household_2013 %>%
  left_join(project_2013, by = 'CONTROL')

# Remove all 2011-2013 household/project datasets, all this data now lies in the 'data_[YEAR] datasets
rm(household_2011, household_2013, project_2011, project_2013, project_list, household_list, selected_datasets,
   i, select_columns, years, household_variables, project_variables)

# Clean the yearly (joined) data ----

# Custom function for reclassifying variables
reclassify <- function(data) {

  data <- data %>%

    # Unquote all variables
    mutate_all(.funs = unquote) %>%

    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(RAS == '01' ~ 'DisRepairs',
                                   RAS %in% c('02', '03', '05', '06', '07', '08', '09', '10', '35', '36', '73') ~ 'RoomAdd',
                                   RAS == '71' ~ 'Bathroom',
                                   RAS =='72' ~ 'Kitchen',
                                   RAS %in% c('11', '12', '13', '14', '67') ~ 'OutsideAtt',
                                   RAS %in% c('15', '37', '38', '45') ~ 'Exterior',
                                   RAS %in% c('49', '52', '53', '55', '64') ~ 'Interior',
                                   RAS %in% c('40', '42', '47', '57', '58', '61', '62', '63', '74') ~ 'Systems',
                                   RAS %in% c('60', '65', '66', '68', '69', '70') ~ 'LotYardOther')) %>%
    
    # Create a 'jobtype_recode' column for each job type
    mutate(jobtype_recode = case_when(RAS == '01' ~ 'disaster',
                                      RAS %in% c('03', '05', '06', '09', '10', '35', '36') ~ 'room_addition',
                                      RAS %in% c('02', '07', '73') ~ 'bathroom_addition',
                                      RAS == '08' ~ 'kitchen_addition',
                                      RAS == '71' ~ 'bathroom_remodel',
                                      RAS == '72' ~ 'kitchen_remodel',
                                      RAS %in% c('11', '14', '65', '69') ~ 'garage_driveway_shed',
                                      RAS %in% c('12', '13', '67') ~ 'decking_porch',
                                      RAS == '37' ~ 'roofing',
                                      RAS == '38' ~ 'siding',
                                      RAS == '45' ~ 'windows_doors',
                                      RAS == '15' ~ 'other_exterior_improvements',
                                      RAS == '49' ~ 'insulation',
                                      RAS %in% c('40', '60') ~ 'water_pipes_septic',
                                      RAS == '47' ~ 'plumbing_fixtures',
                                      RAS %in% c('42', '74') ~ 'electrical_security',
                                      RAS %in% c('52', '53', '55')  ~ 'flooring',
                                      RAS %in% c('57', '58') ~ 'hvac',
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
                           SMSA %in% c("5600", "9992", "9993") ~ 'NYC',
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

    # Edit the CSA variable to read 'Others' for those in 2011-2013 but not in 2015-2021
    mutate(CSA = ifelse(is.na(CSA), 'Others', CSA)) %>%

    # Amend the DIVISION variable
    mutate(DIVISION = case_when(DIVISION == "01" ~ 'New England',
                                DIVISION == "02" ~ 'Mid Atlantic',
                                DIVISION == "03" ~ 'East North Central',
                                DIVISION == "04" ~ 'West North Central',
                                DIVISION == "56" ~ 'South Atlantic/East South Central',
                                DIVISION == "07" ~ 'West South Central',
                                DIVISION == "89" ~ 'Mountain/Pacific')) %>%

    # Amend the CUSHOM variable
    # CUSHOM = HOWBUY (2021)
    mutate(CUSHOM = case_when(CUSHOM == "1" ~ 'Bought house already built',
                              CUSHOM == "2" ~ 'Signed sales agreement that included land/cost of building unit',
                              CUSHOM == "3" ~ 'General contractor built unit on own land (includes leased land)',
                              CUSHOM == "4" ~ 'Built house themselves on own land',
                              CUSHOM == "5" ~ 'Received as gift/inheritance',
                              CUSHOM == '-9' ~ 'NR',
                              CUSHOM == '-6' ~ NA)) %>%

    # Amend the ZINC2 variable (-6 here is NA, -9 here is 'Not Reported')
    # ZINC2 = HINCP (2021)
    mutate(ZINC2 = case_when(ZINC2 %in% c('99999', '-6') ~ NA,
                             ZINC2 %in%  c('99998', '-9') ~ 'NR',
                             !ZINC2 %in% c('-6', '-9', '99999', '99998') ~ ZINC2)) %>%

    # Amend the ZSMHC variable (-6 here is NA)
    # ZSMHC = TOTHCAMT (2021)
    mutate(ZSMHC = ifelse(ZSMHC == '-6', NA, ZSMHC)) %>%

    # Amend the CSTMNT variable (-6 here is NA, -9 here is 'Not Reported')
    # CSTMNT = MAINTAMT (2021)
    mutate(CSTMNT = case_when(CSTMNT == c('-6', '.') ~ NA,
                              CSTMNT == '-9' ~ 'NR',
                              !CSTMNT %in% c('-6', '-9', '.') ~ CSTMNT)) %>%

    # Amend the VALUE variable (-6 here is NA, -9 here is 'Not Reported')
    # VALUE = MARKETVAL (2021)
    mutate(VALUE = case_when(VALUE %in% c('-6', '.') ~ NA,
                              VALUE == '-9' ~ 'NR',
                              !VALUE %in% c('-6', '-9', '.') ~ VALUE)) %>%

    # Amend the HHAGE variable (-6 here is NA)
    mutate(HHAGE = ifelse(HHAGE == '-6', NA, HHAGE)) %>%

    # Amend the TENURE variable (-6 here is NA)
    mutate(TENURE = case_when(TENURE == '1' ~ 'Owned/Bought',
                              TENURE == '2' ~ 'Rented',
                              TENURE == '3' ~ 'Occupied Without Rent',
                              TENURE == '-6' ~ NA)) %>%

    # Amend the NUNIT2 variable
    # NUNIT2 = BLD (2021)
    mutate(NUNIT2 = case_when(NUNIT2 == '1' ~ 'SF Detached',
                              NUNIT2 == '2' ~ 'SF Attached',
                              NUNIT2 == '3' ~ '2+ Apartments',
                              NUNIT2 == '4' ~ 'Manufactured/Mobile')) %>%

    # Amend the UNITSF variable (-9 here is 'Not Reported')
    # UNITSF = UNITSIZE (2021)
    mutate(UNITSF = case_when(UNITSF == -9 ~ 'NR',
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
    # LOT = LOTSIZE (2021)
    ## LOT is reported in SF through 2013. LOTSIZE is reported in buckets of acre-based size. 1 acre = 43,650 SF.
    mutate(LOT = as.numeric(LOT),
           LOT = case_when(LOT == '-6' ~ NA,
                           LOT/43560 < .125 ~ '< 1/8 acre',
                           LOT/43560 >= .125 & LOT/43560 < .25 ~ '1/8 - 1/4 acre',
                           LOT/43560 >= .25 & LOT/43560 < .5 ~ '1/4 - 1/2 acre.',
                           LOT/43560 >= .5 & LOT/43560 < 1 ~ '1/2 - 1 acre',
                           LOT/43560 >= 1 & LOT/43560 < 5 ~ '1 - 5 acres',
                           LOT/43560 >= 5 & LOT/43560 < 10 ~ '5 - 10 acres',
                           LOT/43560 >= 10 ~ '>= 10 acres')) %>%

    # Amend the ISTATUS variable
    # ISTATUS = INTSTATUS (2021)
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

    # Amend the RAH variable
    # RAH = JOBDIY (2021)
    mutate(RAH = case_when(RAH == '1' ~ 'DIY',
                           RAH == '2' ~ 'Not DIY')) %>%

    # Amend the HHMOVE variable (-6 here is NA)
    mutate(HHMOVE = ifelse(HHMOVE == '-6', NA, HHMOVE)) %>%

    # Amend the RAD variable (-9 here is 'Not Reported')
    # RAD = JOBCOST (2021)
    mutate(RAD = ifelse(RAD == '-9', 'NR', RAD)) %>%

    # Create the HMRENEFF variable from ENEFIC (-6 here is NA, -9 here is 'Not Reported')
    mutate(ENEFIC = case_when(ENEFIC == '1' ~ 'Yes',
                                ENEFIC == '2' ~ 'No',
                                ENEFIC == '-6' ~ NA,
                                ENEFIC == '-9' ~ 'NR',
                                ENEFIC == ' ' ~ NA)) %>%

    # Amend the SINHV variable (-6 here is NA, -9 here is 'Not Reported')
    # SINHV = VACINHER (2021)
    mutate(SINHV = case_when(SINHV == '1' ~ 'Yes',
                             SINHV == '2' ~ 'No',
                             SINHV == '-9' ~ 'NR',
                             SINHV == '-6' ~ NA)) %>%

    # Amend the SINVV variable (-6 here is NA, -9 here is 'Not Reported')
    # SINVV = VACINVEST (2021)
    mutate(SINVV = case_when(SINVV == '1' ~ 'Yes',
                             SINVV == '2' ~ 'No',
                             SINVV == '-9' ~ 'NR',
                             SINVV == '-6' ~ NA)) %>%

    # Rename variables to match 2015-2021
    rename(BLD = NUNIT2, HOWBUY = CUSHOM, UNITSIZE = UNITSF, LOTSIZE = LOT, HINCP = ZINC2, INTSTATUS = ISTATUS,
           MAINTAMT = CSTMNT, TOTHCAMT = ZSMHC, MARKETVAL = VALUE, OMB13CBSA = SMSA, WEIGHT = WGT90GEO,
           YRBUILT = BUILT, JOBTYPE = RAS, JOBDIY = RAH, JOBCOST = RAD, HMRENEFF = ENEFIC,
           VACINHER = SINHV, VACINVEST = SINVV)
}

# Finalize the 2011-2013 data by using the custom function above
data_2011 <- reclassify(data_2011) %>%
  # Specify AHSYEAR is 2011
  mutate(AHSYEAR = 2011)

data_2013 <- reclassify(data_2013) %>%
  # Specify AHSYEAR is 2013
  mutate(AHSYEAR = 2013)

# Function to replace 'NR' with -1 in a vector
replace_non_response_values_1 <- function(x) {
  ifelse(x == 'NR', -1, x)
}

# Function to apply the replacement to all columns in a data frame
replace_non_response_values_2 <- function(Data) {
  Data %>% mutate(across(everything(), replace_non_response_values_1))
}

data_2011_2013 <- data_2011 %>%
  # Attach 2011 data to 2013 by row
  rbind(data_2013)

# Apply replace_non_response_values_2() to the 2011-2013 data, this will replace any 'NR' values with -1
data_2011_2013 <- replace_non_response_values_2(data_2011_2013)

data_2011_2013 <- data_2011_2013 %>%
  # Convert numeric columns to numeric values
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         HHAGE = as.numeric(HHAGE),
         HHMOVE = as.numeric(HHMOVE),
         #REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST),
         # Added the following on 12.28, make sure they join with old columns
         MAINTAMT = as.numeric(MAINTAMT),
         #TOTBALAMT = as.numeric(TOTBALAMT),
         TOTHCAMT = as.numeric(TOTHCAMT))

# Joining to recent AHS data ---- 

# Read in the 2015-2021 data and store as data_2015_2023
data_2015_2023 <- read.xlsx(data_2015_2023_file_path)

#Read in column names from 2015-2021 cleaned data
col_order_check <- names(data_2015_2023)
#Specify the column order for 2011-2013
col_order <- col_order_check[col_order_check %in% names(data_2011_2013)]
#Find columns missing from 2011-2013 though present in 2015-2021
missing_cols <- col_order_check[!col_order_check %in% col_order]

# Reorder 2011-2013 data to match the column order in 2015-2021
data_2011_2013 <- data_2011_2013[col_order]

# Bind the 2011-2013 data to the 2015-2021 data. rbind.fill() fills all columns from 2011-2013 that are present in 2015-2021 (binding them as new rows)
# 2011-2013 data will have NA values for all of the 'missing_cols'
data_2011_2023 <- data_2015_2023 %>%
  rbind.fill(data_2011_2013)

# Order the 2011-2021 data by year
data_2011_2023 <- data_2011_2023[order(data_2011_2023$AHSYEAR), ]

# Create a 'logic_check' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998'
# These are all typical NA classifications for AHS variables, if logic_check is empty after running the following line, you are good to go!
# If it is not empty, check the rows for potentially missed NA values.

logic_check <- data_2011_2023 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998')))

# Output data ----

# Output the dataset to the specified file path
write.xlsx(data_2011_2023, output_file_path)