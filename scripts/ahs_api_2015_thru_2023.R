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

output_file_path_2015_2023 <- "C:/Users/ianwe/Downloads/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_2015_to_2023_project_level.xlsx"
output_file_path_2023 <- "C:/Users/ianwe/Downloads/AHS Historical Data/Cleaned Project Level AHS Files/ahs_data_2023_project_level.xlsx"

# Set the file path to the folder that contains all yearly AHS data folders you are interested in
folder_location <- "C:/Users/ianwe/Downloads/AHS Historical Data/Raw AHS Files/15_23"
folder_location

# Before running any further code, make sure the file folders are located in the directory printed in the console below
# Each year should have its own folder (i.e. '1997', '1999', etc.)

# Define the 'files_to_pull'. If needing to include additional files, paste them at the end of 'files_to_pull' as follows:
## Example for all files: files_to_pull <- c('household.csv', 'project.csv', 'mortgage.csv', 'people.csv)
## Make sure to uncomment portions of the for-in loop below if reading in mortgage/person data!
files_to_pull <- c('household.csv', 'project.csv')

# Define the years of survey coverage for files that will be read in
years <- as.character(c(seq(2015, 2023, 2)))
# Create a vector of folder locations for each survey year, this vector should contain file paths to the yearly, unzipped file folders (one file path for each year)
year_folders <- paste0(folder_location, '/', years)

# For each folder year
for (i in 1:length(year_folders)) {

  # Assign the variable 'household_{YEAR}' for each household csv
  assign(paste0("household_", years[i]),
         value = read.csv(paste0(year_folders[i], '/' , files_to_pull[1])))

  # Assign the variable 'project_{YEAR}' for each project csv
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

# Remove unnecessary objects
rm(year_folders, folder_location, i, files_to_pull)

# Read in variable lists ---- 

# Read in the variable matching sheet for household variables
household_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                                sheet = 'Household')

# Filter for household variables relevant to 2021 (All non-NA values within the 'variable' column)
household_variables <- household_variables %>%
  filter(Year == 2021 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2023
household_variables <- household_variables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
household_variables <- household_variables[!household_variables %in% c("DEGREE", 'GUTREHB')]

# Read in the variable matching sheet for project variables
project_variables <- read.xlsx("variable-matching/ahs_variables_by_year.xlsx",
                              sheet = 'Project')

# Filter for project variables relevant to 2021 (All non-NA values within the 'variable' column)
project_variables <- project_variables %>%
  filter(Year == 2021 & !is.na(Variable))

# Create a vector storing the relevant project variables for 2023
project_variables <- project_variables$Variable

# Join Household and Project data ----

# Create a list of all 2015-2023 household datasets
household_list <- list(household_2015, household_2017, household_2019, household_2021, household_2023)
# Create a list of all 2015-2023 project datasets
project_list <- list(project_2015, project_2017, project_2019, project_2021, project_2023)

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

# Finalize the 2015-2023 data by joining amended household/project datasets
data_2015 <- household_2015 %>%
  left_join(project_2015, by = 'CONTROL')
data_2017 <- household_2017 %>%
  left_join(project_2017, by = 'CONTROL')
data_2019 <- household_2019 %>%
  left_join(project_2019, by = 'CONTROL')
data_2021 <- household_2021 %>%
  left_join(project_2021, by = 'CONTROL')
data_2023 <- household_2023 %>%
  left_join(project_2023, by = 'CONTROL')

# Remove all 2015-2023 household/project datasets, all this data now lies in the 'Data_[YEAR] datasets
rm(household_2015, household_2017, household_2019, household_2021, household_2023, household_variables,
   project_2015, project_2017, project_2019, project_2021, project_2023, project_variables,
   household_list, project_list, selected_datasets, i, select_columns, 
   packages, install_if_missing)

# Clean the yearly (joined) data ----

# Custom function for reclassifying variables
reclassify <- function(data) {

  data <- data %>%

    # Unquote all variables
   mutate_all(.funs = unquote) %>%

    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(JOBTYPE %in% c('01', '02', '03', '04', '05', '06') ~ 'DisRepairs',
                                   JOBTYPE %in% c('07', '08', '09', '10', '11') ~ 'RoomAdd',
                                   JOBTYPE %in% c('12') ~ 'Bathroom',
                                   JOBTYPE %in% c('13') ~ 'Kitchen',
                                   JOBTYPE %in% c('14', '15') ~ 'OutsideAtt',
                                   JOBTYPE %in% c('16', '17', '18', '19') ~ 'Exterior',
                                   JOBTYPE %in% c('20', '25', '31') ~ 'Interior',
                                   JOBTYPE %in% c('21', '22', '23', '24', '26', '27', '29', '30') ~ 'Systems',
                                   JOBTYPE %in% c('28', '32', '33', '34', '35', '36', '37') ~ 'LotYardOther')) %>%
    
    # Create a 'jobtype_recode' column for each job type
    mutate(jobtype_recode = case_when(JOBTYPE %in% c('01', '02', '03', '04', '05', '06') ~ 'disaster',
                                      JOBTYPE %in% c('07', '09', '11') ~ 'room_addition',
                                      JOBTYPE == '08' ~ 'bathroom_addition',
                                      JOBTYPE == '10' ~ 'kitchen_addition',
                                      JOBTYPE == '12' ~ 'bathroom_remodel',
                                      JOBTYPE == '13' ~ 'kitchen_remodel',
                                      JOBTYPE %in% c('14', '32', '35') ~ 'garage_driveway_shed',
                                      JOBTYPE == '15' ~ 'decking_porch',
                                      JOBTYPE == '16' ~ 'roofing',
                                      JOBTYPE == '17' ~ 'siding',
                                      JOBTYPE == '18' ~ 'windows_doors',
                                      JOBTYPE == '19' ~ 'other_exterior_improvements',
                                      JOBTYPE == '20' ~ 'insulation',
                                      JOBTYPE %in% c('21', '28') ~ 'water_pipes_septic',
                                      JOBTYPE == '22' ~ 'plumbing_fixtures',
                                      JOBTYPE %in% c('23', '24') ~ 'electrical_security',
                                      JOBTYPE == '25' ~ 'flooring',
                                      JOBTYPE %in% c('26', '27') ~ 'hvac',
                                      JOBTYPE == '29' ~ 'water_heaters',
                                      JOBTYPE == '30' ~ 'dishwashers',
                                      JOBTYPE == '31' ~ 'other_interior_improvements',
                                      JOBTYPE %in% c('33', '34', '36', '37') ~ 'other_outdoor_improvements')) %>%
    
    # Create a 'CSA' variable
    mutate(CSA = case_when(OMB13CBSA == "12060" ~ 'ATL',
                           OMB13CBSA == "12580" ~ 'BAL',
                           OMB13CBSA == "13820" ~ 'BIR',
                           OMB13CBSA == "14460" ~ 'BOS',
                           OMB13CBSA == "16980" ~ 'CHI',
                           OMB13CBSA == "17140" ~ 'CIN',
                           OMB13CBSA == "17460" ~ 'CLE',
                           OMB13CBSA == "19100" ~ 'DAL',
                           OMB13CBSA == "19740" ~ 'DEN',
                           OMB13CBSA == "19820" ~ 'DET',
                           OMB13CBSA == "26420" ~ 'HOU',
                           OMB13CBSA == "28140" ~ 'KC',
                           OMB13CBSA == "29820" ~ 'LV',
                           OMB13CBSA == "31080" ~ 'LA',
                           OMB13CBSA == "32820" ~ 'MEM',
                           OMB13CBSA == "33100" ~ 'MIA',
                           OMB13CBSA == "33340" ~ 'MIL',
                           OMB13CBSA == "33460" ~ 'MIN',
                           OMB13CBSA == "35380" ~ 'NO',
                           OMB13CBSA == "35620" ~ 'NYC',
                           OMB13CBSA == "36420" ~ 'OKC',
                           OMB13CBSA == "37980" ~ 'PHI',
                           OMB13CBSA == "38060" ~ 'PHX',
                           OMB13CBSA == "38300" ~ 'PIT',
                           OMB13CBSA == "38900" ~ 'POR',
                           OMB13CBSA == "39580" ~ 'RAL',
                           OMB13CBSA == "40060" ~ 'RIC',
                           OMB13CBSA == "40140" ~ 'RIV',
                           OMB13CBSA == "40380" ~ 'ROC',
                           OMB13CBSA == "41700" ~ 'SA',
                           OMB13CBSA == "41860" ~ 'SF',
                           OMB13CBSA == "41940" ~ 'SJ',
                           OMB13CBSA == "42660" ~ 'SEA',
                           OMB13CBSA == "45300" ~ 'TAM',
                           OMB13CBSA == "47900" ~ 'DC',
                           OMB13CBSA == "99998" ~ 'Others',
                           OMB13CBSA == "99999" ~ 'Not in Metro')) %>%

    # Amend the DIVISION variable
    mutate(DIVISION = case_when(DIVISION == "1" ~ 'New England',
                                DIVISION == "2" ~ 'Mid Atlantic',
                                DIVISION == "3" ~ 'East North Central',
                                DIVISION == "4" ~ 'West North Central',
                                DIVISION == "5" ~ 'South Atlantic',
                                DIVISION == "6" ~ 'East South Central',
                                DIVISION == "7" ~ 'West South Central',
                                DIVISION == "8" ~ 'Mountain',
                                DIVISION == "9" ~ 'Pacific')) %>%

    # Amend the HOWBUY variable
    mutate(HOWBUY = case_when(HOWBUY == "1" ~ 'Bought house already built',
                              HOWBUY == "2" ~ 'Signed sales agreement that included land/cost of building unit',
                              HOWBUY == "3" ~ 'General contractor built unit on own land (includes leased land)',
                              HOWBUY == "4" ~ 'Built house themselves on own land',
                              HOWBUY == "5" ~ 'Received as gift/inheritance',
                              HOWBUY == '-9' ~ 'NR',
                              HOWBUY == '-6' ~ NA)) %>%

    # Amend the HINCP variable (-6 here is NA)
    mutate(HINCP = ifelse(HINCP == '-6', NA, HINCP)) %>%

    # Amend the TOTHCAMT variable (-6 here is NA)
    mutate(TOTHCAMT = ifelse(TOTHCAMT == '-6', NA, TOTHCAMT)) %>%

    # Amend the MAINTAMT variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(MAINTAMT = case_when(MAINTAMT == '-6' ~ NA,
                                MAINTAMT == '-9' ~ 'NR',
                                !MAINTAMT %in% c('-6', '-9') ~ MAINTAMT)) %>%

    # Amend the MARKETVAL variable (-6 here is NA)
    mutate(MARKETVAL = ifelse(MARKETVAL == '-6', NA, MARKETVAL)) %>%

    # Amend the HHAGE variable (-6 here is NA)
    mutate(HHAGE = ifelse(HHAGE == '-6', NA, HHAGE)) %>%
    
    # Amend the HHRACE variable (-6 here is NA)
    mutate(HHRACE = case_when(HHRACE == '01' ~ 'White only',
                              HHRACE == '02' ~ 'Black only',
                              HHRACE == '03' ~ 'American Indian, Alaska Nativa only',
                              HHRACE == '04' ~ 'Asian only',
                              HHRACE == '05' ~ 'Hawaiian, Pacific Islander only',
                              
                              HHRACE == '06' ~ 'White / Black',
                              HHRACE == '07' ~ 'White / American Indian, Alaska Native',
                              HHRACE == '08' ~ 'White / Asian',
                              HHRACE == '09' ~ 'White / Hawaiian, Pacific Islander',
                              HHRACE == '10' ~ 'Black / American Indian, Alaska Native',
                              HHRACE == '11' ~ 'Black / Asian',
                              HHRACE == '12' ~ 'Black / Hawaiian, Pacific Islander',
                              HHRACE == '13' ~ 'American Indian, Alaska Native / Asian',
                              HHRACE == '14' ~ 'Asian / Hawaiian, Pacific Islander',
                              
                              HHRACE == '15' ~ 'White / Black / American Indian, Alaska Native',
                              HHRACE == '16' ~ 'White / Black / Asian',
                              HHRACE == '17' ~ 'White / American Indian, Alaska Native / Asian',
                              HHRACE == '18' ~ 'White / Asian / Hawaiian, Pacific Islander',
                              HHRACE == '19' ~ 'White / Black / American Indian, Alaska Native / Asian',
                              HHRACE == '20' ~ 'Other combinations of 2 or 3 races',
                              HHRACE == '21' ~ 'Other combinations of 4 or more races',
                              HHRACE == '-6' ~ NA)) %>%
    
    # Amend the HHSPAN variable (-6 here is NA)
    mutate(HHSPAN = case_when(HHSPAN == '1' ~ 'Yes',
                              HHSPAN == '2' ~ 'No',
                              HHSPAN == '-6' ~ NA)) %>%

    # Amend the TENURE variable (-6 here is NA)
    mutate(TENURE = case_when(TENURE == '1' ~ 'Owned/Bought',
                              TENURE == '2' ~ 'Rented',
                              TENURE == '3' ~ 'Occupied Without Rent',
                              TENURE == '-6' ~ NA)) %>%

    # Amend the REMODAMT variable (-6 here is NA)
    mutate(REMODAMT = ifelse(REMODAMT == '-6', NA, REMODAMT)) %>%

    # Amend the MORTAMT variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(MORTAMT = case_when(MORTAMT == '-6' ~ NA,
                               MORTAMT == '-9' ~ 'NR',
                               !MORTAMT %in% c('-6', '-9') ~ MORTAMT)) %>%
    
    # Amend the TOTBALAMT variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(TOTBALAMT = case_when(TOTBALAMT == '-6' ~ NA,
                                 TOTBALAMT == '-9' ~ 'NR',
                                 !TOTBALAMT %in% c('-6', '-9') ~ TOTBALAMT)) %>%

    # Amend the BLD variable
    mutate(BLD = case_when(BLD == '01' ~ 'Mobile',
                           BLD == '02' ~ 'SF Detached',
                           BLD == '03' ~ 'SF Attached',
                           BLD == '04' ~ '2 Apartments',
                           BLD == '05' ~ '3-4 Apartments',
                           BLD == '06' ~ '5-9 Apartments',
                           BLD == '07' ~ '10-19 Apartments',
                           BLD == '08' ~ '20-49 Apartments',
                           BLD == '09' ~ '50+ Apartments',
                           BLD == '10' ~ 'Boat, RV, Van, Etc.')) %>%

    # Amend the UNITSIZE variable (-9 here is 'Not Reported')
    mutate(UNITSIZE = case_when(UNITSIZE == '1' ~ '< 500 sq. ft.',
                                UNITSIZE == '2' ~ '500 - 749 sq. ft.',
                                UNITSIZE == '3' ~ '750 - 999 sq. ft.',
                                UNITSIZE == '4' ~ '1,000 - 1,499 sq. ft.',
                                UNITSIZE == '5' ~ '1,500 - 1,999 sq. ft.',
                                UNITSIZE == '6' ~ '2,000 - 2,499 sq. ft.',
                                UNITSIZE == '7' ~ '2,500 - 2,999 sq. ft.',
                                UNITSIZE == '8' ~ '3,000 - 3,999 sq. ft.',
                                UNITSIZE == '9' ~ '>= 4,000 sq. ft.',
                                UNITSIZE == '-9' ~ 'NR')) %>%

    # Amend the LOTSIZE variable (-6 here is NA)
     mutate(LOTSIZE = case_when(LOTSIZE == '1' ~ '< 1/8 acre',
                                LOTSIZE == '2' ~ '1/8 - 1/4 acre',
                                LOTSIZE == '3' ~ '1/4 - 1/2 acre.',
                                LOTSIZE == '4' ~ '1/2 - 1 acre',
                                LOTSIZE == '5' ~ '1 - 5 acres',
                                LOTSIZE == '6' ~ '5 - 10 acres',
                                LOTSIZE == '7' ~ '>= 10 acres',
                                LOTSIZE == '-6' ~ NA)) %>%

    # Amend the INTSTATUS variable
    mutate(INTSTATUS = case_when(INTSTATUS == '1' ~ 'Occupied',
                                 INTSTATUS == '2' ~ 'URE',
                                 INTSTATUS == '3' ~ 'Vacant')) %>%

    # Create the VACANCY_RECLASS variable, grouping into 'Year Round Vacant', 'Seasonal', & 'Occupied' (-6 here is NA)
    mutate(VACANCY_RECLASS = case_when(VACANCY %in% c('01', '02', '03', '04', '05') ~ 'Year Round Vacant',
                                       VACANCY %in% c('06', '08', '09', '10', '11') ~ 'Seasonal',
                                       VACANCY == '-6' & INTSTATUS == 'Occupied' ~ 'Occupied',
                                       VACANCY == '-6' & INTSTATUS != 'Occupied' ~ NA)) %>%

    # Amend the VACANCY variable (-6 here is NA)
    mutate(VACANCY = case_when(VACANCY %in% c('01', '04') ~ 'For Rent or Rented',
                               VACANCY == '02' ~ 'Rent or Sale',
                               VACANCY %in% c('03', '05') ~ 'For Sale or Sold',
                               #VACANCY == '04' ~ 'Rent, Not Yet Occupied',
                               #VACANCY == '05' ~ 'Sold, Not Yet Occupied',
                               VACANCY == '06' ~ 'Occasional Use',
                               VACANCY == '07' ~ 'Other',
                               VACANCY == '08' ~ 'Seasonal, Summer Only',
                               VACANCY == '09' ~ 'Seasonal, Winter Only',
                               VACANCY == '10' ~ 'Seasonal, Other',
                               VACANCY == '11' ~ 'Migratory',
                               VACANCY == '-6' ~ NA)) %>%

    # Amend the VACANCY variable (-6 here is NA, -9 here is 'Not Reported')
    # mutate(GUTREHB = case_when(GUTREHB == '1' ~ 'Yes',
    #                            GUTREHB == '2' ~ 'No',
    #                            GUTREHB == '-9' ~ 'NR',
    #                            GUTREHB == '-6' ~ NA)) %>%

    # Amend the DISHH variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(DISHH = case_when(DISHH == '1' ~ '1+ Disabled Occupants',
                             DISHH == '2' ~ 'No Disabled Occupants',
                             DISHH == '-9' ~ 'NR',
                             DISHH == '-6' ~ NA)) %>%

    # Amend the NUMCARE variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMCARE = case_when(NUMCARE == '1' ~ 'No one has this disability',
                               NUMCARE == '2' ~ '1 person has this disability',
                               NUMCARE == '3' ~ '2+ people have this disability',
                               NUMCARE == '-9' ~ 'NR',
                               NUMCARE == '-6' ~ NA)) %>%

    # Amend the NUMMEMRY variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMMEMRY = case_when(NUMMEMRY == '1' ~ 'No one has this disability',
                                NUMMEMRY == '2' ~ '1 person has this disability',
                                NUMMEMRY == '3' ~ '2+ people have this disability',
                                NUMMEMRY == '-9' ~ 'NR',
                                NUMMEMRY == '-6' ~ NA)) %>%

    # Amend the NUMERRND variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMERRND = case_when(NUMERRND == '1' ~ 'No one has this disability',
                                NUMERRND == '2' ~ '1 person has this disability',
                                NUMERRND == '3' ~ '2+ people have this disability',
                                NUMERRND == '-9' ~ 'NR',
                                NUMERRND == '-6' ~ NA)) %>%

    # Amend the NUMHEAR variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMHEAR = case_when(NUMHEAR == '1' ~ 'No one has this disability',
                               NUMHEAR == '2' ~ '1 person has this disability',
                               NUMHEAR == '3' ~ '2+ people have this disability',
                               NUMHEAR == '-9' ~ 'NR',
                               NUMHEAR == '-6' ~ NA)) %>%

    # Amend the NUMSEE variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMSEE = case_when(NUMSEE == '1' ~ 'No one has this disability',
                              NUMSEE == '2' ~ '1 person has this disability',
                              NUMSEE == '3' ~ '2+ people have this disability',
                              NUMSEE == '-9' ~ 'NR',
                              NUMSEE == '-6' ~ NA)) %>%

    # Amend the NUMWALK variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(NUMWALK = case_when(NUMWALK == '1' ~ 'No one has this disability',
                               NUMWALK == '2' ~ '1 person has this disability',
                               NUMWALK == '3' ~ '2+ people have this disability',
                               NUMWALK == '-9' ~ 'NR',
                               NUMWALK == '-6' ~ NA)) %>%

    # Amend the HMRACCESS variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRACCESS = case_when(HMRACCESS == '1' ~ 'Yes',
                                 HMRACCESS == '2' ~ 'No',
                                 HMRACCESS == '-9' ~ 'NR',
                                 HMRACCESS == '-6' ~ NA)) %>%

    # Amend the HMRENEFF variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRENEFF = case_when(HMRENEFF == '1' ~ 'Yes',
                                HMRENEFF == '2' ~ 'No',
                                HMRENEFF == '-9' ~ 'NR',
                                HMRENEFF == '-6' ~ NA)) %>%

    # Amend the HMRSALE variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRSALE = case_when(HMRSALE == '1' ~ 'Yes',
                               HMRSALE == '2' ~ 'No',
                               HMRSALE == '-9' ~ 'NR',
                               HMRSALE == '-6' ~ NA)) %>%

    # Amend the VACINHER variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(VACINHER = case_when(VACINHER == '1' ~ 'Yes',
                                VACINHER == '2' ~ 'No',
                                VACINHER == '-9' ~ 'NR',
                                VACINHER == '-6' ~ NA)) %>%

    # Amend the VACINVEST variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(VACINVEST = case_when(VACINVEST == '1' ~ 'Yes',
                                 VACINVEST == '2' ~ 'No',
                                 VACINVEST == '-9' ~ 'NR',
                                 VACINVEST == '-6' ~ NA)) %>%

    # Amend the JOBDIY variable
    mutate(JOBDIY = case_when(JOBDIY == '1' ~ 'DIY',
                              JOBDIY == '2' ~ 'Not DIY')) %>%

    # Amend the JOBCOST variable (-9 here is 'Not Reported')
    mutate(JOBCOST = ifelse(JOBCOST == '-9', 'NR', JOBCOST)) %>%

    # Amend the JOBCOMP variable (-9 here is 'Not Reported')
    mutate(JOBCOMP = case_when(JOBCOMP == '1' ~ 'Completed',
                               JOBCOMP == '2' ~ 'Not Completed',
                               JOBCOMP == '-9' ~ 'NR')) %>%

    # Amend the JOBCOMPYR variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(JOBCOMPYR = case_when(JOBCOMPYR == '1' ~ '20XX - 2',
                               JOBCOMPYR == '2' ~ '20XX - 1',
                               JOBCOMPYR == '3' ~ '20XX',
                               JOBCOMPYR == '-9' ~ 'NR',
                               JOBCOMPYR == '-6' ~ NA)) %>%

    # Amend the JOBWORKYR variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(JOBWORKYR = case_when(JOBWORKYR == '1' ~ '20XX - 2',
                                 JOBWORKYR == '2' ~ '20XX - 1',
                                 JOBWORKYR == '3' ~ '20XX',
                                 JOBWORKYR == '-9' ~ 'NR',
                                 JOBWORKYR == '-6' ~ NA)) %>%

    # Amend the JOBFUNDS variable (-9 here is 'Not Reported')
    mutate(JOBFUNDS = case_when(JOBFUNDS == '1' ~  'Cash, Savings',
                                JOBFUNDS == '2' ~  'Cash, Refinance',
                                JOBFUNDS == '3' ~  'Home Equity Line',
                                JOBFUNDS == '4' ~  'Homeowner Insurance Settlement',
                                JOBFUNDS == '5' ~  'Credit/Retail Card',
                                JOBFUNDS == '6' ~  'Contractor Financing',
                                JOBFUNDS == '7' ~  'Other',
                                JOBFUNDS == '-9' ~  'NR')) %>%
    
    # Amend the CONDO variable
    mutate(CONDO = case_when(CONDO == '1' ~ 'condo',
                             CONDO == '2' ~ 'not_a_condo')) %>%
    
    # Amend the HOA variable (-9 here is 'Not Reported')
    mutate(HOA = case_when(HOA == '1' ~ 'part_of_hoa',
                           HOA == '2' ~ 'not_part_of_hoa',
                           HOA == '-9' ~ 'NR')) %>%
    
    # Amend the UNITFLOORS variable (-6 here is NA)
    mutate(UNITFLOORS = case_when(UNITFLOORS == 1 ~ 'one_floor',
                                  UNITFLOORS == 2 ~ 'two_floors',
                                  UNITFLOORS == 3 ~ 'three_plus_floors',
                                  UNITFLOORS == '-6' ~ NA)) %>%
    
    # Amend the BATHROOMS variable (-6 here is NA)
    mutate(BATHROOMS = case_when(BATHROOMS == '01' ~ '1 full bathroom',
                                 BATHROOMS == '02' ~ '1.5 bathrooms',
                                 BATHROOMS == '03' ~ '2 full bathrooms',
                                 BATHROOMS == '04' ~ '2.5 bathrooms',
                                 BATHROOMS == '05' ~ '3 full bathrooms',
                                 BATHROOMS == '06' ~ 'More than 3 full bathrooms',
                                 BATHROOMS == '07' ~ 'No full bath: sink and tub present',
                                 BATHROOMS == '08' ~ 'No full bath: sink and toilet present',
                                 BATHROOMS == '09' ~ 'No full bath: tub and toilet present',
                                 BATHROOMS == '10' ~ 'No full bath: sink only',
                                 BATHROOMS == '11' ~ 'No full bath: tub only',
                                 BATHROOMS == '12' ~ 'No full bath: toilet only',
                                 BATHROOMS == '13' ~ 'No full bath: no sink, tub or toilet',
                                 )) %>%

    # Amend the HHMOVE variable (-6 here is NA)
    mutate(HHMOVE = ifelse(HHMOVE == '-6', NA, HHMOVE)) %>%

    # Drop the original CSA variable
    select(-c(OMB13CBSA))
}

# Convert years to numeric (it was a character string up to this point)
years <- as.numeric(years)

for (i in years){

  assign(paste0('data_', i),
         # Use the reclassification function on each 'data_[YEAR]' and re save as 'data_[YEAR]'
         value = reclassify(get(paste0('data_', i))) %>%
                              mutate(AHSYEAR = i))
  assign(paste0('data_', i),
         # Amend the JOBCOMPYR and JOBWORKYR for each 'data_[YEAR]' and re save as 'data_[YEAR]'
         value = get(paste0('data_', i)) %>%
                              # Amend the JOBCOMPYR variable to read the correct year
                              mutate(JOBCOMPYR = case_when(JOBCOMPYR == '20XX - 2' ~ (i - 2),
                                                           JOBCOMPYR == '20XX - 1' ~ (i - 1),
                                                           JOBCOMPYR == '20XX' ~ i))  %>%
                              # Amend the JOBWORKYR variable to read the correct year
                              mutate(JOBWORKYR = case_when(JOBWORKYR == '20XX - 2' ~ (i - 2),
                                                           JOBWORKYR == '20XX - 1' ~ (i - 1),
                                                           JOBWORKYR == '20XX' ~ i)))
}

# Attach 2017-2023 datasets to 2015 by row
data_2015_2023 <- data_2015 %>%
  rbind(data_2017, data_2019, data_2021, data_2023) %>%
  # Need to remove 'JOBFUNDS.1', this was created in the reclassify call above due to differences in class of the output (character) vs the input (numeric)
  select(-JOBFUNDS.1)

names(data_2023)

# Function to replace 'NR' with -1 in a vector
replace_non_response_values_1 <- function(x) {
  ifelse(x == 'NR', -1, x)
}
# Function to apply the replacement to all columns in a data frame
replace_non_response_values_2 <- function(data) {
  data %>% mutate(across(everything(), replace_non_response_values_1))
}

# Apply reaplce_non_response_values() to the 2015-2023 data, this will replace any 'NR' values with -1
data_2015_2023 <- replace_non_response_values_2(data_2015_2023)

data_2015_2023 <- data_2015_2023 %>%
  # Convert columns that should be numeric to numeric values (they are character strings up to this point)
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         HHAGE = as.numeric(HHAGE),
         HHMOVE = as.numeric(HHMOVE),
         REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST),
         # Added the following on 12.28, make sure they join with old columns
         MAINTAMT = as.numeric(MAINTAMT),
         MORTAMT = as.numeric(MORTAMT),
         TOTBALAMT = as.numeric(TOTBALAMT),
         TOTHCAMT = as.numeric(TOTHCAMT),
         # Added the following on 9.27.24
         TOTROOMS = as.numeric(TOTROOMS),
         BEDROOMS = as.numeric(BEDROOMS)
         )

# Create a 'logic_check' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998'
# These are all typical NA classifications for AHS variables, if logic_check is empty after running the following line, you are good to go!
# If it is not empty, check the rows for potentially missed NA values.

logic_check <- data_2015_2023 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998', 'NR')))

logic_check_2023 <- logic_check %>%
  filter(AHSYEAR == 2023)

data_2015_2023 <- data_2015_2023 %>%
  select(AHSYEAR, CONTROL, WEIGHT, INTSTATUS, DIVISION, CSA, BLD, TENURE, VACANCY, VACANCY_RECLASS, VACINHER, VACINVEST, HOWBUY, CONDO, HOA, YRBUILT, UNITSIZE, LOTSIZE, 
         UNITFLOORS, TOTROOMS, BATHROOMS, BEDROOMS, HINCP, MARKETVAL, HHAGE, HHRACE, HHSPAN, HHMOVE, MORTAMT, TOTBALAMT, TOTHCAMT, REMODAMT, MAINTAMT, JOBTYPE, jobtype_recode, JobCategory,
         JOBDIY, JOBCOMP, JOBCOMPYR, JOBWORKYR, JOBFUNDS, JOBCOST, starts_with('HMR'), DISHH, NUMCARE, NUMERRND, NUMHEAR, NUMMEMRY, NUMSEE, NUMWALK)

data_2023 <- data_2015_2023 %>%
  filter(AHSYEAR == 2023)

# Output data ----

# Output the dataset to the specified file path
write.xlsx(data_2015_2023, output_file_path_2015_2023)
write.xlsx(data_2023, output_file_path_2023)