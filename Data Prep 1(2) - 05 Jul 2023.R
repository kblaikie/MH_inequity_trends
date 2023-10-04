#### Aim 1 Data Prep 1 - 05 July 2023 ####
#Author - Kieran Blaikie
#Date - 05 July 2023

#Runtime - ~10 minutes

#Overview - This script (run locally instead of via the CSDE server):
#             1) Takes year-specific BRFSS datasets from 1993 to 2019 and restricts them to the necessary set of variables
#             2) Appends all datasets to create one 1993-2019 dataset
#             3) Recodes variables to have consistent coding over years
#             4) Create necessary variables for future analyses

#Changes - Compared to 'Data Prep 1 - 10 Jan 2023.R', this script:
#            1) Dichotomizes education in terms of <HS vs. >=HS (vs. <=HS vs. >HS) to better-reflect known educational gradients in health

#NOTE - The dataset produced by this script, 'mid_prep_data_050723.csv', should be identical to 'mid_prep_data_100123.csv', except
#       for the re-coded education variable 'lesshs' instead of 'hsless' 
#     - While several variables are no longer used (e.g. 'poverty_line' based on HHS guidelines), I keep these in the dataset to avoid
#       introducing any possible errors across new and old scripts.
#     - Based on the variables used per script, 'Data Prep 2 (3)' and 'Data Prep 2.5 (2)' should NOT need to be re-written/run, while
#      'Part A Data Prep 3(6)', 'Data Prep 4', 'Part A Data Prep 5(2)', and 'Part A Data Analysis (1)' do.

#Loading packages
library(readr)
library(tidyverse)
library(foreign)

#Setting directory
directory <- "C:/Users/blaik/Desktop/UW/Dissertation/Data/Aim 1/"

#### Loading BRFSS Data ####
#Steps 1) Load BRFSS data .XPT SAS file into R using 'foreign' package
#      2) Same formatted file as .csv
for (year in 1993:2019) {
  assign(paste0("data_", year), read.xport(paste0(directory, "Golden/brfss_", year, ".XPT")))
  write_csv(eval(parse(text = paste0("data_", year))), paste0(directory, "Formatted Golden/brfss_", year, ".csv"))
  rm(list = ls()[startsWith(ls(), "data_")]); rm(year)
}

#Start run-time
start <- Sys.time()

#### Loading each dataset and restricting to necessary variables ####
#Variables to select for per year:
#   Gender (1993-2017: "Sex", 2018: "SEX1", 2019: "SEXVAR")
#   Educational Attainment (1993-2019: EDUCA")
#   Income (1993-1994: "INCOME", 1995: "INCOME95", 1996-2019: "INCOME2")
#   Ethnicity (1993-2000: "HISPANIC", 2001-2019: "HISPANC2")
#   Race (1993-2000: "ORACE", 2001-2012: "X_PRACE", 2013-2019: "X_PRACE1")
#   Age (1993-2019: "AGE")
#   Mental Health (1993-2019: "MENTHLTH")
#   State (1993-2019: "X_STATE")
#   Weighting Variable(s) (1993-2010: "X_FINALWT", 2011-2019: "X_LLCPWT")
#   Record Identifier (1993-2001: "X_RECORD", 2002-2019: "SEQNO")
#   Number of adults (1993-2019: "NUMADULT")
#   Number of children (1993: "CHILDREN", 1994-2000: c("CHLD04","CHLD0512","CHLD1317"), 2001-2019: "CHILDREN")
#   Marital status (1993-2019: "MARITAL")

for (year in 1993:2019) {
  print("")
  print(paste0("Year: ", year))
  temp <- read_csv(paste0(directory, "Formatted Golden/brfss_", year, ".csv"))
  if (year %in% c(1993)) {
    temp <- temp[,c("X_RECORD", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME", "HISPANIC", "ORACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN")]
  }
  if (year %in% c(1994)) {
    temp <- temp[,c("X_RECORD", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME", "HISPANIC", "ORACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHLD04", "CHLD0512", "CHLD1317")]
  }
  if (year %in% c(1995)) {
    temp <- temp[,c("X_RECORD", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME95", "HISPANIC", "ORACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHLD04", "CHLD0512", "CHLD1317")]
  }
  if (year %in% c(1996:2000)) {
    temp <- temp[,c("X_RECORD", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME2", "HISPANIC", "ORACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHLD04", "CHLD0512", "CHLD1317")]
  }
  if (year %in% c(2001)) {
    temp <- temp[,c("X_RECORD", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME2", "HISPANC2", "X_PRACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN")]
  }
  if (year %in% c(2002:2010)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_FINALWT", "AGE", "SEX", "EDUCA", "INCOME2", "HISPANC2", "X_PRACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN")]
  }
  if (year %in% c(2011:2012)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_LLCPWT", "AGE", "SEX", "EDUCA", "INCOME2", "HISPANC2", "X_PRACE", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN")]
  }
  if (year %in% c(2013)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_LLCPWT", "X_AGEG5YR", "SEX", "EDUCA", "INCOME2", "X_HISPANC", "X_PRACE1", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN")]
  }
  if (year %in% c(2014:2017)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_LLCPWT", "X_AGEG5YR", "SEX", "EDUCA", "INCOME2", "X_HISPANC", "X_PRACE1", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN", "HHADULT")]
  }
  if (year %in% c(2018)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_LLCPWT", "X_AGEG5YR", "SEX1", "EDUCA", "INCOME2", "X_HISPANC", "X_PRACE1", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN", "HHADULT")]
  }
  if (year %in% c(2019)) {
    temp <- temp[,c("SEQNO", "X_STATE", "X_LLCPWT", "X_AGEG5YR", "X_SEX", "EDUCA", "INCOME2", "X_HISPANC", "X_PRACE1", "MENTHLTH", "MARITAL", "NUMADULT", "CHILDREN", "HHADULT")]
  }
  
  if (year %in% c(1993, 2001:2013)) {
    names(temp) <- c("unique_id", "state_fips", "weight", "age", "sex", "educ", "income", "hisp", "race", "mental", "marital", "n_adults", "n_children")
  }
  if (year %in% c(2014:2019)) {
    names(temp) <- c("unique_id", "state_fips", "weight", "age", "sex", "educ", "income", "hisp", "race", "mental", "marital", "n_adults", "n_children", "cell_n_adults")
  }
  if (year %in% c(1994:2000)) {
    names(temp) <- c("unique_id", "state_fips", "weight", "age", "sex", "educ", "income", "hisp", "race", "mental", "marital", "n_adults", "n_children04", "n_children0512", "n_children1317")
  }
  
  temp$year <- year
  
  if (year < 2014) { 
    temp$cell_n_adults <- NA_real_
  }
  
  assign(paste0("data_", year), temp)
  rm(temp, year)
}

#Reordering 2014-2019 datasets for merging
data_2014 <- data_2014[, names(data_2013)]
data_2015 <- data_2015[, names(data_2013)]
data_2016 <- data_2016[, names(data_2013)]
data_2017 <- data_2017[, names(data_2013)]
data_2018 <- data_2018[, names(data_2013)]
data_2019 <- data_2019[, names(data_2013)]

#Creating 1993,2001-2019 dataset and 1994-2000 dataset
data93_0119 <- rbind(data_1993, data_2001, data_2002, data_2003, data_2004, 
                     data_2005, data_2006, data_2007, data_2008, data_2009, 
                     data_2010, data_2011, data_2012, data_2013, data_2014, 
                     data_2015, data_2016, data_2017, data_2018, data_2019)
data9400 <- rbind(data_1994, data_1995, data_1996, data_1997, data_1998, 
                  data_1999, data_2000)

rm(list=ls()[startsWith(ls(), "data_")])

#### Reformatting 1994-2000 and 1993,2001-2019 datasets to have consistent variables ####
#NOTE - Some individuals were missing information on 1+ N child categories
#     - n_children has N reported children, ignoring NA N child categories
#     - n_children_actual reports NA for all those missing 1+ N child categories

#Coding - 1994-2000 dataset - Children under 4, 5-12, 13-17 years
#   1994 - 8 = none, 1-7 = actual N, else = NA
#   1995-2000 - 8 = none, 1-6 = actual N, 7 = 7+, else = NA
#Coding - 1993,2001-2019 dataset - N children
#   1993 - 88 = 0, 1-25 = actual N, 99 = NA
#   2001-2002 - 88 = 0, 1-7 = actual N, 99 = NA
#   2003-2019 - 88 = 0, 1-87 = actual N, 99 = NA
data9400 %>% mutate(n_children04 = case_when(n_children04 == 8 ~ 0,
                                             n_children04 %in% c(1:7) ~ n_children04,
                                             TRUE ~ NA_real_),
                    n_children0512 = case_when(n_children0512 == 8 ~ 0,
                                               n_children0512 %in% c(1:7) ~ n_children0512,
                                               TRUE ~ NA_real_),
                    n_children1317 = case_when(n_children1317 == 8 ~ 0,
                                               n_children1317 %in% c(1:7) ~ n_children1317,
                                               TRUE ~ NA_real_)) -> data9400
data9400 %>% mutate(n_children = case_when(!is.na(n_children04) & !is.na(n_children0512) & !is.na(n_children1317) ~ (n_children04 + n_children0512 + n_children1317),
                                           is.na(n_children04) & !is.na(n_children0512) & !is.na(n_children1317) ~ (n_children0512 + n_children1317),
                                           !is.na(n_children04) & is.na(n_children0512) & !is.na(n_children1317) ~ (n_children04 + n_children1317),
                                           !is.na(n_children04) & !is.na(n_children0512) & is.na(n_children1317) ~ (n_children04 + n_children0512),
                                           is.na(n_children04) & is.na(n_children0512) & !is.na(n_children1317) ~ n_children1317,
                                           is.na(n_children04) & !is.na(n_children0512) & is.na(n_children1317) ~ n_children0512,
                                           !is.na(n_children04) & is.na(n_children0512) & is.na(n_children1317) ~ n_children04,
                                           TRUE ~ NA_real_),
                    n_children_actual = ifelse(is.na(n_children04) | is.na(n_children0512) | is.na(n_children1317), NA_real_, n_children)) -> data9400
data9400[["n_children04"]] <- NULL; data9400[["n_children0512"]] <- NULL; data9400[["n_children1317"]] <- NULL
data93_0119 %>% mutate(n_children = case_when(is.na(n_children) | n_children >88 ~ NA_real_,
                                              n_children == 88 ~ 0, TRUE ~ n_children)) -> data93_0119
data93_0119$n_children_actual <- data93_0119$n_children

#Reordering variables to match
data93_0119 <- data93_0119[,names(data9400)]

#Creating single dataset
data <- rbind(data9400, data93_0119)
rm(data9400, data93_0119)

#### Reformatting data to have consistent coding ####
#NOTE - Mentally unhealthy days was a *module* question in 2002, so there is no data for several states

#Coding - Income
#   1993 - 1=<10, 2=<15, 3=<20, 4=<25, 5=<35, 6=<50, 7=>50, >=8 = NA
#   1994-2019 - 1=<10, 2=<15, 3=<20, 4=<25, 5=<35, 6=<50, 7=<75, 8 = >75, >=9 = NA
#Coding - Age
#   1993-2012 - <18=NA, else = actual age
#   2013-2019 - 1=18-24, 2=25-29, 3=30-34, 4=35-39, 5=40-44, 6=45-49, 7=50-54, 
#               8=55-59, 9=60-64, 10=65-69, 11=70-74, 12=75-79, 13=80+, else = NA
#Coding - Sex
#   1993-2019 - 1=Male, 2=Female, else = NA
#Coding - Hispanic and/or Latinx
#   1993-2019 - 1=Yes, 2=No, else = NA
#Coding - Education
#   1993-2019 - <=3 = <HS/GED, 4-8 = >=HS, >=9 = NA
#Coding - Race
#   1993-1995 - 1=NHWhite, 2=NHBlack, 3=HWhite, 4=HBlack, 5=HOther, 6=API, 7=NA/AN, 8=NHOther, >=9 = NA
#   1996-2000 - 1=White, 2=Black, 3=API, 4=NA/AN, 5=Other, >=6 = NA
#   2001-2012 - 1=White, 2=Black, 3=Asian, 4=NH/PI, 5=NA/AN, 6=Other, >=7 = NA
#   2013-2019 - 1=White, 2=Black, 3=NA/AN, 4=Asian, 5=NH/PI, 6=Other, >=7 = NA 
#Coding - Mental Health
#   1993-2019 - 88 = None, 1-30 = N days, else = NA
#Coding - State FIPS
#   1993-2019 - See (https://www2.census.gov/programs-surveys/decennial/2010/partners/pdf/FIPS_StateCounty_Code.pdf)
#Coding - Number of adults
#   NOTE - Only asked of landline interviews, not cell phones (variable: CELPHONE), so NA for large groups 2011-2019
#   NOTE - Could use HHADULT for those with cell phone interview
#   1993-2019 - N = Adults, NA = NA
#Coding - Marital status
#   1993-2019 - 1 = Married, 2 = Divorced, 3 = Widowed, 4 = Separated, 
#               5 = Never married, 6 = Unmarried couple, else = NA

data %>% mutate(age = ifelse(is.na(age), NA_real_,
                             ifelse(year <2013,  case_when(age < 18 ~ NA_real_,
                                                           TRUE ~ age),
                                    case_when(age == 1 ~ 18, age == 2 ~ 25,
                                              age == 3 ~ 30, age == 4 ~ 35,
                                              age == 5 ~ 40, age == 6 ~ 45,
                                              age == 7 ~ 50, age == 8 ~ 55,
                                              age == 9 ~ 60, age == 10 ~ 65,
                                              age == 11 ~ 70, age == 12 ~ 75,
                                              age == 13 ~ 80, TRUE ~ NA_real_))),
                sex = ifelse(is.na(sex) | sex >2, NA_real_, sex),
                male = ifelse(sex == 1, 1, ifelse(sex == 2, 0, NA_real_)),
                hisp = ifelse(is.na(hisp) | hisp >2, NA_real_, 
                              ifelse(hisp == 1, 1, 0)),
                educ = ifelse(is.na(educ) | educ == 9, NA_real_, educ),
                lesshs = ifelse(is.na(educ), NA_real_, ifelse(educ <=3, 1, 0)),
                income = ifelse(is.na(income), NA_real_,
                                ifelse(year == 1993 & income >=8, NA_real_,
                                       ifelse(year >1993 & income >=9, NA_real_, income))),
                race = ifelse(is.na(race), NA_character_,
                              ifelse(year <= 1995, case_when(race == 1 | race == 3 ~ "White",
                                                             race == 2 | race == 4 ~ "Black",
                                                             race == 5 | race == 8 ~ "Other",
                                                             race == 6 ~ "AsianORPI",
                                                             race == 7 ~ "NAAN", TRUE ~ NA_character_),
                                     ifelse(year <= 2000, case_when(race == 1 ~ "White",
                                                                    race == 2 ~ "Black",
                                                                    race == 3 ~ "AsianORPI",
                                                                    race == 4 ~ "NAAN",
                                                                    race == 5 ~ "Other", TRUE ~ NA_character_),
                                            ifelse(year <= 2012, case_when(race == 1 ~ "White", 
                                                                           race == 2 ~ "Black",
                                                                           race == 3 ~ "Asian",
                                                                           race == 4 ~ "NHPI",
                                                                           race == 5 ~ "NAAN",
                                                                           race == 6 ~ "Other", TRUE ~ NA_character_),
                                                   case_when(race == 1 ~ "White",
                                                             race == 2 ~ "Black",
                                                             race == 3 ~ "NAAN",
                                                             race == 4 ~ "Asian",
                                                             race == 5 ~ "NHPI",
                                                             race == 6 ~ "Other", TRUE ~ NA_character_))))),
                mental = case_when(is.na(mental) ~ NA_real_,
                                   mental == 88 ~ 0,
                                   mental != 88 & mental > 30 ~ NA_real_,
                                   TRUE ~ mental),
                state_acronym   = case_when(state_fips == 1  ~ "AL", state_fips == 2  ~ "AK", state_fips == 4  ~ "AZ",
                                            state_fips == 5  ~ "AR", state_fips == 6  ~ "CA", state_fips == 8  ~ "CO",
                                            state_fips == 9  ~ "CT", state_fips == 10 ~ "DE", state_fips == 11 ~ "DC", state_fips == 12 ~ "FL",
                                            state_fips == 13 ~ "GA", state_fips == 15 ~ "HI", state_fips == 16 ~ "ID",
                                            state_fips == 17 ~ "IL", state_fips == 18 ~ "IN", state_fips == 19 ~ "IA", state_fips == 20 ~ "KS",
                                            state_fips == 21 ~ "KY", state_fips == 22 ~ "LA", state_fips == 23 ~ "ME", state_fips == 24 ~ "MD",
                                            state_fips == 25 ~ "MA", state_fips == 26 ~ "MI", state_fips == 27 ~ "MN", state_fips == 28 ~ "MS",
                                            state_fips == 29 ~ "MO", state_fips == 30 ~ "MT", state_fips == 31 ~ "NE", state_fips == 32 ~ "NV",
                                            state_fips == 33 ~ "NH", state_fips == 34 ~ "NJ", state_fips == 35 ~ "NM", state_fips == 36 ~ "NY",
                                            state_fips == 37 ~ "NC", state_fips == 38 ~ "ND", state_fips == 39 ~ "OH", state_fips == 40 ~ "OK",
                                            state_fips == 41 ~ "OR", state_fips == 42 ~ "PA", state_fips == 44 ~ "RI",
                                            state_fips == 45 ~ "SC", state_fips == 46 ~ "SD", state_fips == 47 ~ "TN", state_fips == 48 ~ "TX",
                                            state_fips == 49 ~ "UT", state_fips == 50 ~ "VT", state_fips == 51 ~ "VA", state_fips == 53 ~ "WA",
                                            state_fips == 54 ~ "WV", state_fips == 55 ~ "WI", state_fips == 56 ~ "WY", 
                                            state_fips == 66 ~ "Guam", state_fips == 72 ~ "PR", state_fips == 78 ~ "US Virgin Islands",
                                            TRUE ~ NA_character_),
                marital = case_when(is.na(marital) | marital == 9 ~ NA_real_,
                                    marital == 1 ~ 1, TRUE ~ 0),
                cell_n_adults = case_when(year < 2014 | is.na(cell_n_adults) ~ NA_real_,
                                          cell_n_adults > 76 ~ NA_real_,
                                          TRUE ~ cell_n_adults)) -> data
data_backup <- data

### Formatting data to create analysis variables ####
#Creating new variables for analysis
#Coding - "race_analysis"
#   - Categorical "Hispanic", "NHWhite", "NHBlack", "NHOther" based on 'hisp' & 'race'
#Coding - "n_children_analysis"
#   - Categorical N children 0, 1, 2, 3+ based on 'n_children'
#Coding - "FMD"
#   - Binary 0=No, 1=Yes based on 'mental' >=14
#Coding - "n_persons"
#   - Calculated as N adults + N children based on 'n_adults' & 'n_children'
#Coding - "poverty_line"
#   - This guideline matches annual HHS poverty guidelines, = X amount for person 1 + Y amount per additional N persons
#   - Available at: (https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references)
#   - Note for 2016 the 'Y' value of the Y*N multiplier depends on N, =4140 for N=1-5, =4150 for N=6, =4160 for N=7
#      - Based on (https://aspe.hhs.gov/computations-2016-poverty-guidelines)
#Coding - "weight_2000"
#   - Based on weights in 'Klein & Schoenborn (2001) Age Adjustment Using the 2000 Projected US Population'
#   - Weights sum to 0.742266 excluding weights for those <18 (=0.257734)
#   - Those 18-24 have weight 0.029133 + 0.066478 (=0.095611) combining 18-19 and 20-24 category weights
#   - Those 80+ have weight 0.0174842 + 0.015508 (=0.0329922) combining 80-84 and 85+ category weights
#Coding - "weight_final" 
#   - PSID final weight multiplied by age standardization weight

data %>% mutate(race_analysis = ifelse(hisp == 1, "Hispanic", case_when(race == "White" ~ "NHWhite",
                                                                        race == "Black" ~ "NHBlack",
                                                                        race %in% c("Other", "NAAN", "AsianORPI", "Asian", "NHPI") ~ "NHOther",
                                                                        TRUE ~ NA_character_)),
                n_children_analysis = case_when(is.na(n_children) ~ NA_real_,
                                                n_children >=3 ~ 3, TRUE ~ n_children),
                FMD = ifelse(mental >= 14, 1, 0),
                weight_2000 = case_when(is.na(age) ~ NA_real_,
                                        age %in% c(18:24) ~ 0.095611,
                                        age %in% c(25:29) ~ 0.064530,
                                        age %in% c(30:34) ~ 0.071044,
                                        age %in% c(35:39) ~ 0.080762,
                                        age %in% c(40:44) ~ 0.081851,
                                        age %in% c(45:49) ~ 0.072118,
                                        age %in% c(50:54) ~ 0.062716,
                                        age %in% c(55:59) ~ 0.048454,
                                        age %in% c(60:64) ~ 0.038793,
                                        age %in% c(65:69) ~ 0.034264,
                                        age %in% c(70:74) ~ 0.031773,
                                        age %in% c(75:79) ~ 0.027000,
                                        age >=80 ~ 0.0329922, 
                                        TRUE ~ NA_real_),
                weight_final = ifelse(is.na(weight) | is.na(weight_2000), NA_real_, weight*weight_2000),
                n_persons = case_when(is.na(n_adults) & is.na(cell_n_adults) & is.na(n_children) ~ NA_real_, #Missing all people information
                                      is.na(n_adults) & is.na(cell_n_adults) & !is.na(n_children) ~ (1 + n_children), #Assuming 1 adult + children
                                      is.na(n_adults) & !is.na(cell_n_adults) & is.na(n_children) ~ cell_n_adults, #Assuming no children
                                      !is.na(n_adults) & is.na(cell_n_adults) & is.na(n_children) ~ n_adults, #Assuming no children
                                      !is.na(n_adults) & is.na(cell_n_adults) & !is.na(n_children) ~ (n_adults + n_children),
                                      is.na(n_adults) & !is.na(cell_n_adults) & !is.na(n_children) ~ (cell_n_adults + n_children),
                                      !is.na(n_adults) & !is.na(cell_n_adults) & is.na(n_children) ~ n_adults, #Preferencing regular Q, assuming no children
                                      !is.na(n_adults) & !is.na(cell_n_adults) & !is.na(n_children) ~ (n_adults + n_children)), #Preferencing regular Q
                poverty_line = ifelse(is.na(n_persons), NA_real_, case_when(year == 1993 ~ (6970 + ((n_persons-1)*2460)),
                                                                            year == 1994 ~ (7360 + ((n_persons-1)*2480)),
                                                                            year == 1995 ~ (7470 + ((n_persons-1)*2560)),
                                                                            year == 1996 ~ (7740 + ((n_persons-1)*2620)),
                                                                            year == 1997 ~ (7890 + ((n_persons-1)*2720)),
                                                                            year == 1998 ~ (8050 + ((n_persons-1)*2800)),
                                                                            year == 1999 ~ (8240 + ((n_persons-1)*2820)),
                                                                            year == 2000 ~ (8350 + ((n_persons-1)*2900)),
                                                                            year == 2001 ~ (8590 + ((n_persons-1)*3020)),
                                                                            year == 2002 ~ (8860 + ((n_persons-1)*3080)),
                                                                            year == 2003 ~ (8980 + ((n_persons-1)*3140)),
                                                                            year == 2004 ~ (9310 + ((n_persons-1)*3180)),
                                                                            year == 2005 ~ (9570 + ((n_persons-1)*3260)),
                                                                            year == 2006 ~ (9800 + ((n_persons-1)*3400)),
                                                                            year == 2007 ~ (10210 + ((n_persons-1)*3480)),
                                                                            year == 2008 ~ (10400 + ((n_persons-1)*3600)),
                                                                            year == 2009 ~ (10830 + ((n_persons-1)*3740)),
                                                                            year == 2010 ~ (10830 + ((n_persons-1)*3740)),
                                                                            year == 2011 ~ (10890 + ((n_persons-1)*3820)),
                                                                            year == 2012 ~ (11170 + ((n_persons-1)*3960)),
                                                                            year == 2013 ~ (11490 + ((n_persons-1)*4020)),
                                                                            year == 2014 ~ (11670 + ((n_persons-1)*4060)),
                                                                            year == 2015 ~ (11770 + ((n_persons-1)*4160)),
                                                                            year == 2016 & n_persons == 1 ~ 11880,
                                                                            year == 2016 & n_persons == 2 ~ 16020,
                                                                            year == 2016 & n_persons == 3 ~ 20160,
                                                                            year == 2016 & n_persons == 4 ~ 24300,
                                                                            year == 2016 & n_persons == 5 ~ 28440,
                                                                            year == 2016 & n_persons == 6 ~ 32580,
                                                                            year == 2016 & n_persons == 7 ~ 36740,
                                                                            year == 2016 & n_persons >= 8 ~ (11880 + ((n_persons-1)*4140)),
                                                                            year == 2017 ~ (12060 + ((n_persons-1)*4180)),
                                                                            year == 2018 ~ (12140 + ((n_persons-1)*4320)),
                                                                            year == 2019 ~ (12490 + ((n_persons-1)*4420)), TRUE ~ NA_real_))) -> data

#### Loading in & merging UKCPR data for State EITC information ####
ukcpr <- read_csv(paste0(directory, "ukcpr_data.csv"))
ukcpr <- ukcpr[ukcpr$year %in% c(1993:2019), c("state_name", "year", "State EITC Rate", "Refundable State EITC (1=Yes)")]
names(ukcpr) <- c("state_acronym", "year", "state_eitc_generosity", "state_eitc_availability")
ukcpr <- na.omit(ukcpr)

data <- merge(data, ukcpr, by = c("year", "state_acronym"), all.x = T)

#Changing State EITC to not available for Guam, PR, US Virgin Islands
data$state_eitc_availability[data$state_acronym %in% c("Guam", "PR", "US Virgin Islands")] <- 0
data$state_eitc_generosity[data$state_acronym %in% c("Guam", "PR", "US Virgin Islands")] <- 0

#### Saving created dataset ####
write_csv(data, paste0(directory, "mid_prep_data_050723.csv"))

#Total run-time
runtime <- Sys.time() - start
