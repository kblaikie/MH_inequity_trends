#### Aim 1 - Part A - Data Prep 3 (7) - 05 July 2023 ####
#Author - Kieran Blaikie
#Date - 05 July 2023

#Runtime - 52 minutes

# Note - This script uses the dataset 'mid_prep_data_050723.qs', which is identical 
#        to 'mid_prep_data_050723.csv' except 1) it's a .qs file, and 2) the variable
#        'unique_id' is actually a unique ID across years/rows. The variable in the
#        .RDS version is based on year-specific BRFSS 'SEQNO' or 'X_RECORD' variables
#        so is not helpful here once multiple years have been merged.

#Overview - This script creates strata-level summaries per year per outcome:
#             - Overall
#             - By 1-5 way intersections across race, ethnicity, gender,
#               education, and poverty status 
#             - By single intersectionaland 2-5-way stratifying 
#         - This script repeats the above 500 times, which can be used to estimate bootstrap SE and 95% LCI and UCI

#Changes - Compared to Data Prep 3(4), this script:
#             - Stratifies findings by race and ethnicity separately
#             - Uses the newly constructed random incomes from the 'Data Prep 2 (3)' script
#        - Compared to Data Prep 3(5), this script:
#             - Does not restrict based on upper age, as we're focusing on the entire adult population
#             - Uses the outcomes of those a priori socially most-advantaged as referent in inequity measures
#             - No longer restricts to those with known marital status, as this is only relevant for State EITC amounts
#        - Compared to Data Prep 3(6), this script:
#             - Uses the dataset 'mid_prep_data_050723.qs' instead of 'mid_prep_data_100123.qs'
#             - Replaces all mentions of the variable 'hsless' with the variable 'lesshs', which
#               categorizes respondents as <HS vs. >=HS instead of <=HS vs. >HS

#Loading packages
library(data.table)
library(magrittr)
library(tidyverse)
library(doParallel)
library(qs)

#Runtime start
start <- Sys.time()

#### Creating empty dataset at the group-level ####
#Creating indicator for all possible group combinations
groups <- data.frame(race = rep_len(rep(c("White", "Black"), each = 16), length.out = 32),
                     hisp = rep_len(rep(c(0,1), each = 8), length.out = 32),
                     male = rep_len(rep(c(0,1), each = 4), length.out = 32),
                     lesshs = rep_len(rep(c(0, 1), each = 2), length.out = 32),
                     in_poverty_element = rep_len(rep(c(0,1), each = 1), length.out = 32))
groups$intersectional_element <- paste0("lesshs", groups$lesshs, "_male", groups$male, "_pov", groups$in_poverty_element, 
                                        "_race", groups$race, "_hisp", groups$hisp)

#Creating empty frame for group-year summary estimates
group_year <- data.frame(race = NA_character_, hisp = NA_real_,
                         male = NA_real_, lesshs = NA_real_,
                         in_poverty_element = NA_real_, intersectional_element = NA_character_, 
                         year = NA_real_)[0,]

for (year in 1993:2019) {
  temp <- groups
  temp$year <- year
  group_year <- rbind(group_year, temp)
  rm(temp, year)
}
rm(groups)

#Creating extra needed variables
#Variable coding is as follows:
#CODING - 'boot'
#          - Indicator from 1:1000 for bootstrap resample, with 0 = the original sample
#CODING - 'n_X' (overall, intersectional, educ, gender, poverty, race)
#          - Number belonging to X group (or overall) in resample
#CODING - 'mu_X_poor_days' (overall, intersectional, educ, gender, poverty, race, hisp)
#          - Weighted average N mentally unhealthy days within X group (or overall) in resample
#CODING - 'mu_X_prev_FMD' (overall, intersectional, educ, gender, poverty, race, hisp)
#          - Weighted prevalence of FMD within X group (or overall) in resample

#Creating larger dataframe with 'boot' variable
group_year_reps <- group_year
group_year_reps$boot <- NA_real_
rm(group_year)

#Creating variables for every uni, bi, tri, and quadravariate combination of 
#race, hisp, 'gender', 'educ', and 'poverty' (+ overall and intersectional)
#for N per strata and mu per strata for FMD prevalence and mentally poor days
vars <- c("race", "hisp", "gender", "educ", "poverty")
combinations <- c()
for (i in 1:length(vars)) { #Creates a string vector with each combination
  combos <- combn(vars, i, simplify = FALSE)
  for (j in 1:length(combos)) {
    combinations <- c(combinations, paste(combos[[j]], collapse = "_"))
  }
}
rm(vars, combos, i, j)

#Creating 'n_' variables
group_year_reps[, c("n_overall", "n_intersectional")] <- NA_real_
for (i in 1:length(combinations)) {
  colname <- paste("n", combinations[i], sep = "_")
  group_year_reps[, colname] <- NA_real_
}

#Creating 'mu_' variables for FMD
group_year_reps[, c("FMD_mu_overall", "FMD_mu_intersectional")] <- NA_real_
for (i in 1:length(combinations)) {
  colname <- paste("FMD_mu", combinations[i], sep = "_")
  group_year_reps[, colname] <- NA_real_
}

#Creating 'mu_' variables for mentally poor days
group_year_reps[, c("days_mu_overall", "days_mu_intersectional")] <- NA_real_
for (i in 1:length(combinations)) {
  colname <- paste("days_mu", combinations[i], sep = "_")
  group_year_reps[, colname] <- NA_real_
}
rm(i, colname, combinations)

#Convert to data.table
group_year_reps <- as.data.table(group_year_reps)
group_year_reps[,c("n_race_hisp_gender_educ_poverty", 
                   "FMD_mu_race_hisp_gender_educ_poverty", 
                   "days_mu_race_hisp_gender_educ_poverty")] <- NULL

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading data
dataset <- qread(paste0(directory, "mid_prep_data_050723.qs"))

#Restricting to necessary variables then 'complete' observations
dataset <- dataset %>% select(unique_id, weight_final, year, age, n_persons, n_children, state_acronym, 
                              lesshs, male, race, hisp, income, mental, FMD) %>% na.omit()

#Creating variables needed to merge in OPM-MRI poverty threshold information
dataset %>% mutate(state = case_when(state_acronym == "United States" ~ "USA", 
                                     state_acronym == "AL" ~ "Alabama", state_acronym == "AK" ~ "Alaska", state_acronym == "AZ" ~ "Arizona", state_acronym == "AR" ~ "Arkansas", 
                                     state_acronym == "CA" ~ "California", state_acronym == "CO" ~ "Colorado", state_acronym == "CT" ~ "Connecticut", state_acronym == "DE" ~ "Delaware", 
                                     state_acronym == "DC" ~ "District of Columbia", state_acronym == "FL" ~ "Florida", state_acronym == "GA" ~ "Georgia", state_acronym == "HI" ~ "Hawaii", 
                                     state_acronym == "ID" ~ "Idaho", state_acronym == "IL" ~ "Illinois", state_acronym == "IN" ~ "Indiana", state_acronym == "IA" ~ "Iowa", 
                                     state_acronym == "KS" ~ "Kansas", state_acronym == "KY" ~ "Kentucky", state_acronym == "LA" ~ "Louisiana", state_acronym == "ME" ~ "Maine", 
                                     state_acronym == "MD" ~ "Maryland", state_acronym == "MA" ~ "Massachusetts", state_acronym == "MI" ~ "Michigan", state_acronym == "MN" ~ "Minnesota", 
                                     state_acronym == "MS" ~ "Mississippi", state_acronym == "MO" ~ "Missouri", state_acronym == "MT" ~ "Montana", state_acronym == "NE" ~ "Nebraska", 
                                     state_acronym == "NV" ~ "Nevada", state_acronym == "NH" ~ "New Hampshire", state_acronym == "NJ" ~ "New Jersey", state_acronym == "NM" ~ "New Mexico", 
                                     state_acronym == "NY" ~ "New York", state_acronym == "NC" ~ "North Carolina", state_acronym == "ND" ~ "North Dakota", state_acronym == "OH" ~ "Ohio", 
                                     state_acronym == "OK" ~ "Oklahoma", state_acronym == "OR" ~ "Oregon", state_acronym == "PA" ~ "Pennsylvania", state_acronym == "PR" ~ "Puerto Rico",
                                     state_acronym == "Guam" ~ "Guam", state_acronym == "RI" ~ "Rhode Island", state_acronym == "SC" ~ "South Carolina", state_acronym == "SD" ~ "South Dakota", 
                                     state_acronym == "TN" ~ "Tennessee", state_acronym == "TX" ~ "Texas", state_acronym == "UT" ~ "Utah", state_acronym == "VT" ~ "Vermont", 
                                     state_acronym == "US Virgin Islands" ~ "Virgin Islands", state_acronym == "VA" ~ "Virginia", state_acronym == "WA" ~ "Washington", 
                                     state_acronym == "WV" ~ "West Virginia", state_acronym == "WI" ~ "Wisconsin", state_acronym == "WY" ~ "Wyoming", TRUE ~ "Mistake")) -> dataset

dataset$n_persons[dataset$n_persons == 0 & !is.na(dataset$n_persons)] <- 1 #Correcting issue 
dataset %>% mutate(age_th = ifelse(n_persons > 2 | age < 65, "u65", "o65"), #OPM thresholds only vary by age for households with <=2 persons
                   n_persons_th = ifelse(n_persons >9, 9, n_persons),
                   n_children_th = ifelse(n_children > 8, 8, n_children)) -> dataset

#Restricting to those Black or White and from the 50 US States or DC
dataset <- dataset[dataset$state != "Guam" & dataset$state != "Puerto Rico" & dataset$state != "Virgin Islands" & (dataset$race == "White" | dataset$race == "Black"), ]

#Merging in threshold information
thresholds <- read_csv(paste0(directory, "state_thresholds 01 Mar 2023.csv"))
thresholds <- thresholds[,c(1:5,9)]
names(thresholds) <- c("state", "year", "age_th", "n_persons_th", "n_children_th", "poverty_line")
dataset <- merge(dataset[,c(1:3,8:18)], thresholds, by = c("state", "year", "age_th", "n_persons_th", "n_children_th"), all.x = T)
rm(thresholds)

#Converting to data.table for faster processing
dataset <- as.data.table(dataset)
dataset[, ':=' (year = as.integer(year),
                n_persons_th = as.integer(n_persons_th),
                n_children_th = as.integer(n_children_th),
                lesshs = as.integer(lesshs),
                male = as.integer(male),
                hisp = as.integer(hisp),
                poverty_line = as.integer(poverty_line),
                mental = as.integer(mental),
                FMD = as.integer(FMD))]

#### Stratified bootstrap re-sampling and summary estimate aggregation ####
#NOTE - This section involves:
#         1) Creating a temporary variables used/relevant to stratified resampling
#         2) Restrict to those with 'complete' data
#         3) Create 500 resampled datasets (each 'boot')
#         4) Computing group-level summary variables within resample
#         5) Populating 'group_year_reps' with group summary estimates

#Creating function to: 1) Resample original data, 
#                      2) Calculate weighted strata estimates, and 
#                      3) Populate group summary dataset
boot_fun <- function(data, type) {
  #Loading packages within each thread
  library(tidyverse)
  library(data.table)
  library(qs)
  library(magrittr)
  
  #Set seed for replicability based on boot
  set.seed((2023*boot))
  
  #0) Creating temporary dataset from 'group_year_reps' with boot = boot
  temp <- group_year_reps
  temp$boot <- boot
  
  #1) Load in income_cont needed for in_poverty_element and intersectional_element used in stratified resampling
  imp <- qread(paste0(directory, "Repetitions/260423_income_", boot, ".qs"))
  names(data)[12] <- "income_cat"
  data <- merge(data, imp, by = c("unique_id", "income_cat"), all.x = T, all.y = F)
  rm(imp)
  
  data[, in_poverty_element := fifelse(income_cont < poverty_line, 1, 0)]
  data[, intersectional_element := paste0("lesshs", lesshs, "_male", male, "_pov", in_poverty_element, 
                                          "_race", race, "_hisp", hisp)]  
  
  #3) Creating resample of data
  resample <- data[, .SD[sample(.N, size = .N, replace = T)], by = .(year, intersectional_element)]
  resample[, boot := boot]
  rm(data)
  
  #4) Computing group-level summary variables within resample
  #Overall
  resample[, ':=' (n_overall = .N,
                   days_mu_overall = weighted.mean(mental, weight_final),
                   FMD_mu_overall = weighted.mean(FMD, weight_final)), by = .(year)]
  #Fully intersectionally
  resample[, ':=' (n_intersectional = .N,
                   days_mu_intersectional = weighted.mean(mental, weight_final),
                   FMD_mu_intersectional = weighted.mean(FMD, weight_final)), by = .(year, intersectional_element)]
  
  #By univariate stratifications
  resample[, ':=' (n_race = .N,
                   days_mu_race = weighted.mean(mental, weight_final),
                   FMD_mu_race = weighted.mean(FMD, weight_final)), by = .(year, race)]
  
  resample[, ':=' (n_hisp = .N,
                   days_mu_hisp = weighted.mean(mental, weight_final),
                   FMD_mu_hisp = weighted.mean(FMD, weight_final)), by = .(year, hisp)]
  
  resample[, ':=' (n_gender = .N,
                   days_mu_gender = weighted.mean(mental, weight_final),
                   FMD_mu_gender = weighted.mean(FMD, weight_final)), by = .(year, male)]
  
  resample[, ':=' (n_educ = .N,
                   days_mu_educ = weighted.mean(mental, weight_final),
                   FMD_mu_educ = weighted.mean(FMD, weight_final)), by = .(year, lesshs)]
  
  resample[, ':=' (n_poverty = .N,
                   days_mu_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_poverty = weighted.mean(FMD, weight_final)), by = .(year, in_poverty_element)]
  
  #By bivariate stratifications
  resample[, ':=' (n_race_hisp = .N,
                   days_mu_race_hisp = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp = weighted.mean(FMD, weight_final)), by = .(year, race, hisp)]
  
  resample[, ':=' (n_race_gender = .N,
                   days_mu_race_gender = weighted.mean(mental, weight_final),
                   FMD_mu_race_gender = weighted.mean(FMD, weight_final)), by = .(year, race, male)]
  
  resample[, ':=' (n_race_educ = .N,
                   days_mu_race_educ = weighted.mean(mental, weight_final),
                   FMD_mu_race_educ = weighted.mean(FMD, weight_final)), by = .(year, race, lesshs)]
  
  resample[, ':=' (n_race_poverty = .N,
                   days_mu_race_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, in_poverty_element)]
  
  resample[, ':=' (n_hisp_gender = .N,
                   days_mu_hisp_gender = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_gender = weighted.mean(FMD, weight_final)), by = .(year, hisp, male)]
  
  resample[, ':=' (n_hisp_educ = .N,
                   days_mu_hisp_educ = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_educ = weighted.mean(FMD, weight_final)), by = .(year, hisp, lesshs)]
  
  resample[, ':=' (n_hisp_poverty = .N,
                   days_mu_hisp_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_poverty = weighted.mean(FMD, weight_final)), by = .(year, hisp, in_poverty_element)]
  
  resample[, ':=' (n_gender_educ = .N,
                   days_mu_gender_educ = weighted.mean(mental, weight_final),
                   FMD_mu_gender_educ = weighted.mean(FMD, weight_final)), by = .(year, male, lesshs)]
  
  resample[, ':=' (n_gender_poverty = .N,
                   days_mu_gender_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_gender_poverty = weighted.mean(FMD, weight_final)), by = .(year, male, in_poverty_element)]
  
  resample[, ':=' (n_educ_poverty = .N,
                   days_mu_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, lesshs, in_poverty_element)]
  
  #By trivariate stratifications
  resample[, ':=' (n_race_hisp_gender = .N,
                   days_mu_race_hisp_gender = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_gender = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, male)]
  
  resample[, ':=' (n_race_hisp_educ = .N,
                   days_mu_race_hisp_educ = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_educ = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, lesshs)]
  
  resample[, ':=' (n_race_hisp_poverty = .N,
                   days_mu_race_hisp_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, in_poverty_element)]
  
  resample[, ':=' (n_race_gender_educ = .N,
                   days_mu_race_gender_educ = weighted.mean(mental, weight_final),
                   FMD_mu_race_gender_educ = weighted.mean(FMD, weight_final)), by = .(year, race, male, lesshs)]
  
  resample[, ':=' (n_race_gender_poverty = .N,
                   days_mu_race_gender_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_gender_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, male, in_poverty_element)]
  
  resample[, ':=' (n_race_educ_poverty = .N,
                   days_mu_race_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, lesshs, in_poverty_element)]
  
  resample[, ':=' (n_hisp_gender_educ = .N,
                   days_mu_hisp_gender_educ = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_gender_educ = weighted.mean(FMD, weight_final)), by = .(year, hisp, male, lesshs)]
  
  resample[, ':=' (n_hisp_gender_poverty = .N,
                   days_mu_hisp_gender_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_gender_poverty = weighted.mean(FMD, weight_final)), by = .(year, hisp, male, in_poverty_element)]
  
  resample[, ':=' (n_hisp_educ_poverty = .N,
                   days_mu_hisp_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, hisp, lesshs, in_poverty_element)]
  
  resample[, ':=' (n_gender_educ_poverty = .N,
                   days_mu_gender_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_gender_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, male, lesshs, in_poverty_element)]
  
  #By quadra-variate stratifications
  resample[, ':=' (n_race_hisp_gender_educ = .N,
                   days_mu_race_hisp_gender_educ = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_gender_educ = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, male, lesshs)]
  
  resample[, ':=' (n_race_hisp_gender_poverty = .N,
                   days_mu_race_hisp_gender_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_gender_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, male, in_poverty_element)]
  
  resample[, ':=' (n_race_hisp_educ_poverty = .N,
                   days_mu_race_hisp_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_hisp_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, hisp, lesshs, in_poverty_element)]
  
  resample[, ':=' (n_race_gender_educ_poverty = .N,
                   days_mu_race_gender_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_race_gender_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, race, male, lesshs, in_poverty_element)]
  
  resample[, ':=' (n_hisp_gender_educ_poverty = .N,
                   days_mu_hisp_gender_educ_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_hisp_gender_educ_poverty = weighted.mean(FMD, weight_final)), by = .(year, hisp, male, lesshs, in_poverty_element)]
  
  #Selecting needed columns
  resample <- resample[, .(
    #Identifier variables
    boot, year, intersectional_element, race, hisp, male, lesshs, in_poverty_element, 
    #n_variables
    #Overall
    n_overall,
    #Fully intersectional
    n_intersectional,
    #Univariate
    n_race, n_hisp, n_gender, n_educ, n_poverty,
    #Bivariate
    n_race_hisp, n_race_gender, n_race_educ, n_race_poverty, 
    n_hisp_gender, n_hisp_educ, n_hisp_poverty,
    n_gender_educ, n_gender_poverty,
    n_educ_poverty,
    #Trivariate
    n_race_hisp_gender, n_race_hisp_educ, n_race_hisp_poverty, 
    n_race_gender_educ, n_race_gender_poverty,
    n_race_educ_poverty,
    n_hisp_gender_educ, n_hisp_gender_poverty, 
    n_hisp_educ_poverty,
    n_gender_educ_poverty,
    #Quadra-variate
    n_race_hisp_gender_educ, n_race_hisp_gender_poverty, 
    n_race_hisp_educ_poverty, n_race_gender_educ_poverty,
    n_hisp_gender_educ_poverty,
    #days_ variables
    #Overall
    days_mu_overall,
    #Fully intersectinoal
    days_mu_intersectional,
    #Univariate
    days_mu_race, days_mu_hisp, days_mu_gender, days_mu_educ, days_mu_poverty,
    #Bivariate
    days_mu_race_hisp, days_mu_race_gender, days_mu_race_educ, days_mu_race_poverty, 
    days_mu_hisp_gender, days_mu_hisp_educ, days_mu_hisp_poverty,
    days_mu_gender_educ, days_mu_gender_poverty,
    days_mu_educ_poverty,
    #Trivariate
    days_mu_race_hisp_gender, days_mu_race_hisp_educ, days_mu_race_hisp_poverty, 
    days_mu_race_gender_educ, days_mu_race_gender_poverty,
    days_mu_race_educ_poverty,
    days_mu_hisp_gender_educ, days_mu_hisp_gender_poverty, 
    days_mu_hisp_educ_poverty,
    days_mu_gender_educ_poverty,
    #Quadra-variate
    days_mu_race_hisp_gender_educ, days_mu_race_hisp_gender_poverty, 
    days_mu_race_hisp_educ_poverty, days_mu_race_gender_educ_poverty,
    days_mu_hisp_gender_educ_poverty,
    #FMD_ variables
    #Overall
    FMD_mu_overall,
    #Fully intersectinoal
    FMD_mu_intersectional,
    #Univariate
    FMD_mu_race, FMD_mu_hisp, FMD_mu_gender, FMD_mu_educ, FMD_mu_poverty,
    #Bivariate
    FMD_mu_race_hisp, FMD_mu_race_gender, FMD_mu_race_educ, FMD_mu_race_poverty, 
    FMD_mu_hisp_gender, FMD_mu_hisp_educ, FMD_mu_hisp_poverty,
    FMD_mu_gender_educ, FMD_mu_gender_poverty,
    FMD_mu_educ_poverty,
    #Trivariate
    FMD_mu_race_hisp_gender, FMD_mu_race_hisp_educ, FMD_mu_race_hisp_poverty, 
    FMD_mu_race_gender_educ, FMD_mu_race_gender_poverty,
    FMD_mu_race_educ_poverty,
    FMD_mu_hisp_gender_educ, FMD_mu_hisp_gender_poverty, 
    FMD_mu_hisp_educ_poverty,
    FMD_mu_gender_educ_poverty,
    #Quadra-variate
    FMD_mu_race_hisp_gender_educ, FMD_mu_race_hisp_gender_poverty, 
    FMD_mu_race_hisp_educ_poverty, FMD_mu_race_gender_educ_poverty,
    FMD_mu_hisp_gender_educ_poverty)] %>% distinct()
  grouped_data <- resample
  rm(resample)
  
  #5) Populating 'group_year_reps' with group summary estimates
  setcolorder(grouped_data, names(temp))
  temp[grouped_data, c(colnames(temp)[c(9:104)]) := mget(paste0("i.", colnames(temp)[c(9:104)])), on = colnames(temp)[c(1:8)]]
  
  return(temp)
}

#### Performing analysis using 'boot_fun' function ####
#Whole population
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_all"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "all"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)

#Setting all aggregated datasets to data.frames
data_aggregated_all <- as.data.frame(data_aggregated_all)

#Re-ordering dataset rows
data_aggregated_all <- data_aggregated_all %>% arrange(boot, year, intersectional_element)

#### Saving summary-level estimate dataset ####
qsave(x = data_aggregated_all, file = paste0(directory, "data_aggregated_partA_050723.qs"))

#Runtime end
duration <- Sys.time() - start