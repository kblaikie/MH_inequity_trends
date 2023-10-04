#### Aim 1 - Part B - Data Prep 3 (7) - 10 Sep 2023 ####
#Author - Kieran Blaikie
#Date - 10 Sep 2023

#Runtime - 1 hour 13 minutes

# Note - This script uses the dataset 'mid_prep_data_050723.qs', which is identical 
#        to 'mid_prep_data_050723.csv' except 1) it's a .qs file, and 2) the variable
#        'unique_id' is actually a unique ID across years/rows. The variable in the
#        .RDS version is based on year-specific BRFSS 'SEQNO' or 'X_RECORD' variables
#        so is not helpful here once multiple years have been merged.

#Overview - This script creates strata-level summaries per year per outcome:
#             1) Overall
#             2) By State EITC availability (No, Yes)
#                 - Overall
#                 - By 1-3 way intersections across race and ethnicity, sex, and poverty status
#             3) By State EITC receipt strata (No in non-State EITC state while not Federal EITC eligible, 
#                                              No in non-State EITC state while Federal EITC eligible,
#                                              No in State EITC state, Yes in State EITC state)
#                 - Overall
#                 - By 1-3 way intersections across race and ethnicity, sex, and poverty status
#             4) By State EITC generosity as a fraction of the Federal EITC (<= 1993-2019 Median, > 1993-2019 Median)
#                 - Overall
#                 - By 1-3 way intersections across race and ethnicity, sex, and poverty status
#         - This script repeats the above 500 times, which can be used to estimate bootstrap SE and 95% LCI and UCI

#Changes - Compared to Data Prep 3(4), this script:
#             - Stratifies findings by race and ethnicity separately
#             - Uses the newly constructed random incomes from the 'Data Prep 2 (3)' script
#        - Compared to Data Prep 3(5), this script:
#             - Uses the outcomes of those a priori socially most-advantaged as referent in inequity measures
#             - Uses the dataset 'mid_prep_data_050723.qs' instead of 'mid_prep_data_100123.qs'
#        - Compared to Data Prep 3(6), this script:
#             - Restricts to the year range 2000-2019
#             - Uses the 2000-2019 median State EITC generosity as a fraction of the Federal EITC (among states with a State EITC policy)

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
groups <- data.frame(racehisp = rep_len(rep(c("NHWhite", "NHBlack", "Hispanic"), each = 4), length.out = 12),
                     male = rep_len(rep(c(0,1), each = 2), length.out = 12),
                     in_poverty_element = rep_len(rep(c(0,1), each = 1), length.out = 12))
groups$intersectional_element <- paste0("racehisp", groups$racehisp, "_male", groups$male, "_pov", groups$in_poverty_element)

#Creating empty frame for group-year summary estimates
group_year <- data.frame(racehisp = NA_character_, male = NA_real_, in_poverty_element = NA_real_, 
                         intersectional_element = NA_character_, year = NA_real_)[0,]

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
#CODING - 'n_X' (overall, intersectional, sex, poverty, race/ethnicity)
#          - Number belonging to X group (or overall) in resample
#CODING - 'mu_X_poor_days' (overall, intersectional, sex, poverty, race/ethnicity)
#          - Weighted average N mentally unhealthy days within X group (or overall) in resample
#CODING - 'mu_X_prev_FMD' (overall, intersectional, sex, poverty, race/ethnicity)
#          - Weighted prevalence of FMD within X group (or overall) in resample

#Creating larger dataframe with 'boot' variable
group_year_reps <- group_year
group_year_reps$boot <- NA_real_
rm(group_year)

#Creating variables for every uni, bi, and trivariate combination of 
#race/ethnicity, 'sex', and 'poverty' (+ overall and intersectional)
#for N per strata and mu per strata for FMD prevalence and mentally poor days
vars <- c("racehisp", "sex", "poverty")
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
group_year_reps[,c("n_racehisp_sex_poverty", 
                   "FMD_mu_racehisp_sex_poverty", 
                   "days_mu_racehisp_sex_poverty")] <- NULL

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading data
dataset <- qread(paste0(directory, "mid_prep_data_050723.qs"))

#Restricting to necessary variables then 'complete' observations
dataset <- dataset %>% select(unique_id, weight_final, year, age, marital, n_persons, n_children, state_acronym, 
                              male, race, hisp, income, mental, FMD) %>% na.omit()

#Restricting to those aged 18-64
dataset <- dataset[dataset$age %in% c(18:64),]

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

#Restricting to the year range 2000-2019
dataset <- dataset[dataset$year >= 2000, ]

#Merging in threshold information
thresholds <- read_csv(paste0(directory, "state_thresholds 01 Mar 2023.csv"))
thresholds <- thresholds[,c(1:5,9)]
names(thresholds) <- c("state", "year", "age_th", "n_persons_th", "n_children_th", "poverty_line")
dataset <- merge(dataset[,c(1:3,8:18)], thresholds, by = c("state", "year", "age_th", "n_persons_th", "n_children_th"), all.x = T)
rm(thresholds)

#Merging in State EITC availability and generosity information
state_eitc <- read_csv(paste0(directory, "state_eitc_data_030323.csv"))
state_eitc <- state_eitc[state_eitc$year >= 2000, ]
state_eitc$prior_eitc_generosity_moremedian <- ifelse(state_eitc$prior_eitc_generosity > quantile(state_eitc$prior_eitc_generosity[state_eitc$prior_eitc_any == 1], 0.5, na.rm = T), 1, 0)
dataset <- merge(dataset, state_eitc[,c("state_acronym", "year", "prior_eitc_any", "prior_eitc_generosity", "prior_eitc_generosity_moremedian")], by = c("state_acronym", "year"), all.x = T)
names(dataset)[length(names(dataset))-2] <- "any_state_eitc"
names(dataset)[length(names(dataset))-1] <- "state_eitc_generosity"
names(dataset)[length(names(dataset))] <- "state_eitc_generosity_moremedian"
rm(state_eitc)

#Converting to data.table for faster processing
dataset <- as.data.table(dataset)
dataset[, ':=' (year = as.integer(year),
                n_persons_th = as.integer(n_persons_th),
                n_children_th = as.integer(n_children_th),
                male = as.integer(male),
                hisp = as.integer(hisp),
                poverty_line = as.integer(poverty_line),
                mental = as.integer(mental),
                FMD = as.integer(FMD),
                any_state_eitc = as.integer(any_state_eitc))]
dataset[, racehisp := fifelse(hisp == 1, "Hispanic",
                              fifelse(race == "White", "NHWhite", "NHBlack"))]

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
  data[, intersectional_element := paste0("racehisp", racehisp, "_male", male, "_pov", in_poverty_element)]  
  
  #2) Create State EITC receipt variable and subset dataset based on 'type'
  #'type' options will include: 
  #   'all',
  #   'state_eitc_no', 'state_eitc_yes', 'state_eitc_no_federal_no', 'state_eitc_no_federal_yes', 
  #   'state_eitc_yes_receipt_no', 'state_eitc_yes_receipt_yes', 
  #   'state_eitc_yes_receipt_yes_moremedian_no', 'state_eitc_yes_receipt_yes_moremedian_yes'
  
  if (type == "all") {}
  if (type == "state_eitc_no") {
    data <- data[any_state_eitc == 0, ]
  }
  if (type == "state_eitc_yes") {
    data <- data[any_state_eitc == 1, ]
  }
  if (type %in% c("state_eitc_no_federal_no", "state_eitc_no_federal_yes",
                  "state_eitc_yes_receipt_no", "state_eitc_yes_receipt_yes",
                  "state_eitc_yes_receipt_yes_moremedian_no",
                  "state_eitc_yes_receipt_yes_moremedian_yes")) {
    imp_state <- qread(paste0(directory, "Repetitions/090523_eitc_", boot, ".qs"))
    data <- merge(data, imp_state, by = c("unique_id"), all.x = T, all.y = F)
    imp_fed <- qread(paste0(directory, "Repetitions/090523_federal_eitc_", boot, ".qs"))
    data <- merge(data, imp_fed, by = c("unique_id"), all.x = T, all.y = F)
    
    data[, state_eitc_receipt := fifelse(round(state_eitc_amount,0) >= 1, 1, 0)] 
    data[, federal_eitc_receipt := fifelse(round(eitc_amount,0) >= 1, 1, 0)] 
    
    if (type == "state_eitc_no_federal_no") {
      data <- data[any_state_eitc == 0 & federal_eitc_receipt == 0, ]
    }
    if (type == "state_eitc_no_federal_yes") {
      data <- data[any_state_eitc == 0 & federal_eitc_receipt == 1, ]
    }
    if (type == "state_eitc_yes_receipt_no") {
      data <- data[any_state_eitc == 1 & state_eitc_receipt == 0, ]
    }
    if (type == "state_eitc_yes_receipt_yes") {
      data <- data[any_state_eitc == 1 & state_eitc_receipt == 1, ]
    }
    if (type == "state_eitc_yes_receipt_yes_moremedian_no") {
      data <- data[any_state_eitc == 1 & state_eitc_receipt == 1 & state_eitc_generosity_moremedian == 0, ]
    }
    if (type == "state_eitc_yes_receipt_yes_moremedian_yes") {
      data <- data[any_state_eitc == 1 & state_eitc_receipt == 1 & state_eitc_generosity_moremedian == 1, ]
    }
  }
  
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
  resample[, ':=' (n_racehisp = .N,
                   days_mu_racehisp = weighted.mean(mental, weight_final),
                   FMD_mu_racehisp = weighted.mean(FMD, weight_final)), by = .(year, racehisp)]
  
  resample[, ':=' (n_sex = .N,
                   days_mu_sex = weighted.mean(mental, weight_final),
                   FMD_mu_sex = weighted.mean(FMD, weight_final)), by = .(year, male)]
  
  resample[, ':=' (n_poverty = .N,
                   days_mu_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_poverty = weighted.mean(FMD, weight_final)), by = .(year, in_poverty_element)]
  
  #By bivariate stratifications
  resample[, ':=' (n_racehisp_sex = .N,
                   days_mu_racehisp_sex = weighted.mean(mental, weight_final),
                   FMD_mu_racehisp_sex = weighted.mean(FMD, weight_final)), by = .(year, racehisp, male)]
  
  resample[, ':=' (n_racehisp_poverty = .N,
                   days_mu_racehisp_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_racehisp_poverty = weighted.mean(FMD, weight_final)), by = .(year, racehisp, in_poverty_element)]
  
  resample[, ':=' (n_sex_poverty = .N,
                   days_mu_sex_poverty = weighted.mean(mental, weight_final),
                   FMD_mu_sex_poverty = weighted.mean(FMD, weight_final)), by = .(year, male, in_poverty_element)]
  
  #Selecting needed columns
  resample <- resample[, .(
    #Identifier variables
    boot, year, intersectional_element, racehisp, male, in_poverty_element, 
    #n_variables
    #Overall
    n_overall,
    #Fully intersectional
    n_intersectional,
    #Univariate
    n_racehisp, n_sex, n_poverty,
    #Bivariate
    n_racehisp_sex, n_racehisp_poverty, n_sex_poverty, 
    #days_ variables
    #Overall
    days_mu_overall,
    #Fully intersectional
    days_mu_intersectional,
    #Univariate
    days_mu_racehisp, days_mu_sex, days_mu_poverty,
    #Bivariate
    days_mu_racehisp_sex, days_mu_racehisp_poverty, days_mu_sex_poverty, 
    #FMD_ variables
    #Overall
    FMD_mu_overall,
    #Fully intersectional
    FMD_mu_intersectional,
    #Univariate
    FMD_mu_racehisp, FMD_mu_sex, FMD_mu_poverty,
    #Bivariate
    FMD_mu_racehisp_sex, FMD_mu_racehisp_poverty, FMD_mu_sex_poverty)] %>% distinct()
  grouped_data <- resample
  rm(resample)
  
  #5) Populating 'group_year_reps' with group summary estimates
  setcolorder(grouped_data, names(temp))
  temp[grouped_data, c(colnames(temp)[c(7:30)]) := mget(paste0("i.", colnames(temp)[c(7:30)])), on = colnames(temp)[c(1:6)]]
  
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
names(data_aggregated_all)[c(7:30)] <- paste(names(data_aggregated_all)[c(7:30)], "_all", sep = "")

#No State EITC States
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_no"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_no"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_no)[c(7:30)] <- paste(names(data_aggregated_state_eitc_no)[c(7:30)], "_state_eitc_no", sep = "")

#State EITC States
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_yes"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_yes"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_yes)[c(7:30)] <- paste(names(data_aggregated_state_eitc_yes)[c(7:30)], "_state_eitc_yes", sep = "")

#No State EITC States (Individuals not receiving a Federal EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_no_federal_no"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_no_federal_no"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_no_federal_no)[c(7:30)] <- paste(names(data_aggregated_state_eitc_no_federal_no)[c(7:30)], "_state_eitc_no_federal_no", sep = "")

#No State EITC States (Individuals receiving a Federal EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_no_federal_yes"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_no_federal_yes"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_no_federal_yes)[c(7:30)] <- paste(names(data_aggregated_state_eitc_no_federal_yes)[c(7:30)], "_state_eitc_no_federal_yes", sep = "")

#State EITC States (Individuals not receiving a State EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_yes_receipt_no"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_yes_receipt_no"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_yes_receipt_no)[c(7:30)] <- paste(names(data_aggregated_state_eitc_yes_receipt_no)[c(7:30)], "_state_eitc_yes_receipt_no", sep = "")

#State EITC States (Individuals receiving a State EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_yes_receipt_yes"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_yes_receipt_yes"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_yes_receipt_yes)[c(7:30)] <- paste(names(data_aggregated_state_eitc_yes_receipt_yes)[c(7:30)], "_state_eitc_yes_receipt_yes", sep = "")

#State EITC States with <= 1993-2019 Median State EITC Generosity as % of Federal EITC (Individuals receiving a State EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_yes_receipt_yes_moremedian_no"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_yes_receipt_yes_moremedian_no"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_yes_receipt_yes_moremedian_no)[c(7:30)] <- paste(names(data_aggregated_state_eitc_yes_receipt_yes_moremedian_no)[c(7:30)], "_state_eitc_yes_receipt_yes_moremedian_no", sep = "")

#State EITC States with > 1993-2019 Median State EITC Generosity as % of Federal EITC (Individuals receiving a State EITC)
n_clusters <- detectCores()-1
clusters <- parallel::makeCluster(n_clusters)
doParallel::registerDoParallel(clusters)
assign(paste0("data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes"), foreach(boot = 1:500, .combine = rbind) %dopar% boot_fun(data = dataset, type = "state_eitc_yes_receipt_yes_moremedian_yes"))
parallel::stopCluster(clusters)
rm(n_clusters, clusters)
names(data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes)[c(7:30)] <- paste(names(data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes)[c(7:30)], "_state_eitc_yes_receipt_yes_moremedian_yes", sep = "")

#Setting all aggregated datasets to data.frames
data_aggregated_all <- as.data.frame(data_aggregated_all)
data_aggregated_state_eitc_no <- as.data.frame(data_aggregated_state_eitc_no)
data_aggregated_state_eitc_yes <- as.data.frame(data_aggregated_state_eitc_yes)
data_aggregated_state_eitc_no_federal_no <- as.data.frame(data_aggregated_state_eitc_no_federal_no)
data_aggregated_state_eitc_no_federal_yes <- as.data.frame(data_aggregated_state_eitc_no_federal_yes)
data_aggregated_state_eitc_yes_receipt_no <- as.data.frame(data_aggregated_state_eitc_yes_receipt_no)
data_aggregated_state_eitc_yes_receipt_yes <- as.data.frame(data_aggregated_state_eitc_yes_receipt_yes)
data_aggregated_state_eitc_yes_receipt_yes_moremedian_no <- as.data.frame(data_aggregated_state_eitc_yes_receipt_yes_moremedian_no)
data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes <- as.data.frame(data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes)

#Re-ordering dataset rows
data_aggregated_all <- data_aggregated_all %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_no <- data_aggregated_state_eitc_no %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_yes <- data_aggregated_state_eitc_yes %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_no_federal_no <- data_aggregated_state_eitc_no_federal_no %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_no_federal_yes <- data_aggregated_state_eitc_no_federal_yes %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_yes_receipt_no <- data_aggregated_state_eitc_yes_receipt_no %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_yes_receipt_yes <- data_aggregated_state_eitc_yes_receipt_yes %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_yes_receipt_yes_moremedian_no <- data_aggregated_state_eitc_yes_receipt_yes_moremedian_no %>% arrange(boot, year, intersectional_element)
data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes <- data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes %>% arrange(boot, year, intersectional_element)

#Combining datasets
data_aggregated <- cbind(data_aggregated_all, 
                         data_aggregated_state_eitc_no[, c(7:30)],
                         data_aggregated_state_eitc_yes[, c(7:30)],
                         data_aggregated_state_eitc_no_federal_no[, c(7:30)],
                         data_aggregated_state_eitc_no_federal_yes[, c(7:30)],
                         data_aggregated_state_eitc_yes_receipt_no[, c(7:30)],
                         data_aggregated_state_eitc_yes_receipt_yes[, c(7:30)],
                         data_aggregated_state_eitc_yes_receipt_yes_moremedian_no[, c(7:30)],
                         data_aggregated_state_eitc_yes_receipt_yes_moremedian_yes[, c(7:30)])

#### Saving summary-level estimate dataset ####
qsave(x = data_aggregated, file = paste0(directory, "data_aggregated_partB_100923.qs"))

#Runtime end
duration <- Sys.time() - start