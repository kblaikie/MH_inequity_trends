#### Aim 1 Data Prep 5 (3) - 05 July 2023 ####
#Author - Kieran Blaikie
#Date - 05 July 2023

#Runtime - 9 minutes

#Changes - Comared to 'Data Visualisation 1(3)', this script:
#           1) Does not create graphs
#           2) Is based on different social stratifications (race and ethnicity separately, only Black and White race)
#        - Compared to 'Data Prep 5 (1)', this script:
#           1) Only creates figures for the overall population, not State EITC strata
#           2) Suppresses all strata estimates where: 1) N is < 50 or 2) the RSE is >50%
#           3) Uses three comparators for difference measures: 1) those a priori the most privileged across strata or 2) the national average
#        - Compared to 'Part A Data Prep 5 (2)', this script:
#           1) Uses the dataset 'data_aggregated_partA_050723.qs' generated using the script 'Part A Data Prep 3(7)'
#           2) Replaces all mentions of 'hsless' which dichotomized education as <=HS vs. >HS, with 'lesshs' which dichotomizes education as <HS vs. >=HS

#Overview - This script:
#             2) Estimates unweighted average outcomes across strata and difference measures vs. those averages within bootstraps
#             3) Creates bootstrap average figures, SE, LCI, and UCI measures across 500 reps
#             4) Saves this more-aggregated dataset as 'data_final_120523.qs'

#Loading packages
library(tidyverse)
library(data.table)
library(magrittr)
library(qs)

#Runtime start
start <- Sys.time()

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading data
data <- qread(paste0(directory, "data_aggregated_partA_050723.qs"))
data <- data[,c(8,1:7,9:104)]

#Renaming and reformatting
names(data)[6] <- "in_poverty"
names(data)[7] <- "intersectional"

data %>% mutate(lesshs = as.factor(lesshs),
                male = as.factor(male),
                in_poverty = as.factor(in_poverty),
                race = as.factor(race),
                hisp = as.factor(hisp)) -> data

#### Step 0) Set to 0 all year and boot-specific N where it is currently NA ####
for (col in c(9:40)) {
  var <- names(data)[col]
  eval(parse(text = paste0("data$", var, " <- ifelse(is.na(data$", var, "), 0, data$", var, ")")))
}
rm(col, var)

#### Step 1) Exclude 2002 measures as mental health was not included in the main BRFSS survey ####
data <- data[data$year != 2002, ]

#### Step 2) Creating sociall privileged reference variables ####
#NOTES - For socially privileged referents, those 1) male, 2) >=HS, 3) Not in Poverty, 4) White, and 5) Non-Hispanic are used
strat <- substr(names(data)[10:40], start = 3, stop = 26)
privileged_strat <- "lesshs0_male1_pov0_raceWhite_hisp0"
for (outcome in c("FMD", "days")) {
  for (var in strat) {
    print(paste0("Making: ", outcome, "_priv_", var))
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(", outcome, "_priv_", var, " = ", 
                             outcome, "_mu_", var, "[intersectional == quote(", privileged_strat, ")]) %>% ungroup() -> data")))
  }
}
rm(outcome, var, vars, strat, privileged_strat)

#### Step 3) Creating difference variables with each referent ####
for (col in c(42:72, 74:104)) {
  print(paste0("Var: ", col, ": ", names(data)[col]))
  col_name <- names(data)[col]
  names(data)[col] <- "temp"
  if (col %in% c(42:72)) {
    #National Average Referent
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(nat_diff_abs_", col_name, " = temp - FMD_mu_overall,
                                                                       nat_diff_mult_", col_name, " = temp / FMD_mu_overall) %>% ungroup() -> data")))
    #Socially Privileged Referent
    strat <- substr(col_name, start = 8, stop = 100)
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(priv_diff_abs_", col_name, " = temp - FMD_priv_", strat, ",
                                                                       priv_diff_mult_", col_name, " = temp / FMD_priv_", strat, ") %>% ungroup() -> data")))
    names(data)[names(data) == "temp"] <- col_name
  }
  if (col %in% c(74:104)) {
    #National Average Referent
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(nat_diff_abs_", col_name, " = temp - FMD_mu_overall,
                                                                       nat_diff_mult_", col_name, " = temp / FMD_mu_overall) %>% ungroup() -> data")))
    #Socially Privileged Referent
    strat <- substr(col_name, start = 9, stop = 100)
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(priv_diff_abs_", col_name, " = temp - FMD_priv_", strat, ",
                                                                       priv_diff_mult_", col_name, " = temp / FMD_priv_", strat, ") %>% ungroup() -> data")))
    names(data)[col] <- col_name
  }
  rm(col_name, strat, col)
}

#### Step 4) Creating summary dataset for mean and SD estimates by strata ####
frame <- data
frame$boot <- NULL
rm(data)

#### Step 5) Calculating mean and SD estimates by strata ####
#Getting all uni, bi, tri, quadra, and quintivariate combinations
strata <- gsub("n_", "", names(frame)[8:39])

#Selecting variables to calculate SD and 95% CI for
vars <- names(frame[0, c(8:413)])

#Setting 'frame' to data.table'
frame <- as.data.table(frame)

for (strat in strata) { #This generates mean, SD, LCI, and UCI for all variables with a given stratification
  by_strat <- ifelse(strat == "overall", "year",                            #This creates a vector with all variables to stratify by
                     paste0("year, ", gsub("_", ", ", strat)))              #replacing general terms (e.g. 'gender') with the variable name (i.e. 'male')
  by_strat <- gsub("gender", "male", by_strat)
  by_strat <- gsub("educ", "lesshs", by_strat)
  by_strat <- gsub("poverty", "in_poverty", by_strat)
  strat_vars <- which(str_detect(vars, strat))                              #This selects all variables including the given stratifications
  not_strat_vars <- c(which(str_detect(vars, paste0("gender_", strat))),    #And this section finds all variables with more or less stratifications than desired
                      which(str_detect(vars, paste0("educ_", strat))), 
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("race_", strat))), 
                      which(str_detect(vars, paste0("hisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_gender"))),
                      which(str_detect(vars, paste0(strat, "_educ"))), 
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_race"))), 
                      which(str_detect(vars, paste0(strat, "_hisp"))))
  strat_vars <- setdiff(strat_vars, not_strat_vars)                         #Then this section restricts to only the relevant variables with the correct stratifications
  for (col in strat_vars) { #Here, for each of the relevant variables we create SD, LCI, and UCI variables
    print(paste0("Strat: ", strat, ", Var ", col, ": ", vars[col]))
    col_name <- vars[col]
    names(frame)[names(frame) == col_name] <- "temp"
    if (col >= 33) {
      eval(parse(text = paste0("frame[, paste0(", "'", col_name, "'", ", '_sd') := sd(temp, na.rm=T), by = .(", by_strat, ")]")))
      eval(parse(text = paste0("frame[, paste0(", "'", col_name, "'", ", '_lci') := quantile(temp, c(0.025), na.rm=T), by = .(", by_strat, ")]")))
      eval(parse(text = paste0("frame[, paste0(", "'", col_name, "'", ", '_uci') := quantile(temp, c(0.975), na.rm=T), by = .(", by_strat, ")]")))
    }      
    eval(parse(text = paste0("frame[, temp := mean(temp, na.rm=T), by = .(", by_strat, ")]"))) #Then here we replace bootstrap-specific estimates with the mean estimate across bootstraps
    names(frame)[names(frame) == "temp"] <- col_name
    rm(col_name, col)
  }
  rm(by_strat, strat_vars, not_strat_vars)
}
rm(strat, strata, vars)
frame <- frame[1:832, ] #And here we restrict to 1 of the (now) identical 500 bootstrap rows per fully intersectional strata per year

#### Step 6) Creating datasets with rows suppressed due to low data quality/estimate reliability ####
#NOTE - We base all suppression on strata N and FMD prevalence estimates, not mean mentally poor days
#     - We create three copies of the data, determined by the variable 'suppression'
#         - 'Suppression' has values: 'none' (the data itself including unreliable estimates)
#                                     'BRFSS' (following the looser suppression guidelines recommended by BRFSS)
#                                     'NCHS' (following the suppression guidelines recommended by the NCHS)
#         - BRFSS has a 2-step data suppression decision tree, suppressing data if:
#             1) The unweighted strata sample size is < 50
#             2) The Relative SE (RSE) for p is > 50%, where RSE = (SE/p)*100
#         - NCHS has a 4-step data suppression decision tree, suppressing data if:
#             1) The effective strata sample size is < 30, where n_e = minimum(n, [p(1-p)/Var(p)])
#             2) FMD prevalence (p) != {0,1}
#             3) Absolute 95% CI width for p is > 0.3 (i.e. 30%)
#             4) Relative 95% CI width for p is > 130%

## No suppression dataset
none <- frame %>% mutate(suppression = "none")

## BRFSS suppression dataset
brfss <- frame %>% mutate(suppression = "BRFSS")

#Getting all uni, bi, tri, quadra, and quintivariate stratifications
strata <- gsub("n_", "", names(frame)[8:39])

#Getting all variables which (may) need to be suppressed)
vars <- names(frame[0, c(40:1535)])

#Decision Tree Suppression
for (strat in strata) {
  strat_vars <- which(str_detect(vars, strat))                              #Selecting potentially relevant vars to censor, if appropriate
  not_strat_vars <- c(which(str_detect(vars, paste0("n_", strat))),         #Selecting the subset not relevant (e.g. too many stratifications)
                      which(str_detect(vars, paste0("gender_", strat))),    
                      which(str_detect(vars, paste0("educ_", strat))), 
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("race_", strat))), 
                      which(str_detect(vars, paste0("hisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_gender"))),
                      which(str_detect(vars, paste0(strat, "_educ"))), 
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_race"))), 
                      which(str_detect(vars, paste0(strat, "_hisp"))))
  strat_vars <- setdiff(strat_vars, not_strat_vars)                         #Selecting the appropriate vars only by removing non-relevant vars from the list
  # Decision Tree
  for (var in strat_vars) {
    col_name <- vars[var]
    print(paste0("Strata: ", strat, ", Var: ", col_name))
    #Decision 1 - Unweighted strata sample size is < 50
    eval(parse(text = paste0("brfss <- brfss %>% mutate(", col_name, " = ifelse(n_", strat, " < 50, NA_real_, ", col_name, "))")))
    #Decision 2 - RSE is > 50%
    eval(parse(text = paste0("brfss <- brfss %>% mutate(rse = (FMD_mu_", strat, "_sd / FMD_mu_", strat, ")*100, ", col_name, " = ifelse(rse > 50 | is.na(rse), NA_real_, ", col_name, "))")))
    brfss$rse <- NULL
  }
  rm(strat_vars, not_strat_vars, var, col_name)
}
rm(strat, strata, vars)

## NCHS suppression dataset
nchs <- frame %>% mutate(suppression = "NCHS")

#Getting all uni, bi, tri, quadra, and quintivariate stratifications
strata <- gsub("n_", "", names(frame)[8:39])

#Getting all variables which (may) need to be suppressed)
vars <- names(frame[0, c(40:1535)])

#Decision Tree Suppression
for (strat in strata) {
  strat_vars <- which(str_detect(vars, strat))                              #Selecting potentially relevant vars to censor, if appropriate
  not_strat_vars <- c(which(str_detect(vars, paste0("n_", strat))),         #Selecting the subset not relevant (e.g. too many stratifications)
                      which(str_detect(vars, paste0("gender_", strat))),    
                      which(str_detect(vars, paste0("educ_", strat))), 
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("race_", strat))), 
                      which(str_detect(vars, paste0("hisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_gender"))),
                      which(str_detect(vars, paste0(strat, "_educ"))), 
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_race"))), 
                      which(str_detect(vars, paste0(strat, "_hisp"))))
  strat_vars <- setdiff(strat_vars, not_strat_vars)                         #Selecting the appropriate vars only by removing non-relevant vars from the list
  # Decision Tree
  for (var in strat_vars) {
    col_name <- vars[var]
    print(paste0("Strata: ", strat, ", Var: ", col_name))
    #Decision 1 - Effective strata sample size is < 30
    eval(parse(text = paste0("nchs <- nchs %>% mutate(n_deff = (FMD_mu_", strat, "*(1 - FMD_mu_", strat, "))/(FMD_mu_", strat, "_sd^2), 
                                                      n_e = ifelse(n_", strat, " < n_deff | is.na(n_deff), n_", strat, ", n_deff), ", 
                                                      col_name, " = ifelse(n_e < 30 | is.na(n_e), NA_real_, ", col_name, "))")))
    #Decision 2 - FMD Prevalence = 0 or 1
    eval(parse(text = paste0("nchs <- nchs %>% mutate(", col_name, " = ifelse(FMD_mu_", strat, " %in% c(0,1) | is.na(FMD_mu_", strat, "), NA_real_, ", col_name, "))")))
    #Decision 3 - Absolute FMD Prevalence 95% CI width > 30% (=0.3)
    eval(parse(text = paste0("nchs <- nchs %>% mutate(", col_name, " = ifelse(is.na(FMD_mu_", strat, ") | (FMD_mu_", strat, "_uci - FMD_mu_", strat, "_lci) > 0.3, NA_real_, ", col_name, "))")))
    #Decision 4 - Relative FMD Prevalence 95% CI width > 130%
    eval(parse(text = paste0("nchs <- nchs %>% mutate(", col_name, " = ifelse(is.na(FMD_mu_", strat, ") | ((FMD_mu_", strat, "_uci - FMD_mu_", strat, "_lci)/FMD_mu_", strat, ") > 1.3, NA_real_, ", col_name, "))")))
    nchs$n_deff <- NULL
    nchs$n_e <- NULL
  }
  rm(strat_vars, not_strat_vars, var, col_name)
}
rm(strat, strata, vars)

##Stitching together datasets
data <- rbind(none, brfss, nchs)
rm(frame, none, brfss, nchs)

#### Step 7) Saving complete analysis-ready dataset ####
qsave(data, file = paste0(directory, "data_final_partA_050723.qs"))

#Runtime total
duration <- Sys.time() - start
