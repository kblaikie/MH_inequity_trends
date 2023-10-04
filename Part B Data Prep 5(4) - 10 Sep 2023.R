#### Aim 1 Part B Data Prep 5 (4) - 10 Sep 2023 ####
#Author - Kieran Blaikie
#Date - 10 Sep 2023

#Runtime - 41 minutes

#Changes - Comared to 'Data Visualisation 1(3)', this script:
#           1) Does not create graphs
#           2) Is based on different social stratifications (race and ethnicity separately, only Black and White race)
#        - Compared to 'Data Prep 5 (1)', this script:
#           1) Creates stata estimates for more State EITC stratifications (State EITC availability w/ or w/o Federal EITC receipt, State EITC receipt w/ or w/o > Median generosity)
#           2) Suppresses all strata estimates following BRFSS and NCHS data suppression guidelines
#           3) Uses two comparators for difference measures: 1) those a priori the most privileged across strata or 2) the national average
#           4) Considers fewer social axes in constructing intersectional strata
#           5) Creates difference variables comparison social strata mental health and inequities across State EITC contexts individually and in-tandem
#              (e.g. constructing a triple-difference variable comparing the difference in inequity magnitude for those receiving/not receiving a State EITC in states with a State EITC 
#                    policy vs. the difference in inequity magnitude for those in states without a State EITC policy receiving/not receiving a Federal EITC)
#        - Compared to 'Data Prep 5(2)', this script:
#           1) Uses a restricted analysis dataset generated in 'Part B Data Prep 3(7)' where:
#               - The analysis period is restricted to 2000-2019
#               - The indicator for State EITC policies having greater than median generosity is based on the median State EITC generosity from 2000-2019, not 1993-2019
#               - Estimates for State EITC scenario comparisons and triple difference comparisons are censored where contributing strata and inequity estimates are suppressed

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
data <- qread(paste0(directory, "data_aggregated_partB_100923.qs"))
data <- data[,c(6,1:5,7:222)]

#Renaming and reformatting
names(data)[4] <- "in_poverty"
names(data)[5] <- "intersectional"

data %>% mutate(male = as.factor(male),
                in_poverty = as.factor(in_poverty),
                racehisp = as.factor(racehisp)) -> data

#### Step 0) Set to 0 all year and boot-specific N where it is currently NA ####
for (col in names(data)[startsWith(names(data), "n_")]) {
  print(paste0("Variable: ", substr(col, 3, nchar(col))))
  eval(parse(text = paste0("data$", col, " <- ifelse(is.na(data$", col, "), 0, data$", col, ")")))
}
rm(col)

#### Step 1) Exclude 2002 measures as mental health was not included in the main BRFSS survey ####
data <- data[data$year != 2002, ]

#### Step 2) Creating socially privileged reference variables #### 
#NOTES - For socially privileged referents, those 1) male, 2) Not in Poverty, and 4) non-Hispanic White are used
strat <- substr(names(data)[8:14], start = 3, stop = 26)
strat <- gsub("_all", "", strat)
privileged_strat <- "racehispNHWhite_male1_pov0"
for (outcome in c("FMD", "days")) {
  for (var in strat) {
    for (eitc in c("_all", "_state_eitc_no", "_state_eitc_yes", 
                   "_state_eitc_no_federal_no", "_state_eitc_no_federal_yes", 
                   "_state_eitc_yes_receipt_no", "_state_eitc_yes_receipt_yes", 
                   "_state_eitc_yes_receipt_yes_moremedian_no", "_state_eitc_yes_receipt_yes_moremedian_yes")) {
      print(paste0("Making: ", outcome, "_priv_", var, " for Strata: ", eitc))
      eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(", outcome, "_priv_", var, eitc, " = ", 
                               outcome, "_mu_", var, eitc, "[intersectional == quote(", privileged_strat, ")]) %>% ungroup() -> data")))      
    }
  }
}
rm(outcome, var, strat, privileged_strat, eitc)

#### Step 3) Creating difference variables ####
#Within-State EITC strata comparisons
for (col in c(16:22, 24:30, 40:46, 48:54, 64:70, 72:78, 88:94, 96:102, 112:118, 120:126, 136:142, 144:150, 160:166, 168:174, 184:190, 192:198, 208:214, 216:222)) {
  print(paste0("Var: ", col, ": ", names(data)[col]))
  col_name <- names(data)[col]
  names(data)[col] <- "temp"
  eitc_strat <- ifelse(col <= 30, "_all", ifelse(col <= 54, "_state_eitc_no", ifelse(col <= 78, "_state_eitc_yes",
                                                                                     ifelse(col <= 102, "_state_eitc_no_federal_no", ifelse(col <= 126, "_state_eitc_no_federal_yes", ifelse(col <= 150, "_state_eitc_yes_receipt_no",
                                                                                                                                                                                             ifelse(col <= 174, "_state_eitc_yes_receipt_yes", ifelse(col <= 198, "_state_eitc_yes_receipt_yes_moremedian_no", "_state_eitc_yes_receipt_yes_moremedian_yes"))))))))
  if (col %in% c(16:22, 40:46, 64:70, 88:94, 112:118, 136:142, 160:166, 184:190, 208:214)) {
    #National Average Referent
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(nat_diff_abs_", col_name, " = temp - FMD_mu_overall", eitc_strat, ", 
                                                                       nat_diff_mult_", col_name, " = temp / FMD_mu_overall", eitc_strat, ") %>% ungroup() -> data")))
    #Socially Privileged Referent
    strat <- substr(col_name, start = 8, stop = 100)
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(priv_diff_abs_", col_name, " = temp - FMD_priv_", strat, ",
                                                                       priv_diff_mult_", col_name, " = temp / FMD_priv_", strat, ") %>% ungroup() -> data")))
    names(data)[names(data) == "temp"] <- col_name
  }
  if (col %in% c(24:30, 48:54, 72:78, 96:102, 120:126, 144:150, 168:174, 192:198, 216:222)) {
    #National Average Referent
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(nat_diff_abs_", col_name, " = temp - days_mu_overall", eitc_strat, ", 
                                                                       nat_diff_mult_", col_name, " = temp / days_mu_overall", eitc_strat, ") %>% ungroup() -> data")))
    #Socially Privileged Referent
    strat <- substr(col_name, start = 9, stop = 100)
    eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(priv_diff_abs_", col_name, " = temp - days_priv_", strat, ",
                                                                       priv_diff_mult_", col_name, " = temp / days_priv_", strat, ") %>% ungroup() -> data")))
    names(data)[names(data) == "temp"] <- col_name
  }
  rm(col_name, strat, col, eitc_strat)
}
#Between-State EITC strata comparisons
vars <- names(data)[15:30]
vars <- gsub("_all", "", vars)
vars <- c(vars, paste0("nat_diff_abs_", vars[c(2:8,10:16)]), paste0("priv_diff_abs_", vars[c(2:8,10:16)]))
for (comparison in vars) {
  print(paste0("Comparison Var: ", comparison))
  #Each State EITC scenario vs. Overall
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% ",
                    "mutate(state_eitc_no_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_no - ", comparison, "_all,
                                                                     state_eitc_no_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_no / ", comparison, "_all,",
                           "state_eitc_yes_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_yes - ", comparison, "_all,
                                                                     state_eitc_yes_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_yes / ", comparison, "_all,",
                           "state_eitc_no_federal_no_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_no_federal_no - ", comparison, "_all,
                                                                     state_eitc_no_federal_no_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_no_federal_no / ", comparison, "_all,",
                           "state_eitc_no_federal_yes_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_no_federal_yes - ", comparison, "_all,
                                                                     state_eitc_no_federal_yes_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_no_federal_yes / ", comparison, "_all,",
                           "state_eitc_yes_receipt_no_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_no - ", comparison, "_all,
                                                                     state_eitc_yes_receipt_no_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_no / ", comparison, "_all,",
                           "state_eitc_yes_receipt_yes_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes - ", comparison, "_all,
                                                                     state_eitc_yes_receipt_yes_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes / ", comparison, "_all,",
                           "state_eitc_yes_receipt_yes_moremedian_no_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no - ", comparison, "_all,
                                                                     state_eitc_yes_receipt_yes_moremedian_no_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no / ", comparison, "_all,",
                           "state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes - ", comparison, "_all,
                                                                     state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes / ", comparison, "_all) %>% ungroup() -> data")))
  #State EITC available vs. not available
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_vs_state_eitc_no_abs_", comparison, " = ", comparison, "_state_eitc_yes - ", comparison, "_state_eitc_no,
                                                                     state_eitc_yes_vs_state_eitc_no_mult_", comparison, " = ", comparison, "_state_eitc_yes / ", comparison, "_state_eitc_no) %>% ungroup() -> data")))
  #No State EITC states: Individuals with vs. without Federal EITC
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, " = ", comparison, "_state_eitc_no_federal_yes - ", comparison, "_state_eitc_no_federal_no,
                                                                     state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_mult_", comparison, " = ", comparison, "_state_eitc_no_federal_yes / ", comparison, "_state_eitc_no_federal_no) %>% ungroup() -> data")))
  #State EITC states: Individuals with vs. without State EITC
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes - ", comparison, "_state_eitc_yes_receipt_no,
                                                                     state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes / ", comparison, "_state_eitc_yes_receipt_no) %>% ungroup() -> data")))
  #Less Generous State EITC states: Individuals with vs. without State EITC
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no - ", comparison, "_state_eitc_yes_receipt_no,
                                                                     state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no / ", comparison, "_state_eitc_yes_receipt_no) %>% ungroup() -> data")))
  #More Generous State EITC states: Individuals with vs. without State EITC
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes - ", comparison, "_state_eitc_yes_receipt_no,
                                                                     state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes / ", comparison, "_state_eitc_yes_receipt_no) %>% ungroup() -> data")))
  #Individuals receiving any State EITC vs. in a state without a State EITC but hypothetically eligible for one based on Federal EITC receipt
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes - ", comparison, "_state_eitc_no_federal_yes,
                                                                     state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes / ", comparison, "_state_eitc_no_federal_yes) %>% ungroup() -> data")))
  #Individuals receiving a less generous State EITC vs. in a state without a State EITC but hypothetically eligible for one based on Federal EITC receipt
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no - ", comparison, "_state_eitc_no_federal_yes,
                                                                     state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_no / ", comparison, "_state_eitc_no_federal_yes) %>% ungroup() -> data")))
  #Individuals receiving a more generous State EITC vs. in a state without a State EITC but hypothetically eligible for one based on Federal EITC receipt
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_abs_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes - ", comparison, "_state_eitc_no_federal_yes,
                                                                     state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_mult_", comparison, " = ", comparison, "_state_eitc_yes_receipt_yes_moremedian_yes / ", comparison, "_state_eitc_no_federal_yes) %>% ungroup() -> data")))
  #Triple difference: (Any State EITC receipt vs. no receipt) vs. (No State EITC and Federal EITC receipt vs. no receipt)
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", comparison, " = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " - state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ",
                                                                     triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", comparison, " = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " / state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ") %>% ungroup() -> data")))
  #Triple difference: (Less Generous State EITC receipt vs. no receipt) vs. (No State EITC and Federal EITC receipt vs. no receipt)
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", comparison, " = state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_abs_", comparison, " - state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ",
                                                                     triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", comparison, " = state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_abs_", comparison, " / state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ") %>% ungroup() -> data")))
  #Triple difference: (More Generous State EITC receipt vs. no receipt) vs. (No State EITC and Federal EITC receipt vs. no receipt)
  eval(parse(text = paste0("data %>% group_by(year, boot) %>% mutate(triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", comparison, " = state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " - state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ",
                                                                     triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", comparison, " = state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_abs_", comparison, " / state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_", comparison, ") %>% ungroup() -> data")))
}
rm(comparison, vars)

#### Step 4) Creating summary dataset for mean and SD estimates by strata ####
frame <- data
frame$boot <- NULL
rm(data)

#### Step 5) Calculating mean and SD estimates by strata #### 
#Getting all uni, bi, and trivariate combinations
strata <- gsub("n_", "", names(frame)[6:13])
strata <- gsub("_all", "", strata)

#Selecting variables to calculate SD and 95% CI for 
vars <- names(frame[0, c(6:2523)])

#Setting 'frame' to data.table'
frame <- as.data.table(frame)

for (strat in strata) { #This generates mean, SD, LCI, and UCI for all variables with a given stratification
  by_strat <- ifelse(strat == "overall", "year",                            #This creates a vector with all variables to stratify by
                     paste0("year, ", gsub("_", ", ", strat)))              #replacing general terms (e.g. 'gender') with the variable name (i.e. 'male')
  by_strat <- gsub("sex", "male", by_strat)
  by_strat <- gsub("poverty", "in_poverty", by_strat)
  strat_vars <- which(str_detect(vars, strat))                              #This selects all variables including the given stratifications
  not_strat_vars <- c(which(str_detect(vars, paste0("sex_", strat))),       #And this section finds all variables with more or less stratifications than desired
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("racehisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_sex"))),
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_racehisp"))))
  strat_vars <- setdiff(strat_vars, not_strat_vars)                         #Then this section restricts to only the relevant variables with the correct stratifications
  for (col in strat_vars) { #Here, for each of the relevant variables we create SD, LCI, and UCI variables
    print(paste0("Strat: ", strat, ", Var ", col, ": ", vars[col]))
    col_name <- vars[col]
    names(frame)[names(frame) == col_name] <- "temp"
    if ((col_name %in% vars[startsWith(vars, "n_")]) == F) {
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
frame <- frame[1:312, ] #And here we restrict to 1 of the (now) identical 500 bootstrap rows per fully intersectional strata per year

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
strata <- gsub("n_", "", names(frame)[6:13])
strata <- gsub("_all", "", strata)
strata <- c(paste0(strata, "_all"), paste0(strata, "_state_eitc_no"), paste0(strata, "_state_eitc_yes"), 
            paste0(strata, "_state_eitc_no_federal_no"), paste0(strata, "_state_eitc_no_federal_yes"),
            paste0(strata, "_state_eitc_yes_receipt_no"), paste0(strata, "_state_eitc_yes_receipt_yes"),
            paste0(strata, "_state_eitc_yes_receipt_yes_moremedian_no"), paste0(strata, "_state_eitc_yes_receipt_yes_moremedian_yes"))

#Getting all variables which (may) need to be suppressed)
vars <- names(frame[0, c(6:9861)])
not_vars <- vars[startsWith(vars, "n_")]
vars <- vars[(vars %in% not_vars) == F] #Not suppressing N estimates, or triple-diff estimates at this time

#Decision Tree Suppression
for (strat in strata) {
  strat_vars <- which(str_detect(vars, strat))                              #Selecting potentially relevant vars to censor, if appropriate
  not_strat_vars <- c(which(str_detect(vars, paste0("n_", strat))),         #Selecting the subset not relevant (e.g. too many stratifications)
                      which(str_detect(vars, paste0("sex_", strat))),    
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("racehisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_sex"))),
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_racehisp"))), 
                      which(str_detect(vars, paste0(strat, "_federal"))),
                      which(str_detect(vars, paste0(strat, "_receipt"))),
                      which(str_detect(vars, paste0(strat, "_moremedian"))))
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
strata <- gsub("n_", "", names(frame)[6:13])
strata <- gsub("_all", "", strata)
strata <- c(paste0(strata, "_all"), paste0(strata, "_state_eitc_no"), paste0(strata, "_state_eitc_yes"), 
            paste0(strata, "_state_eitc_no_federal_no"), paste0(strata, "_state_eitc_no_federal_yes"),
            paste0(strata, "_state_eitc_yes_receipt_no"), paste0(strata, "_state_eitc_yes_receipt_yes"),
            paste0(strata, "_state_eitc_yes_receipt_yes_moremedian_no"), paste0(strata, "_state_eitc_yes_receipt_yes_moremedian_yes"))

#Getting all variables which (may) need to be suppressed)
vars <- names(frame[0, c(6:9861)])
not_vars <- vars[startsWith(vars, "n_")]
vars <- vars[(vars %in% not_vars) == F] #Not suppressing N estimates, or triple-diff estimates at this time

#Decision Tree Suppression
#NOTE: Currently only works for FMD and FMD inequity measures, not State EITC context comparisons 
for (strat in strata) {
  strat_vars <- which(str_detect(vars, strat))                              #Selecting potentially relevant vars to censor, if appropriate
  not_strat_vars <- c(which(str_detect(vars, paste0("n_", strat))),         #Selecting the subset not relevant (e.g. too many stratifications)
                      which(str_detect(vars, paste0("sex_", strat))),    
                      which(str_detect(vars, paste0("poverty_", strat))), 
                      which(str_detect(vars, paste0("racehisp_", strat))), 
                      which(str_detect(vars, paste0(strat, "_sex"))),
                      which(str_detect(vars, paste0(strat, "_poverty"))), 
                      which(str_detect(vars, paste0(strat, "_racehisp"))), 
                      which(str_detect(vars, paste0(strat, "_federal"))),
                      which(str_detect(vars, paste0(strat, "_receipt"))),
                      which(str_detect(vars, paste0(strat, "_moremedian"))))
  strat_vars <- setdiff(strat_vars, not_strat_vars)                         #Selecting the appropriate vars only by removing non-relevant vars from the list
  not_strat_vars2 <- c(grep("_lci", vars), 
                       grep("_uci", vars), 
                       grep("_sd", vars), 
                       grep("days", vars))
  strat_vars <- strat_vars[(strat_vars %in% not_strat_vars2) == F] #Don't want to include SD, LCI, or UCI variables in this step
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
  }
  rm(strat_vars, not_strat_vars, not_strat_vars2, var, col_name)
}
rm(strat, strata, vars)
nchs$n_deff <- NULL
nchs$n_e <- NULL

##Stitching together datasets
data <- rbind(none, brfss, nchs)
rm(frame, none, brfss, nchs)

## Suppressing State Scenario estimates where 1+ contributing estimate is censored
#Getting state comparison variable prefixes 
state_comparisons <- c(#Comparing to Overall
                       "state_eitc_no_vs_overall_", 
                       "state_eitc_no_federal_no_vs_overall_", "state_eitc_no_federal_yes_vs_overall_", 
                       "state_eitc_yes_vs_overall_", 
                       "state_eitc_yes_receipt_no_vs_overall_", 
                       "state_eitc_yes_receipt_yes_vs_overall_", "state_eitc_yes_receipt_yes_moremedian_no_vs_overall_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_", 
                       #Comparing to State EITC contexts
                       "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", 
                       "state_eitc_yes_vs_state_eitc_no_", 
                       "state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_", "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_",
                       "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_", "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_")

#Metrics assessed across each scenario, without scenario suffixes in original variables (e.g. 'FMD_mu_intersectional_all' without '_all') 
vars <- names(data[0, c(6:9861)]) #9862 = 'suppression' so not included
not_vars <- vars[startsWith(vars, "n_")]
not_vars <- c(not_vars, vars[startsWith(vars, "state_")])
not_vars <- c(not_vars, vars[startsWith(vars, "triple")])
not_vars <- c(not_vars, vars[startsWith(vars, "FMD_priv")])
not_vars <- c(not_vars, vars[(grepl("days_", vars)) == T]) #We only apply censoring to FMD variables, so can only censor state comparison variables for measures using FMD. 
not_vars <- c(not_vars, vars[(grepl("diff_mult_", vars)) == T]) #We do not create multiplicative differences of multiplicative differences, as their interpretation is too complicated to be informative.
vars <- vars[(vars %in% not_vars) == F]
vars <- vars[(grepl("_lci", vars)) == F]; vars <- vars[(grepl("_uci", vars)) == F]; vars <- vars[(grepl("_sd", vars)) == F]
vars <- gsub("_all", "", vars)
vars <- gsub("_state_eitc_yes_receipt_yes_moremedian_yes", "", vars)
vars <- gsub("_state_eitc_yes_receipt_yes_moremedian_no", "", vars)
vars <- gsub("_state_eitc_yes_receipt_yes", "", vars)
vars <- gsub("_state_eitc_yes_receipt_no", "", vars)
vars <- gsub("_state_eitc_no_federal_yes", "", vars)
vars <- gsub("_state_eitc_no_federal_no", "", vars)
vars <- gsub("_state_eitc_yes", "", vars)
vars <- gsub("_state_eitc_no", "", vars)
vars <- unique(vars)

for (suppression in c("BRFSS", "NCHS")) {
  for (comp in state_comparisons) {
    for (scale in c("abs_", "mult_")) {
      for (var in vars) {
        censor_var <- paste0(comp, scale, var)
        print(paste0("Suppression: ", suppression, ". Variable: ", censor_var))
        if (comp == "state_eitc_no_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_no_federal_no_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_no_federal_yes_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_no_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_all", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_no", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_vs_state_eitc_no_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_no", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_yes", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_yes", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_no_federal_yes", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_no", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_no", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_no", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_yes_moremedian_yes", "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", var, "_state_eitc_yes_receipt_no", "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
      }
    }
  }
}
rm(suppression, comp, scale, var, censor_var, vars, not_vars, state_comparisons) 

## Suppressing Triple Difference estimates where 1+ contributing estimate is censored
triple_comparisons <- c("triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_")

#Selecting relevant vars to censor through
vars <- names(data[0, c(6:9861)]) #9862 = 'suppression' so not included
vars <- vars[startsWith(vars, "triple_")]
vars <- vars[(grepl("_lci", vars)) == F]; vars <- vars[(grepl("_uci", vars)) == F]; vars <- vars[(grepl("_sd", vars)) == F]; vars <- vars[(grepl("days", vars)) == F]
vars <- gsub("triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", "", vars)
vars <- gsub("triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", "", vars)
vars <- gsub("triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_", "", vars)
vars <- gsub("triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", "", vars)
vars <- gsub("triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", "", vars)
vars <- gsub("triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_mult_", "", vars)
vars <- unique(vars)

for (suppression in c("BRFSS", "NCHS")) {
  for (comp in triple_comparisons) {
    for (scale in c("abs_", "mult_")) {
      for (var in vars) {
        censor_var <- paste0(comp, scale, var)
        print(paste0("Suppression: ", suppression, ". Variable: ", censor_var))
        if (comp == "triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_", scale, var, "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", scale, var, "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_", scale, var, "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", scale, var, "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
        if (comp == "triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
          eval(parse(text = paste0("data$", censor_var, "[data$suppression == '", suppression, "'] <- ifelse(", 
                                                                                                             "is.na(data$", "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_", scale, var, "[data$suppression == '", suppression, "']) | ",
                                                                                                             "is.na(data$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", scale, var, "[data$suppression == '", suppression, "']), ",
                                                                                                                   "NA_real_, data$", censor_var, "[data$suppression == '", suppression, "'])")))
        }
      }
    }
  }
}
rm(triple_comparisons, vars, suppression, comp, scale, var, censor_var)

## Suppressing SD, LCI, and UCI variables where point-estimates are suppressed
#Getting a list of all estimates to suppress
vars <- names(data[0, c(6:9861)])
vars <- vars[(grepl("_lci", vars)) == T]
vars <- vars[(grepl("days", vars)) == F]
vars <- vars[(grepl("FMD_priv", vars)) == F]
vars <- gsub("_lci", "", vars)

for (var in vars) {
  print(paste0("Suppressing SD, LCI, and UCI for: ", var))
  eval(parse(text = paste0("data$", var, "_lci <- ifelse(is.na(data$", var, "), NA_real_, data$", var, "_lci)")))
  eval(parse(text = paste0("data$", var, "_uci <- ifelse(is.na(data$", var, "), NA_real_, data$", var, "_uci)")))
  eval(parse(text = paste0("data$", var, "_sd <- ifelse(is.na(data$", var, "), NA_real_, data$", var, "_sd)")))
}
rm(var, vars)

#Restricting to the study period (2000 - 2019)
data <- data[data$year >= 2000, ]

#### Step 7) Saving complete analysis-ready dataset ####
qsave(data, file = paste0(directory, "data_final_partB_100923.qs"))

#Runtime total
duration <- Sys.time() - start
