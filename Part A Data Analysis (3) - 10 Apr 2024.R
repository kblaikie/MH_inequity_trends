#### Aim 1 Part A Data Analysis (2) - 10 April 2024 ####
#Author - Kieran Blaikie
#Date - 10 April 2024

#Runtime - 9.2 minutes

#Overview - This script:
#             1) Creates figures using BRFSS and NCHS data suppression
#                 - Note: As default, BRFSS data suppression is used
#                 -  Creates absolute FMD and mean MPD figures over time:
#                     - In absolute terms
#                 - Creates FMD and mean MPD inequity figures over time:
#                     - Using as referent: 1) national average, 2) those socially privileged
#                     - In absolute and relative terms (vs. referents)
#                     - Ranked across social strata
#                 - Using Loess curves over time
#             2) Provides summary statistics accompanying each figure

#Changes - Compared to 'Part A Data Analysis (1)', this script:
#             1) Uses the dataset 'data_final_partA_050723' instead of 'data_final_partA_140623'
#             2) Uses the stratifying variable 'lesshs' instead of 'hsless' 
#        - Compared to 'Part A Data Analysis (2)', this script:
#             1) Uses the harmonic mean in calculating average annual changes for relative inequities 
#             2) Use a weighted mean with 'lag_years' instead of unweighted mean in calculating the 1993-2019 average annual change
#                   - Not weighting by 'lag_years' allowed short time-spans with more uncensored estimates to count more towards the 
#                     overall average than other periods with fewer uncensored estimates.
#                   - For multiplicative measures this is a weighted *harmonic* mean, since we're working with ratios

#Loading packages
library(qs)
library(data.table)
library(tidyverse)
library(stringr)
library(gridExtra)

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading dataset
data <- qread(paste0(directory, "data_final_partA_050723.qs"))

#### Step 0) Reformatting for clarity ####
data <- as.data.frame(data)
data$lesshs <- ifelse(data$lesshs == 0, ">=HS", "<HS")
data$male <- ifelse(data$male == 0, "Female", "Male")
data$in_poverty <- ifelse(data$in_poverty == 0, "Not In Poverty", "In Poverty")
data$hisp <- ifelse(data$hisp == 0, "Non-Hispanic", "Hispanic")
data$intersectional <- paste0(data$hisp, " ", data$race, " ", data$male, " ", data$in_poverty, " with ", data$lesshs)
names(data)[c(1:5)] <- c("race", "ethnicity", "gender", "education", "poverty")

#Variable stratification name stems across measures
variables <- gsub("n_", "", names(data)[8:39])

#Inequity variable name stems
inequities <- c("FMD_mu_overall", "days_mu_overall", "FMD_priv_", "days_priv_", "nat_diff_abs_", "nat_diff_mult_", "priv_diff_abs_", "priv_diff_mult_")

#Creating BRFSS and NCHS datasets
none <- data[data$suppression == "none",]
brfss <- data[data$suppression == "BRFSS", ]
nchs <- data[data$suppression == "NCHS", ]

#Creating rank variables for absolute annual mental health summaries
for (dataset in c("brfss", "nchs","none")) {
  for (var in variables[2:32]) {
    for (outcome in c("FMD_mu_", "days_mu_", "priv_diff_abs_FMD_mu_", "priv_diff_mult_FMD_mu_", "nat_diff_abs_FMD_mu_", "nat_diff_mult_FMD_mu_")) {
      print(paste0("Dataset: ", dataset, ", Variable: ", outcome, var, "_rank"))
      strata <- ifelse(var == "intersectional", "year", gsub("_", ", ", var))
      strata <- gsub("hisp", "ethnicity", strata)
      strata <- gsub("educ", "education", strata)
      strata <- ifelse(var == "intersectional", strata, paste0(strata, ", year"))
      eval(parse(text = paste0(dataset, " <- ", dataset, " %>% mutate(strata = paste0(", strata, "))")))
      eval(parse(text = paste0(dataset, " <- ", dataset, " %>% group_by(year) %>% mutate(", outcome, var, "_rank = dense_rank(x = ", outcome, var, ")) %>% ungroup()")))
      eval(parse(text = paste0(dataset, " <- ", dataset, "%>% group_by(year) %>% mutate(", outcome, var, "_rank = ifelse(is.na(", outcome, var, "), NA_real_, ", outcome, var, "_rank))")))
      eval(parse(text = paste0(dataset, "$strata <- NULL")))
    }
  }
}
rm(dataset, var, outcome, strata)

#Creating annual change and annual change rank variables for all strata for each mental health summary 
for (dataset in c("brfss", "nchs","none")) {
  for (var in variables[2:32]) {
    for (outcome in c("FMD_mu_", "days_mu_", "priv_diff_abs_FMD_mu_", "priv_diff_mult_FMD_mu_", "nat_diff_abs_FMD_mu_", "nat_diff_mult_FMD_mu_")) {
      print(paste0("Dataset: ", dataset, ", Variable: ", outcome, var, "_annual_change_rank"))
      strata <- ifelse(var == "intersectional", "intersectional", gsub("_", ", ", var))
      strata <- gsub("hisp", "ethnicity", strata)
      strata <- gsub("educ", "education", strata)
      
      if (outcome %in% c("priv_diff_mult_FMD_mu_", "nat_diff_mult_FMD_mu_")) { 
        scale_operator = " / "; diff_outcome_fun = "diff_outcome^(1/lag_years)"; mu_diff_outcome_fun = "sum(lag_years, na.rm=T)/sum(lag_years/diff_outcome, na.rm=T)"} else {
        scale_operator = " - "; diff_outcome_fun = "diff_outcome / lag_years"; mu_diff_outcome_fun = "sum(lag_years*diff_outcome, na.rm=T)/sum(lag_years, na.rm=T)"
      }
      
      eval(parse(text = paste0(dataset, " <- ", dataset, " %>% mutate(strata = paste0(", strata, "))")))
      eval(parse(text = paste0("temp_ranks <- ", dataset, " %>% ",
                               "arrange(year) %>% ",
                               "filter(!is.na(", outcome, var, ")) %>% ",
                               "group_by(strata) %>% ",
                               "filter(NROW(strata) >= 2) %>% ",
                               "mutate(lag_years = year - lag(year, 1), 
                                       diff_outcome = ", outcome, var, scale_operator, "lag(", outcome, var, ", 1),
                                       diff_outcome = ", diff_outcome_fun, ",
                                       mu_diff_outcome = ", mu_diff_outcome_fun, ", ) %>% ",
                               "ungroup() %>% ",
                               "mutate(diff_outcome_rank = dense_rank(mu_diff_outcome)) %>% ",
                               "group_by(", strata, ") %>% ",
                               "mutate(diff_outcome_rank = min(diff_outcome_rank, na.rm = T)) %>% ",
                               "ungroup() %>% ",
                               "select(strata, mu_diff_outcome, diff_outcome_rank) %>% ",
                               "distinct() %>% ",
                               "mutate(diff_outcome_rank = ifelse(is.infinite(diff_outcome_rank), NA_integer_, diff_outcome_rank))")))
      eval(parse(text = paste0(dataset, "<- merge(", dataset, ", temp_ranks, by = c('strata'), all.x = T)")))
      eval(parse(text = paste0("names(", dataset, ")[NROW(names(", dataset, "))-1] <- '", outcome, var, "_annual_change'")))
      eval(parse(text = paste0("names(", dataset, ")[NROW(names(", dataset, "))] <- '", outcome, var, "_annual_change_rank'")))
      eval(parse(text = paste0(dataset, "$strata <- NULL")))
      rm(strata, temp_ranks)
    }
  }
}
rm(dataset, var, outcome, strata, diff_outcome_fun, mu_diff_outcome_fun, scale_operator)

#Creating overall annual change variables for each dataset
#None
none %>% arrange(year) %>% select(year, FMD_mu_overall) %>% distinct() %>% mutate(FMD_mu_overall_annual_change = FMD_mu_overall - lag(FMD_mu_overall, 1),
                                                                                  FMD_mu_overall_annual_change = mean(FMD_mu_overall_annual_change, na.rm=T))  -> none_to_merge
none$FMD_mu_overall_annual_change <- none_to_merge$FMD_mu_overall_annual_change[1]
#BRFSS
brfss %>% arrange(year) %>% select(year, FMD_mu_overall) %>% distinct() %>% mutate(FMD_mu_overall_annual_change = FMD_mu_overall - lag(FMD_mu_overall, 1),
                                                                                   FMD_mu_overall_annual_change = mean(FMD_mu_overall_annual_change, na.rm=T))  -> brfss_to_merge
brfss$FMD_mu_overall_annual_change <- brfss_to_merge$FMD_mu_overall_annual_change[1]
#NCHS
nchs %>% arrange(year) %>% select(year, FMD_mu_overall) %>% distinct() %>% mutate(FMD_mu_overall_annual_change = FMD_mu_overall - lag(FMD_mu_overall, 1),
                                                                                  FMD_mu_overall_annual_change = mean(FMD_mu_overall_annual_change, na.rm=T))  -> nchs_to_merge
nchs$FMD_mu_overall_annual_change <- nchs_to_merge$FMD_mu_overall_annual_change[1]
rm(none_to_merge, brfss_to_merge, nchs_to_merge)

data <- rbind(none, nchs, brfss)

#Creating shorter intersectional strata labels to keep all plots on the same page
nchs %>% mutate(intersectional_short = intersectional,
                #Race and Ethnicity
                intersectional_short = case_when(str_detect(intersectional_short, "Non-Hispanic White ") ~ str_replace(intersectional_short, "Non-Hispanic White ", "NH-W-"),
                                                 str_detect(intersectional_short, "Non-Hispanic Black ") ~ str_replace(intersectional_short, "Non-Hispanic Black ", "NH-B-"),
                                                 str_detect(intersectional_short, "Hispanic Black ") ~ str_replace(intersectional_short, "Hispanic Black ", "H-B-"),
                                                 TRUE ~ str_replace(intersectional_short, "Hispanic White ", "H-W-")),
                #Sex
                intersectional_short = case_when(str_detect(intersectional_short, "Female ") ~ str_replace(intersectional_short, "Female ", "F-"),
                                                 TRUE ~ str_replace(intersectional_short, "Male ", "M-")),
                #Poverty Status
                intersectional_short = case_when(str_detect(intersectional_short, "Not In Poverty ") ~ str_replace(intersectional_short, "Not In Poverty ", "NP-"),
                                                 TRUE ~ str_replace(intersectional_short, "In Poverty ", "P-")),
                #Educational Attainment
                intersectional_short = case_when(str_detect(intersectional_short, "with <HS") ~ str_replace(intersectional_short, "with <HS", "LHS"),
                                                 TRUE ~ str_replace(intersectional_short, "with >=HS", "HSM"))) -> nchs

#### Creating a dataset containing summaries by stratification ####
#Creating empty dataset to fill with summaries
data_summary <- data.frame(stratifying_by = NA_character_, n_unsuppressed_estimates = NA_integer_, estimate_range = NA_character_,
                           ethnicity = NA_character_, race = NA_character_, gender = NA_character_, education = NA_character_, poverty = NA_character_,
                           FMD_prev_1993 = NA_real_, FMD_prev_1993_lci = NA_real_, FMD_prev_1993_uci = NA_real_, FMD_prev_1993_rank = NA_real_,
                           FMD_prev_2019 = NA_real_, FMD_prev_2019_lci = NA_real_, FMD_prev_2019_uci = NA_real_, FMD_prev_2019_rank = NA_real_,
                           FMD_prev_change_1993_2019 = NA_real_, FMD_prev_annual_change = NA_real_, FMD_prev_annual_change_rank = NA_real_,
                           FMD_ineq_abs_1993 = NA_real_, FMD_ineq_abs_1993_lci = NA_real_, FMD_ineq_abs_1993_uci = NA_real_, FMD_ineq_abs_1993_rank = NA_real_,
                           FMD_ineq_abs_2019 = NA_real_, FMD_ineq_abs_2019_lci = NA_real_, FMD_ineq_abs_2019_uci = NA_real_, FMD_ineq_abs_2019_rank = NA_real_,
                           FMD_ineq_abs_change_1993_2019 = NA_real_, FMD_ineq_abs_annual_change = NA_real_, FMD_ineq_abs_annual_change_rank = NA_real_,
                           FMD_ineq_mult_1993 = NA_real_, FMD_ineq_mult_1993_lci = NA_real_, FMD_ineq_mult_1993_uci = NA_real_, FMD_ineq_mult_1993_rank = NA_real_,
                           FMD_ineq_mult_2019 = NA_real_, FMD_ineq_mult_2019_lci = NA_real_, FMD_ineq_mult_2019_uci = NA_real_, FMD_ineq_mult_2019_rank = NA_real_,
                           FMD_ineq_mult_change_1993_2019 = NA_real_, FMD_ineq_mult_annual_change = NA_real_, FMD_ineq_mult_annual_change_rank = NA_real_)[0,]

for (var in variables) {
  #Creating a string to group by
  strata <- gsub("_", ", ", var)
  strata <- gsub("hisp", "ethnicity", strata)
  strata <- gsub("educ", "education", strata)
  strata <- ifelse(var == "intersectional", "ethnicity, race, gender, education, poverty", strata)
  
  print(paste0("Variables: ", strata))
  
  #Calculating group summaries
  if (strata == "overall") {
    eval(parse(text = paste0("nchs %>% ",
                             "select(year, FMD_mu_", var, ", FMD_mu_", var, "_lci, FMD_mu_", var, "_uci, FMD_mu_", var, "_annual_change) %>% ",
                             "distinct() %>% ",
                             "mutate(stratifying_by = var, 
                                     n_unsuppressed_estimates = NROW(!is.na(FMD_mu_", var, ")),
                                     estimate_range = ifelse(n_unsuppressed_estimates == 0, NA_character_,
                                                             paste0((min(year[!is.na(FMD_mu_", var, ")], na.rm=T)), '-', (max(year[!is.na(FMD_mu_", var, ")], na.rm=T)))),
                                     #Absolute FMD Prevalence
                                     FMD_prev_1993 = ifelse(is.na(FMD_mu_", var, "[year == 1993]), NA_real_, FMD_mu_", var, "[year == 1993]*100), 
                                     FMD_prev_1993_lci = ifelse(is.na(FMD_mu_", var, "_lci[year == 1993]), NA_real_, FMD_mu_", var, "_lci[year == 1993]*100), 
                                     FMD_prev_1993_uci = ifelse(is.na(FMD_mu_", var, "_uci[year == 1993]), NA_real_, FMD_mu_", var, "_uci[year == 1993]*100),
                                     FMD_prev_2019 = ifelse(is.na(FMD_mu_", var, "[year == 2019]), NA_real_, FMD_mu_", var, "[year == 2019]*100),
                                     FMD_prev_2019_lci = ifelse(is.na(FMD_mu_", var, "_lci[year == 2019]), NA_real_, FMD_mu_", var, "_lci[year == 2019]*100),
                                     FMD_prev_2019_uci = ifelse(is.na(FMD_mu_", var, "_uci[year == 2019]), NA_real_, FMD_mu_", var, "_uci[year == 2019]*100),
                                     FMD_prev_change_1993_2019 = ifelse(is.na(FMD_prev_1993) | is.na(FMD_prev_2019), NA_real_, (FMD_prev_2019 - FMD_prev_1993)),
                                     FMD_prev_annual_change = unique(FMD_mu_", var, "_annual_change)*100) %>% ",
                             "select(-c(year, FMD_mu_", var, ", FMD_mu_", var, "_lci, FMD_mu_", var, "_uci, FMD_mu_", var, "_annual_change)) %>% ", 
                             "distinct() -> data_summary_temp")))
    
  } else {
    eval(parse(text = paste0("nchs %>% ",
                             "group_by(", strata, ") %>% ",
                             "select(year, ", strata, ", 
                                     FMD_mu_", var, ", FMD_mu_", var, "_lci, FMD_mu_", var, "_uci, FMD_mu_", var, "_rank, FMD_mu_", var, "_annual_change, FMD_mu_", var, "_annual_change_rank, 
                                     priv_diff_abs_FMD_mu_", var, ", priv_diff_abs_FMD_mu_", var, "_lci, priv_diff_abs_FMD_mu_", var, "_uci, priv_diff_abs_FMD_mu_", var, "_rank, ",
                             "priv_diff_abs_FMD_mu_", var, "_annual_change, priv_diff_abs_FMD_mu_", var, "_annual_change_rank,
                                     priv_diff_mult_FMD_mu_", var, ", priv_diff_mult_FMD_mu_", var, "_lci, priv_diff_mult_FMD_mu_", var, "_uci, priv_diff_mult_FMD_mu_", var, "_rank, ",
                             "priv_diff_mult_FMD_mu_", var, "_annual_change, priv_diff_mult_FMD_mu_", var, "_annual_change_rank) %>% ",
                             "distinct() %>% ",
                             "mutate(stratifying_by = var, 
                                     n_unsuppressed_estimates = NROW(year[!is.na(FMD_mu_", var, ")]),
                                     estimate_range = ifelse(n_unsuppressed_estimates == 0, NA_character_,
                                                             paste0((min(year[!is.na(FMD_mu_", var, ")], na.rm=T)), '-', (max(year[!is.na(FMD_mu_", var, ")], na.rm=T)))), 
                                     #Absolute FMD Prevalence
                                     FMD_prev_1993 = ifelse(is.na(FMD_mu_", var, "[year == 1993]), NA_real_, FMD_mu_", var, "[year == 1993]*100), 
                                     FMD_prev_1993_lci = ifelse(is.na(FMD_mu_", var, "_lci[year == 1993]), NA_real_, FMD_mu_", var, "_lci[year == 1993]*100), 
                                     FMD_prev_1993_uci = ifelse(is.na(FMD_mu_", var, "_uci[year == 1993]), NA_real_, FMD_mu_", var, "_uci[year == 1993]*100),
                                     FMD_prev_1993_rank = ifelse(is.na(FMD_mu_", var, "[year == 1993]), NA_real_, FMD_mu_", var, "_rank[year == 1993]),
                                     FMD_prev_2019 = ifelse(is.na(FMD_mu_", var, "[year == 2019]), NA_real_, FMD_mu_", var, "[year == 2019]*100),
                                     FMD_prev_2019_lci = ifelse(is.na(FMD_mu_", var, "_lci[year == 2019]), NA_real_, FMD_mu_", var, "_lci[year == 2019]*100),
                                     FMD_prev_2019_uci = ifelse(is.na(FMD_mu_", var, "_uci[year == 2019]), NA_real_, FMD_mu_", var, "_uci[year == 2019]*100),
                                     FMD_prev_2019_rank = ifelse(is.na(FMD_mu_", var, "[year == 2019]), NA_real_, FMD_mu_", var, "_rank[year == 2019]),
                                     FMD_prev_change_1993_2019 = ifelse(is.na(FMD_prev_1993) | is.na(FMD_prev_2019), NA_real_, (FMD_prev_2019 - FMD_prev_1993)),
                                     FMD_prev_annual_change = unique(FMD_mu_", var, "_annual_change)*100,
                                     FMD_prev_annual_change_rank = unique(FMD_mu_", var, "_annual_change_rank),
                                     #FMD Prevalence Absolute Inequity
                                     FMD_ineq_abs_1993 = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "[year == 1993]), NA_real_, priv_diff_abs_FMD_mu_", var, "[year == 1993]*100), 
                                     FMD_ineq_abs_1993_lci = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "_lci[year == 1993]), NA_real_, priv_diff_abs_FMD_mu_", var, "_lci[year == 1993]*100), 
                                     FMD_ineq_abs_1993_uci = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "_uci[year == 1993]), NA_real_, priv_diff_abs_FMD_mu_", var, "_uci[year == 1993]*100),
                                     FMD_ineq_abs_1993_rank = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "[year == 1993]), NA_real_, priv_diff_abs_FMD_mu_", var, "_rank[year == 1993]), 
                                     FMD_ineq_abs_2019 = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "[year == 2019]), NA_real_, priv_diff_abs_FMD_mu_", var, "[year == 2019]*100),
                                     FMD_ineq_abs_2019_lci = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "_lci[year == 2019]), NA_real_, priv_diff_abs_FMD_mu_", var, "_lci[year == 2019]*100),
                                     FMD_ineq_abs_2019_uci = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "_uci[year == 2019]), NA_real_, priv_diff_abs_FMD_mu_", var, "_uci[year == 2019]*100),
                                     FMD_ineq_abs_2019_rank = ifelse(is.na(priv_diff_abs_FMD_mu_", var, "[year == 2019]), NA_real_, priv_diff_abs_FMD_mu_", var, "_rank[year == 2019]*100),
                                     FMD_ineq_abs_change_1993_2019 = ifelse(is.na(FMD_ineq_abs_1993) | is.na(FMD_ineq_abs_2019), NA_real_, (FMD_ineq_abs_2019 - FMD_ineq_abs_1993)),
                                     FMD_ineq_abs_annual_change = unique(priv_diff_abs_FMD_mu_", var, "_annual_change)*100,
                                     FMD_ineq_abs_annual_change_rank = unique(priv_diff_abs_FMD_mu_", var, "_annual_change_rank),
                                     #FMD Prevalence Relative Inequity
                                     FMD_ineq_mult_1993 = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "[year == 1993]), NA_real_, priv_diff_mult_FMD_mu_", var, "[year == 1993]*100-100), 
                                     FMD_ineq_mult_1993_lci = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "_lci[year == 1993]), NA_real_, priv_diff_mult_FMD_mu_", var, "_lci[year == 1993]*100-100), 
                                     FMD_ineq_mult_1993_uci = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "_uci[year == 1993]), NA_real_, priv_diff_mult_FMD_mu_", var, "_uci[year == 1993]*100-100),
                                     FMD_ineq_mult_1993_rank = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "[year == 1993]), NA_real_, priv_diff_mult_FMD_mu_", var, "_rank[year == 1993]), 
                                     FMD_ineq_mult_2019 = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "[year == 2019]), NA_real_, priv_diff_mult_FMD_mu_", var, "[year == 2019]*100-100),
                                     FMD_ineq_mult_2019_lci = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "_lci[year == 2019]), NA_real_, priv_diff_mult_FMD_mu_", var, "_lci[year == 2019]*100-100),
                                     FMD_ineq_mult_2019_uci = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "_uci[year == 2019]), NA_real_, priv_diff_mult_FMD_mu_", var, "_uci[year == 2019]*100-100),
                                     FMD_ineq_mult_2019_rank = ifelse(is.na(priv_diff_mult_FMD_mu_", var, "[year == 2019]), NA_real_, priv_diff_mult_FMD_mu_", var, "_rank[year == 2019]),
                                     FMD_ineq_mult_change_1993_2019 = ifelse(is.na(FMD_ineq_mult_1993) | is.na(FMD_ineq_mult_2019), NA_real_, (FMD_ineq_mult_2019 - FMD_ineq_mult_1993)),
                                     FMD_ineq_mult_annual_change = unique(priv_diff_mult_FMD_mu_", var, "_annual_change)*100,
                                     FMD_ineq_mult_annual_change_rank = unique(priv_diff_mult_FMD_mu_", var, "_annual_change_rank)) %>% ",
                             "select(-c(year, 
                                     FMD_mu_", var, ", FMD_mu_", var, "_lci, FMD_mu_", var, "_uci, FMD_mu_", var, "_rank, FMD_mu_", var, "_annual_change, FMD_mu_", var, "_annual_change_rank,
                                     priv_diff_abs_FMD_mu_", var, ", priv_diff_abs_FMD_mu_", var, "_lci, priv_diff_abs_FMD_mu_", var, "_uci, priv_diff_abs_FMD_mu_", var, "_rank, ",
                             "priv_diff_abs_FMD_mu_", var, "_annual_change, priv_diff_abs_FMD_mu_", var, "_annual_change_rank,
                                     priv_diff_mult_FMD_mu_", var, ", priv_diff_mult_FMD_mu_", var, "_lci, priv_diff_mult_FMD_mu_", var, "_uci, priv_diff_mult_FMD_mu_", var, "_rank, ",
                             "priv_diff_mult_FMD_mu_", var, "_annual_change, priv_diff_mult_FMD_mu_", var, "_annual_change_rank)) %>% ", 
                             "distinct() -> data_summary_temp")))
  }
  
  #Creating missing variables per stratification (e.g. education and poverty if the stratifying_by was 'race_hisp_gender')
  for (new_var in names(data_summary)) {
    if ((new_var %in% names(data_summary_temp)) == F) {
      if (is.character(data_summary[[new_var]])) {
        eval(parse(text = paste0("data_summary_temp$", new_var, " <- NA_character_")))
      } else {
        eval(parse(text = paste0("data_summary_temp$", new_var, " <- NA_real_")))
      }
    }
  }
  
  #Adding each strata row to the empty dataset 'data_summary'
  data_summary_temp <- data_summary_temp[, names(data_summary)]
  data_summary <- rbind(data_summary, data_summary_temp)
}
rm(new_var, var, strata, data_summary_temp)

#Replacing social identifier variables with 'All' if not stratified by
for (row in 1:NROW(data_summary)) {
  strata <- data_summary$stratifying_by[row]
  if (strata == "intersectional") {} else {
    data_summary$ethnicity[row] <- ifelse(grepl("hisp", strata) == TRUE, data_summary$ethnicity[row], "All")
    data_summary$race[row] <- ifelse(grepl("race", strata) == TRUE, data_summary$race[row], "All")
    data_summary$gender[row] <- ifelse(grepl("gender", strata) == TRUE, data_summary$gender[row], "All")
    data_summary$education[row] <- ifelse(grepl("educ", strata) == TRUE, data_summary$education[row], "All")
    data_summary$poverty[row] <- ifelse(grepl("poverty", strata) == TRUE, data_summary$poverty[row], "All")    
  }
  rm(row, strata)
}

#Creating 'stratifications' variable
data_summary %>% mutate(stratifications = case_when(stratifying_by == "overall" ~ 0,
                                                    stratifying_by == "intersectional" ~ 5,
                                                    TRUE ~ (str_count(stratifying_by, "_") + 1))) -> data_summary

#Re-coding 'stratifying_by' variable
data_summary$stratifying_by <- ifelse(data_summary$stratifying_by == "overall", "Overall",
                                      ifelse(data_summary$stratifying_by == "intersectional", "Fully Intersectionally", data_summary$stratifying_by))
data_summary$stratifying_by <- gsub("_", ", ", data_summary$stratifying_by)
data_summary$stratifying_by <- gsub("race", "Race", data_summary$stratifying_by)
data_summary$stratifying_by <- gsub("hisp", "Ethnicity", data_summary$stratifying_by)
data_summary$stratifying_by <- gsub("gender", "Sex", data_summary$stratifying_by)
data_summary$stratifying_by <- gsub("educ", "Education", data_summary$stratifying_by)
data_summary$stratifying_by <- gsub("poverty", "Poverty Status", data_summary$stratifying_by)
names(data_summary)[6] <- "sex"

#Rounding estimates for easier presentation
for (var in 9:41) {
  data_summary[[var]] <- signif(data_summary[[var]], 3)
  rm(var)
}

#Reformatting for visual demonstration
data_summary <- data_summary %>% arrange(stratifications, stratifying_by, FMD_prev_2019_rank)
data_summary$FMD_prev_1993 <- ifelse(is.na(data_summary$FMD_prev_1993), NA_character_,
                                     paste0(data_summary$FMD_prev_1993, " (95% CI: ", data_summary$FMD_prev_1993_lci, " to ", data_summary$FMD_prev_1993_uci, ")"))
data_summary$FMD_prev_2019 <- ifelse(is.na(data_summary$FMD_prev_2019), NA_character_,
                                     paste0(data_summary$FMD_prev_2019, " (95% CI: ", data_summary$FMD_prev_2019_lci, " to ", data_summary$FMD_prev_2019_uci, ")"))
data_summary$FMD_ineq_abs_1993 <- ifelse(is.na(data_summary$FMD_ineq_abs_1993), NA_character_,
                                         paste0(data_summary$FMD_ineq_abs_1993, " (95% CI: ", data_summary$FMD_ineq_abs_1993_lci, " to ", data_summary$FMD_ineq_abs_1993_uci, ")"))
data_summary$FMD_ineq_abs_2019 <- ifelse(is.na(data_summary$FMD_ineq_abs_2019), NA_character_,
                                         paste0(data_summary$FMD_ineq_abs_2019, " (95% CI: ", data_summary$FMD_ineq_abs_2019_lci, " to ", data_summary$FMD_ineq_abs_2019_uci, ")"))
data_summary$FMD_ineq_mult_1993 <- ifelse(is.na(data_summary$FMD_ineq_mult_1993), NA_character_,
                                          paste0(data_summary$FMD_ineq_mult_1993, " (95% CI: ", data_summary$FMD_ineq_mult_1993_lci, " to ", data_summary$FMD_ineq_mult_1993_uci, ")"))
data_summary$FMD_ineq_mult_2019 <- ifelse(is.na(data_summary$FMD_ineq_mult_2019), NA_character_,
                                          paste0(data_summary$FMD_ineq_mult_2019, " (95% CI: ", data_summary$FMD_ineq_mult_2019_lci, " to ", data_summary$FMD_ineq_mult_2019_uci, ")"))

#Getting rid of LCI and UCI variables
data_summary %>% select(-ends_with("_lci")) %>% select(-ends_with("_uci")) -> data_summary

#Reordering variables
data_summary <- data_summary[,c(30, 1:29)]

#Saving all annual estimates and NCHS estimate summaries
write_csv(data_summary, paste0(directory, "/Aim_1a_Strata_Summaries_041024.csv"))
write_csv(data, paste0(directory, "/nchs_annual_estimates_041024.csv"))

#### Figure 1 - Overall FMD Prevalence Over Time ####
ggplot(nchs, aes(x = year, y = FMD_mu_overall*100)) +
  geom_ribbon(aes(ymin = FMD_mu_overall_lci*100, ymax = FMD_mu_overall_uci*100, x = year), alpha = 0.2) +
  geom_point() + geom_line() +
  ylab("FMD Prevalence (%)") +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  expand_limits(y = c(8, 16)) -> fig1

#### Figure 2A - Fully Intersectional Average Change in FMD Prevalence ####
nchs %>%
  group_by(year) %>%
  mutate(n_strata_uncensored = NROW(FMD_mu_intersectional_rank[!is.na(FMD_mu_intersectional_rank)])) %>%
  ungroup() %>%
  group_by(intersectional_short) %>%
  mutate(FMD_mu_intersectional_rank_scaled = FMD_mu_intersectional_rank*(30/n_strata_uncensored),
         strata_mu_FMD_mu_intersectional_rank_scaled = mean(FMD_mu_intersectional_rank_scaled, na.rm=T),
         strata_sd_FMD_mu_intersectional_rank_scaled = sd(FMD_mu_intersectional_rank_scaled, na.rm=T),
         strata_min_FMD_mu_intersectional_rank_scaled = ifelse(NROW(!is.na(FMD_mu_intersectional_rank)) == 0, NA_real_, min(FMD_mu_intersectional_rank_scaled, na.rm=T)),
         strata_max_FMD_mu_intersectional_rank_scaled = ifelse(NROW(!is.na(FMD_mu_intersectional_rank)) == 0, NA_real_, max(FMD_mu_intersectional_rank_scaled, na.rm=T)),
         strata_med_FMD_mu_intersectional_rank_scaled = ifelse(NROW(!is.na(FMD_mu_intersectional_rank)) == 0, NA_real_, quantile(FMD_mu_intersectional_rank_scaled, c(0.5), na.rm=T)),
         strata_1qr_FMD_mu_intersectional_rank_scaled = ifelse(NROW(!is.na(FMD_mu_intersectional_rank)) == 0, NA_real_, quantile(FMD_mu_intersectional_rank_scaled, c(0.25), na.rm=T)),
         strata_3qr_FMD_mu_intersectional_rank_scaled = ifelse(NROW(!is.na(FMD_mu_intersectional_rank)) == 0, NA_real_, quantile(FMD_mu_intersectional_rank_scaled, c(0.75), na.rm=T)),
         n_ests = NROW(FMD_mu_intersectional[!is.na(FMD_mu_intersectional)]),
         n_ests_low = ifelse(n_ests < (0.5*26), "* *", 
                             ifelse(n_ests < (0.8*26), "*", ""))) %>%
  ungroup() -> nchs

nchs %>%  
  filter(!is.na(FMD_mu_intersectional_annual_change)) %>%
  ggplot(aes(x = FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional_short), FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 1.4, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Absolute Annual Change (pp)", breaks = c(-0.5, 0, 0.5, 1), limits = c(-0.6,1.5)) +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("Change in FMD Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) -> fig2a

#### Figure 2B - Fully Intersectional Earliest and Latest FMD Prevalence ####
groups <- c("Earliest" = "darkred", "Latest" = "darkgreen")
nchs %>%
  group_by(intersectional) %>%
  mutate(earliest_year = ifelse(NROW(year[!is.na(FMD_mu_intersectional)]) == 0, NA_real_,
                                min(year[!is.na(FMD_mu_intersectional)], na.rm=T)),
         latest_year = ifelse(NROW(year[!is.na(FMD_mu_intersectional)]) == 0, NA_real_,
                              max(year[!is.na(FMD_mu_intersectional)], na.rm=T)),
         year_range = ifelse(is.na(earliest_year), NA_character_, 
                             ifelse(earliest_year == latest_year, paste0(earliest_year), paste0(earliest_year, " - ", latest_year))),
         earliest_intersectional_prev = FMD_mu_intersectional[year == earliest_year],
         earliest_intersectional_prev_lci = FMD_mu_intersectional_lci[year == earliest_year],
         earliest_intersectional_prev_uci = FMD_mu_intersectional_uci[year == earliest_year],
         latest_intersectional_prev = FMD_mu_intersectional[year == latest_year],
         latest_intersectional_prev_lci = FMD_mu_intersectional_lci[year == latest_year],
         latest_intersectional_prev_uci = FMD_mu_intersectional_uci[year == latest_year]) %>%
  ungroup() %>% select(intersectional_short, earliest_year, latest_year, year_range,
                       earliest_intersectional_prev, earliest_intersectional_prev_lci, earliest_intersectional_prev_uci,
                       latest_intersectional_prev, latest_intersectional_prev_lci, latest_intersectional_prev_uci) %>% distinct() %>% 
  filter(!is.na(year_range)) %>%
  ggplot(aes(x = latest_intersectional_prev*100, y = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev))) +
  geom_segment(aes(x = earliest_intersectional_prev_lci*100, 
                   xend = earliest_intersectional_prev_uci*100, 
                   y = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev, na.rm=T), 
                   yend = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev, na.rm=T)), 
               col = "darkred", linewidth = 1.25, alpha = 0.25) +
  geom_segment(aes(x = latest_intersectional_prev_lci*100, 
                   xend = latest_intersectional_prev_uci*100, 
                   y = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev, na.rm=T), 
                   yend = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev, na.rm=T)), 
               col = "darkgreen", linewidth = 1.25, alpha = 0.25) +
  geom_point(aes(x = earliest_intersectional_prev*100, color = "Earliest"), size = 2) +
  geom_point(aes(x = latest_intersectional_prev*100, color = "Latest"), size = 2) +
  geom_text(aes(x = 60, y = fct_reorder(as.factor(intersectional_short), latest_intersectional_prev, na.rm=T), 
                label = year_range), size = 2.5) +
  labs(y = "Intersectional Strata") +
  scale_x_continuous(name = "FMD Prevalence (%)", breaks = c(0,10,20,30,40,50), limits = c(0,65)) +
  scale_color_manual(name = "Estimate", values = groups) +
  ggtitle("Earliest & Latest FMD Prevalence") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), 
                          legend.position = c(0.65,0.105), legend.margin = margin(0,0,0,0)) -> fig2b

#### Figure 2C - Fully Intersectional Strata Rankings and Variation ####
nchs %>% 
  select(intersectional_short, n_ests, n_ests_low, 
         strata_min_FMD_mu_intersectional_rank_scaled, strata_max_FMD_mu_intersectional_rank_scaled, 
         strata_mu_FMD_mu_intersectional_rank_scaled, strata_sd_FMD_mu_intersectional_rank_scaled,
         strata_med_FMD_mu_intersectional_rank_scaled, strata_1qr_FMD_mu_intersectional_rank_scaled,
         strata_3qr_FMD_mu_intersectional_rank_scaled,
         ethnicity, race, gender, education, poverty) %>%
  distinct() %>%
  mutate(mu_rank_rank = dense_rank(strata_mu_FMD_mu_intersectional_rank_scaled)) -> ranking_summary
names(ranking_summary)[c(4:10)] <- c("min_rank", "max_rank", "mu_rank", "sd_rank", "med_rank", "qr1_rank", "qr3_rank")

ggplot(ranking_summary[!is.na(ranking_summary$mu_rank_rank),], 
       aes(x = mu_rank, y = fct_reorder(as.factor(intersectional_short), mu_rank, na.rm=T))) +
  geom_segment(aes(x = (mu_rank - sd_rank), 
                   xend = (mu_rank + sd_rank), 
                   y = fct_reorder(as.factor(intersectional_short), mu_rank, na.rm=T), 
                   yend = fct_reorder(as.factor(intersectional_short), mu_rank, na.rm=T)), 
               col = "grey", linewidth = 1.25) +
  geom_point(size = 2, col = "grey") +
  geom_text(data = ranking_summary[!is.na(ranking_summary$mu_rank), ],
            aes(x = 33, y = fct_reorder(as.factor(intersectional_short), mu_rank, na.rm=T), label = n_ests_low)) +
  labs(x = "Average Scaled Rank (1=Lowest)", y = "Intersectional Strata", title = "Average FMD Prevalence") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1, 8, 15, 23, 30)) -> fig2c

grid.arrange(fig2a, fig2b, nrow = 1) -> fig2
rm(fig2a, fig2b, ranking_summary)

### Figure 3- Average Changes in Fully Intersectional Social Inequities ####
#Absolute Inequity
nchs %>% 
  filter(!is.na(priv_diff_abs_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_abs_FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional_short), priv_diff_abs_FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 1.2, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Absolute Annual Change (pp)") +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("Absolute Inequity") +
  theme(plot.title = element_text(hjust = 0.5)) -> fig3a
#Relative Inequity
nchs %>% 
  filter(!is.na(priv_diff_mult_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_mult_FMD_mu_intersectional_annual_change, y = fct_reorder(as.factor(intersectional_short), priv_diff_mult_FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 1.03, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 1) +
  scale_x_continuous(name = "Multiplicative Annual Change", breaks = c(0.85, 0.9, 0.95, 1)) +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("Relative Inequity") +
  theme(plot.title = element_text(hjust = 0.5)) -> fig3b
grid.arrange(fig3a, fig3b, nrow = 1) -> fig3
rm(fig3a, fig3b)

#### Figure A1 - % Missing Estimates Over Time Per Group ####
for (dataset in c("nchs", "brfss")) {
  na_obs <- data.frame(strata = variables, n_na_all = NA_real_, perc_na_all = NA_real_, 
                       perc_na_min = NA_real_, perc_na_max = NA_real_,
                       perc_na_qr1 = NA_real_, perc_na_qr2 = NA_real_, perc_na_qr3 = NA_real_)
  na_obs$n_stratifications <- ifelse(na_obs$strata == "intersectional", 5, 
                                     ifelse(na_obs$strata == "overall", 0, (str_count(na_obs$strata, "_")+1)))
  for (strat in unique(na_obs$strata)) {
    na_obs$perc_na_all[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(", dataset, "$FMD_mu_", strat, "))/832*100")))
    na_obs$n_na_all[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(", dataset, "$FMD_mu_", strat, "))")))
    groups <- str_replace_all(strat, "_", ", ")
    groups <- str_replace(groups, "educ", "education")
    groups <- str_replace(groups, "hisp", "ethnicity")
    if (strat == "overall") {
      perc_na <- data.frame(qr1 = na_obs$perc_na_all[na_obs$strata == strat],
                            qr2 = na_obs$perc_na_all[na_obs$strata == strat],
                            qr3 = na_obs$perc_na_all[na_obs$strata == strat],
                            min = na_obs$perc_na_all[na_obs$strata == strat],
                            max = na_obs$perc_na_all[na_obs$strata == strat])
    } else {
      perc_na <- eval(parse(text = paste0(dataset, " %>% ",
                                          "group_by(", groups, ") %>% ",
                                          "mutate(perc_na = sum(is.na(FMD_mu_", strat, "))/832*100) %>% ",
                                          "ungroup() %>% ",
                                          "mutate(qr1 = quantile(perc_na, c(0.25), na.rm=T),
                                                    qr2 = quantile(perc_na, c(0.5), na.rm=T),
                                                    qr3 = quantile(perc_na, c(0.75), na.rm=T),
                                                    min = min(perc_na, na.rm=T),
                                                    max = max(perc_na, na.rm=T)) %>% ",
                                          "select(qr1, qr2, qr3, min, max) %>% distinct()")))
    }
    perc_na$qr1 <- perc_na$qr1 * 2^(na_obs$n_stratifications[na_obs$strata == strat])
    perc_na$qr2 <- perc_na$qr2 * 2^(na_obs$n_stratifications[na_obs$strata == strat])
    perc_na$qr3 <- perc_na$qr3 * 2^(na_obs$n_stratifications[na_obs$strata == strat])
    perc_na$min <- perc_na$min * 2^(na_obs$n_stratifications[na_obs$strata == strat])
    perc_na$max <- perc_na$max * 2^(na_obs$n_stratifications[na_obs$strata == strat])
    na_obs$perc_na_qr1[na_obs$strata == strat] <- perc_na$qr1
    na_obs$perc_na_qr2[na_obs$strata == strat] <- perc_na$qr2
    na_obs$perc_na_qr3[na_obs$strata == strat] <- perc_na$qr3
    na_obs$perc_na_min[na_obs$strata == strat] <- perc_na$min
    na_obs$perc_na_max[na_obs$strata == strat] <- perc_na$max
    rm(perc_na, groups)
  }
  na_obs$n_na_all <- ifelse(na_obs$n_stratifications == 1, na_obs$n_na_all/16,
                            ifelse(na_obs$n_stratifications == 2, na_obs$n_na_all/8,
                                   ifelse(na_obs$n_stratifications == 3, na_obs$n_na_all/4,
                                          ifelse(na_obs$n_stratifications == 4, na_obs$n_na_all/2, na_obs$n_na_all))))
  na_obs <- na_obs %>% arrange(n_stratifications, perc_na_all)
  na_obs$var_order <- 1:32
  na_obs$strata <- gsub("race", "Race", na_obs$strata)
  na_obs$strata <- gsub("hisp", "Ethnicity", na_obs$strata)
  na_obs$strata <- gsub("gender", "Sex", na_obs$strata)
  na_obs$strata <- gsub("educ", "Education", na_obs$strata)
  na_obs$strata <- gsub("poverty", "Poverty", na_obs$strata)
  na_obs$strata <- gsub("_", ", ", na_obs$strata)
  na_obs$strata <- gsub("overall", "None", na_obs$strata)
  na_obs$strata <- gsub("intersectional", "Fully Intersectionally", na_obs$strata)
  na_obs$strata <- reorder(as.factor(na_obs$strata), na_obs$var_order, sum, decreasing = T)
  na_obs$data <- toupper(dataset)
  rm(strat)
  assign(paste0("na_obs_", dataset), na_obs)
  rm(dataset)
}
na_obs <- rbind(na_obs_brfss, na_obs_nchs)
rm(na_obs_brfss, na_obs_nchs)

na_obs %>% mutate(data = ifelse(data == "NCHS", "NCHS Guidelines", "BRFSS Guidelines")) %>%
  ggplot(aes(x = perc_na_qr2, y = strata)) +
  geom_point(size=2) +
  geom_segment(aes(x = perc_na_min, xend = perc_na_max, y = strata, yend = strata)) +
  geom_segment(aes(x = perc_na_qr1, xend = perc_na_qr3, y = strata, yend = strata), linewidth = 1.2) +
  labs(x = "Group Estimates Suppressed (%)", y = "Social Axes Stratified By") + 
  theme_classic() + facet_wrap(~data) -> figA1a

ggplot(na_obs, aes(x = perc_na_all, y = strata, shape = as.factor(data))) +
  geom_point(size=2) +
  scale_shape_manual(name = "", values = c(1,19)) +
  xlim(0, 25) +
  labs(x = "Total Suppressed (%)", y = "", title = "") + 
  theme_classic() + theme(legend.position = c(0.43, 1.04), 
                          legend.direction = "horizontal",
                          legend.margin = margin(0,0,0,0),
                          axis.text.y = element_blank()) -> figA1b

grid.arrange(figA1a, figA1b, nrow = 1, widths = c(0.72, 0.28), 
             top = grid::textGrob("Suppression of Annual Estimates", hjust = 0)) -> figA1
rm(figA1a, figA1b, na_obs)

#### Creating Intersectionally-Oriented Dataset for Intra-categorical Figures ####
#Creating empty data.frame with needed variables
intra <- expand.grid(population = c("Overall", 
                                    "Non-Hispanic", "Hispanic", "White", "Black", "Male", 
                                    "Female", "<HS", ">=HS", "Not In Poverty", "In Poverty", 
                                    unique(nchs$intersectional_short)),
                     year = c(1993:2001, 2003:2019), 
                     FMD = NA_real_, FMD_lci = NA_real_, FMD_uci = NA_real_, FMD_AAC = NA_real_)
intra$common <- as.factor(ifelse(intra$population %in% c("Overall", "Non-Hispanic", "Hispanic", "White", "Black", "Male", 
                                                         "Female", "<HS", ">=HS", "Not In Poverty", "In Poverty"), 1, 0))

#Populating data.frame
for (pop in unique(intra$population)) {
  for (year in unique(intra$year)) {
    if (pop == "Overall") {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_overall[nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_overall_lci[nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_overall_uci[nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_overall_annual_change)
    }
    if (pop %in% c("Non-Hispanic", "Hispanic")) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_hisp[nchs$ethnicity == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_hisp_lci[nchs$ethnicity == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_hisp_uci[nchs$ethnicity == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_hisp_annual_change[nchs$ethnicity == pop])
    } 
    if (pop %in% c("White", "Black")) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_race[nchs$race == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_race_lci[nchs$race == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_race_uci[nchs$race == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_race_annual_change[nchs$race == pop])
    } 
    if (pop %in% c("Male", "Female")) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_gender[nchs$gender == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_gender_lci[nchs$gender == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_gender_uci[nchs$gender == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_gender_annual_change[nchs$gender == pop])
    } 
    if (pop %in% c("<HS", ">=HS")) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_educ[nchs$education == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_educ_lci[nchs$education == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_educ_uci[nchs$education == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_educ_annual_change[nchs$education == pop])
    } 
    if (pop %in% c("Not In Poverty", "In Poverty")) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_poverty[nchs$poverty == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_poverty_lci[nchs$poverty == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_poverty_uci[nchs$poverty == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_poverty_annual_change[nchs$poverty == pop])
    } 
    if (pop %in% unique(nchs$intersectional_short)) {
      intra$FMD[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_intersectional[nchs$intersectional_short == pop & nchs$year == year])
      intra$FMD_lci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_intersectional_lci[nchs$intersectional_short == pop & nchs$year == year])
      intra$FMD_uci[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_intersectional_uci[nchs$intersectional_short == pop & nchs$year == year])
      intra$FMD_AAC[intra$population == pop & intra$year == year] <- unique(nchs$FMD_mu_intersectional_annual_change[nchs$intersectional_short == pop])
    }
  }
}
rm(pop, year)

## Creating second dataset centering common groups and intersectional spread
intra_spread <- data.frame(population = c("Non-Hispanic", "Hispanic", "White", "Black", "Male", 
                                          "Female", "<HS", ">=HS", "Not In Poverty", "In Poverty"),
                           int_trend_n = NA_real_, int_1993_n = NA_real_, int_2019_n = NA_real_,
                           trend = NA_real_, FMD_1993 = NA_real_, FMD_2019 = NA_real_,
                           int_trend_med = NA_real_, int_trend_qr1 = NA_real_, int_trend_qr3 = NA_real_, int_trend_min = NA_real_, int_trend_max = NA_real_,
                           int_trend_below_common = NA_real_, int_trend_above_common = NA_real_,
                           int_1993_med = NA_real_, int_1993_qr1 = NA_real_, int_1993_qr3 = NA_real_, int_1993_min = NA_real_, int_1993_max = NA_real_,
                           int_2019_med = NA_real_, int_2019_qr1 = NA_real_, int_2019_qr3 = NA_real_, int_2019_min = NA_real_, int_2019_max = NA_real_,
                           int_1993_below_common = NA_real_, int_1993_above_common = NA_real_,
                           int_2019_below_common = NA_real_, int_2019_above_common = NA_real_)

# Populating data.frame from 'nchs'
for (pop in unique(intra_spread$population)) {
  if (pop %in% c("Non-Hispanic", "Hispanic")) {
    common = "ethnicity"
    intra_spread$trend[intra_spread$population == pop] <- unique(nchs$FMD_mu_hisp_annual_change[nchs$ethnicity == pop])
    intra_spread$FMD_1993[intra_spread$population == pop] <- unique(nchs$FMD_mu_hisp[nchs$ethnicity == pop & nchs$year == 1993])
    intra_spread$FMD_2019[intra_spread$population == pop] <- unique(nchs$FMD_mu_hisp[nchs$ethnicity == pop & nchs$year == 2019])
  }
  if (pop %in% c("White", "Black")) {
    common = "race"
    intra_spread$trend[intra_spread$population == pop] <- unique(nchs$FMD_mu_race_annual_change[nchs$race == pop])
    intra_spread$FMD_1993[intra_spread$population == pop] <- unique(nchs$FMD_mu_race[nchs$race == pop & nchs$year == 1993])
    intra_spread$FMD_2019[intra_spread$population == pop] <- unique(nchs$FMD_mu_race[nchs$race == pop & nchs$year == 2019])
  }
  if (pop %in% c("Male", "Female")) {
    common = "gender"
    intra_spread$trend[intra_spread$population == pop] <- unique(nchs$FMD_mu_gender_annual_change[nchs$gender == pop])
    intra_spread$FMD_1993[intra_spread$population == pop] <- unique(nchs$FMD_mu_gender[nchs$gender == pop & nchs$year == 1993])
    intra_spread$FMD_2019[intra_spread$population == pop] <- unique(nchs$FMD_mu_gender[nchs$gender == pop & nchs$year == 2019])
  }
  if (pop %in% c("<HS", ">=HS")) {
    common = "education"
    intra_spread$trend[intra_spread$population == pop] <- unique(nchs$FMD_mu_educ_annual_change[nchs$education == pop])
    intra_spread$FMD_1993[intra_spread$population == pop] <- unique(nchs$FMD_mu_educ[nchs$education == pop & nchs$year == 1993])
    intra_spread$FMD_2019[intra_spread$population == pop] <- unique(nchs$FMD_mu_educ[nchs$education == pop & nchs$year == 2019])
  }
  if (pop %in% c("Not In Poverty", "In Poverty")) {
    common = "poverty"
    intra_spread$trend[intra_spread$population == pop] <- unique(nchs$FMD_mu_poverty_annual_change[nchs$poverty == pop])
    intra_spread$FMD_1993[intra_spread$population == pop] <- unique(nchs$FMD_mu_poverty[nchs$poverty == pop & nchs$year == 1993])
    intra_spread$FMD_2019[intra_spread$population == pop] <- unique(nchs$FMD_mu_poverty[nchs$poverty == pop & nchs$year == 2019])
  }
  #Trends
  intra_spread$int_trend_n[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[!is.na(nchs$FMD_mu_intersectional_annual_change) & nchs$", common, " == pop & nchs$year == 2019,])")))
  intra_spread$int_trend_min[intra_spread$population == pop] <- eval(parse(text = paste0("min(nchs$FMD_mu_intersectional_annual_change[nchs$", common, " == pop & nchs$year == 2019], na.rm = T)")))
  intra_spread$int_trend_qr1[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional_annual_change[nchs$", common, " == pop & nchs$year == 2019], c(0.25), na.rm = T)")))
  intra_spread$int_trend_med[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional_annual_change[nchs$", common, " == pop & nchs$year == 2019], c(0.50), na.rm = T)")))
  intra_spread$int_trend_qr3[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional_annual_change[nchs$", common, " == pop & nchs$year == 2019], c(0.75), na.rm = T)")))
  intra_spread$int_trend_max[intra_spread$population == pop] <- eval(parse(text = paste0("max(nchs$FMD_mu_intersectional_annual_change[nchs$", common, " == pop & nchs$year == 2019], na.rm = T)")))
  intra_spread$int_trend_below_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 2019 & !is.na(nchs$FMD_mu_intersectional_annual_change) & ",
                                                                                                  "nchs$FMD_mu_intersectional_annual_change < (intra_spread$trend[intra_spread$population == pop]),])")))
  intra_spread$int_trend_above_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 2019 & !is.na(nchs$FMD_mu_intersectional_annual_change) & ",
                                                                                                  "nchs$FMD_mu_intersectional_annual_change > (intra_spread$trend[intra_spread$population == pop]),])")))
  #2019 FMD Prevalences
  intra_spread$int_1993_n[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[!is.na(nchs$FMD_mu_intersectional) & nchs$", common, " == pop & nchs$year == 1993,])")))
  intra_spread$int_1993_min[intra_spread$population == pop] <- eval(parse(text = paste0("min(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 1993], na.rm = T)")))
  intra_spread$int_1993_qr1[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 1993], c(0.25), na.rm = T)")))
  intra_spread$int_1993_med[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 1993], c(0.50), na.rm = T)")))
  intra_spread$int_1993_qr3[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 1993], c(0.75), na.rm = T)")))
  intra_spread$int_1993_max[intra_spread$population == pop] <- eval(parse(text = paste0("max(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 1993], na.rm = T)")))
  intra_spread$int_1993_below_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 1993 & !is.na(nchs$FMD_mu_intersectional) & ",
                                                                                                 "nchs$FMD_mu_intersectional < (intra_spread$FMD_1993[intra_spread$population == pop]),])")))
  intra_spread$int_1993_above_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 1993 & !is.na(nchs$FMD_mu_intersectional) & ",
                                                                                                 "nchs$FMD_mu_intersectional > (intra_spread$FMD_1993[intra_spread$population == pop]),])")))
  #2019 FMD Prevalences
  intra_spread$int_2019_n[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[!is.na(nchs$FMD_mu_intersectional) & nchs$", common, " == pop & nchs$year == 2019,])")))
  intra_spread$int_2019_min[intra_spread$population == pop] <- eval(parse(text = paste0("min(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 2019], na.rm = T)")))
  intra_spread$int_2019_qr1[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 2019], c(0.25), na.rm = T)")))
  intra_spread$int_2019_med[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 2019], c(0.50), na.rm = T)")))
  intra_spread$int_2019_qr3[intra_spread$population == pop] <- eval(parse(text = paste0("quantile(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 2019], c(0.75), na.rm = T)")))
  intra_spread$int_2019_max[intra_spread$population == pop] <- eval(parse(text = paste0("max(nchs$FMD_mu_intersectional[nchs$", common, " == pop & nchs$year == 2019], na.rm = T)")))
  intra_spread$int_2019_below_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 2019 & !is.na(nchs$FMD_mu_intersectional) & ",
                                                                                                 "nchs$FMD_mu_intersectional < (intra_spread$FMD_2019[intra_spread$population == pop]),])")))
  intra_spread$int_2019_above_common[intra_spread$population == pop] <- eval(parse(text = paste0("NROW(nchs[nchs$", common, " == pop & nchs$year == 2019 & !is.na(nchs$FMD_mu_intersectional) & ",
                                                                                                 "nchs$FMD_mu_intersectional > (intra_spread$FMD_2019[intra_spread$population == pop]),])")))
}
# Rounding figures
for (i in c(5:12,15:24)) {
  intra_spread[[i]] <- round(intra_spread[[i]]*100, 3)
}

#### Figures 4 - Intra-categorical comparisons for each common social group ####
# All trends (overall, common, intersectional)
ggplot(intra[!is.na(intra$FMD_AAC),], aes(x = FMD_AAC*100, y = fct_reorder(population, FMD_AAC), col = common)) +
  geom_point(size = 2) + geom_vline(xintercept = 0) +
  scale_color_manual(values = c("grey80", "black")) +
  theme_classic() + theme(legend.position = "none")

#Empty Plot to Create Legend
ggplot(data = data.frame(Estimate = factor(c("Common Group Estimate", "Intersectional Group Median"), levels = c("Common Group Estimate", "Intersectional Group Median")),
                         Group = c("Intersectional Group IQR"),
                         Point = c(0,1)), aes(x = Point, y = 1, shape = Estimate, size = Estimate, col = Group)) +
  geom_point() + scale_shape_manual(values = c(124, 16)) + scale_size_manual(values = c(3.5, 2)) + 
  geom_line(size = 2.1) + scale_color_manual(values = "black") + 
  theme_classic() + 
  guides(shape = guide_legend(order = 1),
         size = guide_legend(order = 1),
         color = guide_legend(order = 2, keywidth = unit(1, "cm"))) +
  theme(plot.margin = margin(0,0,0,0), legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 10), legend.spacing.x = unit(0, "cm")) -> leg_plot
leg_plot <- ggpubr::get_legend(leg_plot)

# Common group spread - FMD AAC
ggplot(intra_spread, aes(x = int_trend_med, y = fct_reorder(population, trend))) +
  geom_segment(aes(x = int_trend_qr1, xend = int_trend_qr3, y = population, yend = population), linewidth = 2, alpha = 0.4) +
  geom_point(size=2) +
  geom_point(aes(x = trend), size = 4, shape = 124) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(breaks = c(0, 0.15, 0.30, 0.45, 0.6), n.breaks = 5, limits = c(-0.01,0.6)) +
  labs(x = "FMD Prevalence AAC (pp)", y = "Common Social Group") + 
  theme_classic() + theme(plot.margin = margin(15,0,0,5)) -> fig4a
# Common group spread - 2019 FMD
ggplot(intra_spread, aes(x = int_2019_med, y = fct_reorder(population, FMD_2019))) +
  geom_segment(aes(x = int_2019_qr1, xend = int_2019_qr3, y = population, yend = population), linewidth = 1.2) +
  geom_point(size=2) +
  geom_point(aes(x = FMD_2019), size = 4, shape = 124) +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30), n.breaks = 5, limits = c(10,30)) + 
  labs(x = "2019 FMD Prevalence (%)", y = "Common Social Group") + 
  theme_classic() + theme(plot.margin = margin(15,0,0,1), axis.title.y = element_blank()) -> fig4b

grid.arrange(arrangeGrob(fig4a, fig4b2, ncol = 2, nrow = 1),
             arrangeGrob(leg_plot, ncol = 1, nrow = 1), heights=c(5,1),
             top = grid::textGrob("Outcome Variation Within Common Social Groups", gp = grid::gpar(fontsize=14)))

#### Descriptives - Relative Inequities (1993, 2019) ####
nchs %>% 
  filter(intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>% 
  mutate(bottom_1 = ifelse(FMD_mu_intersectional_rank %in% c(28), "bottom", "not")) %>% group_by(intersectional) %>%
  mutate(bottom_1 = ifelse(bottom_1[year == 2019] == "bottom", "bottom", "not")) %>% ungroup() %>% group_by(year) %>%
  mutate(med_diff = quantile(priv_diff_mult_FMD_mu_intersectional*100, c(0.5), na.rm=T),
         q25_diff = quantile(priv_diff_mult_FMD_mu_intersectional*100, c(0.25), na.rm=T),
         q75_diff = quantile(priv_diff_mult_FMD_mu_intersectional*100, c(0.75), na.rm=T),
         sd_diff = sd(priv_diff_mult_FMD_mu_intersectional*100, na.rm=T)) %>% ungroup() %>%
  mutate(med_diff_annual_change = quantile(priv_diff_mult_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.5), na.rm=T),
         q25_diff_annual_change = quantile(priv_diff_mult_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.25), na.rm=T),
         q75_diff_annual_change = quantile(priv_diff_mult_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.75), na.rm=T),
         sd_diff_annual_change = sd(priv_diff_mult_FMD_mu_intersectional*100, na.rm=T)) %>% 
  select(year, med_diff, q25_diff, q75_diff, med_diff_annual_change, q25_diff_annual_change, q75_diff_annual_change) %>% 
  filter(year %in% c(1993, 2019)) %>% distinct()

# Figure 4B - Alternative (1993 & 2019)
groups4 <- c("1993" = "darkred", "2019" = "darkgreen")
intra_spread$ests_low_1993 <- ifelse(intra_spread$int_1993_n < (0.5*16), "**", ifelse(intra_spread$int_1993_n < (0.8*16), "*", ""))
intra_spread$ests_low_2019 <- ifelse(intra_spread$int_2019_n < (0.5*16), "**", ifelse(intra_spread$int_2019_n < (0.8*16), "*", ""))

ggplot(intra_spread, aes(x = int_2019_med, y = fct_reorder(population, FMD_2019))) +
  geom_segment(aes(x = int_2019_qr1, xend = int_2019_qr3, y = population, yend = population), linewidth = 2, col = "darkgreen", alpha = 0.25) +
  geom_point(aes(col = "2019"), size=2) +
  geom_point(aes(x = FMD_2019), size = 4, shape = 124, col = "darkgreen") +
  geom_segment(aes(x = int_1993_qr1, xend = int_1993_qr3, y = population, yend = population), linewidth = 2, col = "darkred", alpha = 0.25) +
  geom_point(aes(x = int_1993_med, col = "1993"), size=2) +
  geom_point(aes(x = FMD_1993), size = 4, shape = 124, col = "darkred") +
  geom_text(aes(x = 30, y = fct_reorder(population, FMD_2019, na.rm=T), label = ests_low_1993), 
            size = 4, col = "darkred") +
  geom_text(aes(x = 31, y = fct_reorder(population, FMD_2019, na.rm=T), label = ests_low_2019), 
            size = 4, col = "darkgreen") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30), n.breaks = 5) + 
  scale_color_manual(name = "Estimate", values = groups4) +
  labs(x = "FMD Prevalence (%)", y = "Common Social Group") + 
  theme_classic() + 
  theme(plot.margin = margin(15,0,0,1), axis.title.y = element_blank(), legend.position = c(0.75, 0.2)) -> fig4b2
