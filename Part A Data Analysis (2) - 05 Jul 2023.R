#### Aim 1 Part A Data Analysis (2) - 05 July 2023 ####
#Author - Kieran Blaikie
#Date - 05 July 2023

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
brfss <- data[data$suppression == "BRFSS", ]
nchs <- data[data$suppression == "NCHS", ]

#Creating rank variables for absolute annual mental health summaries
for (dataset in c("brfss", "nchs")) {
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
for (dataset in c("brfss", "nchs")) {
  for (var in variables[2:32]) {
    for (outcome in c("FMD_mu_", "days_mu_", "priv_diff_abs_FMD_mu_", "priv_diff_mult_FMD_mu_", "nat_diff_abs_FMD_mu_", "nat_diff_mult_FMD_mu_")) {
      print(paste0("Dataset: ", dataset, ", Variable: ", outcome, var, "_annual_change_rank"))
      strata <- ifelse(var == "intersectional", "intersectional", gsub("_", ", ", var))
      strata <- gsub("hisp", "ethnicity", strata)
      strata <- gsub("educ", "education", strata)
      
      eval(parse(text = paste0(dataset, " <- ", dataset, " %>% mutate(strata = paste0(", strata, "))")))
      eval(parse(text = paste0("temp_ranks <- ", dataset, " %>% ",
                               "arrange(year) %>% ",
                               "filter(!is.na(", outcome, var, ")) %>% ",
                               "group_by(strata) %>% ",
                               "filter(NROW(strata) >= 2) %>% ",
                               "mutate(lag_years = year - lag(year, 1), 
                                       diff_outcome = ", outcome, var, " - lag(", outcome, var, ", 1),
                                       diff_outcome = diff_outcome / lag_years,
                                       mu_diff_outcome = mean(diff_outcome, na.rm = T)) %>% ",
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
rm(dataset, var, outcome, strata)

#Creating overall annual change variables for BRFSS and NCHS
#BRFSS
brfss %>% arrange(year) %>% select(year, FMD_mu_overall) %>% distinct() %>% mutate(FMD_mu_overall_annual_change = FMD_mu_overall - lag(FMD_mu_overall, 1),
                                                                                   FMD_mu_overall_annual_change = mean(FMD_mu_overall_annual_change, na.rm=T))  -> brfss_to_merge
brfss$FMD_mu_overall_annual_change <- brfss_to_merge$FMD_mu_overall_annual_change[1]
nchs %>% arrange(year) %>% select(year, FMD_mu_overall) %>% distinct() %>% mutate(FMD_mu_overall_annual_change = FMD_mu_overall - lag(FMD_mu_overall, 1),
                                                                                   FMD_mu_overall_annual_change = mean(FMD_mu_overall_annual_change, na.rm=T))  -> nchs_to_merge
nchs$FMD_mu_overall_annual_change <- nchs_to_merge$FMD_mu_overall_annual_change[1]
rm(brfss_to_merge, nchs_to_merge)

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

#### Figure - % Missing Estimates Over Time Per Group ####
na_obs <- data.frame(strata = variables,n_na = NA_real_, perc_na = NA_real_)
na_obs$n_stratifications <- ifelse(na_obs$strata == "intersectional", 5, 
                            ifelse(na_obs$strata == "overall", 0, (str_count(na_obs$strata, "_")+1)))
for (strat in unique(na_obs$strata)) {
  na_obs$perc_na[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(nchs$FMD_mu_", strat, "))/832*100")))
  na_obs$n_na[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(nchs$FMD_mu_", strat, "))")))
}
na_obs$n_na <- ifelse(na_obs$n_stratifications == 1, na_obs$n_na/16,
               ifelse(na_obs$n_stratifications == 2, na_obs$n_na/8,
               ifelse(na_obs$n_stratifications == 3, na_obs$n_na/4,
               ifelse(na_obs$n_stratifications == 4, na_obs$n_na/2, na_obs$n_na))))
na_obs <- na_obs %>% arrange(n_stratifications, perc_na)
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
rm(strat)

ggplot(na_obs, aes(x = perc_na, y = strata, shape = as.factor(n_stratifications))) +
  geom_point() + 
  theme_classic() +
  scale_shape_manual(name = "No. Axes", values = c(1, 19, 0, 15, 2, 17)) +
  labs(x = "Annual Estimates Suppressed (%)", y = "Social Axes Stratified By",
       title = "NCHS Suppression of Estimates") +
  theme(legend.position = c(0.88, 0.73), legend.background = element_rect(color = "black"),
        panel.grid.major.x = element_line(colour = rgb(235, 235, 235, 275, maxColorValue = 275)),
        plot.title = element_text(hjust = 0.5))

#### Figures - Absolute FMD Over Time ####
#Overall
ggplot(nchs, aes(x = year, y = FMD_mu_overall*100)) +
  geom_ribbon(aes(ymin = FMD_mu_overall_lci*100, ymax = FMD_mu_overall_uci*100, x = year), alpha = 0.2) +
  geom_point() + geom_line() +
  ylab("FMD Prevalence (%)") +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  expand_limits(y = c(8, 16))

#1-Way Stratification
ggplot(nchs, aes(x = year, y = FMD_mu_gender*100, col = gender)) +
  geom_ribbon(aes(ymin = FMD_mu_gender_lci*100, ymax = FMD_mu_gender_uci*100, x = year, fill = gender), alpha = 0.2) +
  geom_point() + geom_line() + 
  ylab("FMD Prevalence (%)") +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Gender"))

#2-Way Stratification
nchs %>% mutate(Strata = paste0(race, " ", gender)) %>%
  ggplot(aes(x = year, y = FMD_mu_race_gender*100, col = Strata)) +
  geom_ribbon(aes(ymin = FMD_mu_race_gender_lci*100, ymax = FMD_mu_race_gender_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + 
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(6, 10, 14, 18)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata"))

#3-Way Stratification
nchs %>% mutate(Strata = paste0(gender, " ", poverty, " with ", education)) %>%
  ggplot(aes(x = year, y = FMD_mu_gender_educ_poverty*100, col = Strata)) +
  geom_ribbon(aes(ymin = FMD_mu_gender_educ_poverty_lci*100, ymax = FMD_mu_gender_educ_poverty_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + 
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(5, 10, 15, 20, 25, 30, 35)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata", nrow = 4))

#3-Way Stratification (smoothed)
nchs %>% mutate(Strata = paste0(gender, " ", poverty, " with ", education)) %>%
  ggplot(aes(x = year, y = FMD_mu_gender_educ_poverty*100, col = Strata)) +
  geom_ribbon(stat = "smooth", aes(ymin = FMD_mu_gender_educ_poverty_lci*100, ymax = FMD_mu_gender_educ_poverty_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_line(stat = "smooth") + 
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(5, 10, 15, 20, 25, 30, 35)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata", nrow = 4))

#4-Way Stratification
nchs %>% mutate(Strata = paste0(ethnicity, " ", race, " ", gender, " with", education)) %>%
  ggplot(aes(x = year, y = FMD_mu_race_hisp_gender_educ*100, group = Strata)) +
  geom_point(col = "grey", alpha = 0.45) + geom_line(col = "grey", alpha = 0.45) + 
  theme_classic() + 
  geom_point(aes(x = year, y = FMD_mu_overall*100), col = "black", size = 2) +
  geom_line(aes(x = year, y = FMD_mu_overall*100), col = "black", size = 1) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(5, 10, 15, 20, 25, 30, 35)) +
  theme(legend.position = "bottom")

#5-Way Stratification
ggplot(nchs, aes(x = year, y = FMD_mu_intersectional*100, group = intersectional)) +
  geom_point(col = "grey", alpha = 0.45) + geom_line(col = "grey", alpha = 0.45) + 
  theme_classic() + 
  geom_point(aes(x = year, y = FMD_mu_overall*100), col = "black", size = 2) +
  geom_line(aes(x = year, y = FMD_mu_overall*100), col = "black", size = 1) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  theme(legend.position = "bottom")

#5-Way Stratification (smoothed)
ggplot(nchs, aes(x = year, y = FMD_mu_intersectional*100, group = intersectional)) +
  geom_line(stat = "smooth", col = "grey", alpha = 0.6) + 
  theme_classic() + 
  geom_line(stat = "smooth", aes(x = year, y = FMD_mu_overall*100), col = "black", size = 1) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(5, 10, 15, 20, 25, 30, 35, 40), limits = c(5,40)) +
  theme(legend.position = "bottom")

#### Figures - Absolute FMD Inequity Over Time (vs. Socially Privileged) ####
#1-Way Stratification
ggplot(nchs[nchs$poverty == "In Poverty",], aes(x = year, y = priv_diff_abs_FMD_mu_poverty*100, col = poverty)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_uci*100, x = year, fill = poverty), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Absolute Inequity in FMD Prevalence (%)", breaks = c(-20, -10, -5, 0, 5, 10, 15, 20)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Household"))

#2-Way Stratification
nchs %>% mutate(Strata = paste0(race, " ", gender)) %>%
  ggplot(aes(x = year, y = priv_diff_abs_FMD_mu_race_gender*100, col = Strata)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_race_gender_lci*100, ymax = priv_diff_abs_FMD_mu_race_gender_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Absolute Inequity", breaks = c(-4, -2, 0, 2, 4, 6, 8)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata"))

#3-Way Stratification
nchs %>% mutate(Strata = paste0(gender, " ", poverty, " with ", education)) %>%
  ggplot(aes(x = year, y = priv_diff_abs_FMD_mu_gender_educ_poverty*100, col = Strata)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_gender_educ_poverty_lci*100, ymax = priv_diff_abs_FMD_mu_gender_educ_poverty_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Absolute Inequity", breaks = c(-5, 0, 5, 10, 15, 20, 25)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata", nrow = 4))

#3-Way Stratification (smoothed)
nchs %>% mutate(Strata = paste0(gender, " ", poverty, " with ", education)) %>%
  ggplot(aes(x = year, y = priv_diff_abs_FMD_mu_gender_educ_poverty*100, col = Strata)) +
  geom_ribbon(stat = "smooth", aes(ymin = priv_diff_abs_FMD_mu_gender_educ_poverty_lci*100, ymax = priv_diff_abs_FMD_mu_gender_educ_poverty_uci*100, x = year, fill = Strata), alpha = 0.2) +
  geom_line(stat = "smooth") + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Absolute Inequity", breaks = c(-5, 0, 5, 10, 15, 20, 25)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata", nrow = 4))

#4-Way Stratification
nchs %>% mutate(Strata = paste0(ethnicity, " ", race, " ", gender, " with", education)) %>%
  ggplot(aes(x = year, y = priv_diff_abs_FMD_mu_race_hisp_gender_educ*100, group = Strata)) +
  geom_point(col = "grey", alpha = 0.45) + geom_line(col = "grey", alpha = 0.45) + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Absolute Inequity", breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35)) +
  theme(legend.position = "bottom")

#5-Way Stratification
nchs %>% 
  filter(intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>% 
  mutate(bottom_1 = ifelse(FMD_mu_intersectional_rank %in% c(28), "bottom", "not")) %>% group_by(intersectional) %>%
  mutate(bottom_1 = ifelse(bottom_1[year == 2019] == "bottom", "bottom", "not")) %>% ungroup() %>% group_by(year) %>%
  mutate(med_diff = quantile(priv_diff_abs_FMD_mu_intersectional, c(0.5), na.rm=T),
         q25_diff = quantile(priv_diff_abs_FMD_mu_intersectional, c(0.25), na.rm=T),
         q75_diff = quantile(priv_diff_abs_FMD_mu_intersectional, c(0.75), na.rm=T)) %>% ungroup() %>%
  ggplot(aes(x = year, y = priv_diff_abs_FMD_mu_intersectional*100, group = intersectional)) +
  geom_point(col = "grey", alpha = 0.4) + geom_line(col = "grey", alpha = 0.25) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Absolute Inequity in FMD Prevalence (%)", breaks = c(-10, -5, 0, 10, 20, 30, 40, 50)) +
  geom_point(data = function(x) subset(x, bottom_1 == "bottom"), col = "darkred") +
  geom_line(data = function(x) subset(x, bottom_1 == "bottom"), col = "darkred") +
  geom_text(data = function(x) subset(x, bottom_1 == "bottom" & year == 2019), aes(x = 2003, y = priv_diff_abs_FMD_mu_intersectional*100, label = intersectional), hjust=0, col = "grey30") +
  geom_line(aes(y = med_diff*100), linewidth = 1, linetype = "solid", col = "black") +
  geom_line(aes(y = q25_diff*100), linewidth = 1, linetype = "longdash", col = "black") +
  geom_line(aes(y = q75_diff*100), linewidth = 1, linetype = "longdash", col = "black") +
  ggtitle("Absolute Inequity") -> abs_inequity

#Descriptive summaries
nchs %>% 
  filter(intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>% 
  mutate(bottom_1 = ifelse(FMD_mu_intersectional_rank %in% c(28), "bottom", "not")) %>% group_by(intersectional) %>%
  mutate(bottom_1 = ifelse(bottom_1[year == 2019] == "bottom", "bottom", "not")) %>% ungroup() %>% group_by(year) %>%
  mutate(med_diff = quantile(priv_diff_abs_FMD_mu_intersectional*100, c(0.5), na.rm=T),
         q25_diff = quantile(priv_diff_abs_FMD_mu_intersectional*100, c(0.25), na.rm=T),
         q75_diff = quantile(priv_diff_abs_FMD_mu_intersectional*100, c(0.75), na.rm=T),
         sd_diff = sd(priv_diff_abs_FMD_mu_intersectional*100, na.rm=T)) %>% ungroup() %>%
  mutate(med_diff_annual_change = quantile(priv_diff_abs_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.5), na.rm=T),
         q25_diff_annual_change = quantile(priv_diff_abs_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.25), na.rm=T),
         q75_diff_annual_change = quantile(priv_diff_abs_FMD_mu_intersectional_annual_change[year == 1993]*100, c(0.75), na.rm=T),
         sd_diff_annual_change = sd(priv_diff_abs_FMD_mu_intersectional*100, na.rm=T)) %>% 
  select(year, med_diff, q25_diff, q75_diff, sd_diff, med_diff_annual_change, q25_diff_annual_change, q75_diff_annual_change, sd_diff_annual_change) %>% 
  filter(year %in% c(1993, 2019)) %>% distinct()

#5-Way Stratification (smoothed)
ggplot(nchs, aes(x = year, y = priv_diff_abs_FMD_mu_intersectional*100, group = intersectional)) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ splines::bs(x, knots = 2), col = "grey", alpha = 0.6) + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Absolute Inequity", breaks = c(-10, -5, 0, 10, 20, 30, 40, 50), limits = c(-5, 30)) +
  theme(legend.position = "bottom")

#### Figures - Relative FMD Inequity Over Time (vs. National Average) ####
#1-Way Stratification
ggplot(nchs, aes(x = year, y = nat_diff_mult_FMD_mu_poverty*100-100, col = poverty)) +
  geom_ribbon(aes(ymin = nat_diff_mult_FMD_mu_poverty_lci*100-100, ymax = nat_diff_mult_FMD_mu_poverty_uci*100-100, x = year, fill = poverty), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Relative Inequity", breaks = c(-20, 0, 20, 40, 60, 80, 100, 120, 140)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Household"))

#2-Way Stratification
nchs %>% mutate(Strata = paste0(race, " ", gender)) %>%
  ggplot(aes(x = year, y = nat_diff_mult_FMD_mu_race_gender*100-100, col = Strata)) +
  geom_ribbon(aes(ymin = nat_diff_mult_FMD_mu_race_gender_lci*100-100, ymax = nat_diff_mult_FMD_mu_race_gender_uci*100-100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Relative Inequity", breaks = c(-20, 0, 20, 40, 60, 80, 100, 120, 140)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata"))

#3-Way Stratification
nchs %>% mutate(Strata = paste0(gender, " ", poverty, " with ", education)) %>%
  ggplot(aes(x = year, y = nat_diff_mult_FMD_mu_gender_educ_poverty*100-100, col = Strata)) +
  geom_ribbon(aes(ymin = nat_diff_mult_FMD_mu_gender_educ_poverty_lci*100-100, ymax = nat_diff_mult_FMD_mu_gender_educ_poverty_uci*100-100, x = year, fill = Strata), alpha = 0.2) +
  geom_point() + geom_line() + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Relative Inequity", breaks = c(-50, -25, 0, 25, 50, 75, 100, 125, 150, 175, 200)) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="Strata", nrow = 4))

#4-Way Stratification
nchs %>% mutate(Strata = paste0(ethnicity, " ", race, " ", gender, " with", education)) %>%
  ggplot(aes(x = year, y = nat_diff_mult_FMD_mu_race_hisp_gender_educ*100-100, group = Strata)) +
  geom_point(col = "grey", alpha = 0.45) + geom_line(col = "grey", alpha = 0.45) + geom_hline(yintercept = 0) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%) Relative Inequity", breaks = c(-50, -25, 0, 25, 50, 75, 100, 125, 150, 175, 200)) +
  theme(legend.position = "bottom")

#5-Way Stratification
nchs %>% 
  filter(intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>% 
  mutate(bottom_1 = ifelse(FMD_mu_intersectional_rank %in% c(28), "bottom", "not")) %>% group_by(intersectional) %>%
  mutate(bottom_1 = ifelse(bottom_1[year == 2019] == "bottom", "bottom", "not")) %>% ungroup() %>% group_by(year) %>%
  mutate(med_diff = quantile(priv_diff_mult_FMD_mu_intersectional, c(0.5), na.rm=T),
         q25_diff = quantile(priv_diff_mult_FMD_mu_intersectional, c(0.25), na.rm=T),
         q75_diff = quantile(priv_diff_mult_FMD_mu_intersectional, c(0.75), na.rm=T)) %>% ungroup() %>%
  ggplot(aes(x = year, y = priv_diff_mult_FMD_mu_intersectional*100-100, group = intersectional)) +
  geom_point(col = "grey", alpha = 0.4) + geom_line(col = "grey", alpha = 0.25) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Relative Inequity in FMD Prevalence (%)") +
  geom_point(data = function(x) subset(x, bottom_1 == "bottom"), col = "darkred") +
  geom_line(data = function(x) subset(x, bottom_1 == "bottom"), col = "darkred") +
  geom_text(data = function(x) subset(x, bottom_1 == "bottom" & year == 2019), aes(x = 2004, y = 480, label = intersectional), hjust=0, col = "grey30") +
  geom_line(aes(y = med_diff*100-100), linewidth = 1, linetype = "solid", col = "black") +
  geom_line(aes(y = q25_diff*100-100), linewidth = 1, linetype = "longdash", col = "black") +
  geom_line(aes(y = q75_diff*100-100), linewidth = 1, linetype = "longdash", col = "black") +
  ggtitle("Relative Inequity") -> rel_inequity

#Descriptive summaries
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

#### Figures - Strata FMD Prevalence Ranking Over Time ####
#1-Way Stratification
ggplot(brfss, aes(x = year, y = FMD_mu_hisp_rank, col = ethnicity)) +
  geom_point() + geom_line() +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prev. (1 = Lowest)", breaks = c(1, 2, 3, 4)) +
  guides(col = guide_legend(title = "Ethnicity")) + theme(legend.position = "bottom")

#2-Way Stratification
brfss %>% mutate(Strata = paste0(ethnicity, " ", race)) %>%
  ggplot(aes(x = year, y = FMD_mu_race_hisp_rank, col = Strata)) +
  geom_point() + geom_line() +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prev. (1 = Lowest)", breaks = c(1, 2, 3, 4)) +
  guides(col = guide_legend(title = "Strata", nrow = 2)) + theme(legend.position = "bottom")

#3-Way Stratification
brfss %>% mutate(Strata = paste0(ethnicity, " ", race, " ", gender)) %>%
  ggplot(aes(x = year, y = FMD_mu_race_hisp_gender_rank, col = Strata)) +
  geom_point() + geom_line() +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prev. (1 = Lowest)", breaks = c(1, 2, 4, 6, 8)) +
  guides(col = guide_legend(title = "Strata", nrow = 4)) + theme(legend.position = "bottom")

#4-Way Stratification
brfss %>% mutate(Strata = paste0(ethnicity, " ", race, " ", gender, " ", poverty)) %>%
  ggplot(aes(x = year, y = FMD_mu_race_hisp_gender_poverty_rank, group = Strata)) +
  geom_point(col = "grey") + geom_line(col = "grey") +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prev. (1 = Lowest)", breaks = c(1, 4, 8, 12, 16)) +
  theme(legend.position = "none") + expand_limits(x = c(1993, 2035))

#5-Way Stratification
nchs %>% 
  mutate(top_bottom_4 = ifelse(FMD_mu_intersectional_rank %in% c(1:4), "top", ifelse(FMD_mu_intersectional_rank %in% c(25:28), "bottom", "neither"))) %>% group_by(intersectional) %>%
  mutate(top_bottom_4 = ifelse(top_bottom_4[year == 2019] == "top", "top", ifelse(top_bottom_4[year == 2019] == "bottom", "bottom", "neither"))) %>% ungroup() %>%
  ggplot(aes(x = year, y = FMD_mu_intersectional_rank, group = intersectional)) +
    geom_point(col = "grey", alpha = 0.4) + geom_line(col = "grey", alpha = 0.4) +
    theme_classic() +
    scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
    scale_y_continuous(name = "FMD Prevalence Rank (1 = Lowest)", breaks = c(1, 4, 8, 12, 16, 20, 24, 28, 32)) +
    geom_point(data = function(x) subset(x, top_bottom_4 == "top"), col = "darkgreen") +
    geom_line(data = function(x) subset(x, top_bottom_4 == "top"), col = "darkgreen") +
    geom_point(data = function(x) subset(x, top_bottom_4 == "bottom"), col = "darkred") +
    geom_line(data = function(x) subset(x, top_bottom_4 == "bottom"), col = "darkred") +
    geom_text(data = function(x) subset(x, top_bottom_4 %in% c("bottom", "top") & year == 2019), aes(x = year+0.5, y = FMD_mu_intersectional_rank, label = intersectional), hjust=0, col = "grey30") +
    theme(legend.position = "none") + expand_limits(x = c(1993, 2036))

#Fully Intersectional Strata Rankings and Variation
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
  geom_point(size = 3, col = "grey") +
  geom_text(data = ranking_summary[!is.na(ranking_summary$mu_rank), ],
            aes(x = 33, y = fct_reorder(as.factor(intersectional_short), mu_rank, na.rm=T), label = n_ests_low)) +
  labs(x = "Average Scaled Rank (1=Lowest)", y = "Intersectional Strata", title = "FMD Prevalence (1993-2019)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1, 8, 15, 23, 30))

#### Figures - Within-Strata Change in FMD Prevalence Over Time ####
#Point Estimates
ggplot(nchs[!is.na(nchs$FMD_mu_intersectional_annual_change), ], aes(x = FMD_mu_intersectional_annual_change, y = fct_reorder(as.factor(intersectional), FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2, col = "grey") + 
  geom_point(data = nchs[nchs$FMD_mu_intersectional_annual_change_rank %in% c(1:4), ], size = 2, col = "darkgreen") +
  geom_point(data = nchs[nchs$FMD_mu_intersectional_annual_change_rank %in% c(26:29), ], size = 2, col = "darkred") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Annual Change in FMD Prevalence (%)", breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_discrete(name = "Intersectional Strata")

#Point Estimates as Loess
ggplot(brfss, aes(x = year, y = change_start*100, group = intersectional)) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ splines::bs(x, knots = 4), se = F, col = "grey", size = 0.5) + geom_hline(yintercept = 0) +
  #geom_line(aes(x = year, y = change_start_overall*100), stat = "smooth", method = "lm", formula = y ~ splines::bs(x, knots = 4), se = F, col = "black", size = 2) + geom_hline(yintercept = 0) +
  theme_classic() +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Change Over Time in Strata FMD Prevalence (%)") +
  theme(legend.position = "none")

#### Figures - Within-Strata Change in FMD Inequities Over Time ####
## Absolute FMD Inequities
#Point Estimates
nchs %>% group_by(intersectional) %>% mutate(n_lags = NROW(intersectional[!is.na(FMD_mu_intersectional)]) - 1) %>% ungroup() %>% 
  filter(!is.na(priv_diff_abs_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_abs_FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional), priv_diff_abs_FMD_mu_intersectional_annual_change))) +
    geom_point(size = 2, col = "grey") + 
    geom_point(data = function(x) subset(x, priv_diff_abs_FMD_mu_intersectional_annual_change_rank <= 4), size = 2, col = "darkgreen") +
    geom_point(data = function(x) subset(x, priv_diff_abs_FMD_mu_intersectional_annual_change_rank >=26), size = 2, col = "darkred") +
    geom_text(aes(x = 1.2, y = intersectional, label = n_lags), size = 2.5, col = "grey30") +
    theme_classic() + geom_vline(xintercept = 0) +
    scale_x_continuous(name = "Annual Change in Absolute Inequity in FMD Prevalence (%)") +
    scale_y_discrete(name = "Intersectional Strata") + ggtitle("Absolute Inequity") -> abs_inequity_change_plot

## Relative FMD Inequities
#Point Estimates
nchs %>% group_by(intersectional) %>% mutate(n_lags = NROW(intersectional[!is.na(FMD_mu_intersectional)]) - 1) %>% ungroup() %>% 
  filter(!is.na(priv_diff_mult_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_mult_FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional), priv_diff_mult_FMD_mu_intersectional_annual_change))) +
    geom_point(size = 2, col = "grey") + 
    geom_point(data = function(x) subset(x, priv_diff_mult_FMD_mu_intersectional_annual_change_rank <= 4), size = 2, col = "darkgreen") +
    geom_point(data = function(x) subset(x, priv_diff_mult_FMD_mu_intersectional_annual_change_rank >=26), size = 2, col = "darkred") +
    geom_text(aes(x = 6, y = intersectional, label = n_lags), size = 2.5, col = "grey30") +
    theme_classic() + geom_vline(xintercept = 0) +
    scale_x_continuous(name = "Annual Change in Relative Inequity in FMD Prevalence (%)", breaks = c(-10, -7.5, -5, -2.5, 0, 2.5, 5)) +
    scale_y_discrete(name = "Intersectional Strata") + ggtitle("Relative Inequity") -> rel_inequity_change_plot

### Figures - Within-Strata Change (Prevalences, Absolute, and Relative Inequities Together) ####
## Fitting FMD sub-plot
#FMD Prevalence
nchs %>%  
  filter(!is.na(FMD_mu_intersectional_annual_change)) %>%
  ggplot(aes(x = FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional_short), FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 1.4, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Annual Change (%)", breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("FMD Prevalence") +
  theme(plot.title = element_text(hjust = 0.5)) -> prevalence_plot
#Absolute Inequity
nchs %>% 
  filter(!is.na(priv_diff_abs_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_abs_FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional_short), priv_diff_abs_FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 1.2, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Annual Change (%)") +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("Absolute Inequity") +
  theme(plot.title = element_text(hjust = 0.5)) -> abs_inequity_plot
#Relative Inequity
nchs %>% 
  filter(!is.na(priv_diff_mult_FMD_mu_intersectional_annual_change) & intersectional != "Non-Hispanic White Male Not In Poverty with >=HS") %>%
  ggplot(aes(x = priv_diff_mult_FMD_mu_intersectional_annual_change*100, y = fct_reorder(as.factor(intersectional_short), priv_diff_mult_FMD_mu_intersectional_annual_change))) +
  geom_point(size = 2.5, col = "grey") + 
  geom_text(aes(x = 6.3, y = intersectional_short, label = n_ests_low), size = 2.5, col = "grey30") +
  theme_classic() + geom_vline(xintercept = 0) +
  scale_x_continuous(name = "Annual Change (%)", breaks = c(-10, -5, 0, 5)) +
  scale_y_discrete(name = "Intersectional Strata") + ggtitle("Relative Inequity") +
  theme(plot.title = element_text(hjust = 0.5)) -> rel_inequity_plot
grid.arrange(prevalence_plot, abs_inequity_plot, rel_inequity_plot, nrow = 1)

#### Figures - Single Socializations - FMD Prevalence & Inequities ####
#FMD Plots
ggplot(nchs, aes(x = year, y = FMD_mu_hisp*100, col = ethnicity)) +
  geom_ribbon(aes(ymin = FMD_mu_hisp_lci*100, ymax = FMD_mu_hisp_uci*100, x = year, fill = ethnicity), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("FMD Prevalence (%)") + ylim(6, 26) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  scale_fill_manual(name = "Ethnicity", values = c("#661100", "#117733")) +
  scale_color_manual(name = "", values = c("#661100", "#117733")) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="")) +
  ggtitle("Ethnicity") +
  theme(plot.title = element_text(hjust = 0.5), legend.margin = margin(0,0,0,0))  -> plot_fmd_hisp
ggplot(nchs, aes(x = year, y = FMD_mu_race*100, col = race)) +
  geom_ribbon(aes(ymin = FMD_mu_race_lci*100, ymax = FMD_mu_race_uci*100, x = year, fill = race), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("FMD Prevalence (%)") + ylim(6, 26) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  scale_fill_manual(name = "Race", values = c("#661100", "#117733")) +
  scale_color_manual(name = "", values = c("#661100", "#117733")) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="")) +
  ggtitle("Race") +
  theme(plot.title = element_text(hjust = 0.5), legend.margin = margin(0,0,0,0)) -> plot_fmd_race
ggplot(nchs, aes(x = year, y = FMD_mu_gender*100, col = gender)) +
  geom_ribbon(aes(ymin = FMD_mu_gender_lci*100, ymax = FMD_mu_gender_uci*100, x = year, fill = gender), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("FMD Prevalence (%)") + ylim(6, 26) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  scale_fill_manual(name = "Perceived Sex", values = c("#661100", "#117733")) +
  scale_color_manual(name = "", values = c("#661100", "#117733")) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="")) +
  ggtitle("Perceived Sex") +
  theme(plot.title = element_text(hjust = 0.5), legend.margin = margin(0,0,0,0))  -> plot_fmd_gender
ggplot(nchs, aes(x = year, y = FMD_mu_educ*100, col = education)) +
  geom_ribbon(aes(ymin = FMD_mu_educ_lci*100, ymax = FMD_mu_educ_uci*100, x = year, fill = education), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("FMD Prevalence (%)") + ylim(6, 26) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  scale_fill_manual(name = "Educational Attainment", values = c("#661100", "#117733")) +
  scale_color_manual(name = "", values = c("#661100", "#117733")) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="")) +
  ggtitle("Educational Attainment") +
  theme(plot.title = element_text(hjust = 0.5), legend.margin = margin(0,0,0,0))  -> plot_fmd_educ
ggplot(nchs, aes(x = year, y = FMD_mu_poverty*100, col = poverty)) +
  geom_ribbon(aes(ymin = FMD_mu_poverty_lci*100, ymax = FMD_mu_poverty_uci*100, x = year, fill = poverty), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("FMD Prevalence (%)") + ylim(6, 26) +
  theme_classic() + 
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  scale_fill_manual(name = "povertyal Attainment", values = c("#661100", "#117733")) +
  scale_color_manual(name = "", values = c("#661100", "#117733")) +
  theme(legend.position = "bottom") +
  guides(col = "none", fill=guide_legend(title="")) +
  ggtitle("Poverty Status") +
  theme(plot.title = element_text(hjust = 0.5), legend.margin = margin(0,0,0,0))  -> plot_fmd_poverty
grid.arrange(plot_fmd_hisp, plot_fmd_race, plot_fmd_gender, 
             plot_fmd_educ, plot_fmd_poverty, nrow = 1)

#Absolute Inequity Plots
ggplot(nchs[nchs$ethnicity == "Hispanic",], aes(x = year, y = priv_diff_abs_FMD_mu_hisp*100)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_hisp_lci*100, ymax = priv_diff_abs_FMD_mu_hisp_uci*100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Absolute Inequity (%)") + ylim(-3.5, 15) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Hispanic (vs. Non-Hispanic)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_abs_FMD_hisp
ggplot(nchs[nchs$race == "Black",], aes(x = year, y = priv_diff_abs_FMD_mu_race*100)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_race_lci*100, ymax = priv_diff_abs_FMD_mu_race_uci*100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Absolute Inequity (%)") + ylim(-3.5, 15) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Black (vs. White)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0)) -> plot_priv_diff_abs_FMD_race
ggplot(nchs[nchs$gender == "Female",], aes(x = year, y = priv_diff_abs_FMD_mu_gender*100)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_gender_lci*100, ymax = priv_diff_abs_FMD_mu_gender_uci*100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Absolute Inequity (%)") + ylim(-3.5, 15) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Female (vs. Male)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_abs_FMD_gender
ggplot(nchs[nchs$education == "<HS",], aes(x = year, y = priv_diff_abs_FMD_mu_educ*100)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_educ_lci*100, ymax = priv_diff_abs_FMD_mu_educ_uci*100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Absolute Inequity (%)") + ylim(-3.5, 15) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "<HS (vs. >=HS)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_abs_FMD_educ
ggplot(nchs[nchs$poverty == "In Poverty",], aes(x = year, y = priv_diff_abs_FMD_mu_poverty*100)) +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_uci*100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Absolute Inequity (%)") + ylim(-3.5, 15) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "In Poverty (vs. Not In Poverty)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_abs_FMD_poverty
grid.arrange(plot_priv_diff_abs_FMD_hisp, plot_priv_diff_abs_FMD_race, plot_priv_diff_abs_FMD_gender, 
             plot_priv_diff_abs_FMD_educ, plot_priv_diff_abs_FMD_poverty, nrow = 1)

#Relative Inequity Plots
ggplot(nchs[nchs$ethnicity == "Hispanic",], aes(x = year, y = priv_diff_mult_FMD_mu_hisp*100-100)) +
  geom_ribbon(aes(ymin = priv_diff_mult_FMD_mu_hisp_lci*100-100, ymax = priv_diff_mult_FMD_mu_hisp_uci*100-100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Relative Inequity (%)") + ylim(-25, 150) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Hispanic (vs. Non-Hispanic)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_mult_FMD_hisp
ggplot(nchs[nchs$race == "Black",], aes(x = year, y = priv_diff_mult_FMD_mu_race*100-100)) +
  geom_ribbon(aes(ymin = priv_diff_mult_FMD_mu_race_lci*100-100, ymax = priv_diff_mult_FMD_mu_race_uci*100-100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Relative Inequity (%)") + ylim(-25, 150) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Black (vs. White)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0)) -> plot_priv_diff_mult_FMD_race
ggplot(nchs[nchs$gender == "Female",], aes(x = year, y = priv_diff_mult_FMD_mu_gender*100-100)) +
  geom_ribbon(aes(ymin = priv_diff_mult_FMD_mu_gender_lci*100-100, ymax = priv_diff_mult_FMD_mu_gender_uci*100-100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Relative Inequity (%)") + ylim(-25, 150) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "Female (vs. Male)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_mult_FMD_gender
ggplot(nchs[nchs$education == "<HS",], aes(x = year, y = priv_diff_mult_FMD_mu_educ*100-100)) +
  geom_ribbon(aes(ymin = priv_diff_mult_FMD_mu_educ_lci*100-100, ymax = priv_diff_mult_FMD_mu_educ_uci*100-100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Relative Inequity (%)") + ylim(-25, 150) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "<HS (vs. >=HS)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_mult_FMD_educ
ggplot(nchs[nchs$poverty == "In Poverty",], aes(x = year, y = priv_diff_mult_FMD_mu_poverty*100-100)) +
  geom_ribbon(aes(ymin = priv_diff_mult_FMD_mu_poverty_lci*100-100, ymax = priv_diff_mult_FMD_mu_poverty_uci*100-100, x = year), alpha = 0.2) +
  geom_point(size = 1) + geom_line() + 
  ylab("Relative Inequity (%)") + ylim(-25, 150) +
  theme_classic() + geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(1993, 2000, 2006, 2012, 2019)) +
  theme(legend.position = "bottom") +
  labs(title = "In Poverty (vs. Not In Poverty)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "plain"), legend.margin = margin(0,0,0,0))  -> plot_priv_diff_mult_FMD_poverty
grid.arrange(plot_priv_diff_mult_FMD_hisp, plot_priv_diff_mult_FMD_hisp, plot_priv_diff_mult_FMD_gender, 
             plot_priv_diff_mult_FMD_educ, plot_priv_diff_mult_FMD_poverty, nrow = 1)

#All plots
grid.arrange(plot_fmd_hisp, plot_fmd_race, plot_fmd_gender, 
             plot_fmd_educ, plot_fmd_poverty,
             plot_priv_diff_abs_FMD_hisp, plot_priv_diff_abs_FMD_race, plot_priv_diff_abs_FMD_gender, 
             plot_priv_diff_abs_FMD_educ, plot_priv_diff_abs_FMD_poverty,
             plot_priv_diff_mult_FMD_hisp, plot_priv_diff_mult_FMD_race, plot_priv_diff_mult_FMD_gender, 
             plot_priv_diff_mult_FMD_educ, plot_priv_diff_mult_FMD_poverty, nrow = 3, heights = c(6,5,5))
