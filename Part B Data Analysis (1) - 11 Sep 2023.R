#### Part B Data Analysis (1) - 11 Sep 2023.R ####
#Author - Kieran Blaikie
#Date - 11 Sep 2023

#Overview - This script:
#             1) Creates figures using NCHS data suppression guidelines for:
#                 - FMD Prevalence and Inequity Magnitude by State EITC scenario
#                 - DiD and Triple-Difference (DDD) Comparisons across EITC scenarios
#                 - The extent of NCHS suppression across data subsets
#             2) Provides summary statistics accompanying each figure

#Loading packages
library(qs)
library(data.table)
library(tidyverse)
library(stringr)
library(grid)
library(gridExtra)

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading dataset
data <- qread(paste0(directory, "data_final_partB_100923.qs"))

#### Step 0A) Reformatting for clarity ####
data <- as.data.frame(data)
data %>% mutate(male = ifelse(male == 0, "Female", "Male"),
                in_poverty = ifelse(in_poverty == 0, "Not In Poverty", "In Poverty"),
                racehisp = ifelse(racehisp == "NHBlack", "NH Black", ifelse(racehisp == "NHWhite", "NH White", "Hispanic")),
                intersectional = paste0(racehisp, " ", male, " ", in_poverty),
                intersectional_short = intersectional,
                intersectional_short = gsub("NH Black ", "NHB-", intersectional_short),
                intersectional_short = gsub("NH White ", "NHW-", intersectional_short),
                intersectional_short = gsub("Hispanic ", "H-", intersectional_short),
                intersectional_short = gsub("Female ", "F-", intersectional_short),
                intersectional_short = gsub("Male ", "M-", intersectional_short),
                intersectional_short = gsub("Not In Poverty", "NP", intersectional_short),
                intersectional_short = gsub("In Poverty", "P", intersectional_short)) -> data
names(data)[c(2:3)] <- c("gender", "poverty")

#### Step 0A) Restricting to needed variables and rows ####
#Removing 'days' variables, as not currently used in analyses
data <- data[, names(data)[(grepl("days", names(data)) == F)]]

#Creating NCHS dataset
nchs <- data[data$suppression == "NCHS", ]

#### Step 0B) Creating strata rank variables #### 
#Variable stratification name stems across measures
variables <- gsub("n_", "", names(data)[6:13]); variables <- gsub("_all", "", variables)

#State EITC scenario variable name suffixes (for strata-specific FMD prevalence and inequity estimates)
scenarios <- c("_all", 
               "_state_eitc_no", "_state_eitc_yes", 
               "_state_eitc_no_federal_no", "_state_eitc_no_federal_yes", 
               "_state_eitc_yes_receipt_no", "_state_eitc_yes_receipt_yes", 
               "_state_eitc_yes_receipt_yes_moremedian_no", "_state_eitc_yes_receipt_yes_moremedian_yes")

#Creating rank variables for State EITC scenario-specific annual mental health summaries
for (var in variables[2:8]) {
  for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "priv_diff_mult_FMD_mu_", "nat_diff_abs_FMD_mu_", "nat_diff_mult_FMD_mu_")) {
    for (scenario in scenarios) {
      print(paste0("Scenario: ", scenario, ", Outcome: ", outcome, var, "_rank"))
      strata <- ifelse(var == "intersectional", "year", gsub("_", ", ", var))
      strata <- gsub("sex", "gender", strata)
      strata <- ifelse(var == "intersectional", strata, paste0(strata, ", year"))
      eval(parse(text = paste0("nchs <- nchs %>% mutate(strata = paste0(", strata, "))")))
      eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", outcome, var, scenario, "_rank = dense_rank(x = ", outcome, var, scenario, ")) %>% ungroup()")))
      eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", outcome, var, scenario, "_rank = ifelse(is.na(", outcome, var, scenario, "), NA_real_, ", outcome, var, scenario, "_rank))")))
      eval(parse(text = paste0("nchs$strata <- NULL")))
    }
  }
}
rm(scenarios, scenario, var, outcome, strata)

#Creating rank variables for State EITC comparison mental health summaries
comparisons <- c(#Comparing to Overall
                 "state_eitc_no_vs_overall_", 
                 "state_eitc_no_federal_no_vs_overall_", "state_eitc_no_federal_yes_vs_overall_", 
                 "state_eitc_yes_vs_overall_", 
                 "state_eitc_yes_receipt_no_vs_overall_", 
                 "state_eitc_yes_receipt_yes_vs_overall_", 
                 "state_eitc_yes_receipt_yes_moremedian_no_vs_overall_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_", 
                 #Comparing to State EITC contexts
                 "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", 
                 "state_eitc_yes_vs_state_eitc_no_", 
                 "state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_", 
                 "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_",
                 "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_", 
                 "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_", "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_")

for (comp in comparisons) {
  for (scale in c("abs_")) {
    for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_")) {
      for (var in variables[2:8]) {
        print(paste0("Comparison: ", comp, ". Scale: ", scale, ". Outcome: ", outcome, var, "_rank"))
        strata <- ifelse(var == "intersectional", "year", gsub("_", ", ", var))
        strata <- gsub("sex", "gender", strata)
        strata <- ifelse(var == "intersectional", strata, paste0(strata, ", year"))
        eval(parse(text = paste0("nchs <- nchs %>% mutate(strata = paste0(", strata, "))")))
        eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", comp, scale, outcome, var, "_rank = dense_rank(x = ", comp, scale, outcome, var, ")) %>% ungroup()")))
        eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", comp, scale, outcome, var, "_rank = ifelse(is.na(", comp, scale, outcome, var, "), NA_real_, ", comp, scale, outcome, var, "_rank))")))
        eval(parse(text = paste0("nchs$strata <- NULL")))
      }
    }
  }
}
rm(comp, comparisons, scale, var, outcome, strata)

#Creating rank variables for Triple Differences 
triple_comparisons <- c("triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_")

for (comp in triple_comparisons) {
  for (scale in c("abs_")) {
    for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_")) {
      for (var in variables[2:8]) {
        print(paste0("Comparison: ", comp, ". Scale: ", scale, ". Outcome: ", outcome, var, "_rank"))
        strata <- ifelse(var == "intersectional", "year", gsub("_", ", ", var))
        strata <- gsub("sex", "gender", strata)
        strata <- ifelse(var == "intersectional", strata, paste0(strata, ", year"))
        eval(parse(text = paste0("nchs <- nchs %>% mutate(strata = paste0(", strata, "))")))
        eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", comp, scale, outcome, var, "_rank = dense_rank(x = ", comp, scale, outcome, var, ")) %>% ungroup()")))
        eval(parse(text = paste0("nchs <- nchs %>% group_by(year) %>% mutate(", comp, scale, outcome, var, "_rank = ifelse(is.na(", comp, scale, outcome, var, "), NA_real_, ", comp, scale, outcome, var, "_rank))")))
        eval(parse(text = paste0("nchs$strata <- NULL")))
      }
    }
  }
}
rm(triple_comparisons, comp, scale, outcome, var, strata)

#### Step 0C) Creating annual change and annual change rank variables ####
#Creating annual change and annual change rank variables for all strata for each mental health summary 
scenarios <- c("_all", 
               "_state_eitc_no", "_state_eitc_yes", 
               "_state_eitc_no_federal_no", "_state_eitc_no_federal_yes", 
               "_state_eitc_yes_receipt_no", "_state_eitc_yes_receipt_yes", 
               "_state_eitc_yes_receipt_yes_moremedian_no", "_state_eitc_yes_receipt_yes_moremedian_yes")

for (scenario in scenarios) {
  for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "priv_diff_mult_FMD_mu_", "nat_diff_abs_FMD_mu_", "nat_diff_mult_FMD_mu_")) {
    for (var in variables[2:8]) {
      print(paste0("Scenario: ", scenario, ". Variable: ", outcome, var, "_annual_change_rank"))
      strata <- ifelse(var == "intersectional", "intersectional", gsub("_", ", ", var))
      strata <- gsub("sex", "gender", strata)
      
      eval(parse(text = paste0("nchs <- nchs %>% mutate(strata = paste0(", strata, "))")))
      eval(parse(text = paste0("temp_ranks <- nchs %>% ",
                               "arrange(year) %>% ",
                               "filter(!is.na(", outcome, var, scenario, ")) %>% ",
                               "group_by(strata) %>% ",
                               "filter(NROW(strata) >= 2) %>% ",
                               "mutate(lag_years = year - lag(year, 1), 
                                       diff_outcome = ", outcome, var, scenario, " - lag(", outcome, var, scenario, ", 1),
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
      nchs <- merge(nchs, temp_ranks, by = c('strata'), all.x = T)
      eval(parse(text = paste0("names(nchs)[NROW(names(nchs))-1] <- '", outcome, var, scenario, "_annual_change'")))
      eval(parse(text = paste0("names(nchs)[NROW(names(nchs))] <- '", outcome, var, scenario, "_annual_change_rank'")))
      nchs$strata <- NULL
      rm(strata, temp_ranks)
    }
  }
}
rm(scenario, var, outcome, strata)

#Creating overall annual change variables for NCHS
for (scenario in scenarios) {
  print(paste0("Scenario: ", scenario))
  eval(parse(text = paste0("nchs %>% arrange(year) %>% select(year, FMD_mu_overall", scenario, ") %>% distinct() ", 
                           "%>% mutate(FMD_mu_overall", scenario, "_annual_change = FMD_mu_overall", scenario, " - lag(FMD_mu_overall", scenario, ", 1),",
                                      "FMD_mu_overall", scenario, "_annual_change = mean(FMD_mu_overall", scenario, "_annual_change, na.rm=T)) -> nchs_to_merge")))
  eval(parse(text = paste0("nchs$FMD_mu_overall", scenario, "_annual_change <- nchs_to_merge$FMD_mu_overall", scenario, "_annual_change[1]")))
}
rm(scenario, scenarios, nchs_to_merge)

#Creating difference variables for annual change variables
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

for (comp in state_comparisons) {
  for (scale in c("abs_", "mult_")) {
    scale_operator <- ifelse(scale == "abs_", " - ", " / ")
    for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_")) {
      for (var in variables) {
        if (outcome %in% c("priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_") & var == "overall") { #No difference variables without stratifying, so skip here
        } else {
          print(paste0("Comparison: ", comp, ". Scale: ", scale, ". Outcome: ", outcome, var))
          if (comp == "state_eitc_no_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_no_federal_no_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_no_federal_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_no_federal_yes_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_no_federal_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_no_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_overall_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_all", "_annual_change")))
          }
          if (comp == "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_no_federal_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_no_federal_no", "_annual_change")))
          }
          if (comp == "state_eitc_yes_vs_state_eitc_no_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_no", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_vs_state_eitc_no_federal_yes_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_no_federal_yes", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_no_federal_yes_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_no_federal_yes", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_no_federal_yes_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_no_federal_yes", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_yes_receipt_no", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_no", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_yes_receipt_no", "_annual_change")))
          }
          if (comp == "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", outcome, var, "_state_eitc_yes_receipt_yes_moremedian_yes", "_annual_change", scale_operator, 
                                     "nchs$", outcome, var, "_state_eitc_yes_receipt_no", "_annual_change")))
          }
        }
        
      }
    }
  }
}
rm(state_comparisons, comp, scale, outcome, var, scale_operator, state_comparisons) 

#Creating difference variables for annual change variables
triple_comparisons <- c("triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_",
                        "triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_")

for (comp in triple_comparisons) {
  for (scale in c("abs_", "mult_")) {
    scale_operator <- ifelse(scale == "abs_", " - ", " / ")
    for (outcome in c("FMD_mu_", "priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_")) {
      for (var in variables) {
        if (outcome %in% c("priv_diff_abs_FMD_mu_", "nat_diff_abs_FMD_mu_") & var == "overall") {
        } else {
          print(paste0("Comparison: ", comp, ". Scale: ", scale, ". Outcome: ", outcome, ". Variable: ", var))
          if (comp == "triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", 
                                     "state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_", "abs_", outcome, var, "_annual_change", scale_operator, 
                                     "nchs$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", "abs_", outcome, var, "_annual_change")))
          }
          if (comp == "triple_diff_lessgenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", 
                                     "state_eitc_yes_receipt_yes_moremedian_no_vs_state_eitc_yes_receipt_no_", "abs_", outcome, var, "_annual_change", scale_operator, 
                                     "nchs$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", "abs_", outcome, var, "_annual_change")))
          }
          if (comp == "triple_diff_moregenerous_state_eitc_yesno_vs_only_federal_eitc_yesno_") {
            eval(parse(text = paste0("nchs$", comp, scale, outcome, var, "_annual_change <- nchs$", 
                                     "state_eitc_yes_receipt_yes_moremedian_yes_vs_state_eitc_yes_receipt_no_", "abs_", outcome, var, "_annual_change", scale_operator, 
                                     "nchs$", "state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_", "abs_", outcome, var, "_annual_change")))
          }
        }
      }
    }
  }
}
rm(triple_comparisons, comp, scale, scale_operator, outcome, var, variables)

#### Step 1A) Creating Figures for FMD Across State EITC Contexts ####
#Overall - Availability
ggplot(nchs, aes(x = year, y = FMD_mu_overall_all*100)) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_no*100), col = "#661100", size = 2) +
  geom_line(aes(y = FMD_mu_overall_state_eitc_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_no_lci*100, ymax = FMD_mu_overall_state_eitc_no_uci*100, fill = "No State EITC"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_yes*100), col = "#117733", size = 2) +
  geom_line(aes(y = FMD_mu_overall_state_eitc_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_yes_lci*100, ymax = FMD_mu_overall_state_eitc_yes_uci*100, fill = "State EITC"), alpha = 0.2) +
  labs(title = "State EITC Policy Availability") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(7.5, 22.5)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> eitc_availability_FMD_plot

#Overall
ggplot(nchs, aes(x = year, y = FMD_mu_overall_all*100)) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_no_federal_no*100), col = "#661100") +
  geom_line(aes(y = FMD_mu_overall_state_eitc_no_federal_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_no_federal_no_lci*100, ymax = FMD_mu_overall_state_eitc_no_federal_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_line(aes(y = FMD_mu_overall_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_no_federal_yes_lci*100, ymax = FMD_mu_overall_state_eitc_no_federal_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  labs(title = "Federal EITC Receipt", subtitle = "In States Without State EITC Policies") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(7.5, 22.5)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top", legend.margin = margin(0,0,0,0)) -> diff_abs_fed_eitc_receipt_FMD_plot

ggplot(nchs, aes(x = year, y = FMD_mu_overall_all*100)) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_line(aes(y = FMD_mu_overall_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_yes_receipt_no_lci*100, ymax = FMD_mu_overall_state_eitc_yes_receipt_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_overall_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_line(aes(y = FMD_mu_overall_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_overall_state_eitc_yes_receipt_yes_lci*100, ymax = FMD_mu_overall_state_eitc_yes_receipt_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  labs(title = "State EITC Receipt", subtitle = "In States With State EITC Policies") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(7.5, 22.5)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top", legend.margin = margin(0,0,0,0)) -> diff_abs_state_eitc_receipt_FMD_plot

ggplot(nchs, aes(x = year, y = FMD_mu_overall_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_overall*100), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_overall*100), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_overall_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_overall_uci*100,
                  fill = "Federal EITC"), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_overall*100), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_overall*100), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_overall_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_overall_uci*100,
                  fill = "Federal & State EITC"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "Abs. Difference (FMD Prev. %)", title = "Receipt vs. Non-Receipt", subtitle = "Across Tax Credit Contexts") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top", legend.margin = margin(0,0,0,0)) -> did_abs_receipt_by_context_FMD_plot

ggplot(nchs, aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_overall*100)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_overall_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_overall_uci*100,
                  fill = ""), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_overall_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_overall_uci*100), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "DiD (Abs. Diff. FMD Prev. %)", title = "Modification by State EITCs", subtitle = "Federal & State EITC (vs. Federal EITC)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("white")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top", legend.margin = margin(0,0,0,0)) -> ddd_abs_receipt_by_context_FMD_plot

grid.arrange(diff_abs_fed_eitc_receipt_FMD_plot, diff_abs_state_eitc_receipt_FMD_plot, did_abs_receipt_by_context_FMD_plot, ddd_abs_receipt_by_context_FMD_plot, nrow = 1) -> overall_fmd_plots

#Poverty
ggplot(nchs, aes(x = year, y = FMD_mu_poverty_all*100)) +
  geom_point(aes(y = FMD_mu_poverty_state_eitc_no_federal_no*100, shape = poverty), col = "#661100") +
  geom_line(aes(y = FMD_mu_poverty_state_eitc_no_federal_no*100, linetype = poverty), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_poverty_state_eitc_no_federal_no_lci*100, ymax = FMD_mu_poverty_state_eitc_no_federal_no_uci*100, group = poverty, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_poverty_state_eitc_no_federal_yes*100, shape = poverty), col = "#117733") +
  geom_line(aes(y = FMD_mu_poverty_state_eitc_no_federal_yes*100, linetype = poverty), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_poverty_state_eitc_no_federal_yes_lci*100, ymax = FMD_mu_poverty_state_eitc_no_federal_yes_uci*100, group = poverty, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,33)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> diff_abs_fed_eitc_receipt_FMD_poverty_plot

ggplot(nchs, aes(x = year, y = FMD_mu_poverty_all*100)) +
  geom_point(aes(y = FMD_mu_poverty_state_eitc_yes_receipt_no*100, shape = poverty), col = "#661100") +
  geom_line(aes(y = FMD_mu_poverty_state_eitc_yes_receipt_no*100, linetype = poverty), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_poverty_state_eitc_yes_receipt_no_lci*100, ymax = FMD_mu_poverty_state_eitc_yes_receipt_no_uci*100, group = poverty, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_poverty_state_eitc_yes_receipt_yes*100, shape = poverty), col = "#117733") +
  geom_line(aes(y = FMD_mu_poverty_state_eitc_yes_receipt_yes*100, linetype = poverty), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_poverty_state_eitc_yes_receipt_yes_lci*100, ymax = FMD_mu_poverty_state_eitc_yes_receipt_yes_uci*100, group = poverty, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,33)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0))  -> diff_abs_state_eitc_receipt_FMD_poverty_plot

ggplot(nchs, aes(x = year, y = FMD_mu_poverty_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_poverty*100, shape = poverty), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_poverty*100, linetype = poverty), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_poverty_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_poverty_uci*100,
                  fill = "Federal EITC", group = poverty), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_poverty*100, shape = poverty), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_poverty*100, linetype = poverty), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_poverty_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_poverty_uci*100,
                  fill = "Federal & State EITC", group = poverty), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(y = "Abs. Difference (FMD Prev. %)") +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> did_abs_receipt_by_context_FMD_poverty_plot

ggplot(nchs, aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_poverty*100)) +
  geom_point(aes(shape = poverty)) + 
  geom_line(aes(linetype = poverty)) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_poverty_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_poverty_uci*100,
                  group = poverty), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_poverty_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_poverty_uci*100,
                  group = poverty), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "DiD (Abs. Diff. FMD Prev. %)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  guides(shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> ddd_abs_receipt_by_context_FMD_poverty_plot

grid.arrange(diff_abs_fed_eitc_receipt_FMD_poverty_plot, diff_abs_state_eitc_receipt_FMD_poverty_plot, did_abs_receipt_by_context_FMD_poverty_plot, ddd_abs_receipt_by_context_FMD_poverty_plot, 
             top=textGrob("Household Poverty Status: In Poverty and Not In Poverty", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> poverty_fmd_plots

#Sex
ggplot(nchs, aes(x = year, y = FMD_mu_sex_all*100)) +
  geom_point(aes(y = FMD_mu_sex_state_eitc_no_federal_no*100, shape = gender), col = "#661100") +
  geom_line(aes(y = FMD_mu_sex_state_eitc_no_federal_no*100, linetype = gender), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_sex_state_eitc_no_federal_no_lci*100, ymax = FMD_mu_sex_state_eitc_no_federal_no_uci*100, group = gender, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_sex_state_eitc_no_federal_yes*100, shape = gender), col = "#117733") +
  geom_line(aes(y = FMD_mu_sex_state_eitc_no_federal_yes*100, linetype = gender), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_sex_state_eitc_no_federal_yes_lci*100, ymax = FMD_mu_sex_state_eitc_no_federal_yes_uci*100, group = gender, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(0, 5, 10, 15, 20, 25), limits = c(0,27)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> diff_abs_fed_eitc_receipt_FMD_sex_plot

ggplot(nchs, aes(x = year, y = FMD_mu_sex_all*100)) +
  geom_point(aes(y = FMD_mu_sex_state_eitc_yes_receipt_no*100, shape = gender), col = "#661100") +
  geom_line(aes(y = FMD_mu_sex_state_eitc_yes_receipt_no*100, linetype = gender), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_sex_state_eitc_yes_receipt_no_lci*100, ymax = FMD_mu_sex_state_eitc_yes_receipt_no_uci*100, group = gender, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_sex_state_eitc_yes_receipt_yes*100, shape = gender), col = "#117733") +
  geom_line(aes(y = FMD_mu_sex_state_eitc_yes_receipt_yes*100, linetype = gender), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_sex_state_eitc_yes_receipt_yes_lci*100, ymax = FMD_mu_sex_state_eitc_yes_receipt_yes_uci*100, group = gender, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", breaks = c(0, 5, 10, 15, 20, 25), limits = c(0,27)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0))  -> diff_abs_state_eitc_receipt_FMD_sex_plot

ggplot(nchs, aes(x = year, y = FMD_mu_sex_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_sex*100, shape = gender), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_sex*100, linetype = gender), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_sex_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_sex_uci*100,
                  fill = "Federal EITC", group = gender), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_sex*100, shape = gender), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_sex*100, linetype = gender), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_sex_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_sex_uci*100,
                  fill = "Federal & State EITC", group = gender), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(y = "Abs. Difference (FMD Prev. %)") +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> did_abs_receipt_by_context_FMD_sex_plot

ggplot(nchs, aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_sex*100)) +
  geom_point(aes(shape = gender)) + 
  geom_line(aes(linetype = gender)) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_sex_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_sex_uci*100,
                  group = gender), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_sex_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_sex_uci*100,
                  group = gender), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "DiD (Abs. Diff. FMD Prev. %)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  guides(shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> ddd_abs_receipt_by_context_FMD_sex_plot

grid.arrange(diff_abs_fed_eitc_receipt_FMD_sex_plot, diff_abs_state_eitc_receipt_FMD_sex_plot, did_abs_receipt_by_context_FMD_sex_plot, ddd_abs_receipt_by_context_FMD_sex_plot, 
             top=textGrob("Gender: Male and Female", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> sex_fmd_plots

#Race and Ethnicity (Hispanic vs. NH White)
nchs %>% filter(racehisp != "NH Black") %>%
ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_no_federal_no*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_no_federal_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_no_federal_no_lci*100, ymax = FMD_mu_racehisp_state_eitc_no_federal_no_uci*100, group = racehisp, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_no_federal_yes*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_no_federal_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_no_federal_yes_lci*100, ymax = FMD_mu_racehisp_state_eitc_no_federal_yes_uci*100, group = racehisp, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,25)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> diff_abs_fed_eitc_receipt_FMD_racehisp_hisp_plot

nchs %>% filter(racehisp != "NH Black") %>% 
ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_no*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_yes_receipt_no_lci*100, ymax = FMD_mu_racehisp_state_eitc_yes_receipt_no_uci*100, group = racehisp, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_yes*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_yes_receipt_yes_lci*100, ymax = FMD_mu_racehisp_state_eitc_yes_receipt_yes_uci*100, group = racehisp, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,25)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0))  -> diff_abs_state_eitc_receipt_FMD_racehisp_hisp_plot

nchs %>% filter(racehisp != "NH Black") %>% 
ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal EITC", group = racehisp), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal & State EITC", group = racehisp), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(y = "Abs. Difference (FMD Prev. %)") +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> did_abs_receipt_by_context_FMD_racehisp_hisp_plot

nchs %>% filter(racehisp != "NH Black") %>% 
ggplot(aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp*100)) +
  geom_point(aes(shape = racehisp)) + 
  geom_line(aes(linetype = racehisp)) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_uci*100,
                  group = racehisp), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_uci*100,
                  group = racehisp), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "DiD (Abs. Diff. FMD Prev. %)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  guides(shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> ddd_abs_receipt_by_context_FMD_racehisp_hisp_plot

grid.arrange(diff_abs_fed_eitc_receipt_FMD_racehisp_hisp_plot, diff_abs_state_eitc_receipt_FMD_racehisp_hisp_plot, did_abs_receipt_by_context_FMD_racehisp_hisp_plot, ddd_abs_receipt_by_context_FMD_racehisp_hisp_plot, 
             top=textGrob("Race and Ethnicity: Hispanic and Non-Hispanic White", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> racehisp_hisp_fmd_plots

#Race and Ethnicity (NH Black vs. NH White)
nchs %>% filter(racehisp != "Hispanic") %>%
  ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_no_federal_no*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_no_federal_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_no_federal_no_lci*100, ymax = FMD_mu_racehisp_state_eitc_no_federal_no_uci*100, group = racehisp, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_no_federal_yes*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_no_federal_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_no_federal_yes_lci*100, ymax = FMD_mu_racehisp_state_eitc_no_federal_yes_uci*100, group = racehisp, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,28)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> diff_abs_fed_eitc_receipt_FMD_racehisp_nhblack_plot

nchs %>% filter(racehisp != "Hispanic") %>% 
  ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_no*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_yes_receipt_no_lci*100, ymax = FMD_mu_racehisp_state_eitc_yes_receipt_no_uci*100, group = racehisp, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_yes*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = FMD_mu_racehisp_state_eitc_yes_receipt_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = FMD_mu_racehisp_state_eitc_yes_receipt_yes_lci*100, ymax = FMD_mu_racehisp_state_eitc_yes_receipt_yes_uci*100, group = racehisp, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "FMD Prevalence (%)", limits = c(0,28)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0))  -> diff_abs_state_eitc_receipt_FMD_racehisp_nhblack_plot

nchs %>% filter(racehisp != "Hispanic") %>% 
  ggplot(aes(x = year, y = FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp*100, shape = racehisp), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal EITC", group = racehisp), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp*100, shape = racehisp), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal & State EITC", group = racehisp), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "", values = c(16,1)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(y = "Abs. Difference (FMD Prev. %)") +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  guides(fill = "none", shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> did_abs_receipt_by_context_FMD_racehisp_nhblack_plot

nchs %>% filter(racehisp != "Hispanic") %>% 
  ggplot(aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp*100)) +
  geom_point(aes(shape = racehisp)) + 
  geom_line(aes(linetype = racehisp)) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_uci*100,
                  group = racehisp), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_FMD_mu_racehisp_uci*100,
                  group = racehisp), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "DiD (Abs. Diff. FMD Prev. %)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_shape_manual(name = "", values = c(16,1)) +
  guides(shape = "none", linetype = guide_legend(title = "")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0)) -> ddd_abs_receipt_by_context_FMD_racehisp_nhblack_plot

grid.arrange(diff_abs_fed_eitc_receipt_FMD_racehisp_nhblack_plot, diff_abs_state_eitc_receipt_FMD_racehisp_nhblack_plot, did_abs_receipt_by_context_FMD_racehisp_nhblack_plot, ddd_abs_receipt_by_context_FMD_racehisp_nhblack_plot, 
             top=textGrob("Race and Ethnicity: Non-Hispanic Black and Non-Hispanic White", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> racehisp_nhblack_fmd_plots

grid.arrange(poverty_fmd_plots, sex_fmd_plots, nrow = 2)
grid.arrange(racehisp_hisp_fmd_plots, racehisp_nhblack_fmd_plots, nrow = 2)

#### Step 1B) Creating Figures for FMD Inequities Across State EITC Contexts ####
#Poverty Inequities
ggplot(nchs[nchs$poverty == "In Poverty",], aes(x = year, y = priv_diff_abs_FMD_mu_poverty_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_no_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_yes_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_state_eitc_no_federal_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", limits = c(-3,22)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_fed_eitc_receipt_abs_ineq_pov_plot

ggplot(nchs[nchs$poverty == "In Poverty",], aes(x = year, y = priv_diff_abs_FMD_mu_poverty_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_no_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_yes_lci*100, ymax = priv_diff_abs_FMD_mu_poverty_state_eitc_yes_receipt_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", limits = c(-3,22)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_state_eitc_receipt_abs_ineq_pov_plot

ggplot(nchs[nchs$poverty == "In Poverty", ], aes(x = year, y = priv_diff_abs_FMD_mu_poverty_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_poverty*100), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_poverty*100), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_poverty_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_poverty_uci*100,
                  fill = "Federal EITC"), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_poverty*100), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_poverty*100), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_poverty_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_poverty_uci*100,
                  fill = "Federal & State EITC"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "DiD - Abs. Inequity (FMD Prev. %)") +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  theme_classic() +
  theme(legend.position = "none") -> did_abs_receipt_by_context_abs_ineq_pov_plot

ggplot(nchs[nchs$poverty == "In Poverty", ], aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_poverty*100)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_poverty_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_poverty_uci*100,
                  fill = ""), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_poverty_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_poverty_uci*100), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "Triple Diff - Abs. Inequity (FMD Prev. %)") +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("white")) +
  theme_classic() +
  theme(legend.position = "none") -> ddd_abs_receipt_by_context_abs_ineq_pov_plot

grid.arrange(diff_abs_fed_eitc_receipt_abs_ineq_pov_plot, diff_abs_state_eitc_receipt_abs_ineq_pov_plot, 
             did_abs_receipt_by_context_abs_ineq_pov_plot, ddd_abs_receipt_by_context_abs_ineq_pov_plot, 
             top=textGrob("Households In Poverty (vs. Not in Poverty)", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> poverty_ineq_plots

#Gender Inequities
ggplot(nchs[nchs$gender == "Female",], aes(x = year, y = priv_diff_abs_FMD_mu_sex_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_no_lci*100, ymax = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_yes_lci*100, ymax = priv_diff_abs_FMD_mu_sex_state_eitc_no_federal_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(0, 5, 10), limits = c(-2,11)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_fed_eitc_receipt_abs_ineq_gender_plot

ggplot(nchs[nchs$gender == "Female",], aes(x = year, y = priv_diff_abs_FMD_mu_sex_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_no_lci*100, ymax = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_yes_lci*100, ymax = priv_diff_abs_FMD_mu_sex_state_eitc_yes_receipt_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(0, 5, 10), limits = c(-2,11)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_state_eitc_receipt_abs_ineq_gender_plot

ggplot(nchs[nchs$gender == "Female", ], aes(x = year, y = priv_diff_abs_FMD_mu_sex_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_sex*100), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_sex*100), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_sex_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_sex_uci*100,
                  fill = "Federal EITC"), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_sex*100), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_sex*100), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_sex_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_sex_uci*100,
                  fill = "Federal & State EITC"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "DiD - Abs. Inequity (FMD Prev. %)", breaks = c(-6,-3,0,3,6), limits = c(-6,6)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  theme_classic() +
  theme(legend.position = "none") -> did_abs_receipt_by_context_abs_ineq_gender_plot

ggplot(nchs[nchs$gender == "Female", ], aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_sex*100)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_sex_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_sex_uci*100,
                  fill = ""), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_sex_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_sex_uci*100), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "Triple Diff - Abs. Inequity (FMD Prev. %)", breaks = c(-6,-3,0,3,6), limits = c(-6,6)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("white")) +
  theme_classic() +
  theme(legend.position = "none") -> ddd_abs_receipt_by_context_abs_ineq_gender_plot

grid.arrange(diff_abs_fed_eitc_receipt_abs_ineq_gender_plot, diff_abs_state_eitc_receipt_abs_ineq_gender_plot, 
             did_abs_receipt_by_context_abs_ineq_gender_plot, ddd_abs_receipt_by_context_abs_ineq_gender_plot, 
             top=textGrob("Female (vs. Male)", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> sex_ineq_plots

#Race and Ethnicity Inequities - Hispanic (vs. NH White)
ggplot(nchs[nchs$racehisp == "Hispanic",], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(-15, -7.5, 0, 7.5, 15), limits = c(-16,16)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_fed_eitc_receipt_abs_ineq_racehisp_hisp_plot

ggplot(nchs[nchs$racehisp == "Hispanic",], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(-15, -7.5, 0, 7.5, 15), limits = c(-16,16)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_state_eitc_receipt_abs_ineq_racehisp_hisp_plot

ggplot(nchs[nchs$racehisp == "Hispanic", ], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal EITC"), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal & State EITC"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "DiD - Abs. Inequity (FMD Prev. %)", limits = c(-16,16)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  theme_classic() +
  theme(legend.position = "none") -> did_abs_receipt_by_context_abs_ineq_racehisp_hisp_plot

ggplot(nchs[nchs$racehisp == "Hispanic", ], aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp*100)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = ""), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_uci*100), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "Triple Diff - Abs. Inequity (FMD Prev. %)", limits = c(-15, 15)) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("white")) +
  theme_classic() +
  theme(legend.position = "none") -> ddd_abs_receipt_by_context_abs_ineq_racehisp_hisp_plot

grid.arrange(diff_abs_fed_eitc_receipt_abs_ineq_racehisp_hisp_plot, diff_abs_state_eitc_receipt_abs_ineq_racehisp_hisp_plot, 
             did_abs_receipt_by_context_abs_ineq_racehisp_hisp_plot, ddd_abs_receipt_by_context_abs_ineq_racehisp_hisp_plot, 
             top=textGrob("Hispanic (vs. NH White)", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> racehisp_hisp_ineq_plots

#Race and Ethnicity Inequities - NH Black (vs. NH White)
ggplot(nchs[nchs$racehisp == "NH Black",], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no*100, linetype = racehisp), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes*100, linetype = racehisp), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_no_federal_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(-10,-5,0,5,10), limits = c(-12,15)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_fed_eitc_receipt_abs_ineq_racehisp_nhblack_plot

ggplot(nchs[nchs$racehisp == "NH Black",], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no*100), col = "#661100") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_no_uci*100, fill = "No Receipt"), alpha = 0.2) +
  geom_point(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_line(aes(y = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes*100), col = "#117733") +
  geom_ribbon(aes(ymin = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes_lci*100, ymax = priv_diff_abs_FMD_mu_racehisp_state_eitc_yes_receipt_yes_uci*100, fill = "Receipt"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(name = "Abs. Inequity (FMD Prevalence %)", breaks = c(-10,-5,0,5,10), limits = c(-12,15)) +
  scale_fill_manual(name = "", values = c("#661100", "#117733")) +
  theme_classic() +
  theme(legend.position = "none") -> diff_abs_state_eitc_receipt_abs_ineq_racehisp_nhblack_plot

ggplot(nchs[nchs$racehisp == "NH Black", ], aes(x = year, y = priv_diff_abs_FMD_mu_racehisp_all*100)) +
  geom_point(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#661100") +
  geom_line(aes(y = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#661100") +
  geom_ribbon(aes(ymin = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_no_federal_yes_vs_state_eitc_no_federal_no_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal EITC"), alpha = 0.2) +
  geom_point(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#117733") +
  geom_line(aes(y = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp*100), col = "#117733") +
  geom_ribbon(aes(ymin = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp_lci*100, 
                  ymax = state_eitc_yes_receipt_yes_vs_state_eitc_yes_receipt_no_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = "Federal & State EITC"), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(y = "DiD - Abs. Inequity (FMD Prev. %)") + ylim(-12,15) +
  scale_fill_manual(name = "", values = c("#661100", "#117733"), breaks = c("Federal EITC", "Federal & State EITC")) +
  theme_classic() +
  theme(legend.position = "none") -> did_abs_receipt_by_context_abs_ineq_racehisp_nhblack_plot

ggplot(nchs[nchs$racehisp == "NH Black", ], aes(x = year, y = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp*100)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_uci*100,
                  fill = ""), alpha = 0.2) +
  geom_ribbon(aes(ymin = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_lci*100,
                  ymax = triple_diff_any_state_eitc_yesno_vs_only_federal_eitc_yesno_abs_priv_diff_abs_FMD_mu_racehisp_uci*100), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  labs(y = "Triple Diff - Abs. Inequity (FMD Prev. %)") + ylim(-12,15) +
  scale_x_continuous(name = "Year", breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_fill_manual(name = "", values = c("white")) +
  theme_classic() +
  theme(legend.position = "none") -> ddd_abs_receipt_by_context_abs_ineq_racehisp_nhblack_plot

grid.arrange(diff_abs_fed_eitc_receipt_abs_ineq_racehisp_nhblack_plot, diff_abs_state_eitc_receipt_abs_ineq_racehisp_nhblack_plot, 
             did_abs_receipt_by_context_abs_ineq_racehisp_nhblack_plot, ddd_abs_receipt_by_context_abs_ineq_racehisp_nhblack_plot, 
             top=textGrob("NH Black (vs. NH White)", just = "centre", gp = gpar(fill = "white")), nrow = 1) -> racehisp_nhblack_ineq_plots

#Plotting all inequity plots together
grid.arrange(poverty_plots_abs, sex_plots_abs, nrow = 2)
grid.arrange(racehisp_hisp_plots_abs, racehisp_nhblack_plots_abs, nrow = 2)

#### Step 1C) Creating Figures for Intersectional FMD and Inequities Across State EITC Contexts
#Creating long-format subset with intersectional annual change and rank variables for FMD and inequities
nchs %>% 
  group_by(intersectional) %>%
  mutate(n = NROW(intersectional[!is.na(FMD_mu_intersectional_state_eitc_no_federal_no)]),
         n_flag = ifelse(n < (0.5*19), "* *", ifelse(n < (0.8*19), "*", ""))) %>% ungroup() %>% 
  select(intersectional, intersectional_short, n, n_flag,
         FMD_mu_intersectional_state_eitc_no_federal_no_annual_change, FMD_mu_intersectional_state_eitc_no_federal_no_annual_change_rank,
         priv_diff_abs_FMD_mu_intersectional_state_eitc_no_federal_no_annual_change, priv_diff_abs_FMD_mu_intersectional_state_eitc_no_federal_no_annual_change_rank,
         priv_diff_mult_FMD_mu_intersectional_state_eitc_no_federal_no_annual_change, priv_diff_mult_FMD_mu_intersectional_state_eitc_no_federal_no_annual_change_rank) %>%
  mutate(scenario = "No EITC (State EITC Not Available)") %>%
  distinct() -> state_no_fed_no_long
names(state_no_fed_no_long) <- c("intersectional", "intersectional_short", "n", "n_flag",
                                 "FMD_annual_change", "FMD_annual_change_rank",
                                 "abs_ineq_annual_change", "abs_ineq_annual_change_rank",
                                 "mult_ineq_annual_change", "mult_ineq_annual_change_rank", "scenario")
nchs %>% 
  group_by(intersectional) %>%
  mutate(n = NROW(intersectional[!is.na(FMD_mu_intersectional_state_eitc_no_federal_no)]),
         n_flag = ifelse(n < (0.5*19), "* *", ifelse(n < (0.8*19), "*", ""))) %>% ungroup() %>% 
  select(intersectional, intersectional_short, n, n_flag,
         FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change, FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change_rank,
         priv_diff_abs_FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change, priv_diff_abs_FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change_rank,
         priv_diff_mult_FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change, priv_diff_mult_FMD_mu_intersectional_state_eitc_no_federal_yes_annual_change_rank) %>%
  mutate(scenario = "EITC (State EITC Not Available)") %>%
  distinct() -> state_no_fed_yes_long
names(state_no_fed_yes_long) <- c("intersectional", "intersectional_short", "n", "n_flag",
                                  "FMD_annual_change", "FMD_annual_change_rank",
                                  "abs_ineq_annual_change", "abs_ineq_annual_change_rank",
                                  "mult_ineq_annual_change", "mult_ineq_annual_change_rank", "scenario")
nchs %>% 
  group_by(intersectional) %>%
  mutate(n = NROW(intersectional[!is.na(FMD_mu_intersectional_state_eitc_no_federal_no)]),
         n_flag = ifelse(n < (0.5*19), "* *", ifelse(n < (0.8*19), "*", ""))) %>% ungroup() %>% 
  select(intersectional, intersectional_short, n, n_flag,
         FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change, FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change_rank,
         priv_diff_abs_FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change, priv_diff_abs_FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change_rank,
         priv_diff_mult_FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change, priv_diff_mult_FMD_mu_intersectional_state_eitc_yes_receipt_no_annual_change_rank) %>%
  mutate(scenario = "No EITC or State EITC") %>%
  distinct() -> state_yes_receipt_no_long
names(state_yes_receipt_no_long) <- c("intersectional", "intersectional_short", "n", "n_flag",
                                      "FMD_annual_change", "FMD_annual_change_rank",
                                      "abs_ineq_annual_change", "abs_ineq_annual_change_rank",
                                      "mult_ineq_annual_change", "mult_ineq_annual_change_rank", "scenario")
nchs %>% 
  group_by(intersectional) %>%
  mutate(n = NROW(intersectional[!is.na(FMD_mu_intersectional_state_eitc_no_federal_no)]),
         n_flag = ifelse(n < (0.5*19), "* *", ifelse(n < (0.8*19), "*", ""))) %>% ungroup() %>% 
  select(intersectional, intersectional_short, n, n_flag,
         FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change, FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change_rank,
         priv_diff_abs_FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change, priv_diff_abs_FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change_rank,
         priv_diff_mult_FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change, priv_diff_mult_FMD_mu_intersectional_state_eitc_yes_receipt_yes_annual_change_rank) %>%
  mutate(scenario = "EITC and State EITC") %>%
  distinct() -> state_yes_receipt_yes_long
names(state_yes_receipt_yes_long) <- c("intersectional", "intersectional_short", "n", "n_flag",
                                       "FMD_annual_change", "FMD_annual_change_rank",
                                       "abs_ineq_annual_change", "abs_ineq_annual_change_rank",
                                       "mult_ineq_annual_change", "mult_ineq_annual_change_rank", "scenario")
nchs_annual_change_long <- rbind(state_no_fed_no_long, state_no_fed_yes_long, state_yes_receipt_no_long, state_yes_receipt_yes_long)
nchs_annual_change_long$scenario <- factor(nchs_annual_change_long$scenario, levels = c("No EITC (State EITC Not Available)", "No EITC or State EITC", "EITC (State EITC Not Available)", "EITC and State EITC"))
nchs_annual_change_long$FMD_annual_change <- nchs_annual_change_long$FMD_annual_change*100
nchs_annual_change_long$abs_ineq_annual_change <- nchs_annual_change_long$abs_ineq_annual_change*100
nchs_annual_change_long$mult_ineq_annual_change <- nchs_annual_change_long$mult_ineq_annual_change*100
nchs_annual_change_long$n_flag <- ifelse(nchs_annual_change_long$n_flag == "", ">= 80%", ifelse(nchs_annual_change_long$n_flag == "*", ">= 50-80%", "< 50%"))

#FMD Prevalence
nchs_annual_change_long %>%
  filter(n >=3) %>%
ggplot(aes(x = FMD_annual_change, y = fct_reorder(intersectional_short, FMD_annual_change), col = scenario, shape = n_flag)) +
  geom_point(size = 3) +
  labs(x = "Average Annual Change (%)", y = "Intersectional Strata") +
  theme_classic() +
  geom_vline(xintercept = 0) +
  scale_color_manual(name = "Credit Receipt", values = c("#730101", "#ab2c2c", "#2cab38", "#01730c")) +
  scale_shape_manual(name = "Annual Estimates", values = c(10,16)) +
  guides(col = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) + 
  labs(title = "FMD Prevalence (%)") +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0),
        plot.title = element_text(hjust = 0.5)) -> fmd_annual_change_plot

#Absolute Inequity
nchs_annual_change_long %>%
  filter(n >= 3) %>%
  filter(intersectional_short != "NHW-M-NP") %>%
ggplot(aes(x = abs_ineq_annual_change, y = fct_reorder(intersectional_short, abs_ineq_annual_change), col = scenario, shape = n_flag)) +
  geom_point(size = 3) +
  labs(x = "Average Annual Change (%)", y = "Intersectional Strata") +
  theme_classic() +
  geom_vline(xintercept = 0) +
  scale_color_manual(name = "Credit Receipt", values = c("#730101", "#ab2c2c", "#2cab38", "#01730c")) +
  scale_shape_manual(name = "Annual Estimates", values = c(10,16)) +
  guides(col = guide_legend(nrow = 4), shape = guide_legend(nrow = 3)) +
  labs(title = "Absolute Inequity") +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0),
        plot.title = element_text(hjust = 0.5)) -> abs_ineq_annual_change_plot

#Relative Inequity
nchs_annual_change_long %>%
  filter(n >= 3) %>%
  filter(intersectional_short != "NHW-M-NP") %>%
ggplot(aes(x = mult_ineq_annual_change, y = fct_reorder(intersectional_short, mult_ineq_annual_change), col = scenario, shape = n_flag)) +
  geom_point(size = 3) +
  labs(x = "Average Annual Change (%)", y = "Intersectional Strata") +
  theme_classic() +
  geom_vline(xintercept = 0) +
  scale_color_manual(name = "Credit Receipt", values = c("#730101", "#ab2c2c", "#2cab38", "#01730c")) +
  scale_shape_manual(name = "Annual Estimates", values = c(10,16)) +
  guides(col = guide_legend(nrow = 4), shape = guide_legend(nrow = 3)) +
  labs(title = "Relative Inequity") +
  theme(legend.position = "bottom", legend.margin = margin(0,0,0,0),
        plot.title = element_text(hjust = 0.5)) -> rel_ineq_annual_change_plot

grid.arrange(fmd_annual_change_plot, abs_ineq_annual_change_plot, rel_ineq_annual_change_plot, nrow = 1)

#### Step 1C) Creating Figures for NCHS Suppression Across State EITC Contexts ####
## Identifying unique stratifications and State EITC scenarios
variables <- gsub("n_", "", names(data)[6:13]); variables <- gsub("_all", "", variables) #Unique strata

scenarios <- c("_all", 
               "_state_eitc_no", "_state_eitc_yes", 
               "_state_eitc_no_federal_no", "_state_eitc_no_federal_yes", 
               "_state_eitc_yes_receipt_no", "_state_eitc_yes_receipt_yes", 
               "_state_eitc_yes_receipt_yes_moremedian_no", "_state_eitc_yes_receipt_yes_moremedian_yes")

for (scen in scenarios) {
  print(paste0("Working on Scenario: ", scen))
  
  #Creating an empty dataset to populate
  na_obs <- data.frame(strata = variables,n_na = NA_real_, perc_na = NA_real_, scenario = scen)
  na_obs$n_stratifications <- ifelse(na_obs$strata == "intersectional", 3, 
                              ifelse(na_obs$strata == "overall", 0, (str_count(na_obs$strata, "_")+1)))
  
  #Populating scenario estimates
  for (strat in unique(na_obs$strata)) {
    na_obs$perc_na[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(nchs$FMD_mu_", strat, scen, "))/228*100")))
    na_obs$n_na[na_obs$strata == strat] <- eval(parse(text = paste0("sum(is.na(nchs$FMD_mu_", strat, scen, "))")))
  }
  #Correcting number NA for repeated identical rows based on the stratification level
  na_obs %>% mutate(n_na = case_when(strata == "overall" ~ n_na/12, 
                                     strata == "racehisp" ~ n_na/4,
                                     strata == "sex" ~ n_na/6,
                                     strata == "poverty" ~ n_na/6,
                                     strata == "racehisp_sex" ~ n_na/2,
                                     strata == "racehisp_poverty" ~ n_na/2,
                                     strata == "sex_poverty" ~ n_na/3,
                                     TRUE ~ n_na)) -> na_obs
  
  #Re-labelling groups and scenarios
  na_obs$strata <- gsub("racehisp", "Race/Ethnicity", na_obs$strata)
  na_obs$strata <- gsub("sex", "Sex", na_obs$strata)
  na_obs$strata <- gsub("poverty", "Poverty", na_obs$strata)
  na_obs$strata <- gsub("_", ", ", na_obs$strata)
  na_obs$strata <- gsub("overall", "None", na_obs$strata)
  na_obs$strata <- gsub("intersectional", "Fully Intersectionally", na_obs$strata)
  
  na_obs %>% mutate(scenario = case_when(scenario == "_all" ~ "All",
                                         scenario == "_state_eitc_no" ~ "States without State EITCs",
                                         scenario == "_state_eitc_yes" ~ "States with State EITCs",
                                         scenario == "_state_eitc_no_federal_no" ~ "States without State EITCs - EITC Non-Recipients",
                                         scenario == "_state_eitc_no_federal_yes" ~ "States without State EITCs - EITC Recipients",
                                         scenario == "_state_eitc_yes_receipt_no" ~ "States with State EITCs - EITC Non-Recipients",
                                         scenario == "_state_eitc_yes_receipt_yes" ~ "States with State EITCs - EITC Recipients",
                                         scenario == "_state_eitc_yes_receipt_yes_moremedian_no" ~ "States with Less Generous State EITCs - EITC Recipients",
                                         TRUE ~ "States with More Generous State EITCs - EITC Recipients")) -> na_obs
  
  #Assigning na_obs as new object
  assign(paste0("na_obs", scen), na_obs)
  rm(na_obs)
}
na_obs <- rbind(na_obs_all, 
                na_obs_state_eitc_no, 
                na_obs_state_eitc_no_federal_no, na_obs_state_eitc_no_federal_yes, 
                na_obs_state_eitc_yes, 
                na_obs_state_eitc_yes_receipt_no, na_obs_state_eitc_yes_receipt_yes, 
                na_obs_state_eitc_yes_receipt_yes_moremedian_no, na_obs_state_eitc_yes_receipt_yes_moremedian_yes)

rm(strat, scen, variables, scenarios,
   na_obs_all, 
   na_obs_state_eitc_no, 
   na_obs_state_eitc_no_federal_no, na_obs_state_eitc_no_federal_yes, 
   na_obs_state_eitc_yes, 
   na_obs_state_eitc_yes_receipt_no, na_obs_state_eitc_yes_receipt_yes, 
   na_obs_state_eitc_yes_receipt_yes_moremedian_no, na_obs_state_eitc_yes_receipt_yes_moremedian_yes)

#Converting to factors with set order
na_obs$strata <- factor(na_obs$strata, 
                        levels = c("Fully Intersectionally",
                                   "Race/Ethnicity, Poverty", "Race/Ethnicity, Sex", "Sex, Poverty", 
                                   "Race/Ethnicity", "Poverty", "Sex", "None"), ordered = T)
na_obs$scenario <- factor(na_obs$scenario,
                          levels = c("All",
                                     "States without State EITCs",
                                     "States with State EITCs",
                                     "States without State EITCs - EITC Non-Recipients",
                                     "States with State EITCs - EITC Non-Recipients",
                                     "States without State EITCs - EITC Recipients",
                                     "States with State EITCs - EITC Recipients",
                                     "States with Less Generous State EITCs - EITC Recipients",
                                     "States with More Generous State EITCs - EITC Recipients"),
                          ordered = T)

## Creating a plot for each EITC group
ggplot(na_obs[na_obs$scenario %in% c("All", 
                                     "States without State EITCs - EITC Non-Recipients",
                                     "States with State EITCs - EITC Non-Recipients",
                                     "States without State EITCs - EITC Recipients",
                                     "States with State EITCs - EITC Recipients"), ], 
       aes(x = perc_na, y = strata, col = scenario, shape = as.factor(n_stratifications))) +
  geom_point() + 
  theme_classic() +
  scale_color_manual(name = "Strata Subset", values = c("black", "#730101", "#ab2c2c", "#2cab38", "#01730c")) +
  scale_shape_manual(name = "No. Axes", values = c(1, 19, 0, 15)) +
  labs(x = "Annual Estimates Suppressed (%)", y = "Social Axes Stratified By",
       title = "NCHS Suppression of Estimates") +
  guides(col = guide_legend(nrow = 5), shape = guide_legend(nrow = 1)) +
  theme(legend.position = "right", legend.direction = "vertical",
        panel.grid.major.x = element_line(colour = rgb(235, 235, 235, 275, maxColorValue = 275)),
        plot.title = element_text(hjust = 0.5))
