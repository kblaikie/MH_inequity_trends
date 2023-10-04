#### Aim 1 Data Prep 2.5 - 9 May 2023 ####
#Author - Kieran Blaikie
#Date - 9 May 2023

#Runtime - ~8 minutes per repetition, so ~2 days, 21 hours with 500 reps

# Note - This script uses the dataset 'mid_prep_data_100123.qs', which is identical 
#        to 'mid_prep_data_100123.RDS' except 1) it's a .qs file, and 2) the variable
#        'unique_id' is actually a unique ID across years/rows. The variable in the
#        .RDS version is based on year-specific BRFSS 'SEQNO' or 'X_RECORD' variables
#        so is not helpful here once multiple years have been merged.

# Overview - This script takes imputed incomes from Data Prep 2(3), and calculates
#            State EITC amount eligibilities using the NBER TAXSIM model and 
#            {usincometaxes} package

#Loading packages
library(data.table)
library(magrittr)
library(qs)
library(usincometaxes)
library(doParallel)

# Runtime start
start <- Sys.time()

#Setting directory
directory <- "R:/Project/precarityR01/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

#Loading data
dataset <- qread(paste0(directory, "mid_prep_data_100123.qs"))
dataset <- as.data.table(dataset)

#Rename categorical income
names(dataset)[9] <- "income_cat"

#Restricting to necessary variables then 'complete' observations
dataset[, age := fifelse(is.na(age), 18, age)] #Assuming all those included are >= 18
dataset[, n_children := fifelse(n_children >3, 3, n_children)] #TAXSIM throws errors for large n_children
dataset <- dataset[, .(unique_id, state_acronym, year, age, marital, n_children, income_cat)] #Only keeping vars needed for package

#### Formatting dataset as-needed for 'usincometaxes' ####
# 'taxsimid' = Integer identifying individuals
# 'year' = 4-digit year
# 'mstat' = filing status (1=single, 2=married, jointly')
# 'state' = two-letter abbreviation (e.g. New York = NY)
# 'page' = age of primary taxpayer as of 31/12 of tax year
# 'sage' = age of spouse (coded 0 or NA if no spouse)
# 'depx' = number of dependents
# 'age1/age2/age3' = age of youngest 3 dependents
# 'pwages' = Primary taxpayer wage and salary income
# 'swages' = Spouse wage and salary income (coded 0 or NA if no spouse)

#Subsetting to remove respondents with incomplete data on needed variables
dataset <- dataset[!is.na(marital) & !is.na(n_children) & !is.na(income_cat), ]

#Subsetting to exclude Guam, VI, PR respondents, whose areas aren't modeled by TAXSIM and can't receive State EITCs
dataset <- dataset[(dataset$state_acronym %in% c("Guam", "US Virgin Islands", "PR")) == F, ]

#Creating needed variables
dataset[,  ':=' (mstat = fifelse(marital == 1, 2, 1), #Assumes married ind's file jointly
                 sage = 0, #Not relevant if filing jointly, assume all income is from primary taxpayer
                 age1 = fifelse(n_children == 0, NA_integer_, 5), #Assuming children are 5. Only relevant to State EITC $ pre-2021
                 age2 = fifelse(n_children <= 1, NA_integer_, 5), #in Oregon, who provides more where dependents are < 3 years old 
                 age3 = fifelse(n_children <= 2, NA_integer_, 5),
                 swages = 0)] #All houlsehold income reported is attribute to the primary taxpayer

#Renaming variables as needed
names(dataset)[c(1,2,4,6)] <- c("taxsimid", "state", "page", "depx")

#Keeping relevant variables for TAXSIM process (except pwages, added in the loop)
dataset <- dataset[, .(taxsimid, year, state, mstat, page, sage, depx, age1, age2, age3, swages)]

#### Creating a loop function to perform the TAXSIM portion quicker ####
#The TAXSIM model only accommodates a few thousand obs per run, so our dataset
#needs to be split into segments, fed into TAXSIM, then re-combined with results. 
#To do so quicker we employ parallel processing using the below function
loop_fun <- function(data, boot) {
  #Loading needed package within function
  library(usincometaxes)
  
  #Restrict to the boot portion of the data
  data <- data[data$boot == boot, ]
  data$boot <- NULL
  
  #Split dataset into 20 ~equally sized segments 
  data$seq <- rep_len(1:20, length.out = NROW(data))
  
  #Perform the TAXSIM modelling across all segments
  for (i in unique(data$seq)) {
    #Calculate State EITC credit amount eligibility for rows in each segment
    temp <- taxsim_calculate_taxes(.data = data[data$seq == i, c(1:12)],
                                   marginal_tax_rates = 'Wages',
                                   return_all_information = T)
    
    #Keep only unique ID and State EITC credit amount variables
    temp <- temp[, c("taxsimid", "v39_state_eitc", "v25_eitc")]
    
    #Re-merge all 'seq' segments together
    if (i == 1) {
      eitc <- temp
    } else {
      eitc <- rbind(eitc, temp)
    }  
    rm(temp, i)
  }
  #Return dataset segment
  return(eitc)
}

#### Starting loop process ####
for (rep in 1:500) {
  #Print current repetition and % completed
  print(paste0("Rep: ", rep, ", ", round(((rep/500)*100), 1), "% Complete"))
  
  #Loading in 'income' data per rep and restrict to needed vars
  income_n <- qread(paste0(directory, "Repetitions/260423_income_", rep, ".qs"))
  income_n <- income_n[,c("unique_id", "income_cont")]
  
  #Merge into TAXSIM formatted dataset
  dataset <- merge(dataset, income_n, by.x = "taxsimid", by.y = "unique_id", all.y = F)
  names(dataset)[c(12)] <- c("pwages")
  rm(income_n)
  
  #Formatting data as needed for 'taxsim_calculate_taxes' function
  dataset <- create_dataset_for_taxsim(dataset)
  
  #The NBER TAXSIM model only works with small datasets (~ <20,000)
  #To accommodate this, we use parallel processing to split our data into
  #fewer segments, and within each segment split the data further into 20 chunks.
  #We run the TAXSIM model on each chunk, rbind those, then rbind across the 
  #parallel processed segments to regain our complete dataset
  
  #Creating 'boot' variable to split parallel processing by
  dataset$boot <- rep_len(1:(detectCores() - 1), length.out = NROW(dataset))
  
  #Performing parallel processing
  n_clusters <- detectCores() - 1
  clusters <- parallel::makeCluster((n_clusters - 1))
  doParallel::registerDoParallel(clusters)
  assign(paste0("eitc_all"), foreach(boot = 1:n_clusters, .combine = rbind) %dopar% loop_fun(dataset, boot))
  parallel::stopCluster(clusters)
  
  #Renaming variables
  names(eitc_all)[c(1,2,3)] <- c("unique_id", "state_eitc_amount", "eitc_amount")

  #Saving the completed, stitched dataset
  qsave(x = eitc_all[,c(1,2)], file = paste0(directory, "Repetitions/090523_eitc_", rep, ".qs"))
  qsave(x = eitc_all[,c(1,3)], file = paste0(directory, "Repetitions/090523_federal_eitc_", rep, ".qs"))
  rm(eitc_all)
  
  #Removing 'seq', 'pwages' vars to be replaced with the next 'rep' incomes & boot-specific seq
  dataset[, ':=' (pwages = NULL, seq = NULL)]
}

#Runtime end and duration
end <- Sys.time()
duration <- (end - start)
