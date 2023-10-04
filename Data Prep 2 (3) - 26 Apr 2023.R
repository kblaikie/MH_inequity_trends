#### Aim 1 Data Prep 2 - 26 Apr 2023 ####
#Author - Kieran Blaikie
#Date - 26 Apr 2023

#Runtime - 18 minutes using parallel processing across 39 cores for generating the income list variable
#        - 172 minutes to extract each income and save it as a list

# Note - This script uses the dataset 'mid_prep_data_100123.qs', which is identical 
#        to 'mid_prep_data_100123.RDS' except 1) it's a .qs file, and 2) the variable
#        'unique_id' is actually a unique ID across years/rows. The variable in the
#        .RDS version is based on year-specific BRFSS 'SEQNO' or 'X_RECORD' variables
#        so is not helpful here once multiple years have been merged.

# Overview - This script creates 1000 datasets, each containing: 
#               1) a unique row identifier and 2) a continuous income
#          - These continuous incomes are based on reported categorical income bands
#          - For computing speed, each dataset is created, saved, then deleted from 
#            the environment in-parallel across CPU cores

# Loading required R packages
library(tidyverse) #
library(qs) # Need to save created datasets as .qs files
library(doParallel) #Needed for parallel processing
library(data.table) #Needed for extracting elements from list post-imputation

# Runtime start
start <- Sys.time()

# Setting directory
directory <- "R:/PSID/analyses/Mediation/Kieran/Aim 1 Project/"

# Load the 1993-2019 BRFSS dataset
dataset <- qread(paste0(directory, "mid_prep_data_100123.qs"))
names(dataset)[9] <- "income_cat"

# Restrict to necessary variables (unique_id and income_cat)
dataset <- dataset[,c("unique_id", "income_cat")]

# Create 'segment' indicator variable for use in looping
dataset$segment <- rep_len(1:(detectCores() - 1), length.out = NROW(dataset))

#### Creating function for 'income_imp' ####
loop_function <- function(data) {
  #Setting seed
  set.seed(2023)
  
  #Splitting data into segments
  data <- dataset[dataset$segment == boot, c("unique_id", "income_cat")]
  data$income_cat <- as.integer(data$income_cat)
  income <- as.vector(data$income_cat)
  
  #Creating empty list variables
  income_imp <- list(1)[0]
  
  #Creating 'income_imp' function
  income_fun <- function(x) {
    income_imp[x]   <- ifelse(is.na(income[x]), list(rep(NA_integer_, 500)),
                       ifelse(income[x] == 1, list(as.integer(round(runif(n = 500, min = 1, max = 9999), 0))),
                       ifelse(income[x] == 2, list(as.integer(round(runif(n = 500, min = 10000, max = 14999), 0))),
                       ifelse(income[x] == 3, list(as.integer(round(runif(n = 500, min = 15000, max = 19999), 0))),
                       ifelse(income[x] == 4, list(as.integer(round(runif(n = 500, min = 20000, max = 24999), 0))),
                       ifelse(income[x] == 5, list(as.integer(round(runif(n = 500, min = 25000, max = 34999), 0))),
                       ifelse(income[x] == 6, list(as.integer(round(runif(n = 500, min = 35000, max = 49999), 0))),
                       ifelse(income[x] == 7, list(as.integer(round(runif(n = 500, min = 50000, max = 74999), 0))),
                       ifelse(income[x] == 8, list(as.integer(round(rep(75000, 500), 0))))))))))))
  }
  
  #Performing each operation
  income_imputed <- sapply(X = c(1:NROW(data)), FUN = income_fun)
  
  #Assign continuous imputed incomes
  data$income_imp <- income_imputed
  
  #Returning 'filled' data.frame
  return(data)
}

#### Performing split-loop-join process ####
start <- Sys.time()
n_clusters <- detectCores() - 1
clusters <- parallel::makeCluster((n_clusters - 1))
doParallel::registerDoParallel(clusters)
assign(paste0("dataset_postimp"), foreach(boot = 1:n_clusters, .combine = rbind) %dopar% loop_function(dataset))
parallel::stopCluster(clusters)
end <- Sys.time()
duration <- (end - start) #Time taken to run
rm(end, start, n_clusters, clusters)

#### Extract each continuous income element and save as separate 'income' file ####
#This step 1) converts to a data.table for faster processing, 
#          2) creates a function to extract and store each element from the list,
#          3) merge that back in with the unique IDs 

#Creating a dataset without the income list for faster merging
dataset_nolist <- dataset_postimp[, c("unique_id", "income_cat")]

#Converting to data.table for faster processing
dataset_nolist <- as.data.table(dataset_nolist)
dataset_postimp <- as.data.table(dataset_postimp)

#Function to extract n'th element from each list variable and store it as vectors
split_fun <- function(list, n) {
  sapply(list, `[`, n)
}

#Extracting each n'th element, saving it (+ unique ID) as a separate file
start <- Sys.time()
for (n in 1:500) {
  print(paste0("Rep: ", n))
  income_cont <- split_fun(dataset_postimp$income_imp, n)
  dataset_nolist$income_cont <- income_cont
  qsave(x = dataset_nolist, file = paste0(directory, "Repetitions/260423_income_", n, ".qs"))
  rm(temp)
}
end <- Sys.time()
duration2 <- (end - start)
rm(split_fun, n, income_cont)
