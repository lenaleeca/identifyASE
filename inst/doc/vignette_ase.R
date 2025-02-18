## ----setup, include=FALSE-----------------------------------------------------
suppressWarnings({
  suppressPackageStartupMessages({
    loadNamespace("knitr") # for opts_chunk only
    library("identifyASE")
    library("magrittr")
    })
  })
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----install, eval = FALSE----------------------------------------------------
# install.packages("devtools", repos = "https://cran.rstudio.com")
# library(devtools)

## ----eval = FALSE-------------------------------------------------------------
# # Replace 'your_personal_access_token' with your GitHub personal access token
# install_github("lenaleeca/identifyASE", auth_token = "your_personal_access_token")

## -----------------------------------------------------------------------------
library(identifyASE)

## ----load-package, echo=FALSE-------------------------------------------------
devtools::load_all("/Users/aprilng/Library/CloudStorage/OneDrive-UniversityofCalgary/April_DrLi_Sepsis_Projects/ASE Package/identifyASE")

## ----message = FALSE, warning = FALSE, eval = FALSE---------------------------
# install.packages(c("dplyr", "purrr", "future", "furrr", "devtools"),
# 								 repos = "https://cran.rstudio.com")

## -----------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(future)
library(furrr)
library(devtools)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("bit64")
# library(bit64)

## -----------------------------------------------------------------------------
# Input data
daily_data <- data.frame(
  unique_pt_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4),
  seqnum = c(12602, 12602, 12602, 18613, 18613, 18613, 54928, 54928, 54928, 
             27201, 27201, 27201, 27201, 27201),
  day = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 3, 4),
  death = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ALL_DAYS = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5),
  bcx_daily = c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
  vasop_daily = c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0),
  imv_daily = c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0),
  lact_daily_hi = c(1.5, 2.5, 1.8, 1.9, 2.1, 3.0, 1.5, 2.5, 1.8, 
                    1.5, 1.9, 2.1, 2.1, 3.0),
  tbili_daily_hi = c(30, 35, 40, 32, 36, 38, 30, 35, 40, 30, 32, 35, 36, 50),
  tbili_daily_lo = c(30, 35, 40, 32, 36, 38, 30, 35, 40, 30, 32, 35, 36, 50),
  tbili_baseline = c(20, 20, 20, 25, 25, 25, 20, 20, 20, 25, 25, 25, 25, 25),
  creat_daily_hi = c(40, 45, 50, 35, 60, 55, 40, 45, 50, 35, 35, 45, 60, 55),
  creat_daily_lo = c(40, 45, 50, 35, 60, 55, 40, 45, 50, 35, 35, 45, 60, 55),
  creat_baseline = c(20, 20, 20, 25, 25, 25, 20, 20, 20, 25, 25, 25, 25, 25),
  plt_daily_hi = c(150, 80, 90, 110, 70, 50, 150, 80, 90, 150, 140, 70, 60, 50),
  plt_daily_lo = c(150, 80, 90, 110, 70, 50, 150, 80, 90, 150, 140, 70, 60, 50),
  plt_baseline = c(200, 200, 200, 180, 180, 180, 200, 200, 200, 
                   180, 180, 180, 180, 180),
  esrd_icd = c(0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0,0,0),
  new_abx_start = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0),
  abx_daily = c(0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
)

# Use `transferout_id` to indicate `seqnum` of patients who were transferred 
# out to an acute hospital
transferout_id <- c(12602,54928,27201)

# Use `cohort_id` to select a subgroup by including 2 vectors: 
# The first vector contains patient IDs (`unique_pt_id`)  
# The second vector contains sequence numbers (`seqnum`)

cohort_id <- list(c(1, 2), c(12602, 18613))

# Identify Adult Sepsis Events
result <- define_ase(daily_data = daily_data, 
										 transferout_id = transferout_id, 
										 cohort_id = cohort_id,
										 window = 2,
										 abx_days = 4,
										 creat_hi_lo_ratio = 2,
										 creat_hi_cutoff = 44,
										 tbili_hi_cutoff = 34.2,
										 tbili_hi_lo_ratio = 2,
										 lact_hi_cutoff = 2,
										 plt_lo_cutoff = 100,
										 plt_lo_hi_ratio = 0.5)

## ----eval = FALSE-------------------------------------------------------------
# print(result)

## ----eval = FALSE-------------------------------------------------------------
# ids_list <- result[["IDs"]]
# invisible(
#   # Iterate over the single-layer list
#   lapply(names(ids_list), function(id_name) {
#     # Extract the data (sequence numbers) for the current id_name
#     data <- ids_list[[id_name]]
# 
#     # Convert the data to a data frame
#     data_df <- data.frame(seqnum = data)
# 
#     # Define the output file name
#     file_name <- paste0("IDs_", id_name, ".csv")
# 
#     # Export to CSV
#     write.csv(data_df, file_name, row.names = FALSE)
# 
#     # Print confirmation message
#     cat("Exported:", file_name, "\n")
#   })
# )

## ----eval = FALSE-------------------------------------------------------------
# # Export Data_2days to a csv file
# write.csv(result$Data_2days, "Data_2days.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
# Example sliced_data_list
sliced_data_list <- list(
  list(unique_pt_id = 1, seqnum = 12602, data = data.frame(day = 1:3, 
                                                       bcx_daily = c(0, 1, 0))),
  list(unique_pt_id = 2, seqnum = 18613, data = data.frame(day = 1:3, 
                                                       bcx_daily = c(0, 1, 0)))
)

# Call the function to search for sequence number 12602
search_by_seqnum(sliced_data_list, 12602)

