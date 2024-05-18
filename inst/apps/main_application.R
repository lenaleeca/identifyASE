#' Main Application Script for Defining Acute Sepsis Episodes and Discharge Information
#'
#' This script installs and loads the required packages, loads the data, and applies the functions from the `identifyASE` package to define acute sepsis episodes and discharge information. It generates results for both AIM 3 and AIM 2 analyses, using various parameter settings.
#'
#' @details
#' The script performs the following steps:
#' \enumerate{
#'   \item Installs and loads the required packages (`dplyr`, `data.table`, `purrr`, `furrr`, `future`).
#'   \item Sets global options for the R session.
#'   \item Loads the `identifyASE` package.
#'   \item Loads and processes the required data.
#'   \item Applies the `define_ase_disch` function with different parameter settings for AIM 3 and AIM 2 analyses.
#'   \item Saves the results as RDS files.
#' }
#'
#' @examples
#' \dontrun{
#' # Ensure the script is saved as an R script file, e.g., `main_application.R`
#' source("main_application.R")
#' }
#'
#' @import dplyr
#' @import data.table
#' @import purrr
#' @import furrr
#' @import future
#' @export
main_application <- function(daily_dat_full,
                             transferout_acutecare_seqnum,
                             cohort_id = NULL) {
  # Set global options
  options(scipen = 999)

  # Define required variables
  required_vars <- c(
    "unique_pt_id", "seqnum", "day", "bcx_daily", "vasop_daily",
    "imv_daily", "lact_daily_hi", "tbili_daily_hi", "tbili_daily_lo", "tbili_baseline",
    "creat_daily_hi", "creat_daily_lo", "creat_baseline", "plt_daily_hi", "plt_daily_lo", "plt_baseline",
    "ELX_All_33", "new_abx_start", "abx_daily", "death", "ALL_DAYS"
  )

  daily_dat_sub <- daily_dat_full[, required_vars]

  ### AIM 3 ASE
  define_ase_result <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id)
  define_ase_result_renal <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                              creat_hi_lo_ratio = 1.5, creat_hi_cutoff = 33)
  define_ase_result_liver <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                              tbili_hi_lo_ratio = 1.5, tbili_hi_cutoff = 25.7)
  define_ase_result_lact <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                             lact_hi_cutoff = 1.5)
  define_ase_result_heme <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                             plt_lo_cutoff = 150, plt_lo_hi_ratio = 0.75)
  define_ase_result_aod <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                            creat_hi_lo_ratio = 1.5, creat_hi_cutoff = 33,
                                            tbili_hi_lo_ratio = 1.5, tbili_hi_cutoff = 25.7,
                                            lact_hi_cutoff = 1.5,
                                            plt_lo_cutoff = 150, plt_lo_hi_ratio = 0.75)

  # Combine results for AIM 3
  ase_pid_list_aim3 <- list(
    define_ase_result$IDs,
    define_ase_result_renal$IDs,
    define_ase_result_liver$IDs,
    define_ase_result_lact$IDs,
    define_ase_result_heme$IDs,
    define_ase_result_aod$IDs
  )
  names(ase_pid_list_aim3) <- c("Original", "Renal", "Liver", "Lactate", "Heme", "AOD")

  ### AIM 2 ASE
  define_ase_nodisch <- define_ase(daily_data = daily_dat_full, transferout_id = transferout_acutecare_seqnum, cohort_id)
  define_ase_nodisch_renal <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                               creat_hi_lo_ratio = 1.5, creat_hi_cutoff = 33)
  define_ase_nodisch_liver <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                               tbili_hi_lo_ratio = 1.5, tbili_hi_cutoff = 25.7)
  define_ase_nodisch_lact <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                              lact_hi_cutoff = 1.5)
  define_ase_nodisch_heme <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                              plt_lo_cutoff = 150, plt_lo_hi_ratio = 0.75)
  define_ase_nodisch_aod <- define_ase_disch(daily_data = daily_dat_sub, transferout_id = transferout_acutecare_seqnum, cohort_id,
                                             creat_hi_lo_ratio = 1.5, creat_hi_cutoff = 33,
                                             tbili_hi_lo_ratio = 1.5, tbili_hi_cutoff = 25.7,
                                             lact_hi_cutoff = 1.5,
                                             plt_lo_cutoff = 150, plt_lo_hi_ratio = 0.75)

  # Combine results for AIM 2
  ase_pid_list_aim2 <- list(
    define_ase_nodisch$IDs,
    define_ase_nodisch_renal$IDs,
    define_ase_nodisch_liver$IDs,
    define_ase_nodisch_lact$IDs,
    define_ase_nodisch_heme$IDs,
    define_ase_nodisch_aod$IDs
  )
  names(ase_pid_list_aim2) <- c("Original", "Renal", "Liver", "Lactate", "Heme", "AOD")

  result <- list(ase_pid_list_aim2, ase_pid_list_aim3)
  names(result) <- c("ASE original", "ASE relaxed")
  return(result)
}

# Load daily data
daily_dat_full <- read.csv("data/daily_data.csv")

# Apply function
main_application(daily_dat_full, 1)
