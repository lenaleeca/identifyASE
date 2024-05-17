#' Define Acute Sepsis Episodes and Discharge Information
#'
#' This function processes daily patient data to define acute sepsis episodes and discharge information. It includes steps for data formatting, selecting sub-cohorts, creating transfer indicators, and applying transformations to identify sepsis.
#'
#' @param daily_data A data frame containing daily patient data with columns `unique_pt_id`, `seqnum`, `day`, `death`, `ALL_DAYS`, and other clinical variables.
#' @param transferout_id A vector of sequence numbers (`seqnum`) indicating patients who were transferred out acutely.
#' @param cohort_id A list of two vectors: the first vector contains patient IDs (`unique_pt_id`) and the second vector contains sequence numbers (`seqnum`) for selecting a sub-cohort (default is NULL).
#' @param creat_hi_lo_ratio The ratio of high to low creatinine levels to define renal dysfunction (default is 2).
#' @param creat_hi_cutoff The cutoff value for high creatinine levels (default is 44).
#' @param tbili_hi_cutoff The cutoff value for high bilirubin levels (default is 34.2).
#' @param tbili_hi_lo_ratio The ratio of high to low bilirubin levels to define liver dysfunction (default is 2).
#' @param lact_hi_cutoff The cutoff value for high lactate levels (default is 2).
#' @param plt_lo_cutoff The cutoff value for low platelet counts (default is 100).
#' @param plt_lo_hi_ratio The ratio of low to high platelet counts to define hematologic dysfunction (default is 0.5).
#' @return A list containing sepsis episode IDs, final combined data for 2-day and 3-day windows, and the respective data frames.
#' @examples
#' # Example daily data frame
#' daily_data <- data.frame(
#'   unique_pt_id = c(1, 1, 1, 2, 2, 2),
#'   seqnum = c(1, 1, 1, 1, 1, 1),
#'   day = c(0, 1, 2, 0, 1, 2),
#'   death = c(0, 0, 0, 0, 1, 0),
#'   ALL_DAYS = c(3, 3, 3, 3, 3, 3),
#'   bcx_daily = c(1, 0, 0, 0, 1, 0),
#'   vasop_daily = c(0, 1, 0, 0, 0, 1),
#'   imv_daily = c(0, 0, 1, 0, 1, 0),
#'   lact_daily_hi = c(1.5, 2.5, 1.8, 1.9, 2.1, 3.0),
#'   tbili_daily_hi = c(30, 35, 40, 32, 36, 38),
#'   tbili_baseline = c(20, 20, 20, 25, 25, 25),
#'   creat_daily_hi = c(40, 45, 50, 35, 60, 55),
#'   creat_baseline = c(20, 20, 20, 25, 25, 25),
#'   plt_daily_lo = c(150, 80, 90, 110, 70, 50),
#'   plt_baseline = c(200, 200, 200, 180, 180, 180),
#'   ELX_All_33 = c(0, 0, 0, 0, 0, 0),
#'   new_abx_start = c(0, 1, 0, 0, 1, 0),
#'   abx_daily = c(0, 1, 1, 0, 1, 1)
#' )
#' transferout_id <- c(1)
#' define_ase_disch(daily_data, transferout_id)
#' @import dplyr
#' @import purrr
#' @import future
#' @import furrr
#' @export
define_ase_disch <- function(daily_data,
                             transferout_id,
                             cohort_id = NULL,
                             creat_hi_lo_ratio = 2,
                             creat_hi_cutoff = 44,
                             tbili_hi_cutoff = 34.2,
                             tbili_hi_lo_ratio = 2,
                             lact_hi_cutoff = 2,
                             plt_lo_cutoff = 100,
                             plt_lo_hi_ratio = 0.5) {

  ###### Data formatting #########
  # correct first inpatient day as 1 instead of 0
  if(min(daily_data$day)==0) {
    daily_data <- daily_data %>%
      group_by(unique_pt_id, seqnum) %>%
      mutate(day=day+1) %>%
      ungroup()
  }

  # select a sub-cohort, e.g., ICU
  if(!is.null(cohort_id)) {
    daily_data <- daily_data[daily_data$unique_pt_id %in% unique(cohort_id[[1]]), ]
    daily_data <- daily_data[daily_data$seqnum %in% unique(cohort_id[[2]]), ] %>%
      arrange(unique_pt_id, seqnum, day)
  }

  # create acute transfer out indicator
  daily_data$transfer_acute <- ifelse(daily_data$seqnum %in% unique(transferout_id), 1, 0)
  daily_data$transfer_acute[daily_data$day<daily_data$ALL_DAYS] <- 0

  daily_data <- daily_data %>%
    group_by(unique_pt_id, seqnum) %>%
    mutate(death_day=ifelse(death==1, ALL_DAYS, NA),
           disch_day=ifelse(death==0, ALL_DAYS, NA)) %>%
    ungroup()

  # add bcx window +/-2 days and +/-3 days
  daily_data <- add_window_day(daily_data, window_day_col="window_day", window=2)
  daily_data <- add_window_day(daily_data, window_day_col="window_day3", window=3)

  # slice patient daily data around blood culture days
  sliced_data_list <- slice_bcx_data(daily_data, slide_day_before=2, slide_day_after=6)
  sliced_data_list_window3 <- slice_bcx_data(daily_data, slide_day_before=3, slide_day_after=7)

  # Apply add_window_day to each slice in the sliced_data_list; this function is slow
  updated_sliced_data_list <- apply_all_transformations(sliced_data_list,
                                                        window_day_col="window_day",
                                                        aim=3,
                                                        creat_hi_lo_ratio,
                                                        creat_hi_cutoff,
                                                        tbili_hi_cutoff,
                                                        tbili_hi_lo_ratio,
                                                        lact_hi_cutoff,
                                                        plt_lo_cutoff,
                                                        plt_lo_hi_ratio)
  updated_sliced_data_list_window3 <- apply_all_transformations(sliced_data_list_window3,
                                                                window_day_col="window_day3",
                                                                aim=3,
                                                                creat_hi_lo_ratio,
                                                                creat_hi_cutoff,
                                                                tbili_hi_cutoff,
                                                                tbili_hi_lo_ratio,
                                                                lact_hi_cutoff,
                                                                plt_lo_cutoff,
                                                                plt_lo_hi_ratio)

  final_combined_data <- bind_rows(lapply(updated_sliced_data_list, function(slice_info) slice_info$data))
  final_combined_data <- final_combined_data[!duplicated(final_combined_data), ]
  final_combined_data_window3 <- bind_rows(lapply(updated_sliced_data_list_window3, function(slice_info) slice_info$data))
  final_combined_data_window3 <- final_combined_data_window3[!duplicated(final_combined_data_window3), ]

  #### sepsis seqnum list: bcs +/-2days window
  sepsis_v2_com_rev_seqnum <- unique(final_combined_data$seqnum[final_combined_data$sepsis_com_v2==1])
  sepsis_v2_hosp_rev_seqnum <- unique(final_combined_data$seqnum[final_combined_data$sepsis_hosp_v2==1])
  sepsis_v2_hosp_rev_seqnum <- sepsis_v2_hosp_rev_seqnum[!(sepsis_v2_hosp_rev_seqnum %in% sepsis_v2_com_rev_seqnum)]
  sepsis_v2_rev_seqnum <- unique(c(sepsis_v2_com_rev_seqnum, sepsis_v2_hosp_rev_seqnum))

  #### sepsis seqnum list: bcs +/-3days window
  sepsis_v2_com_rev_seqnum_window3 <- unique(final_combined_data_window3$seqnum[final_combined_data_window3$sepsis_com_v2==1])
  sepsis_v2_hosp_rev_seqnum_window3 <- unique(final_combined_data_window3$seqnum[final_combined_data_window3$sepsis_hosp_v2==1])
  sepsis_v2_hosp_rev_seqnum_window3 <- sepsis_v2_hosp_rev_seqnum_window3[!(sepsis_v2_hosp_rev_seqnum_window3 %in% sepsis_v2_com_rev_seqnum_window3)]
  sepsis_v2_rev_seqnum_window3 <- unique(c(sepsis_v2_com_rev_seqnum_window3, sepsis_v2_hosp_rev_seqnum_window3))

  ase_pid_list0 <- list(sepsis_v2_com_rev_seqnum, sepsis_v2_hosp_rev_seqnum, sepsis_v2_rev_seqnum)
  names(ase_pid_list0) <- c("community-onset", "hospital-onset", "all_ase")
  ase_pid_list1 <- list(sepsis_v2_com_rev_seqnum_window3, sepsis_v2_hosp_rev_seqnum_window3, sepsis_v2_rev_seqnum_window3)
  names(ase_pid_list1) <- c("community-onset", "hospital-onset", "all_ase")

  ase_pid_list <- list(ase_pid_list0, ase_pid_list1)
  names(ase_pid_list) <- c("Original", "Window_day_3")

  result_list <- list(ase_pid_list, final_combined_data, final_combined_data_window3)
  names(result_list) <- c("IDs", "Data_2days", "Data_3days")

  return(result_list)
}
