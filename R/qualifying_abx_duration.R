#' Capture Qualifying Antimicrobials
#'
#' This function identifies qualifying antimicrobial treatments based on specified criteria during a window period around blood culture days.
#'
#' @param data A data frame containing patient data with columns `unique_pt_id`, `seqnum`, `day`, `new_abx_start`, `abx_daily`, `death`, `transfer_acute`, and `ALL_DAYS`.
#' @param window_day_col A string specifying the name of the column indicating the presence of blood culture days within a specified window (default is "window_day").
#' @param aim An integer specifying the aim criteria: 2 for specific criteria or 3 for extended criteria (default is 2).
#' @param abx_days An integer specifying the number of consecutive antimicrobial days.
#' @return A data frame with new columns indicating qualifying antimicrobial treatments and their episodes.
#' @examples
#' # Example data frame
#' data <- data.frame(
#'   unique_pt_id = c(1, 1, 1, 2, 2, 2),
#'   seqnum = c(1, 1, 1, 1, 1, 1),
#'   day = c(1, 2, 3, 1, 2, 3),
#'   new_abx_start = c(0, 1, 0, 0, 1, 0),
#'   abx_daily = c(0, 1, 1, 0, 1, 1),
#'   death = c(0, 0, 0, 0, 1, 0),
#'   transfer_acute = c(0, 0, 0, 0, 0, 1),
#'   ALL_DAYS = c(3, 3, 3, 3, 3, 3),
#'   window_day = c(1, 1, 0, 1, 1, 0)
#' )
#' qualifying_abx_duration(data, "window_day", 2)
#' @import dplyr
#' @import purrr
#' @import future
#' @import furrr
#' @export
qualifying_abx_duration <- function(data, window_day_col="window_day", aim=2, abx_days=4) {
  data <- data %>%
    arrange(unique_pt_id, seqnum, day) %>%
    group_by(unique_pt_id, seqnum) %>%
    mutate(
      # Identify the start of abx during the window period
      abx_window_start = new_abx_start == 1 & .data[[window_day_col]] == 1,
      abx_window_startday = ifelse(any(abx_window_start==TRUE & .data[[window_day_col]] == 1), min(day[abx_window_start==TRUE & .data[[window_day_col]] == 1]), NA),
      abx_daily_new = ifelse(day>=abx_window_startday, abx_daily, 0),
      abx_run_length = ifelse(!all(is.na(abx_window_startday)), rle(abx_daily_new[abx_daily_new==1])$lengths, 0),
      death_bcx_day = ifelse(any(death==1), (day[death == 1] - max(day[.data[[window_day_col]] == 1])), NA),
      transfer_acute_bcx_day = ifelse(any(transfer_acute==1), (day[transfer_acute == 1] - max(day[.data[[window_day_col]] == 1])), NA),
      disch_bcx_day = ifelse(all(death==0) & all(transfer_acute==0), (ALL_DAYS - max(day[.data[[window_day_col]] == 1])), NA)
    ) %>%
    ungroup()

  data <- data %>%
    group_by(unique_pt_id, seqnum)  %>%
    mutate(
      abx_qualifying = ifelse(
        (aim == 2 & (abx_run_length >= abx_days |
                       (death == 1 & !is.na(death_bcx_day) & abx_run_length >= death_bcx_day & ((lag(abx_daily_new)[length(lag(abx_daily_new))] == 1 & !is.na(lag(abx_daily_new)[length(lag(abx_daily_new))])) | (abx_daily_new == 1 & !is.na(abx_daily_new)))) |
                       (transfer_acute == 1 & !is.na(transfer_acute_bcx_day) & abx_run_length >= transfer_acute_bcx_day & ((lag(abx_daily_new)[length(lag(abx_daily_new))] == 1 & !is.na(lag(abx_daily_new)[length(lag(abx_daily_new))])) | (abx_daily_new == 1 & !is.na(abx_daily_new)))))) |
          (aim == 3 & (abx_run_length >= abx_days |
                         (death == 1 & !is.na(death_bcx_day) & abx_run_length >= death_bcx_day & ((lag(abx_daily_new)[length(lag(abx_daily_new))] == 1 & !is.na(lag(abx_daily_new)[length(lag(abx_daily_new))])) | (abx_daily_new == 1 & !is.na(abx_daily_new)))) |
                         (transfer_acute == 1 & !is.na(transfer_acute_bcx_day) & abx_run_length >= transfer_acute_bcx_day & ((lag(abx_daily_new)[length(lag(abx_daily_new))] == 1 & !is.na(lag(abx_daily_new)[length(lag(abx_daily_new))])) | (abx_daily_new == 1 & !is.na(abx_daily_new)))) |
                         (day==ALL_DAYS & transfer_acute == 0 & death == 0 & !is.na(disch_bcx_day) & abx_run_length >= disch_bcx_day & (abx_daily_new == 1 & !is.na(abx_daily_new))))),
        1, 0),
      abx_qualifying_ep = ifelse(any(abx_qualifying == 1), 1, 0)
    ) %>%
    ungroup()

  return(data)
}
