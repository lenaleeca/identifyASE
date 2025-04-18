% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define_sepsis.R
\name{define_sepsis}
\alias{define_sepsis}
\title{Define Sepsis}
\usage{
define_sepsis(data)
}
\arguments{
\item{data}{A data frame containing patient data with columns \code{unique_pt_id}, \code{seqnum}, \code{day}, \code{aod_any_daily_comm}, \code{aod_any_daily_hosp}, \code{bcx_daily}, \code{abx_window_startday}, and \code{abx_qualifying_ep}.}
}
\value{
A data frame with new columns indicating the onset days for community and hospital sepsis and indicators for sepsis onset types.
}
\description{
This function defines sepsis based on the onset of acute organ dysfunction (AOD) and qualifying antimicrobial treatments. It creates indicators for community-acquired and hospital-acquired sepsis.
}
\examples{
# Example data frame
data <- data.frame(
  unique_pt_id = c(1, 1, 1, 2, 2, 2),
  seqnum = c(12602, 12602, 12602, 18613, 18613, 18613),
  day = c(1, 2, 3, 1, 2, 3),
  aod_any_daily_comm = c(0, 1, 0, 0, 0, 1),
  aod_any_daily_hosp = c(0, 0, 1, 0, 1, 0),
  bcx_daily = c(1, 0, 0, 0, 1, 0),
  abx_window_startday = c(1, 1, 1, 2, 2, 2),
  abx_qualifying_ep = c(1, 1, 1, 0, 1, 1)
)
define_sepsis(data)
}
