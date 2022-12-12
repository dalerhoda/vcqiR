#' Parse date from individual month, day, and year components like lubridate::mdy, returning NA if any element is missing
#'
#' @param m A character or numeric vector indicating month, NAs allowed
#' @param d A character or numeric vector indicating day, NAs allowed
#' @param y A character or numeric vector indicating year, NAs allowed
#'
#' @return A vector of dates
#'
#' @import lubridate
#'
#' @examples
#' vcqi_mdy(1,2,2022)
#' vcqi_mdy(10,NA,2022)
#'
#' month <- c(1,2,4,5)
#' date <- c(12,14,16,NA)
#' year <- rep(2022,4)
#' vcqi_mdy(month,date,year)

# VCQI_mdy R version 1.02 - Biostat Global Consulting - 2022-10-28
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-28  1.00      Mia Yu          Original version
# 2022-10-04  1.01      Mia Yu          Package version
# 2022-10-28  1.02      Mia Yu          Add part to convert to numeric
# *******************************************************************************


vcqi_mdy <- function(m,d,y){
  tempm <- as.numeric(m)
  tempd <- as.numeric(d)
  tempy <- as.numeric(y)
  tempm <- ifelse(is.na(tempm), 99, tempm)
  tempd <- ifelse(is.na(tempd), 99, tempd)

  suppressWarnings(mdy(paste0(sprintf("%02d", tempm),
                              sprintf("%02d", tempd),
                              sprintf("%02d", tempy))))
}
