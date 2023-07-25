#' Calculate derived variables for RI_QUAL_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_01_<ANALYSIS_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven

# RI_QUAL_01_03DV R version 1.00 - Biostat Global Consulting - 2022-12-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_01_03DV <- function(VCP = "RI_QUAL_01_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"))

  source <- "card"

  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    source <- c(source, "register")
  }

  for (s in seq_along(source)){
    dat <- dat %>% mutate(tempvar1 = ifelse((psweight > 0 & !is.na(psweight)) %in% TRUE, 0, NA),
                          tempvar2 = ifelse((psweight > 0 & !is.na(psweight)) %in% TRUE, 0, NA))

    for (d in seq_along(RI_DOSE_LIST)){

      date <- rlang::sym(paste0(RI_DOSE_LIST[d],"_",source[s],"_date"))
      tick <- rlang::sym(paste0(RI_DOSE_LIST[d],"_",source[s],"_tick"))

      dat <- dat %>% mutate(tempvar1 = ifelse(!is.na(!!date), tempvar1+1, tempvar1),
                            tempvar2 = ifelse((!!tick == 1) %in% TRUE, tempvar2+1, tempvar2))

    } #end of RI_DOSE_LIST d loop

    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0("Number of Dates on ", str_to_title(source[s]))) %>% suppressWarnings()
    dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                    label = paste0("Number of Tick Marks on ", str_to_title(source[s]))) %>% suppressWarnings()

    names(dat)[which(names(dat) == "tempvar1")] <- paste0(source[s],"_date_count")
    names(dat)[which(names(dat) == "tempvar2")] <- paste0(source[s],"_tick_count")

  } #end of source s loop

  # Create variable to show if Interviewer indicated a card was seen
  dat <- dat %>% mutate(had_card = ifelse((RI27 == 1 | card_date_count > 0 | card_tick_count > 0) %in% TRUE, 1, 0))

  # Create variable to show if card had dates
  dat <- dat %>% mutate(had_card_with_dates = ifelse((card_date_count > 0) %in% TRUE, 1, 0))

  # Create variable to show if card had dates or ticks
  dat <- dat %>% mutate(had_card_with_dates_or_ticks = ifelse((card_date_count > 0 | card_tick_count > 0) %in% TRUE, 1, 0))

  # Create variable to show if card had clean dates only
  dat <- dat %>% mutate(had_card_with_flawless_dates = ifelse((card_date_count > 0 & card_tick_count == 0) %in% TRUE, 1, 0))

  dat <- dat %>% mutate(had_card = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_card),
                        had_card_with_dates = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_card_with_dates),
                        had_card_with_dates_or_ticks = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_card_with_dates_or_ticks),
                        had_card_with_flawless_dates = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_card_with_flawless_dates))

  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    # Create variable to show if Interviewer indicated a register was seen
    dat <- dat %>% mutate(had_register = ifelse((register_date_count > 0 | register_tick_count > 0) %in% TRUE, 1, 0))

    # Create variable to show if register had dates
    dat <- dat %>% mutate(had_register_with_dates = ifelse((register_date_count > 0) %in% TRUE, 1, 0))

    # Create variable to show if register had dates or ticks
    dat <- dat %>% mutate(had_register_with_dates_or_ticks = ifelse((register_date_count > 0 | register_tick_count > 0) %in% TRUE, 1, 0))

    # Create variable to show if register had clean dates only
    dat <- dat %>% mutate(had_register_with_flawless_dates = ifelse((register_date_count > 0 & register_tick_count == 0) %in% TRUE, 1, 0))

    dat <- dat %>% mutate(had_register = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_register),
                          had_register_with_dates = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_register_with_dates),
                          had_register_with_dates_or_ticks = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_register_with_dates_or_ticks),
                          had_register_with_flawless_dates = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_register_with_flawless_dates))
  }

  # Create variable to show if card or register was seen
  dat <- dat %>% mutate(had_card_or_register = 0)
  if ("had_card" %in% names(dat)){
    dat <- dat %>% mutate(had_card_or_register = ifelse((had_card == 1) %in% TRUE, 1, had_card_or_register))
  }
  if ("had_register" %in% names(dat)){
    dat <- dat %>% mutate(had_card_or_register = ifelse((had_register == 1) %in% TRUE, 1, had_card_or_register))
  }

  dat <- dat %>% mutate(had_card_or_register = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, had_card_or_register))

  dat$had_card <- haven::labelled(dat$had_card,
                                  label = "Card Seen by Interviewer") %>% suppressWarnings()
  dat$had_card_with_dates <- haven::labelled(dat$had_card_with_dates,
                                             label = "Card Seen - Dates listed on Card") %>% suppressWarnings()
  dat$had_card_with_dates_or_ticks <- haven::labelled(dat$had_card_with_dates_or_ticks,
                                                      label = "Card Seen - Dates or Tick Marks listed on Card") %>% suppressWarnings()
  dat$had_card_with_flawless_dates <- haven::labelled(dat$had_card_with_flawless_dates,
                                                      label = "Card Seen - Only Clean Dates, No Tick Marks") %>% suppressWarnings()

  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    dat$had_register <- haven::labelled(dat$had_register,
                                    label = "Register Seen by Interviewer") %>% suppressWarnings()
    dat$had_register_with_dates <- haven::labelled(dat$had_register_with_dates,
                                               label = "Register Seen - Dates listed on register") %>% suppressWarnings()
    dat$had_register_with_dates_or_ticks <- haven::labelled(dat$had_register_with_dates_or_ticks,
                                                        label = "Register Seen - Dates or Tick Marks listed on register") %>% suppressWarnings()
    dat$had_register_with_flawless_dates <- haven::labelled(dat$had_register_with_flawless_dates,
                                                        label = "Register Seen - Only clean Dates, No Tick Marks") %>% suppressWarnings()
  }

  dat$had_card_or_register <- haven::labelled(dat$had_card_or_register,
                                  label = "Card or Register seen by Interviewer") %>% suppressWarnings()

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
