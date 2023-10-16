#' Calculate derived variables for RI_QUAL_07B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_07B_<ANALYSIS_COUNTER>)
#'
#' @import stringr
#' @import dplyr
#' @import tidyselect
#' @import tidyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import haven

# RI_QUAL_07B_03DV R version 1.04 - Biostat Global Consulting - 2023-07-14
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Caitlin Clary   Original R version
# 2022-10-06  1.01      Caitlin Clary   In max(got_hypo_<>) calls, deal with Inf
#                                       & -Inf values that should be NA, hide
#                                       "no non-missing" warnings
# 2022-10-11  1.02      Mia Yu          Package version
# 2022-10-18  1.03      Mia Yu          Add variable labels
# 2023-07-14  1.04      Caitlin Clary   For later doses in a multi-dose series
#                                       it's only necessary that the age be >=
#                                       min age for dose 1 and the interval be
#                                       >= the minimum interval. Corrected
#                                       earlier mistake that age be >= the
#                                       minimum for the later dose.
# 2023-07-17  1.05      Caitlin Clary   Outcome missing if the child was not old
#                                       enough for the dose (to correspond with
#                                       RI_COVG_02 denominator)
# ******************************************************************************

RI_QUAL_07B_03DV <- function(VCP = "RI_QUAL_07B_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER, ".rds")) %>%
    arrange(respid, visitdate) %>%
    # Age (in days) of child at visit date
    mutate(age_at_visit = visitdate - dob) %>% # CHECK
    select(-c(dob, visitdate))

  dat$age_at_visit <- haven::labelled(dat$age_at_visit, label = "Age (in days) of child at visit date") %>% suppressWarnings()

  dat <- dat %>%
    group_by(respid) %>%
    mutate(
      ntemp = 1:n(),
      num_days_since_last_visit = age_at_visit - lag(age_at_visit, n = 1),
      num_days_since_last_visit = ifelse(ntemp == 1 & !is.na(age_at_visit), 0,
                                         num_days_since_last_visit)
    ) %>% ungroup() %>% select(-ntemp)

  # NOTE: RI_QUAL_07B_03DV loops over all doses...update if needed

  # Single doses
  if (vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
    for(d in seq_along(RI_SINGLE_DOSE_LIST)){

      dn <- RI_SINGLE_DOSE_LIST[d] %>% stringr::str_to_lower()

      got_hypo <- rlang::sym(paste0("got_hypo_", dn))
      got_hypo_sum <- rlang::sym(paste0("got_hypo_", dn, "_sum"))

      minage <- get(paste0(dn, "_min_age_days"), envir = .GlobalEnv)

      dat <- dat %>%
        group_by(respid) %>%
        mutate(
          !!got_hypo := ifelse(
            !is.na(age_at_visit) & psweight > 0 & !is.na(psweight) & age_at_visit >= minage,
            1, 0),
          !!got_hypo_sum := cumsum(!!got_hypo)
        ) %>%
        ungroup() %>%
        mutate(
          !!got_hypo_sum := ifelse(!!got_hypo_sum > 1, 0, !!got_hypo_sum),
          !!got_hypo_sum := ifelse(psweight %in% 0 | is.na(psweight), NA, !!got_hypo_sum),
          !!got_hypo := !!got_hypo_sum,
          # 2023-07-17 update
          !!got_hypo := ifelse(age_at_interview < minage, NA, !!got_hypo)
        ) %>% select(-!!got_hypo_sum)

    } # end d loop
  } # end if single

  # Multi doses
  multi <- lapply(2:9, function(x) if(vcqi_object_exists(paste0("RI_MULTI_", x, "_DOSE_LIST"))){
    data.frame(
      doselist = paste0("RI_MULTI_", x, "_DOSE_LIST"),
      dosecount = x
    )
  }) %>% do.call(rbind, .)

  if (!is.null(multi)){
    multi <- lapply(seq_along(multi$doselist), function(x) data.frame(
      dose = str_to_lower(get(multi$doselist[x])),
      doselist = multi$doselist[x]
    )) %>%
      do.call(rbind, .) %>%
      full_join(multi, ., by = "doselist")

    for(d in 1:nrow(multi)){
      dn <- multi$dose[d]
      di <- seq(1, multi$dosecount[d], by = 1)

      for(i in seq_along(di)){

        # Minimum age for first dose in series. For later doses in a series,
        # it's only necessary that (a) the age be >= the minimum for *Dose 1* &
        # (b) the interval be >= the minimum interval.
        minage_dose1 <- get(paste0(dn, di[1], "_min_age_days"), envir = .GlobalEnv)

        # Minimum age for current dose
        minage <- get(paste0(dn, di[i], "_min_age_days"), envir = .GlobalEnv)

        got_hypo <- rlang::sym(paste0("got_hypo_", dn, di[i]))
        got_hypo_sum <- rlang::sym(paste0("got_hypo_", dn, di[i], "_sum"))
        num_days_temp <- rlang::sym(paste0("num_days_since_", dn, di[i], "_temp"))
        num_days <- rlang::sym(paste0("num_days_since_", dn, di[i]))

        if (i == 1){
          dat <- dat %>%
            group_by(respid) %>%
            mutate(
              !!got_hypo := ifelse(
                is.na(age_at_visit) | is.na(psweight) | !psweight > 0, NA,
                ifelse(
                  age_at_visit >= minage_dose1, 1, 0
                )
              ),
              !!got_hypo_sum := cumsum(tidyr::replace_na(!!got_hypo, 0))
            ) %>%
            ungroup() %>%
            mutate(
              # Make this temp variable before replacing the _sum var
              !!num_days_temp := num_days_since_last_visit,
              # Update temp variable based on _sum values
              !!num_days_temp := ifelse(!!got_hypo_sum %in% c(0, 1), 0, !!num_days_temp)
            ) %>%
            group_by(respid) %>%
            mutate(
              !!num_days := cumsum(!!num_days_temp)
            ) %>%
            ungroup() %>%
            mutate(
              # Now can replace _sum values
              !!got_hypo_sum := ifelse(!!got_hypo_sum > 1, 0, !!got_hypo_sum),
              # Make sure sum is missing when weight is 0 or missing
              !!got_hypo_sum := ifelse(psweight %in% 0 | is.na(psweight), NA, !!got_hypo_sum),
              !!got_hypo := !!got_hypo_sum,
              # 2023-07-17 update
              !!got_hypo := ifelse(age_at_interview < minage, NA, !!got_hypo)
            ) %>% select(-!!got_hypo_sum, -!!num_days_temp)
        } else {

          num_days_since_prev <- rlang::sym(paste0("num_days_since_", dn, di[i-1]))
          minint <- get(paste0(dn, i, "_min_interval_days"), envir = .GlobalEnv)

          dat <- dat %>%
            group_by(respid) %>%
            mutate(
              !!got_hypo := ifelse(
                is.na(age_at_visit) | is.na(psweight) | !psweight > 0, NA,
                ifelse(
                  age_at_visit >= minage_dose1 & !!num_days_since_prev >= minint, 1, 0
                )
              ),
              !!got_hypo_sum := cumsum(tidyr::replace_na(!!got_hypo, 0))
            ) %>%
            ungroup() %>%
            mutate(
              # Make this temp variable before replacing the _sum var
              !!num_days_temp := num_days_since_last_visit,
              # Update temp variable based on _sum values
              !!num_days_temp := ifelse(!!got_hypo_sum %in% c(0, 1), 0, !!num_days_temp)
            ) %>%
            group_by(respid) %>%
            mutate(
              !!num_days := cumsum(!!num_days_temp)
            ) %>%
            ungroup() %>%
            mutate(
              # Now can replace _sum values
              !!got_hypo_sum := ifelse(!!got_hypo_sum > 1, 0, !!got_hypo_sum),
              # Make sure sum is missing when weight is 0 or missing
              !!got_hypo_sum := ifelse(psweight %in% 0 | is.na(psweight), NA, !!got_hypo_sum),
              !!got_hypo := !!got_hypo_sum,
              # 2023-07-17 update
              !!got_hypo := ifelse(age_at_interview < minage, NA, !!got_hypo)
            ) %>%
            select(-!!got_hypo_sum, -!!num_days_temp)
        }

      } # end i loop

      dat <- dat %>%
        select(-contains(paste0("num_days_since_", dn)))

    } # end d loop

  } # end if multi

  # Save dataset in long form
  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER, "_LONG.rds"))
  vcqi_global(RI_QUAL_07B_TEMP_DATASETS,
              c(RI_QUAL_07B_TEMP_DATASETS,
                paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, "_LONG.rds")))

  # Now save dataset with 1 row per respid
  # NOTE RI_QUAL_07B_03DV loops over all doses - update if needed
  for(d in seq_along(RI_DOSE_LIST)){
    got_hypo <- rlang::sym(paste0("got_hypo_", RI_DOSE_LIST[d]))

    dat <- dat %>%
      group_by(respid) %>%
      mutate(
        !!got_hypo := max(!!got_hypo, na.rm = TRUE),
        !!got_hypo := ifelse(!!got_hypo %in% c(-Inf, Inf), NA_real_, !!got_hypo)
      ) %>%
      ungroup() %>% suppressWarnings()

    f <- paste0("dat$got_hypo_", RI_DOSE_LIST[d]," <- haven::labelled(dat$got_hypo_",
                RI_DOSE_LIST[d],", label = 'Received ",RI_DOSE_LIST[d]," (1=yes; 0=no)') %>% suppressWarnings()")
    eval(parse_expr(f))
  }

  dat <- dat %>% group_by(respid) %>%
    slice(1) %>% ungroup()

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER, ".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

