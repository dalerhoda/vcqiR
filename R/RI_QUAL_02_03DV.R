#' Calculate derived variables for RI_QUAL_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_02_<ANALYSIS_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import haven

# RI_QUAL_02_03DV R version 1.00 - Biostat Global Consulting - 2022-12-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-21  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_02_03DV <- function(VCP = "RI_QUAL_02_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_02_",ANALYSIS_COUNTER,".rds"))

  dat <- dat %>% mutate(ever_had_an_ri_card = ifelse((psweight > 0 & !is.na(psweight)) %in% TRUE,
                                                     ifelse((RI26==1) %in% TRUE, 1, 0), NA))
  dat$ever_had_an_ri_card <- haven::labelled(dat$ever_had_an_ri_card,
                                  label = "Ever had an RI card") %>% suppressWarnings()

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_02_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
