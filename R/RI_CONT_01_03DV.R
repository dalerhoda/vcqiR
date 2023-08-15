#' Calculate derived variables for RI_CONT_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_CONT_01_<ANALYSIS_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven

# RI_CONT_01_03DV R version 1.00 - Biostat Global Consulting - 2022-12-16
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-16  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_CONT_01_03DV <- function(VCP = "RI_CONT_01_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_CONT_01_",ANALYSIS_COUNTER,".rds"))

  j = 1


  while(j <= length(RI_CONT_01_DROPOUT_LIST)){

    d1 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
    j = j+1
    d2 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
    j = j+1

    print(paste0(d1, " to ", d2))

    gotd1 <- rlang::sym(paste0("got_crude_",d1,"_to_analyze"))
    gotd2 <- rlang::sym(paste0("got_crude_",d2,"_to_analyze"))

    dat <- dat %>% mutate(tempvar1 = ifelse(!!gotd1 == 1, ifelse((!!gotd2 == 0) %in% TRUE, 1, 0), NA))
    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("Child received ", d1, " but not", d2)) %>% suppressWarnings()

    # Do not count if the child was not eligible for dose 2
    dat <- dat %>% mutate(tempvar1 = ifelse(is.na(!!gotd2), NA, tempvar1))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("dropout_",d1,"_",d2)

  } #end of while

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_CONT_01_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
