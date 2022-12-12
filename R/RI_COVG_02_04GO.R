#' Generate output databases for RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_COVG_02_04GO R version 1.01 - Biostat Global Consulting - 2022-10-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-11  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_02_04GO <- function(VCP = "RI_COVG_02_04GO"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  for (d in seq_along(RI_DOSE_LIST)){
    print(RI_DOSE_LIST[d])

    du <- str_to_upper(RI_DOSE_LIST[d])
    make_svyp_output_database(variable = paste0("got_valid_",RI_DOSE_LIST[d],"_by_card"),
                              estlabel = paste0("Valid ",du,", by card"),
                              vid = paste0(RI_DOSE_LIST[d],"_c"),
                              measureid = "RI_COVG_02")

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      make_svyp_output_database(variable = paste0("got_valid_",RI_DOSE_LIST[d],"_by_register"),
                                estlabel = paste0("Valid ",du,", by register"),
                                vid = paste0(RI_DOSE_LIST[d],"_r"),
                                measureid = "RI_COVG_02")
      make_svyp_output_database(variable = paste0("got_valid_",RI_DOSE_LIST[d],"_c_or_r"),
                                estlabel = paste0("Valid ",du,", by card or register"),
                                vid = paste0(RI_DOSE_LIST[d],"_cr"),
                                measureid = "RI_COVG_02")
    }

    make_svyp_output_database(variable = paste0("got_valid_",RI_DOSE_LIST[d],"_to_analyze"),
                              estlabel = paste0("Valid ",du,", to analyze"),
                              vid = paste0(RI_DOSE_LIST[d],"_a"),
                              measureid = "RI_COVG_02")

    min_age <- get(paste0(RI_DOSE_LIST[d],"_min_age_days"), envir = .GlobalEnv)

    if (min_age < 365){
      make_svyp_output_database(variable = paste0("valid_",RI_DOSE_LIST[d],"_age1_card"),
                                estlabel = paste0("Valid ",du," by age 1, by card"),
                                vid = paste0(RI_DOSE_LIST[d],"_ca1"),
                                measureid = "RI_COVG_02")
      if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
        make_svyp_output_database(variable = paste0("valid_",RI_DOSE_LIST[d],"_age1_register"),
                                  estlabel = paste0("Valid ",du," by age 1, by register"),
                                  vid = paste0(RI_DOSE_LIST[d],"_ra1"),
                                  measureid = "RI_COVG_02")
        make_svyp_output_database(variable = paste0("valid_",RI_DOSE_LIST[d],"_age1_c_or_r"),
                                  estlabel = paste0("Valid ",du," by age 1, by card or register"),
                                  vid = paste0(RI_DOSE_LIST[d],"_cra1"),
                                  measureid = "RI_COVG_02")
      }

      make_svyp_output_database(variable = paste0("valid_",RI_DOSE_LIST[d],"_age1_to_analyze"),
                                estlabel = paste0("Valid ",du," by age 1, to analyze"),
                                vid = paste0(RI_DOSE_LIST[d],"_aa1"),
                                measureid = "RI_COVG_02")

    }

  }# end of dose loop

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
