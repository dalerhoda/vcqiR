#' Generate output databases for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER

# RI_COVG_01_04GO R version 1.01 - Biostat Global Consulting - 2022-10-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-10-08  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_01_04GO <- function(VCP = "RI_COVG_01_04GO"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  for (d in seq_along(RI_DOSE_LIST)){
    print(RI_DOSE_LIST[d])

    make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_by_card"),
                              estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by card"),
                              vid = paste0(RI_DOSE_LIST[d],"_c"),
                              measureid = "RI_COVG_01")
    make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_by_history"),
                              estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by history"),
                              vid = paste0(RI_DOSE_LIST[d],"_h"),
                              measureid = "RI_COVG_01")

    scar <- NULL
    if (RI_DOSE_LIST[d] == "bcg"){
      make_svyp_output_database(variable = "got_crude_bcg_by_scar",
                                estlabel = "Crude bcg, by scar",
                                vid = "bcg_s",
                                measureid = "RI_COVG_01")
      scar <- " or scar"
    } #end of bcg

    make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_h"),
                              estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by card or history",scar),
                              vid = paste0(RI_DOSE_LIST[d],"_ch"),
                              measureid = "RI_COVG_01")

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_by_register"),
                                estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by register"),
                                vid = paste0(RI_DOSE_LIST[d],"_r"),
                                measureid = "RI_COVG_01")
      make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_r"),
                                estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by card or register"),
                                vid = paste0(RI_DOSE_LIST[d],"_cr"),
                                measureid = "RI_COVG_01")
      make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_h_or_r"),
                                estlabel = paste0("Crude ",RI_DOSE_LIST[d],", by card or history or register",scar),
                                vid = paste0(RI_DOSE_LIST[d],"_chr"),
                                measureid = "RI_COVG_01")
    }

    make_svyp_output_database(variable = paste0("got_crude_",RI_DOSE_LIST[d],"_to_analyze"),
                              estlabel = paste0("Crude ",RI_DOSE_LIST[d],", to analyze"),
                              vid = paste0(RI_DOSE_LIST[d],"_a"),
                              measureid = "RI_COVG_01")

  } # end of dose loop

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

