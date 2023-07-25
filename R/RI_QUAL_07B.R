#' Calculate valid coverage if there had been no missed opportunities for simultaneous vaccination (MOV)
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_QUAL_07B()

# RI_QUAL_07B R version 1.02 - Biostat Global Consulting - 2023-07-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Caitlin Clary   Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2023-07-14  1.02      Caitlin Clary   Issue warnings to screen and log and
#                                       footnote if RI_RECORDS_SOUGHT_FOR_ALL = 1
# *******************************************************************************

RI_QUAL_07B <- function(VCP = "RI_QUAL_07B"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_NO_DOBS == 1){
    vcqi_log_comment(VCP, 2, "Warning", "User requested RI_QUAL_07B but no respondents have full date of birth info, so the indicator will be skipped.")
  } else {
    print(paste0("Calculating ", VCP))

    # Issue warning on the screen and in the log if RI_RECORDS_SOUGHT_FOR_ALL == 1
    if (RI_RECORDS_SOUGHT_FOR_ALL == 1){

      register_07b_warning <- "In this analysis, register records were sought for all (RI_RECORDS_SOUGHT_FOR_ALL == 1).  This makes comparison of RI_QUAL_07B with RI_COVG_02 tricky.  For best results for this comparison, re-run with RI_RECORDS_NOT_SOUGHT or RI_RECORDS_SOUGHT_IF_NO_CARD set to 1.  See the Users Guide for more information."

      cli::cli_warn(register_07b_warning)

      vcqi_log_comment(VCP, 2, "Warning", register_07b_warning)

      i <- 1
      while (vcqi_object_exists(paste0("RI_QUAL_07B_FOOTNOTE_", i))){
        i <- i + 1
      }

      assign(paste0("RI_QUAL_07B_FOOTNOTE_", i), register_07b_warning, envir = .GlobalEnv)
      vcqi_log_global(paste0("RI_QUAL_07B_FOOTNOTE_", i))

      rm(register_07b_warning)

    }

    if (VCQI_PREPROCESS_DATA %in% 1){
      print("Pre-processing dataset")
      RI_QUAL_07B_01PP()
    }

    if (VCQI_GENERATE_DVS %in% 1){
      print("Calculating derived variables")
      RI_QUAL_07B_03DV()
    }

    if (VCQI_GENERATE_DATABASES %in% 1){
      print("Generating output databases")
      RI_QUAL_07B_04GO()
    }

    if (EXPORT_TO_EXCEL %in% 1){
      print("Exporting to Excel")
      RI_QUAL_07B_05TOST()
    }

    if (MAKE_PLOTS %in% 1){
      print("Making plots")
      RI_QUAL_07B_06PO()
    }
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
