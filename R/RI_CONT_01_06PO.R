#' Make plots for RI_CONT_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_CONT_01_06PO R version 1.00 - Biostat Global Consulting - 2022-12-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_CONT_01_06PO <- function(VCP = "RI_CONT_01_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){
    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    j = 1
    while (j <= length(RI_CONT_01_DROPOUT_LIST)){

      d1 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
      j = j+1
      d2 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
      j = j+1

      print(paste0(d1, " to ", d2))

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_CONT_01_",ANALYSIS_COUNTER,"_uwplot_",d1,"_",d2)
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_CONT_01_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
                     title = paste0("RI - Dropout ", str_to_upper(d1), " to ", str_to_upper(d2)),
                     name = paste0("RI_CONT_01_",ANALYSIS_COUNTER,"_uwplot_",d1,"_",d2),
                     savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment",
                       paste0("Dropout plot ",str_to_upper(d1), " to ", str_to_upper(d2)," was created and exported."))
    } #end of while
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
