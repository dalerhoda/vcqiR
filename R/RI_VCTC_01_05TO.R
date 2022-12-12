#' Export VCTC plot values to Excel
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr
#' @import openxlsx

# RI_VCTC_01_05TO R version 1.01 - Biostat Global Consulting - 2022-11-06
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-05  1.00      Mia Yu          Original R version
# 2022-11-06  1.01      Mia Yu          Package version
# *******************************************************************************

RI_VCTC_01_05TO <- function(VCP = "RI_VCTC_01_05TO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds"))

    sheetname <- paste0("RI_VCTC_01_",ANALYSIS_COUNTER)

    if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))){
      wb <- loadWorkbook(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))
      sheetnames <- getSheetNames(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))

      if (sheetname %in% sheetnames){
        removeWorksheet(wb, sheet = sheetname)
      }

    } else{
      wb <- createWorkbook()
    }

    doselist <- str_to_lower(TIMELY_DOSE_ORDER)
    namedat <- data.frame(newname = c("Level","ID","Stratum name","Chart tile order (left to right)"))

    for (d in seq_along(doselist)){
      tempnamedat <- data.frame(newname = c(paste0("Cum pct for ", TIMELY_DOSE_ORDER[d]),
                                            paste0("Pct width of tile for ", TIMELY_DOSE_ORDER[d]),
                                            paste0("Tile span of days for ", TIMELY_DOSE_ORDER[d]),
                                            paste0("Tile label for ", TIMELY_DOSE_ORDER[d])))
      namedat <- rbind(namedat, tempnamedat)
    }

    namedat <- as.data.frame(t(namedat))

    addWorksheet(wb, sheetName = sheetname)
    writeData(wb, sheet = sheetname, namedat, startRow = 1, colNames = FALSE)
    writeData(wb, sheet = sheetname, dat, startRow = 2, colNames = FALSE)

    note1 <- data.frame(note1 = c("Note: This table is not meant to be copied and pasted into a report, but rather to help someone who is looking at a RI_VCTC_01 plot and wants to know (or mention in a report) the horizontal width of some of the colored tiles in the stacked bars."))

    note2 <- data.frame(note2 = c("Note: The sheet is sorted by level and levelid and by left-to-right bar category order."))

    writeData(wb, sheet = sheetname, note1, startRow = nrow(dat) + 3, colNames = FALSE)
    writeData(wb, sheet = sheetname, note2, startRow = nrow(dat) + 4, colNames = FALSE)

    saveWorkbook(wb, file = paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"), overwrite = TRUE)

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
