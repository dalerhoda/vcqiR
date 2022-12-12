#' Check existence, format, and contents of the cluster metadata (CM) dataset
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors and/or warnings if conditions not met
#'
#' @import dplyr
#'

# check_VCQI_CM_metadata R version 1.04 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-28  1.00      Caitlin Clary   Original R version
# 2022-07-28  1.01      Caitlin Clary   Use vcqi_read() to read CM dataset
# 2022-08-02  1.02      Mia Yu          Use vcqi_object_exists() at the beginning
#                                       and add the part that errors out when CM
#                                       data file not in the right format
# 2022-10-04  1.03      Mia Yu          Package version
# 2022-10-18  1.04      Caitlin Clary   Added vcqi_halt_immediately call
# *******************************************************************************

check_VCQI_CM_metadata <- function(VCP = "check_VCQI_CM_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  if(vcqi_object_exists("VCQI_CM_DATASET")) {
    if (file.exists(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))){
      # Read CM dataset
      CM <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))

      # Check if the file is in valid format
      if (is.data.frame(CM) == FALSE){
        errormsgs <- c(errormsgs,paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_CM_DATASET ", paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET)," is not in valid format"))
        exitflag <- 1
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("CM dataset: ", VCQI_DATA_FOLDER, "/",
                                VCQI_CM_DATASET, " is not in valid format"))
      } else{
        # Determine which psweight variable is required
        if (exists("VCQI_RI_DATASET") | exists("VCQI_TT_DATASET")) {
          psw <- "psweight_1year"
        } else if (exists("VCQI_SIA_DATASET")) {
          psw <- "psweight_sia"
        } else {
          psw <- NULL
        }

        # Variable checks

        cm_vars <- c("HH01","HH02","HH03","HH04",psw,"province_id","urban_cluster")
        for(i in seq_along(cm_vars)) {
          if(cm_vars[i] %in% names(CM)) {
            # Check variable format
            if (cm_vars[i] %in% c("HH02", "HH04")) {
              if(is.character(get(cm_vars[i], CM)) == FALSE) {
                warningmsgs <- c(warningmsgs,
                                 paste0(cm_vars[i]," should be a character variable in the CM dataset."))
                vcqi_log_comment(VCP, 2, "Warning",
                                 paste0(cm_vars[i]," should be a character variable in the CM dataset."))
              }
            } else {
              if(is.numeric(get(cm_vars[i], CM)) == FALSE) {
                warningmsgs <- c(warningmsgs,
                                 paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
                vcqi_log_comment(VCP, 2, "Warning",
                                 paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
              }
            }

            # Check for missing values
            if (cm_vars[i] %in% c("province_id", "urban_cluster")) {
              if (sum(is.na(get(cm_vars[i], CM))) > 0) {
                warningmsgs <-
                  c(warningmsgs,paste0(cm_vars[i]," should not have a missing value in the CM dataset."))
                vcqi_log_comment(VCP,2,"Warning",paste0(cm_vars[i]," should not have a missing value in the CM dataset."))
              }
            }

          } else {
            # Error if variable is not present in CM

            errormsgs <-
              c(errormsgs,paste0("Variable ",cm_vars[i]," does not exist in the CM dataset and is required to run VCQI."))
            vcqi_log_comment(VCP,1,"Error",paste0("Variable ",cm_vars[i]," does not exist in the CM dataset and is required to run VCQI."))

            exitflag <- 1
          }
        } # end variable check loop

        # Confirm that every row with the same value of HH01 has the same value of province_id
        # changes here: add if statement so only when those vars in dataset we keep testing
        if("HH01" %in% names(CM) & "province_id" %in% names(CM)){
          prov_test <- CM %>%
            select(HH01, province_id) %>%
            unique() %>% group_by(HH01) %>%
            summarize(n = n())
          if (max(prov_test$n) > 1) {
            errormsgs <-
              c(errormsgs,"The CM dataset has a problem. At least one value of HH01 has more than one value of province_id. Edit the CM dataset so that each value of HH01 is associated with a single value of province_id.")
            vcqi_log_comment(VCP, 1, "Error", "The CM dataset has a problem. At least one value of HH01 has more than one value of province_id. Edit the CM dataset so that each value of HH01 is associated with a single value of province_id.")
            exitflag <- 1
          }
        }

      }
    } else {
      errormsgs <- c(errormsgs, paste0("The file defined by ", VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET, " does not exist."))
      vcqi_log_comment(VCP, 1, "Error", paste0("The file defined by ", VCQI_DATA_FOLDER, "/",VCQI_CM_DATASET, " does not exist."))
      exitflag <- 1
    } #error message for file not exist
  } else {
    errormsgs <-
      c(errormsgs,
        "Please set VCQI_CM_DATASET in the control program.")
    vcqi_log_comment(VCP,
                     1,
                     "Error",
                     "Please set VCQI_CM_DATASET in the control program.")
    exitflag <- 1
  } #error messages for check VCQI_CM_DATASET


  if(!is.null(warningmsgs)){
    warning(warningmsgs)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}


