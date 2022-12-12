#' Check interview date variable(s) and create VCQI-compatible interview date variables if needed
#'
#' @param dat Dataset to check for date variables
#' @param errormsgs Error messages to pass through (string vector)
#' @param exitflag Flag for VCQI error to pass through (a number: 0/1)
#'
#' @return A dataset, saved as <VCQI_RI_DATASET>_preclean in VCQI_OUTPUT_FOLDER; error messages if conditions not met
#'
#' @rawNamespace import(tools, except = makevars_user)
#' @import stringr
#' @import lubridate
#' @import haven

# check_interview_date R version 1.09 - Biostat Global Consulting - 2022-10-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-30  1.00      Mia Yu          Original R version
# 2022-07-14  1.01      Mia Yu          Changed the way comparing dates to allow NA
# 2022-07-15  1.02      Caitlin Clary   Use identical() for vector comparisons and change
#                                       back a VCQI_CHECK_INSTEAD_OF_RUN if() check (was
#                                       changed to %in% from != which reversed meaning)
# 2022-07-23  1.03      Mia Yu          Add the ability to read m/d/y as characters
# 2022-08-17  1.04      Caitlin Clary   Clean up comments; add vcqi_global calls
# 2022-09-20  1.05      Mia Yu          Clean up comments
# 2022-10-04  1.06      Mia Yu          Package version
# 2022-10-10  1.07      Caitlin Clary   Add dat argument for ease of use in package
# 2022-10-18  1.08      Mia Yu          Add variable labels
# 2022-10-25  1.09      Mia Yu          Added a part to pass and inherit errormsgs
#                                       and exitflag from check_RI_analysis_metadata
# *******************************************************************************

check_interview_date <- function(dat, errormsgs, exitflag){

  # First check to see if provided in single variable RI09
  RI09_1 <- 0
  RI09_wrong <- 0
  if("RI09" %in% names(dat)){RI09_1 = 1}

  # See if second option of RI09 variables were provided: RI09m, RI09d, and RI09y
  option2 <- c("RI09m", "RI09d","RI09y")
  RI09_2_list <- c()
  RI09_option2 <- c()
  for(i in seq_along(option2)){
    if(option2[i] %in% names(dat)){
      RI09_option2 <- c(RI09_option2, option2[i])
      RI09_2_list <- stringr::str_c(RI09_2_list, option2[i], sep = " , ")
    }
  }

  RI09_2 <- 0
  if(length(RI09_option2) %in% c(1,2)){
    exitflag <- 1
    errormsgs <- c(errormsgs, paste0("If providing the interview date components the dataset must include all 3 variables: RI09m, RI09d, and RI09y. Dataset only has: ", RI09_2_list))
    vcqi_log_comment(VCP, 1, "Error", paste0("If providing the interview date components the dataset must include all 3 variables: RI09m, RI09d, and RI09y. Dataset only has: ", RI09_2_list))
  } else if(length(RI09_option2) == 3){RI09_2 <- 1}

  # See if third option of RI09 variables were provided: RI09_m, RI09_d and RI09_y
  option3 <- c("RI09_m", "RI09_d","RI09_y")
  RI09_3_list <- c()
  RI09_option3 <- c()
  for(i in seq_along(option3)){
    if(option3[i] %in% names(dat)){
      RI09_option3 <- c(RI09_option3, option3[i])
      RI09_3_list <- stringr::str_c(RI09_3_list, option2[i], sep = " , ")
    }
  }

  RI09_3 <- 0
  if(length(RI09_option3) %in% c(1,2)){
    exitflag <- 1
    errormsgs <- c(errormsgs, paste0("If providing the interview date components the dataset must include all 3 variables: RI09_m, RI09_d and RI09_y. Dataset only has: ", RI09_3_list))
    vcqi_log_comment(VCP, 1, "Error", paste0("If providing the interview date components the dataset must include all 3 variables: RI09_m, RI09_d and RI09_y. Dataset only has: ", RI09_3_list))
  } else if(length(RI09_option3) == 3){RI09_3 <- 1}

  interview_variables <- NULL
  if(RI09_1 == 1){
    interview_variables <- stringr::str_c(interview_variables, "RI09", sep = " and ")
  }
  if(RI09_2 == 1){
    interview_variables <- stringr::str_c(interview_variables, "RI09m/RI09d/RI09y", sep = " and ")
  }
  if(RI09_3 == 1){
    interview_variables <- stringr::str_c(interview_variables, "RI09_m/RI09_d/RI09_y", sep = " and ")
  }

  if(RI09_1 == 1){
    RI09 <- get("RI09", dat)

    x <- try(
      month(RI09),
      silent = TRUE # prevents any error from printing to console
    )

    if(inherits(x, "try-error")){
      exitflag <- 1
      errormsgs <- c(errormsgs, "At least one date in RI09 is not sensible, please check RI09.")
      RI09_1 <- 0
      RI09_wrong <- 1
      vcqi_log_comment(VCP, 1, "Error", "At least one date in RI09 is not sensible, please check RI09.")
    } else {
      month1 <- as.numeric(month(RI09))
      day1 <- as.numeric(day(RI09))
      year1 <- as.numeric(year(RI09))
    }

  }

  if(RI09_2 == 1){
    dat$RI09m <- as.numeric(dat$RI09m)
    dat$RI09d <- as.numeric(dat$RI09d)
    dat$RI09y <- as.numeric(dat$RI09y)
  }

  if(RI09_3 == 1){
    dat$RI09_m <- as.numeric(dat$RI09_m)
    dat$RI09_d <- as.numeric(dat$RI09_d)
    dat$RI09_y <- as.numeric(dat$RI09_y)
  }

  interview_date_mismatch <- 0
  ## Cait 2022-07-15 - using identical() here to compare vectors and handle NAs gracefully
  RI09_count <- sum(RI09_1, RI09_2, RI09_3)
  if(RI09_count == 3){

    if(identical(month1, dat$RI09m) == FALSE |
       identical(month1, dat$RI09_m) == FALSE |
       identical(dat$RI09m, dat$RI09_m) == FALSE){interview_date_mismatch <- 1}

    if(identical(day1, dat$RI09d) == FALSE |
       identical(day1, dat$RI09_d) == FALSE |
       identical(dat$RI09d, dat$RI09_d) == FALSE){interview_date_mismatch <- 1}

    if(identical(year1, dat$RI09y) == FALSE |
       identical(year1, dat$RI09_y) == FALSE |
       identical(dat$RI09y, dat$RI09_y) == FALSE){interview_date_mismatch <- 1}

    if(interview_date_mismatch == 1){
      exitflag <- 1
      errormsgs <- c(errormsgs, "The 3 different interview dates provided in variable RI09, variables RI09m/RI09d/RI09y and variables RI09_m/RI09_d/_RI09_y do not match. Either provide a single interview date or correct values.")
      vcqi_log_comment(VCP, 1, "Error", "The 3 different interview dates provided in variable RI09, variables RI09m/RI09d/RI09y and variables RI09_m/RI09_d/_RI09_y do not match. Either provide a single interview date or correct values.")
    }
  }

  if(RI09_count == 2 & RI09_1 == 1 & RI09_2 == 1){

    if(identical(month1, dat$RI09m) == FALSE){interview_date_mismatch <- 1}
    if(identical(day1, dat$RI09d) == FALSE){interview_date_mismatch <- 1}
    if(identical(year1, dat$RI09y) == FALSE){interview_date_mismatch <- 1}

    if(interview_date_mismatch == 1){
      exitflag <- 1
      errormsgs <- c(errormsgs, "The 2 different interview dates provided in variable RI09 and variables RI09m/RI09d/RI09y do not match. Either provide a single interview date or correct values.")
      vcqi_log_comment(VCP, 1, "Error", "The 2 different interview dates provided in variable RI09 and variables RI09m/RI09d/RI09y do not match. Either provide a single interview date or correct values.")
    }
  }

  if(RI09_count == 2 & RI09_1 == 1 & RI09_3 == 1){

    if(identical(month1, dat$RI09_m) == FALSE){interview_date_mismatch <- 1}
    if(identical(day1, dat$RI09_d) == FALSE){interview_date_mismatch <- 1}
    if(identical(year1, dat$RI09_y) == FALSE){interview_date_mismatch <- 1}

    if(interview_date_mismatch == 1){
      exitflag <- 1
      errormsgs <- c(errormsgs, "The 2 different interview dates provided in variable RI09 and variables RI09_m/RI09_d/RI09_y do not match. Either provide a single interview date or correct values.")
      vcqi_log_comment(VCP, 1, "Error", "The 2 different interview dates provided in variable RI09 and variables RI09_m/RI09_d/RI09_y do not match. Either provide a single interview date or correct values.")
    }
  }

  if(RI09_count == 2 & RI09_2 == 1 & RI09_3 == 1){

    if(identical(dat$RI09m, dat$RI09_m) == FALSE){interview_date_mismatch <- 1}
    if(identical(dat$RI09d, dat$RI09_d) == FALSE){interview_date_mismatch <- 1}
    if(identical(dat$RI09y, dat$RI09_y) == FALSE){interview_date_mismatch <- 1}

    if(interview_date_mismatch == 1){
      exitflag <- 1
      errormsgs <- c(errormsgs, "The 2 different interview dates provided in variable RI09m/RI09d/RI09y  and variables RI09_m/RI09_d/RI09_y do not match. Either provide a single interview date or correct values.")
      vcqi_log_comment(VCP, 1, "Error", "The 2 different interview dates provided in variable RI09m/RI09d/RI09y  and variables RI09_m/RI09_d/RI09_y do not match. Either provide a single interview date or correct values.")
    }
  }

  # Interview date is required, if not provided send error to screen and set exitflag
  if(RI09_count == 0 & RI09_wrong != 1){
    exitflag <- 1
    errormsgs <- c(errormsgs, "Interview date is a required variable to run VCQI RI analysis. Add variables RI09_m, RI09_d and RI09_y to your dataset.")
    vcqi_log_comment(VCP, 1, "Error", "Interview date is a required variable to run VCQI RI analysis. Add variables RI09_m, RI09_d and RI09_y to your dataset.")
  }

  changed_interview_date_varname <- 0

  if(interview_date_mismatch == 0 & length(RI09_option2) %in% c(0,3) & length(RI09_option3) %in% c(0,3) & RI09_count > 0){
    if(RI09_count > 1){
      print(paste0(RI09_count, " different interview dates provided in ", interview_variables, " all match"))
      vcqi_log_comment(VCP, 3, "Comment", paste0(RI09_count, " different interview dates provided in ", interview_variables, " all match"))
    }

    if(VCQI_CHECK_INSTEAD_OF_RUN != 1){
      # If only one set of interview date components was provided,
      # make sure they are the ones with the underscore
      if(RI09_2 == 1 & RI09_3 != 1){
        changed_interview_date_varname = 1

        suffix <- c("m","d","y")
        for(m in seq_along(suffix)){
          varname <- paste0("RI09", suffix[m])
          newname <- paste0("RI09_", suffix[m])
          print(paste0("Variable ", varname," was renamed ", newname," for VCQI consistency."))
          vcqi_log_comment(VCP, 2, "Warning", paste0("Variable ", varname," was renamed ", newname," for VCQI consistency."))
          names(dat)[which(names(dat) == varname)] <- newname
        }
        RI09_3 <- 1
        RI09_2 <- 111
      }

      # If a single interview date variable was provided and variables without underscore were not
      # we want to break apart RI09 into date components
      if(RI09_1 == 1 & RI09_3 != 1){
        changed_interview_date_varname = 1
        print("Breaking variable RI09 into three separate date component variables: RI09_m, RI09_d and RI09_y")
        vcqi_log_comment(VCP, 3, "Comment", "Breaking variable RI09 into three separate date component variables: RI09_m, RI09_d and RI09_y")
        dat <- mutate(dat, RI09_m = month1, RI09_d = day1, RI09_y = year1)

        dat$RI09_m <- haven::labelled(dat$RI09_m, label = "Month of interview taken from variable RI09") %>% suppressWarnings()
        dat$RI09_d <- haven::labelled(dat$RI09_d, label = "Day or interview taken from variable RI09") %>% suppressWarnings()
        dat$RI09_y <- haven::labelled(dat$RI09_y, label = "Year of interview taken from variable RI09") %>% suppressWarnings()

        RI09_3 <- 1
        RI09_1 <- 111
      }

    }

    # Save as a new file name to preserve the original version of the dataset if changes made to variables
    # And point the VCQI_RI_DATASET global to new dataset
    if(changed_interview_date_varname == 1){

      filename <- paste0(VCQI_OUTPUT_FOLDER, "/",
                         tools::file_path_sans_ext(VCQI_RI_DATASET),
                         "_preclean.rds")

      datasetname <- paste0(tools::file_path_sans_ext(VCQI_RI_DATASET), "_preclean.rds")

      print(paste0("Dataset with interview date changes saved as ", filename))

      vcqi_log_comment(VCP, 3, "Comment",
                       paste0("Dataset with interview date changes saved as ", filename))

      # Save the preclean dataset and update globals
      saveRDS(dat, file = filename)

      #2022/12/01 add check for RI_TEMP_DATASETS

      if (vcqi_object_exists("RI_TEMP_DATASETS")){
        RI_TEMP_DATASETS <- get("RI_TEMP_DATASETS",envir = .GlobalEnv)
      } else {
        assign("RI_TEMP_DATASETS",NULL,envir = .GlobalEnv)
      }

      vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, datasetname))
      vcqi_global(VCQI_RI_DATASET, datasetname)
    }

    # When changed_interview_date_varname != 0, then downstream programs will use the RI dataset copied to VCQI_OUTPUT_FOLDER in check_RI_analysis_metadata
  }

  result = list(flag = exitflag, message = errormsgs)
  return(result)
}

