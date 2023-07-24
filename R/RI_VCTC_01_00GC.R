#' Check global macros for RI_VCTC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# RI_VCTC_01_00GC R version 1.02 - Biostat Global Consulting - 2022-12-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-07  1.00      Mia Yu          Original R version
# 2022-11-12  1.01      Mia Yu          Package version
# 2022-12-23  1.02      Mia Yu          Adde parts for previously skipped global values
# 2023-07-23  1.03      Mia Yu          If register records were sought and the
# 										                  user asks to draw the HBR line showing
#										                    only card availability, gently prompt
#										                    them to draw the line at had_card_or_register
# *******************************************************************************

RI_VCTC_01_00GC <- function(VCP = "RI_VCTC_01_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # First global to check: TIMELY_DOSE_ORDER must be defined

  if (!vcqi_object_exists("TIMELY_DOSE_ORDER")) {
    assign("TIMELY_DOSE_ORDER", str_to_upper(RI_DOSE_LIST), envir = .GlobalEnv)
    print(paste0("For RI_VCTC_01, the user did not specify TIMELY_DOSE_ORDER so VCQI will use RI_DOSE_LIST: ",RI_DOSE_LIST))
    vcqi_log_comment(VCP,2,"Warning",
                     paste0("For RI_VCTC_01, the user did not specify TIMELY_DOSE_ORDER so VCQI will use RI_DOSE_LIST: ",RI_DOSE_LIST))
  }

  # Lets start by making a few globals all upper case
  # Make timely dose order upper case
  #TIMELY_DOSE_ORDER <- str_to_upper(TIMELY_DOSE_ORDER)
  assign("TIMELY_DOSE_ORDER", str_to_upper(TIMELY_DOSE_ORDER), envir = .GlobalEnv)
  timely_dose_order <- TIMELY_DOSE_ORDER

  # We need to make a local with this list sorted
  doseorderlist <- str_sort(timely_dose_order)

  # Make CD list upper case
  if(vcqi_object_exists("TIMELY_CD_LIST")){
    assign("TIMELY_CD_LIST", str_to_upper(TIMELY_CD_LIST), envir = .GlobalEnv)
    timely_cd_list <- str_sort(TIMELY_CD_LIST)
  } else {
    assign("TIMELY_CD_LIST", NULL, envir = .GlobalEnv)
    timely_cd_list <- NA_character_
  }

  exitflag <- 0
  errormsgs <- NULL

  # Check to see whether the globals .do file has run
  if ((!(all(suppressWarnings(timely_cd_list == doseorderlist)) %in% TRUE)) & (!vcqi_object_exists("TIMELY_N_DTS"))) {
    errormsgs <- c(errormsgs,
                   "The TIMELY_CD_LIST is not the same as the TIMELY_DOSE_ORDER and TIMELY_N_DTS is empty, so the user does not appear to have included or run the .do file named globals_for_timeliness_plots.  Run or include that .do file before calling RI_VCTC_01.")
    vcqi_log_comment(VCP,1,"Warning",
                     "The TIMELY_CD_LIST is not the same as the TIMELY_DOSE_ORDER and TIMELY_N_DTS is empty, so the user does not appear to have included or run the .do file named globals_for_timeliness_plots.  Run or include that .do file before calling RI_VCTC_01.")
    exitflag <- 1
  }

  # Only conduct the other checks if the user has actually run the globals_for_timeliness_plots .do file
  if (exitflag == 0) {
    # All doses in TIMELY_DOSE_ORDER should be in the RI_DOSE_LIST
    for (t in seq_along(TIMELY_DOSE_ORDER)) {
      if (!(TIMELY_DOSE_ORDER[t] %in% str_to_upper(RI_DOSE_LIST))) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Dose ",TIMELY_DOSE_ORDER[t],
                              " provided in global variable TIMELY_DOSE_ORDER must also be part of global variable RI_DOSE_LIST."))
        errormsgs <- c(errormsgs,
                       paste0("(Add ",TIMELY_DOSE_ORDER[t]," to the appropriate dose list global)"))

        vcqi_log_comment(VCP,1,"Error",
                         paste0("Dose ",TIMELY_DOSE_ORDER[t],
                                " provided in global variable TIMELY_DOSE_ORDER must also be part of global variable RI_DOSE_LIST.",
                                " (Add ",TIMELY_DOSE_ORDER[t]," to the appropriate dose list global)"))
      }
    } #end of TIMELY_DOSE_ORDER t loop

    # User may set the y coordinates of the bars using the global TIMELY_Y_COORDS
    if (vcqi_object_exists("TIMELY_Y_COORDS")) {
      if (length(TIMELY_DOSE_ORDER) != length(TIMELY_Y_COORDS)) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_Y_COORDS has ",length(TIMELY_Y_COORDS),
                              " elements in its list, but global TIMELY_DOSE_ORDER has ",length(TIMELY_DOSE_ORDER),". They should be equal."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_Y_COORDS has ",length(TIMELY_Y_COORDS),
                                " elements in its list, but global TIMELY_DOSE_ORDER has ",length(TIMELY_DOSE_ORDER),". They should be equal."))
      }
    }

    #All elements should be numeric
    for (i in seq_along(TIMELY_Y_COORDS)) {
      if (!is.numeric(TIMELY_Y_COORDS[i])) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_Y_COORDS contains the item ",
                              TIMELY_Y_COORDS[i]," which is not numeric.  All of its entries should be integers."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_Y_COORDS contains the item ",
                                TIMELY_Y_COORDS[i]," which is not numeric.  All of its entries should be integers."))
      } else if (TIMELY_Y_COORDS[i] %% 1 != 0) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_Y_COORDS contains the item ",TIMELY_Y_COORDS[i],
                              " which is not an interger. All of its entries should be integers."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_Y_COORDS contains the item ",TIMELY_Y_COORDS[i],
                                " which is not an interger  All of its entries should be integers."))
      } else {
        #only check the order if it's an integer

        # YCOORD values should be in strictly increasing numeric order
        #
        # If y[i] and y[i-1] are both numeric, then check the order

        if (i > 1) {
          if (is.numeric(TIMELY_Y_COORDS[i - 1]) &
              (TIMELY_Y_COORDS[i - 1] %% 1 == 0)) {
            if (TIMELY_Y_COORDS[i] <= TIMELY_Y_COORDS[i - 1]) {
              exitflag <- 1
              errormsgs <- c(errormsgs,
                             paste0("Global variable TIMELY_Y_COORDS should be in numeric order, but ",
                                    TIMELY_Y_COORDS[i]," appears after ",TIMELY_Y_COORDS[i - 1]))
              vcqi_log_comment(VCP,1,"Error",
                               paste0("Global variable TIMELY_Y_COORDS should be in numeric order, but ",
                                      TIMELY_Y_COORDS[i]," appears after ",TIMELY_Y_COORDS[i - 1]))
            }
          }
        } #end of i > 1

      }

    } #end of TIMELY_Y_COORDS i loop

    # global variable TIMELY_LEGEND_ORDER must be define
    # NOTE: more potential tests can be implemented
    if (!vcqi_object_exists("TIMELY_LEGEND_ORDER")){
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     "Global variable TIMELY_LEGEND_ORDER must be defined")
      vcqi_log_comment(VCP,1,"Error",
                       "Global variable TIMELY_LEGEND_ORDER must be defined")
    } else {

      for (lo in seq_along(TIMELY_LEGEND_ORDER)){

        if (!substr(TIMELY_LEGEND_ORDER[lo], 1, 2) %in% c("CD","DT")){
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         "Elements in TIMELY_LEGEND_ORDER must start with 'DT' or 'CD'.")
          vcqi_log_comment(VCP,1,"Error",
                           "Elements in TIMELY_LEGEND_ORDER must start with 'DT' or 'CD'.")
        } else {

          if (substr(TIMELY_LEGEND_ORDER[lo], 1, 2) == "CD"){
            dose <- gsub(".*[_]([^.]+)[_].*", "\\1", TIMELY_LEGEND_ORDER[lo])
            dose <- str_to_upper(dose)

            if (!vcqi_object_exists("TIMELY_CD_LIST")){
              exitflag <- 1
              errormsgs <- c(errormsgs,
                             "Please define TIMELY_CD_LIST before defining TIMELY_LEGEND_ORDER with customized dose")
              vcqi_log_comment(VCP,1,"Error",
                               "Please define TIMELY_CD_LIST before defining TIMELY_LEGEND_ORDER with customized dose")
            } else {
              cddose <- str_to_upper(TIMELY_CD_LIST)

              if (!dose %in% cddose){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               "Customized dose tile legend must be for doses in TIMELY_CD_LIST.")
                vcqi_log_comment(VCP,1,"Error",
                                 "Customized dose tile legend must be for doses in TIMELY_CD_LIST.")
              } else {
                num <- as.numeric(substr(TIMELY_LEGEND_ORDER[lo], nchar(TIMELY_LEGEND_ORDER[lo]), nchar(TIMELY_LEGEND_ORDER[lo])))
                max <- get(paste0("TIMELY_CD_",dose,"_NTILES"), envir = .GlobalEnv)

                if (num > as.numeric(max)){
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Number of legend can't be more than TIMELY_CD_",dose,"_NTILES."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Number of legend can't be more than TIMELY_CD_",dose,"_NTILES."))
                }

              }
            }

          } else if (substr(TIMELY_LEGEND_ORDER[lo], 1, 2) == "DT") {

            num <- as.numeric(substr(TIMELY_LEGEND_ORDER[lo], nchar(TIMELY_LEGEND_ORDER[lo]), nchar(TIMELY_LEGEND_ORDER[lo])))

            if (num > as.numeric(TIMELY_N_DTS)){
              exitflag <- 1
              errormsgs <- c(errormsgs,
                             "Number of legend can't be more than TIMELY_N_DTS")
              vcqi_log_comment(VCP,1,"Error",
                               "Number of legend can't be more than TIMELY_N_DTS")
            }

          } #end of DT

        }

      } #end of TIMELY_LEGEND_ORDER lo loop

    }

    # If global variable TIMELY_CD_LIST is defined we need to do some checks and make a sorted list of the the values

    if (vcqi_object_exists("TIMELY_CD_LIST")) {
      # Create local with sorted value
      cdlist <- str_sort(timely_cd_list)

      # Second if TIMELY_CD_LIST is defined, it should hold a subset of doses from TIMELY_DOSE_ORDER
      for (t in seq_along(TIMELY_CD_LIST)) {
        if (!(TIMELY_CD_LIST[t] %in% TIMELY_DOSE_ORDER)) {
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         paste0("Dose ",TIMELY_CD_LIST[t],
                                " provided in global variable TIMELY_CD_LIST must be one of the doses provided in global variable TIMELY_DOSE_ORDER"))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Dose ",TIMELY_CD_LIST[t],
                                  " provided in global variable TIMELY_CD_LIST must be one of the doses provided in global variable TIMELY_DOSE_ORDER"))
        }

      } #end of TIMELY_CD_LIST t loop

    } else {
      cdlist <- NA_character_
    }

    # For each dose in the TIMELY_CD_LIST, check the following globals are also populated:
    if (vcqi_object_exists("TIMELY_CD_LIST")) {
      for (t in seq_along(TIMELY_CD_LIST)) {
        if (!vcqi_object_exists(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_NTILES"))) {
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_NTILES must be populated."))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_NTILES must be populated."))
        } else {
          ntile <- get(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_NTILES"),envir = .GlobalEnv)
          if (!is.numeric(ntile) | ntile %% 1 != 0 | ntile < 0) {
            exitflag <- 1
            errormsgs <- c(errormsgs,
                           paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_NTILES takes value ",
                                  ntile,"; it should be missing or a positive integer"))
            vcqi_log_comment(VCP,1,"Error",
                             paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_NTILES takes value ",
                                    ntile,"; it should be missing or a positive integer"))
          } else {
            # For each value of 1/TIMELY_CD_<dose in caps>_NTILES, the following
            for (i in 1:ntile) {
              if (i != ntile) {
                # Confirm global  ${TIMELY_CD_`t'_UB_`i'} is set
                if (!vcqi_object_exists(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_UB_", i))) {
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i," must be defined."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i," must be defined."))

                } else {
                  # TIMELY_CD_<dose in caps>_UB_`i' should be a number greater than TIMELY_CD_<dose in caps>_UB_`=`i'-1'
                  if (i > 1) {
                    ub1 <- as.numeric(get(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_UB_", i),envir = .GlobalEnv))
                    ub0 <- as.numeric(get(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_UB_", i - 1),envir = .GlobalEnv))
                    if (ub1 <= ub0) {
                      exitflag <- 1
                      errormsgs <- c(errormsgs,
                                     paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i,
                                            " should be greater than TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i - 1))
                      vcqi_log_comment(VCP,1,"Error",
                                       paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i,
                                              " should be greater than TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i - 1))
                    }
                  } #end of i > 1
                }

                # TIMELY_CD_<dose in caps>_LABEL_`i' should be defined; if it is not set to default
                if (!vcqi_object_exists(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_LABEL_", i))) {
                  tub <- as.numeric(get(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_UB_", i),envir = .GlobalEnv))
                  assign(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_LABEL_", i),
                         paste0("Within ", tub, "days"),envir = .GlobalEnv)

                  print(paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LABEL_",i," was not set."))
                  print(paste0("Default value will be used: Within ", tub, "days"))
                  vcqi_log_comment(VCP,3,"Comment",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LABEL_",i,
                                          " was not set."," Default value will be used: Within ",tub,"days"))
                }

              } #end of i != ntile

              if (i == ntile) {
                if (vcqi_object_exists(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_UB_", i))) {
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i,
                                        " should not be set as this is for the children whose vaccination timing is unknown."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_UB_",i,
                                          " should not be set as this is for the children whose vaccination timing is unknown."))
                }

                if (!vcqi_object_exists(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_LABEL_", i))) {
                  assign(paste0("TIMELY_CD_", TIMELY_CD_LIST[t], "_LABEL_", i),
                         "Timing unknown",envir = .GlobalEnv)

                  print(paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LABEL_",i," was not set."))
                  print("Default value will be used: 'Timing unknown'")
                  vcqi_log_comment(VCP,3,"Comment",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LABEL_",
                                          i," was not set."," Default value will be used: 'Timing unknown'"))
                }

              } #end of i = ntile

              # TIMELY_CD_<dose in caps>_COLOR_`i' should be defined (and a valid color)
              if (!vcqi_object_exists(paste0("TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i))){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i,
                                      "  should be defined (and a valid color)"))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i,
                                        "  should be defined (and a valid color)"))
              } else {
                colorrec <- get(paste0("TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i),envir = .GlobalEnv)
                if (!(colorrec %in% colors()) & !(nchar(colorrec) == 7 & substr(colorrec,1,1) == "#")){
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i,
                                        " currently takes value ",colorrec,"; it should be a valid color option"))
                  vcqi_log_comment(
                    VCP,1,"Error",
                    paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_COLOR_",i,
                           " currently takes value ",colorrec,"; it should be a valid color option"))
                }

              }

              # TIMELY_CD_<dose in caps>_LCOLOR_`i' should be defined (and a valid color)
              if (!vcqi_object_exists(paste0("TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i))){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i,
                                      "  should be defined (and a valid color)"))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i,
                                        "  should be defined (and a valid color)"))
              } else {
                colorline <- get(paste0("TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i),envir = .GlobalEnv)
                if (!(colorline %in% colors()) & !(nchar(colorline) == 7 & substr(colorline,1,1) == "#")){
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i,
                                        " currently takes value ",colorline,"; it should be a valid color option"))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LCOLOR_",i,
                                          " currently takes value ",colorline,"; it should be a valid color option"))
                }

              }

              if (paste0("CD_",TIMELY_CD_LIST[t],"_",i) %in% TIMELY_LEGEND_ORDER){
                if (!vcqi_object_exists(paste0("TIMELY_CD_",TIMELY_CD_LIST[t],"_LEGEND_LABEL_",i))){
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LEGEND_LABEL_",i,
                                        " should be defined."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_CD_",TIMELY_CD_LIST[t],"_LEGEND_LABEL_",i,
                                          " should be defined."))
                }
              }

            } #end of ntile i loop
          } #end of integer test
        } #end of check exist

      } #end of TIMELY_CD_LIST t loop

    } #end of check TIMELY_CD_LIST exist

    # Check to see if the list of doses in TIMELY_CD_LIST is the same as
    # TIMELY_DOSE_ORDER; if yes, that means the user defined a custom definition
    # for EVERY dose in the list, and we do not need the default definitions, so
    # skip the TIMELY_DT checks.  But if the lists do not hold exactly the same
    # doses, then we need the default definition, so:

    if (!((all(suppressWarnings(cdlist == doseorderlist)) %in% TRUE) & vcqi_object_exists("TIMELY_DOSE_ORDER"))) {
      if (!vcqi_object_exists("TIMELY_N_DTS")) {
        exitflag <- 1
        print("Global variable TIMELY_N_DTS is not populated. User must include the following code in the control program:")
        print("include '<path>/globals_for_timeliness_plots.do' BEFORE calling RI_VCTC_01.")
        vcqi_log_comment(VCP,1,"Error",
                         "Global variable TIMELY_N_DTS is not populated. User must include the following code in the control program: include <path>/globals_for_timeliness_plots.do BEFORE calling RI_VCTC_01.")
      } else {
        # If defined, TIMELY_N_DTS should be a positive integer
        if (!is.numeric(TIMELY_N_DTS) |
            TIMELY_N_DTS %% 1 != 0 | TIMELY_N_DTS < 0) {
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         paste0("Global variable TIMELY_N_DTS takes value ",TIMELY_N_DTS,
                                "; it should be missing or a positive integer"))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Global variable TIMELY_N_DTS takes value ",TIMELY_N_DTS,
                                  "; it should be missing or a positive integer"))
        } else {
          # Loop from 1 up to TIMELY_N_DTS  (number of default tiles)
          for (i in 1:TIMELY_N_DTS) {
            # TIMELY_DT_UB_`i' should be a number that is greater than TIMELY_DT_UB_`=`i'-1'
            if (i != TIMELY_N_DTS) {

              if(vcqi_object_exists(paste0("TIMELY_DT_UB_", i))){
                dtub <- get(paste0("TIMELY_DT_UB_", i),envir = .GlobalEnv)
                if (!is.numeric(dtub)) {
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_DT_UB_",i," is ",dtub,
                                        "; it should be 0 or a positive number."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_DT_UB_",i," is ",dtub,
                                          "; it should be 0 or a positive number."))
                } else if (dtub < 0) {
                  exitflag <- 1
                  errormsgs <- c(errormsgs,
                                 paste0("Global variable TIMELY_DT_UB_",i," is ",dtub,
                                        "; it should be 0 or a positive number."))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("Global variable TIMELY_DT_UB_",i," is ",dtub,
                                          "; it should be 0 or a positive number."))
                }

                if (i != 1) {
                  dtub1 <- get(paste0("TIMELY_DT_UB_", i))
                  dtub0 <- get(paste0("TIMELY_DT_UB_", i - 1))
                  if (is.numeric(dtub1) & is.numeric(dtub0)) {
                    if (!(dtub1 > dtub0)) {
                      exitflag <- 1
                      errormsgs <- c(errormsgs,
                                     paste0("Global variable TIMELY_DT_UB_",i,
                                            " should be greater than global variable TIMELY_DT_UB_",i - 1))
                      vcqi_log_comment(VCP,1,"Error",
                                       paste0("Global variable TIMELY_DT_UB_",i,
                                              " should be greater than global variable TIMELY_DT_UB_",i - 1))
                    }
                  }

                } #end of i != 1

                if (!vcqi_object_exists(paste0("TIMELY_DT_LABEL_", i))) {
                  dtub <- get(paste0("TIMELY_DT_UB_", i),envir = .GlobalEnv)
                  assign(paste0("TIMELY_DT_LABEL_", i),
                         paste0("Received within ",dtub," days of scheduled age"),envir = .GlobalEnv)
                  print(paste0("Global variable TIMELY_DT_LABEL_",i," was not set."))
                  print(paste0("Default value will be used: 'Received within ",dtub," days of scheduled age'"))
                  vcqi_log_comment(VCP,3,"Comment",
                                   paste0("Global variable TIMELY_DT_LABEL_",i,
                                          " was not set. Default value will be used: 'Received within ",dtub," days of scheduled age'"))
                }

              } else {
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_DT_UB_",i," must be defined."))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_DT_UB_",i," must be defined."))
              }

            } #end of i != TIMELY_N_DTS


            if (i == TIMELY_N_DTS) {
              if (vcqi_object_exists(paste0("TIMELY_DT_UB_", i))) {
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_DT_UB_",i,
                                      " should not be set as this is for the children whose vaccination timing is unknown."))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_DT_UB_",i,
                                        " should not be set as this is for the children whose vaccination timing is unknown."))
              }

              if (!vcqi_object_exists(paste0("TIMELY_DT_LABEL_", i))) {
                assign(paste0("TIMELY_DT_LABEL_", i),"Age received is unknown",envir = .GlobalEnv)
                print(paste0("Global variable TIMELY_DT_LABEL_",i," was not set."))
                print("Default value will be used: 'Age received is unknown'")
                vcqi_log_comment(VCP,3,"Comment",
                                 paste0("Global variable TIMELY_DT_LABEL_",i,
                                        " was not set. Default value will be used: 'Age received is unknown'"))
              }

            } #end of i == TIMELY_N_DTS

            # TIMELY_DT_COLOR_`i' should be defined and a valid color
            if (!vcqi_object_exists(paste0("TIMELY_DT_COLOR_",i))){
              exitflag <- 1
              errormsgs <- c(errormsgs,
                             paste0("Global variable TIMELY_DT_COLOR_",i,
                                    "  should be defined (and a valid color)"))
              vcqi_log_comment(VCP,1,"Error",
                               paste0("Global variable TIMELY_DT_COLOR_",i,
                                      "  should be defined (and a valid color)"))
            } else {
              colorrec <- get(paste0("TIMELY_DT_COLOR_",i),envir = .GlobalEnv)
              if (!(colorrec %in% colors()) & !(nchar(colorrec) == 7 & substr(colorrec,1,1) == "#")){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_DT_COLOR_",i," currently takes value ",
                                      colorrec, "; it should be a valid color option."))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_DT_COLOR_",i," currently takes value ",
                                        colorrec, "; it should be a valid color option."))
              }
            }



            # TIMELY_DT_LCOLOR_`i' should be defined and a valid color
            if (!vcqi_object_exists(paste0("TIMELY_DT_LCOLOR_",i))){
              exitflag <- 1
              errormsgs <- c(errormsgs,
                             paste0("Global variable TIMELY_DT_LCOLOR_",i,
                                    "  should be defined (and a valid color)"))
              vcqi_log_comment(VCP,1,"Error",
                               paste0("Global variable TIMELY_DT_LCOLOR_",i,
                                      "  should be defined (and a valid color)"))
            } else {
              colorline <- get(paste0("TIMELY_DT_LCOLOR_",i),envir = .GlobalEnv)
              if (!(colorline %in% colors())  & !(nchar(colorline) == 7 & substr(colorline,1,1) == "#")){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_DT_LCOLOR_",i," currently takes value ",
                                      colorline, "; it should be a valid color option."))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_DT_LCOLOR_",i," currently takes value ",
                                        colorline, "; it should be a valid color option."))
              }
            }

            if (paste0("DT_",i) %in% TIMELY_LEGEND_ORDER){
              if (!vcqi_object_exists(paste0("TIMELY_DT_LEGEND_LABEL_",i))){
                exitflag <- 1
                errormsgs <- c(errormsgs,
                               paste0("Global variable TIMELY_DT_LEGEND_LABEL_",i,
                                      " should be defined."))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable TIMELY_DT_LEGEND_LABEL_",i,
                                        " should be defined."))
              }
            }

          } #end of TIMELY_N_DTS i loop
        }

      }
    } #end of check

    # Look at globals for the TIMELY_FULLY_VXD_NOTE and TIMELY_NOT_VXD_NOTE

    # TIMELY_FULLY_VCD_NOTE should be 0 or 1 or blank
    if (!TIMELY_FULLY_VXD_NOTE %in% c(0,1,NA)){
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_FULLY_VXD_NOTE should take values 0 or 1. It is currently ", TIMELY_FULLY_VXD_NOTE))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_FULLY_VXD_NOTE should take values 0 or 1. It is currently ", TIMELY_FULLY_VXD_NOTE))
    }

    if (TIMELY_FULLY_VXD_NOTE == 0){
      TIMELY_FULLY_VXD_NOTE_VARIABLE <- NA
    } else {
      if (!vcqi_object_exists("TIMELY_FULLY_VXD_NOTE_VARIABLE")){
        TIMELY_FULLY_VXD_NOTE_VARIABLE <- NA
      }
    }

    # TIMELY_FULLY_VXD_NOTE_VARIABLE should be one of the vars available from RI_COVG_03
    if (TIMELY_FULLY_VXD_NOTE == 1 & !(TIMELY_FULLY_VXD_NOTE_VARIABLE %in% c("fully_vaccinated_crude","fully_vaccinated_valid","fully_vaccinated_by_age1","fully_vxd_for_age_crude","fully_vxd_for_age_valid"))) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global TIMELY_FULLY_VXD_NOTE_VARIABLE should take one of these values: fully_vaccinated_crude, fully_vaccinated_valid, fully_vaccinated_by_age1, fully_vxd_for_age_crude, fully_vxd_for_age_valid; It is currently: ", TIMELY_FULLY_VXD_NOTE_VARIABLE))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global TIMELY_FULLY_VXD_NOTE_VARIABLE should take one of these values: fully_vaccinated_crude, fully_vaccinated_valid, fully_vaccinated_by_age1, fully_vxd_for_age_crude, fully_vxd_for_age_valid; It is currently: ", TIMELY_FULLY_VXD_NOTE_VARIABLE))
    }

    # TIMELY_NOT_VCD_NOTE should be 0 or 1 or blank
    if (!TIMELY_NOT_VXD_NOTE %in% c(0,1,NA)){
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_NOT_VXD_NOTE should take values 0 or 1. It is currently ", TIMELY_NOT_VXD_NOTE))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_NOT_VXD_NOTE should take values 0 or 1. It is currently ", TIMELY_NOT_VXD_NOTE))
    }

    if (TIMELY_NOT_VXD_NOTE == 0){
      TIMELY_NOT_VXD_NOTE_VARIABLE <- NA
    } else {
      if (!vcqi_object_exists("TIMELY_NOT_VXD_NOTE_VARIABLE")){
        TIMELY_NOT_VXD_NOTE_VARIABLE <- NA
      }
    }

    # TIMELY_NOT_VXD_NOTE_VARIABLE should be one of the vars available from RI_COVG_03
    if (TIMELY_NOT_VXD_NOTE == 1 & !(TIMELY_NOT_VXD_NOTE_VARIABLE %in% c("not_vaccinated_crude","not_vaccinated_valid","not_vaccinated_by_age1","not_vxd_for_age_crude","not_vxd_for_age_valid"))) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global TIMELY_NOT_VXD_NOTE_VARIABLE should take one of these values: not_vaccinated_crude, not_vaccinated_valid, not_vaccinated_by_age1, not_vxd_for_age_crude, not_vxd_for_age_valid; It is currently: ", TIMELY_NOT_VXD_NOTE_VARIABLE))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global TIMELY_NOT_VXD_NOTE_VARIABLE should take one of these values: not_vaccinated_crude, not_vaccinated_valid, not_vaccinated_by_age1, not_vxd_for_age_crude, not_vxd_for_age_valid; It is currently: ", TIMELY_NOT_VXD_NOTE_VARIABLE))
    }

    # Next we will look at globals to specify the 'Showed HBR' line.

    # If TIMELY_HBR_LINE_PLOT is not 1, the user didn't request adding that line, so we do not need to do the following checks

    if (TIMELY_HBR_LINE_PLOT == 1) {
      # global TIMELY_HBR_LINE_VARIABLE  should be one of the several variables produced by RI_QUAL_01; make it had_card if it is missing but TIMELY_HBR_LINE_PLOT is 1
      # We need to look at both the card and register values if register was seen
      checkvars <-
        (TIMELY_HBR_LINE_VARIABLE %in% c("had_card","had_card_with_dates","had_card_with_dates_or_ticks","had_card_with_flawless_dates"))

      if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1) {
        checkvars2 <-
          (TIMELY_HBR_LINE_VARIABLE %in% c("had_register","had_register_with_dates","had_register_with_dates_or_ticks","had_register_with_flawless_dates","had_card_or_register"))
        checkvars <- ((checkvars | checkvars2) %in% TRUE)
      }

      if (!checkvars) {
        if (!vcqi_object_exists("TIMELY_HBR_LINE_VARIABLE")) {
          assign("TIMELY_HBR_LINE_VARIABLE", "had_card", envir = .GlobalEnv)
          print("Global variable TIMELY_HBR_LINE_VARIABLE was not defined. Default value of 'had_card' will be used")
          vcqi_log_comment(VCP,2,"Warning","Global variable TIMELY_HBR_LINE_VARIABLE was not defined. Default value of 'had_card' will be used")
        } else {
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         paste0("Global variable TIMELY_HBR_LINE_VARIABLE takes the value ", TIMELY_HBR_LINE_VARIABLE,
                                "; it should contain the name of one of the variables produced by RI_QUAL_01"))
          vcqi_log_comment(VCP,1,"Error",paste0("Global variable TIMELY_HBR_LINE_VARIABLE takes the value ",TIMELY_HBR_LINE_VARIABLE,
                                                "; it should contain the name of one of the variables produced by RI_QUAL_01"))
          }
      } #end of if var not qualify

      # global TIMELY_HBR_LINE_WIDTH make it medium if not defined

      if (!vcqi_object_exists("TIMELY_HBR_LINE_WIDTH")) {
        assign("TIMELY_HBR_LINE_WIDTH", 1, envir = .GlobalEnv)
        print("Global variable TIMELY_HBR_LINE_WIDTH was not defined; VCQI will use the default value: 1")
        vcqi_log_comment(VCP,2,"Warning","Global variable TIMELY_HBR_LINE_WIDTH was not defined; VCQI will use the default value: 1")
      } else if (!is.numeric(TIMELY_HBR_LINE_WIDTH)) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_HBR_LINE_WIDTH currently takes value ",TIMELY_HBR_LINE_WIDTH,"; it should be a valid size style option"))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_HBR_LINE_WIDTH currently takes value ",TIMELY_HBR_LINE_WIDTH,"; it should be a valid size style option"))
      } #end of checking TIMELY_HBR_LINE_WIDTH

      # global TIMELY_HBR_LINE_COLOR make it gs8 if not defined
      if (!vcqi_object_exists("TIMELY_HBR_LINE_COLOR")) {
        assign("TIMELY_HBR_LINE_COLOR", "grey8", envir = .GlobalEnv)
        print("Global variable TIMELY_HBR_LINE_COLOR was not defined; VCQI will use the default value: grey8")
        vcqi_log_comment(VCP,2,"Warning","Global variable TIMELY_HBR_LINE_COLOR was not defined; VCQI will use the default value: grey8")
      } else if (!(TIMELY_HBR_LINE_COLOR %in% colors()) & !(nchar(TIMELY_HBR_LINE_COLOR) == 7 & substr(TIMELY_HBR_LINE_COLOR, 1, 1) == "#")) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_HBR_LINE_COLOR currently takes value ",TIMELY_HBR_LINE_COLOR,"; it should be a valid color option."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_HBR_LINE_COLOR currently takes value ",TIMELY_HBR_LINE_COLOR,"; it should be a valid color option."))
      } #end of checking TIMELY_HBR_LINE_COLOR

      if (!vcqi_object_exists("TIMELY_HBR_LINE_LABEL_COLOR")) {
        assign("TIMELY_HBR_LINE_LABEL_COLOR", "grey2", envir = .GlobalEnv)
        print("Global variable TIMELY_HBR_LINE_LABEL_COLOR was not defined; VCQI will use the default value: grey2")
        vcqi_log_comment(VCP,2,"Warning","Global variable TIMELY_HBR_LINE_LABEL_COLOR was not defined; VCQI will use the default value: grey2")
      } else if (!(TIMELY_HBR_LINE_LABEL_COLOR %in% colors()) & !(nchar(TIMELY_HBR_LINE_LABEL_COLOR) == 7 & substr(TIMELY_HBR_LINE_LABEL_COLOR, 1, 1) == "#")) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_HBR_LINE_LABEL_COLOR currently takes value ",TIMELY_HBR_LINE_LABEL_COLOR,"; it should be a valid color option."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_HBR_LINE_LABEL_COLOR currently takes value ",TIMELY_HBR_LINE_LABEL_COLOR,"; it should be a valid color option."))
      } #end of checking TIMELY_HBR_LINE_LABEL_COLOR

      # global TIMELY_HBR_LINE_PATTERN make it 'shortdash' if not defined
      # NOTE: for now only allowing these types

      if (!vcqi_object_exists("TIMELY_HBR_LINE_PATTERN")) {
        assign("TIMELY_HBR_LINE_PATTERN", "dashed", envir = .GlobalEnv)
        print("Global variable TIMELY_HBR_LINE_PATTERN was not defined; VCQI will use the default value: dashed")
        vcqi_log_comment(VCP,2,"Warning","Global variable TIMELY_HBR_LINE_PATTERN was not defined; VCQI will use the default value: dashed")
      } else if ((!TIMELY_HBR_LINE_PATTERN %in% c("solid","dashed","dotted","dotdash","longdash","twodash")) & (!TIMELY_HBR_LINE_PATTERN %in% c(1:6))) {
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_HBR_LINE_PATTERN currently takes value ",TIMELY_HBR_LINE_PATTERN,"; it should be a valid linetype option."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_HBR_LINE_PATTERN currently takes value ",TIMELY_HBR_LINE_PATTERN,"; it should be a valid linetype option."))
      } #end of checking TIMELY_HBR_LINE_PATTERN

      # global TIMELY_HBR_LINE_LABEL must be defined

      if (!vcqi_object_exists("TIMELY_HBR_LINE_LABEL")) {
        exitflag <- 1
        errormsgs <- c(errormsgs,"Global variable TIMELY_HBR_LINE_LABEL needs to be populated since global variable TIMELY_HBR_LINE_PLOT is 1")
        vcqi_log_comment(VCP,1,"Error","Global variable TIMELY_HBR_LINE_LABEL needs to be populated since global variable TIMELY_HBR_LINE_PLOT is 1")
      } #end of checking TIMELY_HBR_LINE_LABEL

    } #end of TIMELY_HBR_LINE_PLOT == 1

    # *********************************************

    # Check each of these Global variables.
    # If not defined, use these defaults.

    if(!vcqi_object_exists("TIMELY_XLABEL_SIZE")){
      assign("TIMELY_XLABEL_SIZE", 8, envir = .GlobalEnv)
      print("Global variable TIMELY_XLABEL_SIZE was not defined; VCQI will use the default value: 8")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_XLABEL_SIZE was not defined; VCQI will use the default value: 8")
    } else if (!is.numeric(TIMELY_XLABEL_SIZE)) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_XLABEL_SIZE currently takes value ",
                            TIMELY_XLABEL_SIZE, "; it should be a valid size style option"))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_XLABEL_SIZE currently takes value ",
                              TIMELY_XLABEL_SIZE, "; it should be a valid size style option"))
    }

    if(!vcqi_object_exists("TIMELY_YLABEL_SIZE")){
      assign("TIMELY_YLABEL_SIZE", 8, envir = .GlobalEnv)
      print("Global variable TIMELY_YLABEL_SIZE was not defined; VCQI will use the default value: 8")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_YLABEL_SIZE was not defined; VCQI will use the default value: 8")
    } else if (!is.numeric(TIMELY_YLABEL_SIZE)) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_YLABEL_SIZE currently takes value ",
                            TIMELY_YLABEL_SIZE, "; it should be a valid size style option"))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_YLABEL_SIZE currently takes value ",
                              TIMELY_YLABEL_SIZE, "; it should be a valid size style option"))
    }

    if (!vcqi_object_exists("TIMELY_XLABEL_COLOR")){
      assign("TIMELY_XLABEL_COLOR", "black", envir = .GlobalEnv)
      print("Global variable TIMELY_XLABEL_COLOR was not defined; VCQI will use the default value: black")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_XLABEL_COLOR was not defined; VCQI will use the default value: black")
    } else if (!(TIMELY_XLABEL_COLOR %in% colors()) & !(nchar(TIMELY_XLABEL_COLOR) == 7 & substr(TIMELY_XLABEL_COLOR,1,1) == "#")) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_XLABEL_COLOR currently takes value ",
                            TIMELY_XLABEL_COLOR, "; it should be a valid color option."))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_XLABEL_COLOR currently takes value ",
                              TIMELY_XLABEL_COLOR, "; it should be a valid color option."))
    }

    if (!vcqi_object_exists("TIMELY_YLABEL_COLOR")){
      assign("TIMELY_YLABEL_COLOR", "black", envir = .GlobalEnv)
      print("Global variable TIMELY_YLABEL_COLOR was not defined; VCQI will use the default value: black")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_YLABEL_COLOR was not defined; VCQI will use the default value: black")
    } else if (!(TIMELY_YLABEL_COLOR %in% colors()) & !(nchar(TIMELY_YLABEL_COLOR) == 7 & substr(TIMELY_YLABEL_COLOR,1,1) == "#")) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_YLABEL_COLOR currently takes value ",
                            TIMELY_YLABEL_COLOR, "; it should be a valid color option."))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_YLABEL_COLOR currently takes value ",
                              TIMELY_YLABEL_COLOR, "; it should be a valid color option."))
    }

    if(!vcqi_object_exists("TIMELY_CI_LWIDTH")){
      assign("TIMELY_CI_LWIDTH", 4, envir = .GlobalEnv)
      print("Global variable TIMELY_CI_LWIDTH was not defined or not in numeric form; VCQI will use the default value: 4")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_CI_LWIDTH was not defined or not in numeric form; VCQI will use the default value: 4")
    } else if (!is.numeric(TIMELY_CI_LWIDTH)) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_CI_LWIDTH currently takes value ",
                            TIMELY_CI_LWIDTH, "; it should be a valid size style option"))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_CI_LWIDTH currently takes value ",
                              TIMELY_CI_LWIDTH, "; it should be a valid size style option"))
    }

    if(!vcqi_object_exists("TIMELY_BARWIDTH")){
      assign("TIMELY_BARWIDTH", 0.67, envir = .GlobalEnv)
      print("Global variable TIMELY_BARWIDTH was not defined or not in numeric form; VCQI will use the default value: 0.67")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_BARWIDTH was not defined or not in numeric form; VCQI will use the default value: 0.67")
    } else if (!is.numeric(TIMELY_BARWIDTH)) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_BARWIDTH currently takes value ",
                            TIMELY_BARWIDTH, "; it should be a valid size style option"))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_BARWIDTH currently takes value ",
                              TIMELY_BARWIDTH, "; it should be a valid size style option"))
    }

    if(!vcqi_object_exists("TIMELY_XSCALE_MAX")){
      assign("TIMELY_XSCALE_MAX", 150, envir = .GlobalEnv)
      print("Global variable TIMELY_XSCALE_MAX was not defined or not in numeric form; VCQI will use the default value: 150")
      vcqi_log_comment(VCP,2,"Warning",
                       "Global variable TIMELY_XSCALE_MAX was not defined or not in numeric form; VCQI will use the default value: 150")
    } else if (!is.numeric(TIMELY_XSCALE_MAX)) {
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("Global variable TIMELY_XSCALE_MAX currently takes value ",
                            TIMELY_XSCALE_MAX, "; it should be a valid size style option"))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable TIMELY_XSCALE_MAX currently takes value ",
                              TIMELY_XSCALE_MAX, "; it should be a valid size style option"))
    }


    # *********************************************
    #
    # Text bar options

    # ORDER is from the top down
    # Check TIMELY_TEXTBAR_ORDER and associated globals if populated

    if (vcqi_object_exists("TIMELY_TEXTBAR_ORDER")){
      #First we want to replace any , with spaces " "
      TIMELY_TEXTBAR_ORDER <- gsub(" ","", TIMELY_TEXTBAR_ORDER, fixed = TRUE)
      assign("TIMELY_TEXTBAR_ORDER",TIMELY_TEXTBAR_ORDER, envir = .GlobalEnv)

      textbar_error <- 0
      textbar_valid_list <- NULL

      # Check to make sure the global only contains valid strings

      textbar <- str_to_upper(TIMELY_TEXTBAR_ORDER)

      for (t in seq_along(textbar)){

        if (textbar[t] %in% c("COVG","CI","N","NHBR","NEFF","DEFF","ICC")){
          textbar_valid_list <- c(textbar_valid_list, textbar[t])
        } else {
          textbar_error <- 1
        }

      } #end of textbar t loop

      if (textbar_error == 1){
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable TIMELY_TEXTBAR_ORDER takes value ",TIMELY_TEXTBAR_ORDER,
                              "; it should only contain string values from list: COVG CI N NHBR NEFF DEFF ICC"))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable TIMELY_TEXTBAR_ORDER takes value ",TIMELY_TEXTBAR_ORDER,
                                "; it should only contain string values from list: COVG CI N NHBR NEFF DEFF ICC"))
      } #end of textbar_error == 1

      covg <- 0
      icc <- 0
      textbar_order <- NULL
      for (t in seq_along(textbar_valid_list)){

        if (vcqi_object_exists(paste0("TIMELY_TEXTBAR_X_",textbar_valid_list[t]))){
          textx <- get(paste0("TIMELY_TEXTBAR_X_",textbar_valid_list[t]), envir = .GlobalEnv)
        } else {
          textx <- NA
        }

        if (!is.numeric(textx)){
          exitflag <- 1
          errormsgs <- c(errormsgs,
                         paste0("Global variable TIMELY_TEXTBAR_X_",textbar_valid_list[t]," should be a number"))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Global variable TIMELY_TEXTBAR_X_",textbar_valid_list[t]," should be a number"))
        } else {
          textbar_order <- c(textbar_order,textbar_valid_list[t])

          if (!vcqi_object_exists(paste0("TIMELY_TEXTBAR_LABEL_",textbar_valid_list[t]))){
            exitflag <- 1
            errormsgs <- c(errormsgs,
                           paste0("Global variable TIMELY_TEXTBAR_LABEL_",textbar_valid_list[t],
                                  " should be populated with a short string."))
            vcqi_log_comment(VCP,1,"Error",
                             paste0("Global variable TIMELY_TEXTBAR_LABEL_",textbar_valid_list[t],
                                    " should be populated with a short string."))
          }
        } #end of check numeric

        if (!vcqi_object_exists(paste0("TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t]))){
          assign(paste0("TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t]), "black", envir = .GlobalEnv)
          print(paste0("Global variable TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t],
                       " was not defined; VCQI will use the default value: black"))
          vcqi_log_comment(VCP,2,"Warning",
                           paste0("Global variable TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t],
                                  " was not defined; VCQI will use the default value: black"))
        } else {
          textcolor <- get(paste0("TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t]), envir = .GlobalEnv)
          if (!(textcolor %in% colors()) & !(nchar(textcolor) == 7 & substr(textcolor,1,1) == "#")){
            exitflag <- 1
            errormsgs <- c(errormsgs,
                           paste0("Global variable TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t],
                                  " currently takes value ",textcolor, "; it should be a valid color option."))
            vcqi_log_comment(VCP,1,"Error",
                             paste0("Global variable TIMELY_TEXTBAR_COLOR_",textbar_valid_list[t],
                                    " currently takes value ",textcolor, "; it should be a valid color option."))
          }
        }

        if (textbar_valid_list[t] == "COVG"){covg <- 1}
        if (textbar_valid_list[t] == "ICC"){icc <- 1}

      } #end of textbar_valid_list t loop

      if (covg == 1 & !vcqi_object_exists("TIMELY_TEXTBAR_COVG_DEC_DIGITS")){
        #NOTE: use vcqi_global here, different from Stata
        vcqi_global(TIMELY_TEXTBAR_COVG_DEC_DIGITS,1)
      }
      if (icc == 1 & !vcqi_object_exists("TIMELY_TEXTBAR_ICC_DEC_DIGITS")){
        #NOTE: use vcqi_global here, different from Stata
        vcqi_global(TIMELY_TEXTBAR_ICC_DEC_DIGITS,3)
      }

    }

    # RI_VCTC_01_LEVELS should include only integers from the set 1 2 3, with no duplicates.
    # If missing, set to 3

    level_error <- 0

    if (!vcqi_object_exists("RI_VCTC_01_LEVELS")){
      #NOTE: use vcqi_global here, different from Stata
      vcqi_global(RI_VCTC_01_LEVELS,3)
    }

    level_list <- RI_VCTC_01_LEVELS
    if (any(duplicated(level_list)) %in% TRUE){
      level_error <- 1
      note <- " with no duplicate values."
    } else {
      note <- NULL
    }

    for (g in seq_along(level_list)){

      if (is.numeric(level_list[g])){

        if (!level_list[g] %in% c(1,2,3)){level_error <- 1}

      } else {
        level_error <- 1
      }

      if (level_error == 1){
        exitflag <- 1
        errormsgs <- c(errormsgs,
                       paste0("Global variable RI_VCTC_01_LEVELS takes value ",RI_VCTC_01_LEVELS,
                              " should only contain integer values of 1 2 and 3",note))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable RI_VCTC_01_LEVELS takes value ",RI_VCTC_01_LEVELS,
                                " should only contain integer values of 1 2 and 3",note))
      }
    } #end of level_list g loop

    # Now add a few more globals to be used later on in program

    # Number of doses in timeliness plot
    assign("TIMELY_N_DOSES",length(TIMELY_DOSE_ORDER), envir = .GlobalEnv)

    if (vcqi_object_exists("TIMELY_N_CUSTOMIZED_DOSES")){
      assign(TIMELY_N_CUSTOMIZED_DOSES, length(TIMELY_N_CUSTOMIZED_DOSES))
    }

    if (!vcqi_object_exists("TIMELY_TEXTBAR_LABEL_Y_SPACE")){
      vcqi_global(TIMELY_TEXTBAR_LABEL_Y_SPACE,TIMELY_BARWIDTH)
    }

    # If the user is drawing a line on the VCTC related only to card availability, but register records were sought in the survey,
    # gently prompt them to maybe draw the line at had_card_or_register.

    if (RI_RECORDS_NOT_SOUGHT == 0 & TIMELY_HBR_LINE_VARIABLE != "had_card_or_register"){
      print(paste0("Global variable TIMELY_HBR_LINE_VARIABLE takes value ",TIMELY_HBR_LINE_VARIABLE,
                   " but RI_RECORDS_NOT_SOUGHT is 0, so consider changing it to had_card_or_register."))
      vcqi_log_comment(VCP,2,"Warning",
                       paste0("Global variable TIMELY_HBR_LINE_VARIABLE takes value ",TIMELY_HBR_LINE_VARIABLE,
                              " but RI_RECORDS_NOT_SOUGHT is 0, so consider changing it to had_card_or_register."))
    }

  } #end of exitflag == 0

  if (exitflag == 1) {
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(halt_message = errormsgs)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

