#' Make plots for RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# RI_COVG_02_06PO R version 1.02 - Biostat Global Consulting - 2023-07-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-12  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2023-07-23  1.02      Mia Yu          Use level3name for the opplot name
# *******************************************************************************

RI_COVG_02_06PO <- function(VCP = "RI_COVG_02_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # ********************************
  # Make organ pipe plots

  if (VCQI_MAKE_OP_PLOTS == 1){
    print("Organ pipe plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP")
    dir.create(newpath, showWarnings = FALSE)

    filename <- paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds")
    dat <- vcqi_read(filename)

    dat <- select(dat, c(stratumid,level3name))
    dat <- distinct(dat)
    dat <- dat %>% arrange(stratumid)

    opp_nstrata <- nrow(dat)

    for (i in 1:nrow(dat)){
      if(dat$stratumid[i] < 10){
        eval(parse_expr(paste0("opp_stratum_id_",i,' = "0', dat$stratumid[i],'"')))
      } else{
        eval(parse_expr(paste0("opp_stratum_id_",i,' = "', dat$stratumid[i],'"')))
      }
      eval(parse_expr(paste0("opp_stratum_name_",i,' = "', dat$level3name[i],'"')))
    } #end of i loop
    rm(i)

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds"))

    for (d in seq_along(RI_DOSE_LIST)){
      print(RI_DOSE_LIST[d])

      subtitle <- paste0("RI_COVG_02: Valid Coverage of ", str_to_upper(RI_DOSE_LIST[d]))

      for (i in 1:opp_nstrata){
        name <- get(paste0("opp_stratum_name_",i))
        id <- get(paste0("opp_stratum_id_",i))

        filestub <- paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_opplot_",RI_DOSE_LIST[d],"_",id,"_",name)
        savegph <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_OP_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        plot <- opplot(
          dat = dat,
          clustvar = as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2],
          yvar = paste0("got_valid_",RI_DOSE_LIST[d],"_to_analyze"),
          weightvar = as.character(VCQI_SVYDESIGN_SYNTAX$weights)[2],
          stratvar = as.character(VCQI_SVYDESIGN_SYNTAX$strata)[2],
          stratum = as.numeric(id),
          barcolor1 = "#9ECAE1", # color for respondents with yvar = 1
          barcolor2 = "#f0f0f0",    # color for respondents with yvar = 0
          title = paste0(id,"-",name),
          subtitle = subtitle,
          output_to_screen = FALSE,
          filename = savegph,
          plotn = TRUE,
          savedata = savedata,
          savedatatype = "rds")

        vcqi_log_comment(VCP, 3, "Comment", paste0("Graphic file: ",filestub,".png was created and saved."))

      }#end of i loop

    }#end of dose loop

  } #end of opplot

  # ********************************
  # Inchworm or barchart plots

  if (VCQI_MAKE_IW_PLOTS == 1){

    print(paste0(IWPLOT_TYPE,"s (4 plots per dose)"))

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    for (d in seq_along(RI_DOSE_LIST)){

      if (IWPLOT_TYPE == "Bar chart"){
        type = "brplot"
      } else{
        type = "iw"
      }

      filestub <- paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",type,"_",RI_DOSE_LIST[d],"_valid")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      print(RI_DOSE_LIST[d])
      vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
                   filename = savepng,
                   datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds"),
                   title = paste0("RI - Valid Coverage of ",str_to_upper(RI_DOSE_LIST[d])),
                   name = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_iwplot_",RI_DOSE_LIST[d]),
                   savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0("Valid coverage ", IWPLOT_TYPE, " for ",RI_DOSE_LIST[d] , " was created and exported."))

      minage <- get(paste0(RI_DOSE_LIST[d], "_min_age_days"), envir = .GlobalEnv)
      if (minage < 365){
        filestub <- paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",type,"_",RI_DOSE_LIST[d],"_age1")
        savepng <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_IW_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
                     filename = savepng,
                     datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds"),
                     title = paste0("RI - Valid Coverage by Age 1 of ",str_to_upper(RI_DOSE_LIST[d])),
                     name = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_iwplot_",RI_DOSE_LIST[d],"_age1"),
                     savedata = savedata)

        vcqi_log_comment(VCP, 3, "Comment", paste0("Valid coverage by age 1 ", IWPLOT_TYPE, " for ",RI_DOSE_LIST[d] , " was created and exported."))
      } #end of age1 valid barplot

      ################################################double bar plots################################################
      filestub <- paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",type,"_",RI_DOSE_LIST[d],"_double")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_double_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
                          database2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
                          filename = savepng,
                          datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds"),
                          datafile2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,".rds"),
                          title = paste0("RI - Valid Coverage of ",str_to_upper(RI_DOSE_LIST[d])),
                          name = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_iwplot_",RI_DOSE_LIST[d],"_double"),
                          note = "Gray shape is crude coverage; colored shape is valid coverage",
                          savedata = savedata)

      if (minage < 365){
        filestub <- paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",type,"_",RI_DOSE_LIST[d],"_age1_double")
        savepng <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_IW_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        vcqi_to_double_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
                            database2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
                            filename = savepng,
                            datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_",ANALYSIS_COUNTER,".rds"),
                            datafile2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,".rds"),
                            title = paste0("RI - Valid Coverage by Age 1 of ",str_to_upper(RI_DOSE_LIST[d])),
                            name = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_iwplot_",RI_DOSE_LIST[d],"_age1_double"),
                            note = "Gray shape is crude coverage; colored shape is valid coverage",
                            savedata = savedata)

      } #end of age1 double barplot

    }#end of dose loop
  } #end of IW plot

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

