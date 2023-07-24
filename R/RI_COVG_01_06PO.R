#' Make plots for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# RI_COVG_01_06PO R version 1.02 - Biostat Global Consulting - 2023-07-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-05  1.00      Mia Yu          Original R version
# 2022-10-09  1.01      Mia Yu          Package version
# 2023-07-23  1.02      Mia Yu          Use level3name for the opplot name
# *******************************************************************************

RI_COVG_01_06PO <- function(VCP = "RI_COVG_01_06PO"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # ********************************
  # Make organ pipe plots
  if (VCQI_MAKE_OP_PLOTS == 1){
    print("Organ pipe plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP")
    dir.create(newpath, showWarnings = FALSE)

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))

    dat <- select(dat, c(stratumid,level3name))
    dat <- distinct(dat)
    dat <- dat %>% arrange(stratumid)

    opp_nstrata <- nrow(dat)

    for (i in 1:nrow(dat)){
      if(dat$stratumid[i] < 10){
        f <- paste0("opp_stratum_id_",i,' = "0', dat$stratumid[i],'"')
        eval(parse_expr(f))
      } else{
        f <- paste0("opp_stratum_id_",i,' = "', dat$stratumid[i],'"')
        eval(parse_expr(f))
      }

      f <- paste0("opp_stratum_name_",i,' = "', dat$level3name[i],'"')
      eval(parse_expr(f))

    } #end of i loop

    filename <- paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,".rds")
    dat <- vcqi_read(filename)

    for (d in seq_along(RI_DOSE_LIST)){
      print(RI_DOSE_LIST[d])

      subtitle <- paste0("RI_COVG_01: Crude Coverage of ", str_to_upper(RI_DOSE_LIST[d]))

      for (i in 1:opp_nstrata){
        name <- get(paste0("opp_stratum_name_",i))
        id <- get(paste0("opp_stratum_id_",i))

        filestub <- paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_opplot_",RI_DOSE_LIST[d],"_",id,"_",name)
        savepng <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_OP_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        plot <- opplot(
          dat = dat,
          clustvar = as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2],
          yvar = paste0("got_crude_",RI_DOSE_LIST[d],"_to_analyze"),
          weightvar = as.character(VCQI_SVYDESIGN_SYNTAX$weights)[2],
          stratvar = as.character(VCQI_SVYDESIGN_SYNTAX$strata)[2],
          stratum = as.numeric(id),
          barcolor1 = "#9ECAE1", # color for respondents with yvar = 1
          barcolor2 = "#f0f0f0",    # color for respondents with yvar = 0
          title = paste0(id,"-",name),
          subtitle = subtitle,
          output_to_screen = FALSE,
          filename = savepng,
          plotn = TRUE,
          savedata = savedata,
          savedatatype = "rds")

        vcqi_log_comment(VCP, 3, "Comment", paste0("Graphic file: ",filestub,".png was created and saved."))

      }#end of i loop

    } #end of dose loop

  } #end of opplot


  # ********************************
  # Inchworm or barchart plots

  if (VCQI_MAKE_IW_PLOTS == 1){

    # The number of plots per dose (ppd) depends on whether
    # we are making level2 iwplots; calculate ppd and send
    # the number to the screen to calibrate the user's expectations

    print(paste0(IWPLOT_TYPE,"s (1 plots per dose)"))

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    for (d in seq_along(RI_DOSE_LIST)){

      if (IWPLOT_TYPE == "Bar chart"){
        type = "brplot"
      } else{
        type = "iw"
      }

      filestub <- paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",type,"_",RI_DOSE_LIST[d])
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      print(RI_DOSE_LIST[d])
      vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
                   filename = savepng,
                   datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_",ANALYSIS_COUNTER,".rds"),
                   title = paste0("RI - Crude Coverage of ",str_to_upper(RI_DOSE_LIST[d])),
                   name = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_iwplot_",RI_DOSE_LIST[d]),
                   savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0("Crude coverage ", IWPLOT_TYPE, " for ",RI_DOSE_LIST[d] , " was created and exported."))

    } #end of dose loop
  } #end of IW plot

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

