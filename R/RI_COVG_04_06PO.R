#' Make plots for RI_COVG_04
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr

# RI_COVG_04_06PO R version 1.01 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-14  1.00      Mia Yu          Original R package version
# 2023-07-23  1.01      Mia Yu          Use level3name for the opplot name
# 2024-05-20	1.02	    Mia Yu      		Added multi lignual globals
#										                    Added call to split_text for title
# *******************************************************************************

RI_COVG_04_06PO <- function(VCP = "RI_COVG_04_06PO"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # ********************************
  # Make organ pipe plots

  if (VCQI_MAKE_OP_PLOTS == 1){
    print("Organ pipe plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP")
    dir.create(newpath, showWarnings = FALSE)

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"))

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

    # Now make the plots themselves - one for each stratum
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"))

    # Skip plots for valid doses if no respondent had DOB data
    vlist <- "not_vaccinated_crude"
    if (VCQI_NO_DOBS != 1){
      vlist <- c("not_vaccinated_crude", "not_vaccinated_valid", "not_vaccinated_by_age1")
    }

    for (v in seq_along(vlist)){

      print(vlist[v])

      if (vlist[v] == "not_vaccinated_crude"){
        subtitle <- "RI_COVG_04: Not Vaccinated - Crude Doses"
        abbrev <- "nvc"
      }

      if (vlist[v] == "not_vaccinated_valid"){
        subtitle <- "RI_COVG_04: Not Vaccinated - Valid Doses"
        abbrev <- "nvv"
      }

      if (vlist[v] == "not_vaccinated_by_age1"){
        subtitle <- "RI_COVG_04: Not Vaccinated by Age 1 - Valid Doses"
        abbrev <- "nva1"
      }

      for (i in 1:opp_nstrata){
        name <- get(paste0("opp_stratum_name_",i))
        id <- get(paste0("opp_stratum_id_",i))

        filestub <- paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_opplot_",abbrev,"_",id,"_",name)
        savepng <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_OP_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        plot <- opplot(
          dat = dat,
          clustvar = as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2],
          yvar = vlist[v],
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

    } #end of vlist v loop

  } #end of opplot

  # ********************************
  # Inchworm or barchart plots

  if (VCQI_MAKE_IW_PLOTS == 1){

    # The number of plots per dose (ppd) depends on whether
    # we are making level2 iwplots; calculate ppd and send
    # the number to the screen to calibrate the user's expectations

    pdd <- 1
    if (VCQI_NO_DOBS != 1){pdd <- 4}

    print(paste0(IWPLOT_TYPE,"s (",pdd," plots per dose)"))

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    # Do not show the plots with only crude or only valid coverage; just show
    # the combined "double" plot referenced below
    # TO DO: not sure what is this trying to do

    # not vaccinated - crude

    if (IWPLOT_TYPE == "Bar chart"){
      type = "brplot"
    } else{
      type = "iw"
    }

    filestub <- paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_",type,"_nvc")
    savepng <- paste0(newpath,"/",filestub)

    if (VCQI_SAVE_IW_PLOT_DATA == 1){
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                           " - ",
                           str_to_title(language_string(language_use = language_use, str = "OS_403")),
                           " - ",
                           language_string(language_use = language_use, str = "OS_14"))
    title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

    vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
                 filename = savepng,
                 datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"),
                 title = title_string,
                 name = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_iwplot_nvc"),
                 savedata = savedata) #title = "RI - Not Vaccinated - Crude"

    vcqi_log_comment(VCP, 3, "Comment", paste0("Not vaccinated (crude) ", IWPLOT_TYPE, " was created and exported."))

    # Skip valid dose plots if no respondent had DOB data
    if (VCQI_NO_DOBS != 1){

      # not vaccinated - valid

      filestub <- paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_",type,"_nvv")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                             " - ",
                             str_to_title(language_string(language_use = language_use, str = "OS_403")),
                             " - ",
                             language_string(language_use = language_use, str = "OS_80"))
      title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

      vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
                   filename = savepng,
                   datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"),
                   title = title_string,
                   name = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_iwplot_nvv"),
                   savedata = savedata)
      #title = "RI - Not Vaccinated - Valid"

      vcqi_log_comment(VCP, 3, "Comment", paste0("Not vaccinated (valid) ", IWPLOT_TYPE, " was created and exported."))

      # valid and crude coverage show together

      filestub <- paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_",type,"_nvv_double")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                             " - ",
                             str_to_title(language_string(language_use = language_use, str = "OS_403")),
                             " - ",
                             language_string(language_use = language_use, str = "OS_80"))
      title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

      vcqi_to_double_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
                          database2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
                          filename = savepng,
                          datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"),
                          datafile2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"),
                          title = "RI - Not Vaccinated - Valid",
                          name = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_iwplot_nvv_double"),
                          note = split_text(text_string = language_string(language_use = language_use, str = "OS_534"),
                                            text_cutoff = FOOTNOTE_CUTOFF),
                          savedata = savedata)
      #title = "RI - Not Vaccinated - Valid"
      #note = "Gray shape is crude coverage; colored shape is valid coverage"

      vcqi_log_comment(VCP, 3, "Comment", paste0("Valid & crude coverage ", IWPLOT_TYPE, " was created and exported."))

      # not vaccinated by age 1

      filestub <- paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_",type,"_nva1")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                             " - ",
                             str_to_title(language_string(language_use = language_use, str = "OS_403")),
                             " - ",
                             language_string(language_use = language_use, str = "OS_80"),
                             " ",
                             language_string(language_use = language_use, str = "OS_441"))
      title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

      vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
                   filename = savepng,
                   datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_",ANALYSIS_COUNTER,".rds"),
                   title = title_string,
                   name = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_iwplot_nva1"),
                   savedata = savedata) #title = "RI - Not Vaccinated - Valid by Age 1"

      vcqi_log_comment(VCP, 3, "Comment", paste0("Not vaccinated (by age 1) ", IWPLOT_TYPE, " was created and exported."))
    }


  } #end of IW plot

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
