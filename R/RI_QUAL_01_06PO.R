#' Make plots for RI_QUAL_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))


# RI_QUAL_01_06PO R version 1.02 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# 2023-07-23  1.01      Mia Yu          Use level3name for the opplot name
# 2024-05-20	1.02	    Mia Yu      		Added multi lignual globals
#										                    Added call to split_text for title
# *******************************************************************************

RI_QUAL_01_06PO<- function(VCP = "RI_QUAL_01_06PO"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # ********************************
  # Make organ pipe plots

  if (VCQI_MAKE_OP_PLOTS == 1){
    print("Organ pipe plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP")
    dir.create(newpath, showWarnings = FALSE)

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"))

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
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"))

    subtitle <- "RI_QUAL_01: RI Card or Register Availability"

    for (i in 1:opp_nstrata){
      name <- get(paste0("opp_stratum_name_",i))
      id <- get(paste0("opp_stratum_id_",i))

      filestub <- paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_opplot_sawcard_","_",id,"_",name)
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_OP_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      plot <- opplot(
        dat = dat,
        clustvar = as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2],
        yvar = "had_card_or_register",
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


  } #end of opplot

  # ********************************
  # Inchworm or barchart plots

  if (VCQI_MAKE_IW_PLOTS == 1){

    # The number of plots per dose (ppd) depends on whether
    # we are making level2 iwplots; calculate ppd and send
    # the number to the screen to calibrate the user's expectations

    pdd <- 1
    if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){pdd <- pdd*2}

    print(paste0(IWPLOT_TYPE,"s (",pdd," plots per dose)"))

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    if (IWPLOT_TYPE == "Bar chart"){
      type = "brplot"
    } else{
      type = "iw"
    }

    filestub <- paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",type)
    savepng <- paste0(newpath,"/",filestub)

    if (VCQI_SAVE_IW_PLOT_DATA == 1){
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                           " - ",
                           str_to_title(language_string(language_use = language_use, str = "OS_414")),
                           " ",
                           language_string(language_use = language_use, str = "OS_417"))
    title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

    vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,"_card_database.rds"),
                 filename = savepng,
                 datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"),
                 title = title_string,
                 name = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_iwplot"),
                 savedata = savedata) #title = "RI - Card Availability"

    vcqi_log_comment(VCP, 3, "Comment", paste0(IWPLOT_TYPE, " was created and exported."))

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      if (IWPLOT_TYPE == "Bar chart"){
        type = "brplot"
      } else{
        type = "iw"
      }

      filestub <- paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",type,"_double")
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                             " - ",
                             language_string(language_use = language_use, str = "OS_421"))
      title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

      vcqi_to_double_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
                          database2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,"_card_database.rds"),
                          filename = savepng,
                          datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"),
                          datafile2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"),
                          title = title_string,
                          name = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_iwplot_double"),
                          note = split_text(text_string = language_string(language_use = language_use, str = "OS_535"),
                                     text_cutoff = FOOTNOTE_CUTOFF),
                          savedata = savedata)
      #title = "RI - Card and Register Availability"
      #note = "Gray shape is card availability; colored shape is card plus register availability"
    }

  } #end of IW plot

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
