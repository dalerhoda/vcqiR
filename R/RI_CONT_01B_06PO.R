#' Make plots for RI_CONT_01B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_CONT_01B_06PO R version 1.01 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-24  1.00      Mia Yu          Original R package version
# 2024-05-20	1.01	    Mia Yu      		Added multi lignual globals
#										                    Added call to split_text for title
# *******************************************************************************

RI_CONT_01B_06PO <- function(VCP = "RI_CONT_01B_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # ********************************
  # Make organ pipe plots
  if (VCQI_MAKE_OP_PLOTS == 1){
    print("Organ pipe plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP")
    dir.create(newpath, showWarnings = FALSE)

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_CONT_01B_",ANALYSIS_COUNTER,".rds"))

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
    filename <- paste0(VCQI_OUTPUT_FOLDER, "/RI_CONT_01B_",ANALYSIS_COUNTER,".rds")
    dat <- vcqi_read(filename)

    j = 1
    while (j <= length(RI_CONT_01B_DROPOUT_LIST)){

      d1 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
      j = j+1
      d2 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
      j = j+1

      print(paste0(d1, " to ", d2))

      subtitle <- paste0("RI - Weighted Dropout ", str_to_upper(d1), " to ", str_to_upper(d2))

      for (i in 1:opp_nstrata){
        name <- get(paste0("opp_stratum_name_",i))
        id <- get(paste0("opp_stratum_id_",i))

        filestub <- paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_opplot_",d1,"_",d2,"_",id,"_",name)
        savepng <- paste0(newpath,"/",filestub)

        if (VCQI_SAVE_OP_PLOT_DATA == 1){
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        plot <- opplot(
          dat = dat,
          clustvar = as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2],
          yvar = paste0("wtd_dropout_",d1,"_",d2),
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

    } #end of j loop

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

    j = 1
    while (j <= length(RI_CONT_01B_DROPOUT_LIST)){

      d1 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
      j = j+1
      d2 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
      j = j+1

      print(paste0(d1, " to ", d2))

      if (IWPLOT_TYPE == "Bar chart"){
        type = "brplot"
      } else{
        type = "iw"
      }

      filestub <- paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",type,"_",d1,"_",d2)
      savepng <- paste0(newpath,"/",filestub)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      title_string <- paste0(language_string(language_use = language_use, str = "OS_416"),
                             " - ",
                             language_string(language_use = language_use, str = "OS_469"),
                             " ", str_to_upper(d1), " ",
                             language_string(language_use = language_use, str = "OS_468"),
                             " ",str_to_upper(d2))
      title_string <- split_text(text_string = title_string, text_cutoff = TITLE_CUTOFF)

      vcqi_to_plot(database = paste0(VCQI_OUTPUT_FOLDER, "/RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
                   filename = savepng,
                   datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_CONT_01B_",ANALYSIS_COUNTER,".rds"),
                   title = title_string,
                   name = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_iwplot_",d1,"_",d2),
                   savedata = savedata) #title = paste0("RI - Weighted Dropout ", str_to_upper(d1), " to ", str_to_upper(d2))

      vcqi_log_comment(VCP, 3, "Comment", paste0("Dropout ", IWPLOT_TYPE, " for ",d1, " to ", d2 , " was created and exported."))
    } #end of j loop

  } #end of IW plot

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
