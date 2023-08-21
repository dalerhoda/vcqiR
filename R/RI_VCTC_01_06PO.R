#' Make plots for RI_VCTC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import survey
#' @import tidyselect

# RI_VCTC_01_06PO R version 1.05 - Biostat Global Consulting - 2023-07-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-08  1.00      Mia Yu          Original R version
# 2022-11-11  1.01      Mia Yu          Package version
# 2022-12-22  1.02      Mia Yu          Add parts for previously skipped objects
# 2023-07-21  1.03      Mia Yu          Put abbreviations on two lines
# 2023-07-24  1.04      Mia Yu          Change aes_string to aes
# 2023-07-25  1.05      Caitlin Clary   Finalize removal of deprecated aes_string,
#                                       left align caption
# *******************************************************************************

RI_VCTC_01_06PO <- function(VCP = "RI_VCTC_01_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_VCTC")
    dir.create(newpath, showWarnings = FALSE)

    for (lvl in seq_along(RI_VCTC_01_LEVELS)){

      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))

      idvar <- get(paste0("level",RI_VCTC_01_LEVELS[lvl],"id"), dat)
      llist <- unique(idvar)

      for (l in seq_along(llist)){

        dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))

        f <- paste0("subdat <- subset(dat,level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l],")")
        eval(rlang::parse_expr(f))
        namevar <- get(paste0("level",RI_VCTC_01_LEVELS[lvl],"name"), subdat)
        stratumname <- namevar[1]

        # Populate a local macro named `textit' with the bars of text that
        # should appear across the top of the chart
        #
        # (The user can request seeing the estimated coverage, sample size,
        # number of respondents with HBRs, effective sample size, design effect
        # or intracluster correlation coefficient.)
        #
        # The order in which these appear is controlled by the TIMELY_TEXTBAR_X
        # coordinates, which should be set by the user in the control program.

        textdat <- data.frame(dose = str_to_lower(TIMELY_DOSE_ORDER))
        for (e in seq_along(TIMELY_TEXTBAR_ORDER)){

          currenttext <- as.data.frame(matrix(nrow = length(TIMELY_DOSE_ORDER), ncol = 2))
          names(currenttext) <- c("dose", TIMELY_TEXTBAR_ORDER[e])
          currenttext$dose <- str_to_lower(TIMELY_DOSE_ORDER)

          doselist <- str_to_lower(TIMELY_DOSE_ORDER)
          for (d in seq_along(doselist)){

            xlocation <- get(paste0("TIMELY_TEXTBAR_X_",TIMELY_TEXTBAR_ORDER[e]), envir = .GlobalEnv)
            if (xlocation + 5 > TIMELY_XSCALE_MAX){
              assign("TIMELY_XSCALE_MAX", xlocation + 5, envir = .GlobalEnv)
            }
            rm(xlocation)

            VCQI_SVYDESIGN_SYNTAX <- get("VCQI_SVYDESIGN_SYNTAX", envir = .GlobalEnv)

            if (substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
              datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                     weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
            } else {
              clusterid <- get(substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

              if (length(unique(clusterid)) == 1){
                datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                       weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
              } else {
                datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                                       strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                       weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
              }
            }

            if (TIMELY_TEXTBAR_ORDER[e] == "COVG"){
              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = paste0("got_crude_", doselist[d], "_to_analyze"),
                subset_condition = paste0("level", RI_VCTC_01_LEVELS[lvl], "id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              currenttext[d,2] <- sprintf(
                paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS, "f"),
                ptest$estimate*100)
            } # end of COVG

            if (TIMELY_TEXTBAR_ORDER[e] == "CI"){
              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = paste0("got_crude_", doselist[d], "_to_analyze"),
                subset_condition = paste0("level", RI_VCTC_01_LEVELS[lvl], "id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              ciultemp <- sprintf(paste0("%.", 1, "f"), ptest$ciul)
              if (ciultemp == "100.0"){
                ci <- paste0(
                  "(",
                  sprintf(
                    paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS, "f"),
                    ptest$cill*100),
                  ", 100)")
              } else {
                ci <- paste0(
                  "(",
                  sprintf(
                    paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS, "f"),
                    ptest$cill*100),
                  ", ",
                  sprintf(
                    paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS ,"f"),
                    ptest$ciul*100),
                  ")")
              }

              currenttext[d,2] <- ci

            }

            if (TIMELY_TEXTBAR_ORDER[e] == "N"){
              varn <- get(paste0("got_crude_", doselist[d], "_to_analyze"), subdat)
              nvalue <- length(which(varn %in% c(0,1)))

              currenttext[d,2] <- prettyNum(round(nvalue), big.mark = ",")
            } #end of N

            if (TIMELY_TEXTBAR_ORDER[e] == "NEFF"){
              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = paste0("got_crude_",doselist[d],"_to_analyze"),
                subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              currenttext[d,2] <- prettyNum(round(ptest$neff), big.mark = ",")

            } #end of NEFF

            if (TIMELY_TEXTBAR_ORDER[e] == "DEFF"){
              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = paste0("got_crude_",doselist[d],"_to_analyze"),
                subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              currenttext[d,2] <- sprintf(paste0("%.",1,"f"), ptest$deff)
            } #end of DEFF

            if (TIMELY_TEXTBAR_ORDER[e] == "ICC"){
              f <- paste0(
                "tempdatdesign <- subset(datdesign,(level", RI_VCTC_01_LEVELS[lvl], "id == ", llist[l], " )%in% TRUE)")
              eval(rlang::parse_expr(f))

              testresult <- calc_icc(
                y = paste0("got_crude_", doselist[d], "_to_analyze"),
                svydata = tempdatdesign, ci.type = "smith",
                showmessages = FALSE)

              icctemp <- testresult$estimates$ICC

              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = paste0("got_crude_",doselist[d],"_to_analyze"),
                subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              if (ptest$estimate == 1){
                icctemp <- 0
              }

              if (is.nan(icctemp)){
                icctemp <- NA
              }

              ptest$icc <- icctemp

              currenttext[d,2] <- sprintf(paste0("%.",TIMELY_TEXTBAR_ICC_DEC_DIGITS,"f"), ptest$icc)
            } #end of ICC

          } #end of doselist d loop

          textdat <- full_join(textdat, currenttext, by = "dose")

        } #end of TIMELY_TEXTBAR_ORDER e loop

        # Estimate the % of respondents with HBRs if the user has asked for it
        if (TIMELY_HBR_LINE_PLOT == 1){
          ptest <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = TIMELY_HBR_LINE_VARIABLE,
            subset_condition = paste0("level", RI_VCTC_01_LEVELS[lvl], "id == ", llist[l]),
            ci_level = 95,
            ci_method = VCQI_CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          x_hbr <- ptest$estimate*100
          hbrlabelnum <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS, "f"),
                                 ptest$estimate*100)
          showlabel1 <- paste0(TIMELY_HBR_LINE_LABEL, " (", hbrlabelnum, "%)")
          hbrlabelx <- x_hbr + nchar(showlabel1)/2
        }

        # Estimate the % of respondents fully vaccinated if the user has asked for it
        if (TIMELY_FULLY_VXD_NOTE == 1){
          ptest <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = TIMELY_FULLY_VXD_NOTE_VARIABLE,
            subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
            ci_level = 95,
            ci_method = VCQI_CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          fvptext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$estimate*100)
          fvlltext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$cill*100)
          fvultext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$ciul*100)

          fvnotetext1 <- paste0("Fully vaccinated: ", fvptext, "% (95% CI: ", fvlltext, " - ", fvultext, "%).")

          if (TIMELY_FULLY_VXD_NOTE_SUPPRESS_CI == 1){
            fvnotetext1 <- paste0("Fully vaccinated: ", fvptext, "%.")
          }
          fvnotetext2 <- TIMELY_FULLY_VXD_DOSELIST_TEXT
        }

        # Estimate the % of respondents not vaccinated if the user has asked for it
        # NOTE: skipped TIMELY_NOT_VXD_NOTE for now
        if (TIMELY_NOT_VXD_NOTE == 1){
          ptest <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = TIMELY_NOT_VXD_NOTE_VARIABLE,
            subset_condition = paste0("level", RI_VCTC_01_LEVELS[lvl], "id == ", llist[l]),
            ci_level = 95,
            ci_method = VCQI_CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          nvptext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$estimate*100)
          nvlltext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$cill*100)
          nvultext <- sprintf(paste0("%.", TIMELY_TEXTBAR_COVG_DEC_DIGITS,"f"), ptest$ciul*100)

          nvnotetext1 <- paste0("Not vaccinated: ", nvptext, "% (95% CI: ", nvlltext, " - ", nvultext, "%).")

          if (TIMELY_NOT_VXD_NOTE_SUPPRESS_CI == 1){
            nvnotetext1 <- paste0("Not vaccinated: ", nvptext, "%.")
          }
          nvnotetext2 <- TIMELY_NOT_VXD_DOSELIST_TEXT
        }

        plotdat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_tplot_",RI_VCTC_01_LEVELS[lvl],"_",llist[l],".rds"))
        breakpoint = plotdat$y + 0.5*TIMELY_BARWIDTH
        ylabel = str_to_upper(plotdat$dose)

        baseplot <- ggplot(data = plotdat) +
          scale_x_continuous(
            limits = c(0, TIMELY_XSCALE_MAX),
            expand = c(0, 0),
            breaks = seq(0, 100 ,by = 25)) +
          scale_y_continuous(
            limits = c(0, max(TIMELY_Y_COORDS) + 2*TIMELY_BARWIDTH),
            breaks = breakpoint,
            labels = ylabel,
            expand = c(0, 0))

        if (TIMELY_HBR_LINE_PLOT == 1){

          baseplot <- baseplot +
            geom_vline(
              aes(xintercept = x_hbr),
              colour = TIMELY_HBR_LINE_COLOR,
              linetype = TIMELY_HBR_LINE_PATTERN,
              linewidth = TIMELY_HBR_LINE_WIDTH) +
            geom_text(
              mapping = aes(
                x = hbrlabelx,
                y = 5,
                label = showlabel1),
              colour = TIMELY_HBR_LINE_LABEL_COLOR)
        }

        TIMELY_PLOT_NOTE <- NULL

        if (vcqi_object_exists("TIMELY_TEXTBAR_ORDER")){
          captiontext <- NULL
          ecount <- 1

          for (e in seq_along(TIMELY_TEXTBAR_ORDER)){
            if (ecount > 1 & length(captiontext) > 0){
              captiontext <- paste0(captiontext,"; ")
            }
            if (vcqi_object_exists(paste0("TIMELY_TEXTBAR_ABBREV_", TIMELY_TEXTBAR_ORDER[e]))){
              abbre <- get(paste0("TIMELY_TEXTBAR_ABBREV_", TIMELY_TEXTBAR_ORDER[e]), envir = .GlobalEnv)
              captiontext <- paste0(captiontext, abbre)
            }
            #e = e+1
            ecount <- ecount + 1
          }
        }

        if (vcqi_object_exists("TIMELY_TEXTBAR_ORDER") & TIMELY_HBR_LINE_PLOT == 1){
          TIMELY_PLOT_NOTE <- paste0(TIMELY_ABBREV_CAPTION_LINE1,"\n",captiontext)
        } else if (vcqi_object_exists("TIMELY_TEXTBAR_ORDER") & TIMELY_HBR_LINE_PLOT != 1){
          TIMELY_PLOT_NOTE <- captiontext
        } else if (!vcqi_object_exists("TIMELY_TEXTBAR_ORDER") & TIMELY_HBR_LINE_PLOT == 1){
          TIMELY_PLOT_NOTE <- TIMELY_ABBREV_CAPTION_LINE1
        }

        if (TIMELY_NOT_VXD_NOTE == 1){
          TIMELY_PLOT_NOTE <- paste0(nvnotetext1, " ", nvnotetext2, "\n", TIMELY_PLOT_NOTE)
        }

        if (TIMELY_FULLY_VXD_NOTE == 1){
          TIMELY_PLOT_NOTE <- paste0(fvnotetext1, " ", fvnotetext2, "\n", TIMELY_PLOT_NOTE)
        }

        doselist <- str_to_lower(TIMELY_DOSE_ORDER)

        for (d in seq_along(doselist)){

          rownum <- which(plotdat$dose == doselist[d])

          if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){

            # Default settings
            tiledf <- data.frame(
              xmn = 0,
              xmx = plotdat[rownum, 1],
              ymn = plotdat$y[rownum],
              ymx = plotdat$y[rownum] + TIMELY_BARWIDTH,
              ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5,
              bclr = get(paste0("TIMELY_DT_COLOR_", 1), envir = .GlobalEnv)
            )

            linecolor <- get(paste0("TIMELY_DT_LCOLOR_", 1), envir = .GlobalEnv)

            baseplot <- baseplot +
              geom_rect(
                data = tiledf,
                aes(
                  xmin = .data$xmn,
                  xmax = .data$xmx,
                  ymin = .data$ymn,
                  ymax = .data$ymx,
                  fill = .data$bclr
                ),
                color = linecolor
              )

            numtile <- as.numeric(TIMELY_N_DTS)
            for (i in 2:numtile){

              tiledf <- data.frame(
                xmn = plotdat[rownum, i-1],
                xmx = plotdat[rownum, i],
                ymn = plotdat$y[rownum],
                ymx = plotdat$y[rownum] + TIMELY_BARWIDTH,
                ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5,
                bclr = get(paste0("TIMELY_DT_COLOR_", i), envir = .GlobalEnv)
              )

              linecolor = get(paste0("TIMELY_DT_LCOLOR_",i), envir = .GlobalEnv)

              baseplot <- baseplot +
                geom_rect(
                  data = tiledf,
                  aes(
                    xmin = .data$xmn,
                    xmax = .data$xmx,
                    ymin = .data$ymn,
                    ymax = .data$ymx,
                    fill = .data$bclr
                  ),
                  color = linecolor
                )

            } # end of numtile loop

            ebdf <- data.frame(
              cilb = plotdat[rownum, ncol(plotdat)-3],
              ciub = plotdat[rownum, ncol(plotdat)-2],
              ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5
            )

            baseplot <- baseplot +
              geom_errorbar(
                data = ebdf,
                aes(
                  y = .data$ymd,
                  xmin = .data$cilb,
                  xmax = .data$ciub
                ),
                color = TIMELY_CI_LCOLOR, width = TIMELY_CI_LWIDTH
              )

          } else {

            tiledf <- data.frame(
              xmn = 0,
              xmx = plotdat[rownum, 1],
              ymn = plotdat$y[rownum],
              ymx = plotdat$y[rownum] + TIMELY_BARWIDTH,
              ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5,
              bclr = get(paste0("TIMELY_CD_",TIMELY_DOSE_ORDER[d],"_COLOR_",1), envir = .GlobalEnv)
            )

            linecolor = get(paste0("TIMELY_CD_",TIMELY_DOSE_ORDER[d],"_LCOLOR_",1), envir = .GlobalEnv)

            baseplot <- baseplot +
              geom_rect(
                data = tiledf,
                aes(
                  xmin = .data$xmn,
                  xmax = .data$xmx,
                  ymin = .data$ymn,
                  ymax = .data$ymx,
                  fill = .data$bclr
                ),
                color = linecolor
              )

            usertile <- get(paste0("TIMELY_CD_", str_to_upper(doselist[d]), "_NTILES"),
                            envir = .GlobalEnv)

            for (i in 2:usertile){

              tiledf <- data.frame(
                xmn = plotdat[rownum, i-1],
                xmx = plotdat[rownum, i],
                ymn = plotdat$y[rownum],
                ymx = plotdat$y[rownum] + TIMELY_BARWIDTH,
                ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5,
                bclr = get(
                  paste0("TIMELY_CD_", TIMELY_DOSE_ORDER[d], "_COLOR_", i),
                  envir = .GlobalEnv)
              )

              linecolor = get(paste0("TIMELY_CD_",TIMELY_DOSE_ORDER[d],
                                     "_LCOLOR_",i), envir = .GlobalEnv)

              baseplot <- baseplot +
                geom_rect(
                  data = tiledf,
                  aes(
                    xmin = .data$xmn,
                    xmax = .data$xmx,
                    ymin = .data$ymn,
                    ymax = .data$ymx,
                    fill = .data$bclr
                  ),
                  color = linecolor
                )

            } #end of usertile loop

            ebdf <- data.frame(
              cilb = plotdat[rownum, ncol(plotdat)-3],
              ciub = plotdat[rownum, ncol(plotdat)-2],
              ymd = plotdat$y[rownum] + TIMELY_BARWIDTH*0.5
            )

            baseplot <- baseplot +
              geom_errorbar(
                data = ebdf,
                aes(
                  y = .data$ymd,
                  xmin = .data$cilb,
                  xmax = .data$ciub
                ),
                color = TIMELY_CI_LCOLOR, width = TIMELY_CI_LWIDTH
              )
          }

        } # end of dose loop

        plottextdat <- full_join(plotdat,textdat, by = "dose")

        for (e in seq_along(TIMELY_TEXTBAR_ORDER)){
          tempdat <- plottextdat %>%
            select(dose, y, all_of(TIMELY_TEXTBAR_ORDER[e])) %>%
            mutate(
              xlocation = get(paste0("TIMELY_TEXTBAR_X_",TIMELY_TEXTBAR_ORDER[e]), envir = .GlobalEnv),
              textlabel = get(paste0("TIMELY_TEXTBAR_LABEL_",TIMELY_TEXTBAR_ORDER[e]), envir = .GlobalEnv),
              labellocation = max(TIMELY_Y_COORDS) + 1.5*TIMELY_TEXTBAR_LABEL_Y_SPACE)

          names(tempdat) <- c("dose", "ylocation", "text","xlocation","textlabel","labellocation")

          textcolor <- get(paste0("TIMELY_TEXTBAR_COLOR_", TIMELY_TEXTBAR_ORDER[e]),
                           envir = .GlobalEnv)

          baseplot <- baseplot +
            geom_text(
              data = tempdat,
              mapping = aes(
                x = xlocation,
                y = ylocation+TIMELY_TEXTBAR_LABEL_Y_SPACE*0.5,
                label = text),
              colour = textcolor) +
            geom_text(data = tempdat,
                      mapping = aes(
                        x = xlocation,
                        y = labellocation,
                        label = textlabel))
        } # end of TIMELY_TEXTBAR_ORDER loop

        lcolor <- NULL
        llabel <- NULL
        for (lo in seq_along(TIMELY_LEGEND_ORDER)){

          if (grepl("DT_", TIMELY_LEGEND_ORDER[lo], fixed = TRUE) %in% TRUE){
            num <- substr(TIMELY_LEGEND_ORDER[lo], nchar(TIMELY_LEGEND_ORDER[lo]),
                          nchar(TIMELY_LEGEND_ORDER[lo]))
            tempcolor <- get(paste0("TIMELY_DT_COLOR_", num), envir = .GlobalEnv)
            templegend <- get(paste0("TIMELY_DT_LEGEND_LABEL_", num), envir = .GlobalEnv)

            lcolor <- c(lcolor, tempcolor)
            llabel <- c(llabel, templegend)

          } else if (grepl("CD_", TIMELY_LEGEND_ORDER[lo],fixed = TRUE) %in% TRUE){
            num <- substr(TIMELY_LEGEND_ORDER[lo],nchar(TIMELY_LEGEND_ORDER[lo]),nchar(TIMELY_LEGEND_ORDER[lo]))
            dose <- gsub(".*[_]([^.]+)[_].*", "\\1", TIMELY_LEGEND_ORDER[lo])

            tempcolor <- get(paste0("TIMELY_CD_",dose,"_COLOR_",num), envir = .GlobalEnv)
            templegend <- get(paste0("TIMELY_CD_",dose,"_LEGEND_LABEL_",num), envir = .GlobalEnv)

            lcolor <- c(lcolor, tempcolor)
            llabel <- c(llabel, templegend)
          }

        } # end of TIMELY_LEGEND_ORDER lo loop

        baseplot <- baseplot +
          theme_bw()+
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = TIMELY_XLABEL_SIZE,
                                       colour = TIMELY_XLABEL_COLOR),
            axis.text.y = element_text(size = TIMELY_YLABEL_SIZE,
                                       colour = TIMELY_YLABEL_COLOR),
            legend.position = "bottom",
            plot.caption = element_text(hjust = 0)
          ) +
          labs(x = "Estimated Coverage (%)", y = NULL,
               caption = TIMELY_PLOT_NOTE,
               title = paste0("Vaccination Coverage & Timeliness: ", stratumname))+
          scale_fill_identity("", guide = "legend", breaks=lcolor,labels = llabel)

        ggsave(plot = baseplot,
               filename = paste0(newpath,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_level_",RI_VCTC_01_LEVELS[lvl],"_id_",llist[l],"_",stratumname,".png"),
               width = TIMELY_PLOT_WIDTH, height = TIMELY_PLOT_HEIGHT, units = "in")

      }# end of llist l loop


    } # end of RI_VCTC_01_LEVELS lvl loop

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

