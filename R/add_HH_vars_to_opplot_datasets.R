#' Add HH dataset variables (cluster-related variables) to opplot datasets
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Organ pipe plot datasets in VCQI_OUTPUT_FOLDER/Plots_OP augmented with cluster ID and cluster name variables
#'
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @importFrom utils glob2rx
#' @import haven

# add_HH_vars_to_opplot_datasets R version 1.02 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-12  1.00      Mia Yu          Original R version
# 2022-10-12  1.01      Mia Yu          Package version
# 2022-10-18  1.02      Mia Yu          Add variable labels
# *******************************************************************************

add_HH_vars_to_opplot_datasets <- function(VCP = "add_HH_vars_to_opplot_datasets"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP"))){

    dlist <- list.files(
      path = paste0(VCQI_OUTPUT_FOLDER, "/Plots_OP"),
      pattern = glob2rx("*opplot*.rds"))

    for (d in seq_along(dlist)){

      surveytype <- NULL
      if (str_to_upper(substr(dlist[d],1,2)) %in% "RI"){
        surveytype <- "RI"
      }

      if (str_to_upper(substr(dlist[d],1,2)) %in% "TT"){
        surveytype <- "TT"
      }

      if (str_to_upper(substr(dlist[d],1,3)) %in% "SIA"){
        surveytype <- "SIA"
      }

      # browser()

      # if (!vcqi_object_exists("surveytype")){
      if(is.null(surveytype)){
        errormsgs <- c(errormsgs,
                       "The opplot data file name does not start with RI, TT or SIA so this program does not know where to find the HH info.")
        errormsgs <- c(errormsgs, dlist[d])
        vcqi_log_comment(VCP, 1, "Error",
                         "The opplot data file name does not start with RI, TT or SIA so this program does not know where to find the HH info.")
        exitflag <- 1
      } else {

        dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP/",dlist[d]))

        vartodrop <- c(paste0(surveytype, "01"),
                       paste0(surveytype, "02"),
                       paste0(surveytype, "03"),
                       paste0(surveytype, "04"),
                       "HH01", "HH03", "HH02", "HH04") # added "HH02", "HH04" here 2022-11-03 (CBC)
        vartodrop <- vartodrop[which(vartodrop %in% names(dat))]

        dat <- dat %>% select(-all_of(vartodrop))

        dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/",
                                 surveytype, "_with_ids.rds"))
        dat2 <- dat2 %>%
          select(clusterid, all_of(c(paste0(surveytype,"01"),
                                     paste0(surveytype,"03"))))

        dat <- left_join(dat,dat2, by = "clusterid")

        dat <- unique(dat)

        vartodrop <- c("HH01", "HH03")
        vartodrop <- vartodrop[which(vartodrop %in% names(dat))]
        dat <- dat %>% select(-all_of(vartodrop))

        names(dat)[which(names(dat) == paste0(surveytype,"01"))] <- "HH01"
        names(dat)[which(names(dat) == paste0(surveytype,"03"))] <- "HH03"

        dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET)) %>%
          select(c(HH01,HH02,HH03,HH04))
        dat <- left_join(dat, dat2, by = c("HH01", "HH03")) %>%
          unique() %>%
          relocate(HH02, .after = stratum) %>%
          relocate(c(HH03, HH04), .after = clusterid)

        dat$clusterid <- haven::labelled(dat$clusterid, label = "VCQI cluster ID (may differ from data cluster ID)") %>% suppressWarnings()
        dat$HH03 <- haven::labelled(dat$HH03, label = "Cluster ID from dataset (may differ from VCQI cluster ID)") %>% suppressWarnings()
        dat$HH04 <- haven::labelled(dat$HH04, label = "Cluster name from dataset") %>% suppressWarnings()

        # Add metadata
        dat <- dat %>% mutate(dataset_filename = dlist[d])
        ncols <- max(str_count(dat$dataset_filename, "_"), na.rm = TRUE) + 1
        dat <- dat %>%
          separate(dataset_filename,
                   into = paste0("snip", 1:ncols),
                   sep = "_") %>%
          mutate(vcqi_indicator = paste0(str_to_upper(snip1), "_",
                                         str_to_upper(snip2), "_",
                                         snip3)) %>%
          mutate(vcqi_analysis_counter = snip4) %>%
          mutate(dataset_filename = dlist[d]) %>%
          relocate(dataset_filename, .before = snip1)

        dat$vcqi_analysis_counter <- as.numeric(dat$vcqi_analysis_counter) %>% suppressWarnings()

        dat$dataset_filename <- haven::labelled(dat$dataset_filename, label = "Name of original dataset with OP plot data") %>% suppressWarnings()
        dat$vcqi_indicator <- haven::labelled(dat$vcqi_indicator, label = "VCQI indicator") %>% suppressWarnings()
        dat$vcqi_analysis_counter <- haven::labelled(dat$vcqi_analysis_counter, label = "VCQI analysis counter when OP plot was made") %>% suppressWarnings()

        dat <- dat %>%
          mutate(opplot_filename = paste0(
            str_to_upper(snip1), "_",
            str_to_upper(snip2),"_",
            snip3, "_", snip4, "_",
            snip5, "_", snip6, "_",
            snip7, "_",
            str_to_title(substr(snip8, 1, nchar(snip8) - 4)),
            substring(snip8, nchar(snip8) - 3)))

        dat <- dat %>% select(-c(starts_with("snip"))) %>% arrange(barorder)

        dat$opplot_filename <- haven::labelled(dat$opplot_filename, label = "VCQI OP plot filename") %>% suppressWarnings()

        file.remove(paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP/",dlist[d]))
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/Plots_OP/",dlist[d]))

      } # end of checking surveytype

    } # end of dlist loop

  }

  # Note that this program is called from within vcqi_halt_immediately so there's no need to call that program here
  if (exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
