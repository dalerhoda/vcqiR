#' Take all the VCQI databases that can be aggregated and create one single database
#'
#' @return A dataset consolidating VCQI databases, saved as vcqi_aggregated_databases.rds in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @importFrom utils glob2rx
#' @import haven

# aggregate_vcqi_databases R version 1.04 - Biostat Global Consulting - 2023-01-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-12  1.00      Mia Yu          Original R version
# 2022-10-07  1.01      Mia Yu          Adapt with QUAL_07B-QUAL_09
# 2022-10-14  1.02      Mia Yu          Package version
# 2022-10-21  1.03      Mia Yu          Add variable labels
# 2023-01-18  1.04      Mia Yu          Drop empty columns
# *******************************************************************************

aggregate_vcqi_databases <- function(){
  vcqi_log_comment(VCP, 3, "Comment",
                   "User has specified that VCQI databases should all be kept. Consolidating into one database and erasing all individual databases.")

  # Erase the datasets that will be created by this program if they already exist
  filesuf <- c("unweighted", "weighted", "other", "all")

  for (f in seq_along(filesuf)){
    filename <- paste0(VCQI_OUTPUT_FOLDER, "/VCQI_aggregated_databases_",
                       filesuf[f], ".rds")
    if (file.exists(filename)){
      file.remove(filename)
    }
  }

  # Grab the list of databases from the output folder
  filelist <- list.files(path = VCQI_OUTPUT_FOLDER,pattern = glob2rx("*database.rds"))
  vcqi_databases <- gsub(".rds", '"', filelist)
  vcqi_databases <- gsub(".rds", "", filelist)

  # Create two globals...one for the aggregated databases and one for all others
  VCQI_AGGREGATED_DATABASES <- NULL
  VCQI_NON_AGGREGATED_DATABASES <- NULL

  for (d in seq_along(vcqi_databases)){
    if ((grepl("RI_COVG_05", vcqi_databases[d]) | grepl("SIA_COVG_05", vcqi_databases[d]) |
         grepl("DESC", vcqi_databases[d]) | grepl("COVG_DIFF", vcqi_databases[d]) |
         grepl("table_order", vcqi_databases[d])) %in% TRUE){
      VCQI_NON_AGGREGATED_DATABASES <- c(VCQI_NON_AGGREGATED_DATABASES, vcqi_databases[d])
    } else {
      VCQI_AGGREGATED_DATABASES <- c(VCQI_AGGREGATED_DATABASES, vcqi_databases[d])
    }
  }

  vcqi_global(VCQI_AGGREGATED_DATABASES, VCQI_AGGREGATED_DATABASES)
  vcqi_global(VCQI_NON_AGGREGATED_DATABASES, VCQI_NON_AGGREGATED_DATABASES)

  # Reset the VCQI_DATABASES global to be the AGGREGATED_DATABASES so the
  # NON_AGGREGATED_DATABASES are not deleted
  vcqi_global(VCQI_DATABASES,VCQI_AGGREGATED_DATABASES)

  if (length(VCQI_AGGREGATED_DATABASES) > 0){
    # Set locals
    first_weighted <- 0
    first_unweighted <- 0
    first_other <- 0
    first_desc <- 0
    number <- 1
    for (f in seq_along(VCQI_AGGREGATED_DATABASES)){

      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/",
                              VCQI_AGGREGATED_DATABASES[f], ".rds"))
      dat <- dat %>%
        arrange(level4id) %>%
        mutate(db_rownum = row_number(),
               db_id = number,
               db_name = VCQI_AGGREGATED_DATABASES[f])

      # Create a local with these new vars to pass through to determine if weighted/unweighted/other
      strcount <- 10
      if(grepl("RI_ACC_", VCQI_AGGREGATED_DATABASES[f])) {strcount = strcount - 1}
      if ((substr(VCQI_AGGREGATED_DATABASES[f],1,11) == "RI_QUAL_07B" | (substr(VCQI_AGGREGATED_DATABASES[f], 1, 3) == "SIA")) %in% TRUE){
        strcount <- 11
      }

      indicator <- substr(VCQI_AGGREGATED_DATABASES[f], 1, strcount)
      analysis_counter <- substr(VCQI_AGGREGATED_DATABASES[f], strcount + 2, strcount + 2)
      name1 <- gsub("_database", "", VCQI_AGGREGATED_DATABASES[f])
      name2 <- substring(name1, strcount + 4)

      # Determine if weighted or unweighted dataset
      weighted <- "other"

      if (all(c("stderr","cilevel", "cill", "ciul", "lcb", "ucb", "deff", "icc", "nwtd", "nclusters", "nwtd_est") %in% names(dat))){
        weighted <- "weighted"
      } else if (all(c("outcome", "estimate", "n") %in% names(dat))){
        weighted <- "unweighted"
      }

      if (indicator == "RI_QUAL_09"){weighted <- "unweighted"}
      temp <- get(paste0("first_", weighted))
      temp <- temp + 1
      assign(paste0("first_", weighted), temp)

      # Create local with values that will be used for indicator variables
      # Note: in Stata VCQI this part is value label
      weighted_value <- "Weighted"
      if (weighted == "unweighted"){weighted_value <- "Unweighted"}
      if (weighted == "other"){weighted_value <- "Other"}

      # Create variables to identify which database the data is from
      dat <- dat %>% mutate(vcqi_db_indicator = indicator,
                            vcqi_db_indicator_type = weighted_value)

      #label define weighted_value 1 "Weighted" 2 "Unweighted" 3 "Other", replace
      #label value vcqi_db_indicator_type weighted_value

      # Create variable with the label
      if ("estimate" %in% names(dat)){
        var <- get("estimate",dat)
        dat <- mutate(dat, vcqi_db_label = attr(var, "label"))

      } else {
        vcqi_db_label = VCQI_AGGREGATED_DATABASES[f]
      }

      dat <- dat %>% mutate(vcqi_db_analysis_counter = analysis_counter,
                            vcqi_db_additional_file_info = name2)

      dat <- dat %>% relocate(starts_with("vcqi_db_"))

      vcqi_log_comment(VCP, 3, "Comment", paste0("Appending ",VCQI_AGGREGATED_DATABASES[f],".rds to VCQI_aggregated_databases_",
                                                 weighted,".rds"))

      if (number == 1){
        VCQI_aggregated_databases_all <- data.frame(vcqi_db_indicator = NA,vcqi_db_indicator_type = NA,vcqi_db_label = NA,vcqi_db_analysis_counter = NA,vcqi_db_additional_file_info = NA,
                                                    level = NA,name = NA,level4id = NA,outcome = NA,db_rownum = NA,db_id= NA,db_name =NA,
                                                    estimate = NA,n = NA,dose = NA,n_eligible = NA,n_mov = NA,n_uncor_mov = NA,n_cor_mov = NA,
                                                    stderr = NA,cilevel = NA,cill = NA,ciul = NA,lcb = NA, ucb = NA,deff = NA,icc = NA,
                                                    nwtd = NA,nclusters = NA,nwtd_est = NA)
        VCQI_aggregated_databases_all <- bind_rows(VCQI_aggregated_databases_all,dat)
        VCQI_aggregated_databases_all <- VCQI_aggregated_databases_all[-1,]
        saveRDS(VCQI_aggregated_databases_all, file = paste0(VCQI_OUTPUT_FOLDER,"/VCQI_aggregated_databases_all.rds"))
      } else{
        VCQI_aggregated_databases_all <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/VCQI_aggregated_databases_all.rds"))
        VCQI_aggregated_databases_all <- bind_rows(VCQI_aggregated_databases_all,dat)
        saveRDS(VCQI_aggregated_databases_all, file = paste0(VCQI_OUTPUT_FOLDER,"/VCQI_aggregated_databases_all.rds"))
      }

      number <- number + 1

    } #end of database loop

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/VCQI_aggregated_databases_all.rds"))
    dat <- dat %>% arrange(db_id, db_rownum)

    colname <- c("estimate","n","dose","n_eligible","n_mov","n_uncor_mov","n_cor_mov",
                 "stderr","cilevel","cill","ciul","lcb", "ucb","deff","icc","nwtd","nclusters","nwtd_est")
    dropcol <- NULL

    for (i in seq_along(colname)){

      var <- get(colname[i], dat)

      if (all(is.na(var)) %in% TRUE){
        dropcol <- c(dropcol, colname[i])
      }

    } #end of colname i loop

    dat <- dat %>% select(-c(all_of(dropcol)))

    dat$db_rownum <- haven::labelled(dat$db_rownum, label = "Row number in original database") %>% suppressWarnings()
    dat$db_id <- haven::labelled(dat$db_id, label = "Unique ID for this database") %>% suppressWarnings()
    dat$db_name <- haven::labelled(dat$db_name, label = "Name of original database") %>% suppressWarnings()
    dat$vcqi_db_indicator <- haven::labelled(dat$vcqi_db_indicator,
                                             label = "Indicator name from VCQI DATABASE file name") %>% suppressWarnings()
    dat$vcqi_db_indicator_type <- haven::labelled(dat$vcqi_db_indicator_type, label = "Type of dataset") %>% suppressWarnings()
    dat$vcqi_db_label <- haven::labelled(dat$vcqi_db_label, label = "Database label") %>% suppressWarnings()
    dat$vcqi_db_analysis_counter <- haven::labelled(dat$vcqi_db_analysis_counter,
                                                    label = "Analysis counter from VCQI DATABASE file name") %>% suppressWarnings()
    dat$vcqi_db_additional_file_info <- haven::labelled(dat$vcqi_db_additional_file_info,
                                                        label = "Additional information from VCQI DATABASE file name") %>% suppressWarnings()

    saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER,"/VCQI_aggregated_databases_all.rds"))
  } #end of if there's database in the folder

}

