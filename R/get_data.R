#' To generate visit data using randomization data
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_visits <- function(df){
  
  ## Visits
  visits <- df %>%  
    mutate(FU1.date = case_when(FU1 == TRUE ~ rando_date.date + 6*30),
           FU2.date = case_when(FU2 == TRUE ~ rando_date.date + 12*30)) %>% 
    mutate(FU1.qol.done = case_when(FU1 == FALSE ~ NA, TRUE ~ FU1.qol.done),
           FU2.qol.done = case_when(FU2 == FALSE ~ NA, TRUE ~ FU2.qol.done),
           bl.qol.compl = case_when(bl.qol.done == FALSE ~ NA, TRUE ~ bl.qol.compl),
           FU1.qol.compl = case_when(FU1.qol.done == FALSE ~ NA, TRUE ~ FU1.qol.compl),
           FU2.qol.compl = case_when(FU2.qol.done == FALSE ~ NA, TRUE ~ FU2.qol.compl)) 
  
  dates.visits <- visits %>% 
    select(pat_id, centre.short, Baseline = rando_date.date, FU1 = FU1.date, FU2 = FU2.date) %>% 
    ## Gather data
    pivot_longer(cols = c(Baseline, FU1, FU2),
                 names_to = c("Visit"),
                 values_to = c("Date")) 
  qol.done <- visits %>% 
    select(pat_id, centre.short, Baseline = bl.qol.done, FU1 = FU1.qol.done, FU2 = FU2.qol.done) %>% 
    ## Gather data
    pivot_longer(cols = c(Baseline, FU1, FU2),
                 names_to = c("Visit"),
                 values_to = c("qol.done")) 
  
  qol.compl <- visits %>% 
    select(pat_id, centre.short, Baseline = bl.qol.compl, FU1 = FU1.qol.compl, FU2 = FU2.qol.compl) %>% 
    ## Gather data
    pivot_longer(cols = c(Baseline, FU1, FU2),
                 names_to = c("Visit"),
                 values_to = c("qol.compl")) 
  
  nr.visits <- dates.visits %>%   
    filter(!is.na(Date)) %>% 
    group_by(centre.short, Visit) %>% 
    count() %>% 
    rename(nr.visits.done = n)
  
  nr.qol <- qol.done %>% 
    filter(qol.done == TRUE) %>% 
    group_by(centre.short, Visit) %>% 
    count() %>% 
    rename(nr.qol.done = n)
  
  nr.qol.compl <- qol.compl %>% 
    filter(qol.compl == TRUE) %>% 
    group_by(centre.short, Visit) %>% 
    count() %>% 
    rename(nr.qol.compl = n)
  
  visits <- dates.visits %>% 
    left_join(qol.done, by = c("centre.short", "Visit","pat_id")) %>% 
    left_join(qol.compl, by = c("centre.short", "Visit","pat_id")) %>% 
    left_join(nr.visits, by = c("centre.short","Visit")) %>% 
    left_join(nr.qol, by = c("centre.short","Visit")) %>% 
    left_join(nr.qol.compl, by = c("centre.short","Visit")) 
  
  return(visits)
  
}


#' Get dataframe of queries
#'
#' @param index 
#' @param df 
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
get_queries <- function(index, df){
  
  nr <- nrow(df)
  
  new.df <- df %>% 
    select(pat_id, centre.short, Visit, rando_date.date) %>% 
    mutate(nr.queries = sample(1:40, nr, replace = TRUE)) %>% 
    slice(index)
  nr.times <- new.df %>% pull(nr.queries)
  result <- new.df %>% slice(rep(1:n(), nr.times))
  
  return(result)
}


#' Load data
#'
#' @return
#' @export
#' @import dplyr
#' @import tidyr
#' @import secuTrialR
#' @import lubridate
#' @importFrom purrr map
#'
get_data <- function(){
  
  #######################################################################################################################
  ###                                                     LOAD DATA                                                   ###
  #######################################################################################################################
  
  data.extraction.date <- Sys.Date()
  
  set.seed(12481498)
  ## Required vars: centre.short, rando_date.date
  # randomized <- data.frame(pat_id = sample(c(1:100), 100),
  #                          centre.short = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
  #                          rando_date.date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 100))
  
  ## Required vars: centre.short, rando_date.date
  randomized <- data.frame(pat_id = sample(c(1:100), 100),
                           centre.short = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
                           rando_date.date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 100),
                           ended.study = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.95,0.05)),
                           bl.qol.done = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7,0.3)),
                           bl.qol.compl = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.6,0.4)),
                           FU1 = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7,0.3)),
                           FU1.qol.done = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.6,0.4)),
                           FU1.qol.compl = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.6,0.4)),
                           FU2 = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.6,0.4)),
                           FU2.qol.done = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.55,0.45)),
                           FU2.qol.compl = sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.45,0.55))) %>%
    mutate(ended.study.reason = case_when(ended.study == TRUE & pat_id %in% c(40, 11, 35) ~ "Death",
                                          ended.study == TRUE & pat_id %in% c(48) ~ "Consent withdrawn",
                                          ended.study == TRUE & pat_id %in% c(24) ~ "Adverse event",
                                          ended.study == TRUE & pat_id %in% c(36) ~ "Switched hospital")) %>%
    ## Get total counts
    group_by(centre.short) %>% mutate(total.ended = sum(ended.study == TRUE), total.randomized = n()) %>% ungroup()
  
  ## Get visit info
  visits <- get_visits(randomized)
  
  ## Merge all info
  df.all <- visits %<>% 
    left_join(randomized %>% select(pat_id, rando_date.date, total.ended, total.randomized, ended.study, ended.study.reason), by = "pat_id") %>% 
    mutate(centre.short = factor(centre.short, levels = c("A", "B", "C", "D", "E"))) 
  
  ## Queries df
  
  nr.rows <- df.all %>% nrow()
  df <- purrr::map_dfr(1:nr.rows, get_queries, df = df.all)
  no <- df %>% nrow()
  set.seed(12481498)
  df.queries <- df %>% mutate(querystatus = sample(c("answered", "open", "closed"), no, replace = TRUE, prob = c(0.5, 0.2, 0.3)),
                              queryform = sample(c("Adverse events", "Diagnosis", "Biobanking", "MRI", "Laboratory"), no, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
                              query = sample(c("Date:Please enter a date", "Date:Event date is greater than current date", "Description: Value required"), no, replace = TRUE, prob = c(0.5, 0.3, 0.2))) %>% 
    separate(query, c("query.field", "query"), sep = ":")
  
  
  locations <- data.frame(centre.short = LETTERS[1:5],
                          long = c(6.5848, 8.9857, 8.9632, 7.4688, 10.2411),
                          lat = c(46.5980, 46.0868, 47.1502, 47.3604, 46.6630),
                          monthly = c(2,1,2,1,5),
                          target = c(25, 35, 30, 30, 30))
  
  study_params <- data.frame(acc_target = 150,
                             study_start = as.Date('2017/12/01'))
  
  
  centers <- data.frame(centre.short = c(LETTERS[1:5], "Overall"),
                        monthly = c(2,1,2,1,5,11),
                        target = c(25, 35, 30, 30, 30, 150),
                        target_qol = c(20, 30, 25, 25, 25, 125),
                        target_fu1 = c(15, 25, 20, 20, 20, 100))
  
  ## read secuTrial test data to illustrate the completeness module
  st_data = system.file("extdata/sT_exports/exp_opt/s_export_CSV-xls_CTU05_all_info.zip",
                        package = "secuTrialR") %>%
    read_secuTrial() %>%
    magrittr::extract(c(
      "esurgeries", "baseline", "outcome", "treatment", "allmedi", "studyterminat", "ae", "sae"
    )) %>%
    map(tibble) %>%
    map(~ .x %>% select(-contains(".factor")))
  
  
  sae_descr <- c("headache", "Headache", "Cancer", "Allergic reaction") # ?
  sae <- data.frame(pat_id = sample(randomized$pat_id, 50, replace = T),
                    
                    sae_date = sample(seq(as.Date('2017/12/01'), as.Date('2022/03/01'), by="day"), 50), 
                    severity_level = sample(c("Mild", "Moderate", "Severe"), 50, replace = T), 
                    causality = sample(c("Certain", "Probable", "Possible", "Unlikely", "Not related", "Not assessable"), 50, replace = T), 
                    expectedness = sample(c("Expected", "Unexpected"), 50, replace = T), 
                    outcome = sample(c("Resolved without sequelae", "Resolved with sequelae", "Ongoing", "Death", "Unknown", "Other"), 50, replace = T), 
                    death = sample(c("Yes", "No"), 50, T), 
                    life_threatening = sample(c("Yes", "No"), 50, T),
                    persistant_disability = sample(c("Yes", "No"), 50, T),
                    hospitalization = sample(c("Yes", "No"), 50, T),
                    congenital_anomalia_birth_defect = sample(c("Yes", "No"), 50, T), 
                    sae_report_type = sample(c("Initial", "Follow-up", "Final"), 50, replace = T), 
                    sae_description = sample(sae_descr, 50, replace = T)) #?
  sae$centre.short <- sapply(sae$pat_id, function(x){randomized$centre.short[randomized$pat_id == x]})
  
  consistency <- randomized %>% 
    mutate(height = rnorm(nrow(randomized), 170, 30),
           height_datetime = sample(seq(as.POSIXct('2019-12-01 00:01:00'), as.POSIXct('2022-03-01 23:59:00'), by="mins"), nrow(randomized)),
           weight = rnorm(nrow(randomized), 70, 30),
           weight_datetime = sample(seq(as.POSIXct('2019-12-01 00:01:00'), as.POSIXct('2022-03-01 23:59:00'), by="mins"), nrow(randomized)),
           fu1_date = if_else(FU1, 
                              sample(seq(as.Date('2018/12/01'), as.Date('2023/03/01'), by="1 day"), nrow(randomized)),
                              NA_Date_),
           fu2_date = if_else(FU2, sample(seq(as.Date('2019/12/01'), as.Date('2024/03/01'), by="day"), nrow(randomized)), NA_Date_),
           ended.study_date = if_else(ended.study, sample(seq(as.Date('2020/12/01'), as.Date('2025/03/01'), by="day"), nrow(randomized)), NA_Date_),
           sex = sample(c("male", "female", "unknown"), nrow(randomized), 
                        replace = TRUE),
           department = sample(c("ICU", "icu", "Icu", "Ic", "Oncology", 
                                 "oncology", "ONcology", "Onco", "ncology"), 
                               nrow(randomized), 
                               replace = TRUE),
           diagnosis = paste0(sample(c("This patient has",
                                       paste0(rep("some long text", 20), collapse = " "),
                                       paste0(rep("some shorter text", 5), collapse = " ")),
                                     nrow(randomized), replace = TRUE),
                              sample(c(" high blood pressure ",
                                       " low blood pressure ",
                                       " lung cancer ",
                                       " breast cancer ",
                                       " fever ",
                                       " an S.aureus infection ",
                                       " an S. aureus infection",
                                       " an E.-Coli infection ",
                                       " an E coli infection "), 
                                     nrow(randomized), replace = TRUE),
                              sample(c(".",
                                       paste0(rep("some long text", 20), collapse = " "),
                                       paste0(rep("some shorter text", 5), collapse = " ")),
                                     nrow(randomized), replace = TRUE)
           )
    )
  
  
  #######################################################################################################################
  ###                                                     SAVE DATA                                                   ###
  #######################################################################################################################
  
  data <- list(
    data.extraction.date = data.extraction.date,
    randomized = randomized,
    all = df.all,
    queries = df.queries,
    locations = locations,
    centers = centers,
    st_data = st_data,
    study_params = study_params,
    consistency = consistency, 
    sae = sae
  )
  
  return(data)
}

#' @importFrom redcaptools redcap_export_meta redcap_export_byform redcap_export_tbl rc_prep
get_rc_data <- function(){

  #######################################################################################################################
  ###                                                     LOAD REDCap DATA                                            ###
  #######################################################################################################################

  # Store the REDCap token first in your System Variables
  #########################################################
  # # store it in your .Renviron file
  # # open the .Renviron file interactively with 
  # usethis::edit_r_environ()
  # # Add the line REDCap_TOKEN_P733 = "enter_token_here" in the file. Save it.
  # # Restart R
  # # The token is now accessible through Sys.getenv('REDCap_TOKEN_P733')
   
  token <- Sys.getenv('REDCap_TOKEN_P733')
  url <- "https://redcap.ctu.unibe.ch/api/"

  meta <- redcap_export_meta(token = token, url = url, tabs = c("metadata", "event", "formEventMapping", "instrument", "dag", "user"))

  # define center & study data using REDCap DAG ID----
  study_params <- data.frame(acc_target = 150,
                             acc_monthly = 11,
                             target = 250,
                             target_qol = 125,
                             target_fu1 = 100,
                             study_start = as.Date('2017/12/01'))

  # workaround: get the link DAG - DAG_ID from DAGs in user df (will be fixed in REDCap version 13)
  center_id <- meta[["user"]] %>%
    select("centre.name" = "data_access_group", "centre.id" = "data_access_group_id") %>%
    mutate(centre.id = as.character(centre.id)) %>%
    unique()
  center_id <- center_id[complete.cases(center_id), ]

  centers <- meta[["dag"]] %>%
    left_join(center_id, by = c("unique_group_name" = "centre.name")) %>%
    rename(centre.short = data_access_group_name) %>%
    rename(centre.name = unique_group_name) %>%
    # rename(centre.id = data_access_group_id) %>%
    arrange(centre.short) %>%
    mutate(monthly = c(2,1,2,1,5)) %>%
    mutate(target = c(25, 35, 30, 30, 30)) %>%
    mutate(target_qol = c(20, 30, 25, 25, 25)) %>%
    mutate(target_fu1 = c(15, 25, 20, 20, 20)) %>%
    mutate(long = c(6.5848, 8.9857, 8.9632, 7.4688, 10.2411)) %>%
    mutate(lat = c(46.5980, 46.0868, 47.1502, 47.3604, 46.6630))

  rm(center_id)

  centers_overall <- centers %>%
    add_row(centre.short = "Overall",
            centre.name = "overall",
            centre.id = NA,
            monthly = study_params$monthly,
            target = study_params$target,
            target_qol = study_params$target_qol,
            target_fu1 = study_params$target_fu1,
            long = NA,
            lat = NA)

    # export by form (best for projects with a visit structure) ----
  forms <- redcap_export_byform(token = token, url = url)
  # add labels, make factors, etc (can be slow...)
  forms_prepped <- sapply(forms, rc_prep, metadata = meta$metadata)

  # add center ID to all forms in forms_prepped
  forms_prepped <- sapply(forms_prepped,
                                 function(x){
                                   if(!is.null(x)){
                                     x %>% mutate(dag = substr(record_id, 1, 4) ) %>%
                                       left_join(centers, by = c("dag" = "centre.id"))
                                   }
                                   })

  # rename to adjust to mock data
  forms_prepped$randomization <- forms_prepped$randomization %>%
    rename("rando_date.date" = rando_date_date)
  names(forms_prepped)[2] <- "randomized"

  # Option:
  # export as single dataset (suitable for projects without a visit structure) ----
  # records <- redcap_export_tbl(token = token, url = url, content = "record")
  # add labels, make factors, etc (can be slow...)
  # prepped <- rc_prep(records, meta$metadata)


  #######################################################################################################################
  ###                                                     SAVE REDCap DATA                                            ###
  #######################################################################################################################

  # return(forms/prepped + locations + study_params + centers)
  tmp <- list(study_params, centers, centers_overall)
  names(tmp) = c("study_params", "centers", "centers_overall")
  data <- append(forms_prepped, tmp)

  return(data)
}


# Get secuTrial data (preparation in secutrial_data.R) ----------------
get_secu_data <- function(){

  # Read secuTrial data ----
  secutrial_data_orig <- read_secuTrial("data-raw/s_export_CSV-xls_TES12_20230112-155842.zip")

  # Prepare secuTrial data for use in shiny app -----
  randomized <- secutrial_data_orig$rando %>%
    select(pat_id, centre.short = centre, starts_with("rand")) %>%
    mutate(centre.short = as.character(centre.short))

  locations <- secutrial_data_orig$ctr %>%
    select(centre.short = mnpctrname) %>%
    mutate(centre.short = as.character(centre.short),
           long = c(6.5848, 8.9857, 8.9632, 7.4688, 10.2411),
           lat = c(46.5980, 46.0868, 47.1502, 47.3604, 46.6630),
           monthly = c(2,1,2,1,5),
           target = c(25, 35, 30, 30, 30))

  centers <- locations %>%
    select(-long, -lat) %>%
    add_row(centre.short = "Overall",
            monthly = 11,
            target = 150) %>%
    mutate(target_qol = c(20, 30, 25, 25, 25, 125),
           target_fu1 = c(15, 25, 20, 20, 20, 100))

  study_params <- data.frame(acc_target = 150,
                             study_start = as.Date('2017/12/01'))

  # Create final secutrial data
  secutrial_data <- list(randomized = randomized,
                         locations = locations,
                         centers = centers,
                         study_params = study_params)

  return(secutrial_data)

}
