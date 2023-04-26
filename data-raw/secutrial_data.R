# ## code to prepare `secutrial_data` dataset
# 
# # This was created using the guide provided here:
# # https://grasshoppermouse.github.io/posts/2017-10-18-put-your-data-in-an-r-package/
# # There, it is recommended to just use "library" to load packages and not
# # import. I don't know about that...
# 
# # Load packages -----
# library("secuTrialR")
# library("dplyr")
# 
# # Read secuTrial data ----
# secutrial_data_orig <- read_secuTrial("data-raw/s_export_CSV-xls_TES12_20230112-155842.zip")
# 
# # Prepare secuTrial data for use in shiny app -----
# randomized <- secutrial_data_orig$rando %>%
#   select(pat_id, centre.short = centre, starts_with("rand")) %>%
#   mutate(centre.short = as.character(centre.short))
# 
# locations <- secutrial_data_orig$ctr %>%
#   select(centre.short = mnpctrname) %>%
#   mutate(centre.short = as.character(centre.short),
#          long = c(6.5848, 8.9857, 8.9632, 7.4688, 10.2411),
#          lat = c(46.5980, 46.0868, 47.1502, 47.3604, 46.6630),
#          monthly = c(2,1,2,1,5),
#          target = c(25, 35, 30, 30, 30))
# 
# centers <- locations %>%
#   select(-long, -lat) %>%
#   add_row(centre.short = "Overall",
#           monthly = 11,
#           target = 150) %>%
#   mutate(target_qol = c(20, 30, 25, 25, 25, 125),
#          target_fu1 = c(15, 25, 20, 20, 20, 100))
# 
# study_params <- data.frame(acc_target = 150,
#                            study_start = as.Date('2017/12/01'))
# 
# # Create final secutrial data
# secutrial_data <- list(randomized = randomized,
#                        locations = locations,
#                        centers = centers,
#                        study_params = study_params)
# 
# # Save final data as .RData
# usethis::use_data(secutrial_data, overwrite = TRUE)
