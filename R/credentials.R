#' The app uses the package 'shinymanager' for user management and login. 
#' 
#' This script creates the initial users for the shiny app in .sqlite database. 
#' Execute the script only once to generate the 'users.sqlite' database.
#' 
#' You can restrict access to the app to specific users, and control their right
#' to access specific modules. 
#' 
#' When a user with administrator rights is logged into the application, he can 
#' create and manage other users directly in  app via the administrator menu.
#' An administrator also controls the users rights to access the different modules. 
#' 
#' You can create a single initial admin user here, who will then manage the other 
#' users in the app. 
#' 
#' @import shinymanager

if(!exists("users.sqlite")){
  #create initial users credentials
  credentials <- data.frame(
    # Usernames and passwords + admin rights (mandatory)
    user = c("admin", "user", "guest"), 
    password = c("admin", "user", "guest"),
    admin = c(TRUE, FALSE, FALSE),
    # Other optional parameters: start date / expiration date / comment
    start = c("2023-04-18", NA, NA),                
    expire = c(NA, NA, "2025-31-12"),
    comment = c("Default admin", "Default user", "Default guest"),
    # User additional parameters here to grant / refuse access to specific modules
    # You can either check a user role or use a specfic boolean in the modules
    role = c("admin", "user", "guest"),             
    access_sae = c(TRUE, TRUE, FALSE),
    access_ae = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  # write the initial user to sqlite database
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = "users.sqlite",
    passphrase = "scto_rshiny_app"
  )
}