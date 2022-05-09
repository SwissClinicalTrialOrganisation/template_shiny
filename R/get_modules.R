#' Retrieves a list of all module aliases
#'
#' Module aliases retrieved via get_modules() are used to define module IDs in shiny::callModule(),
#' and are also used as tabnames for the sidebar, and for the shiny UI label and id.
#'
#' @return list of strings containing module aliases
#' @noRd
#'
get_modules <- function(){
  # a list of all module names
  mod <- list(
    home = "mod_home",
    ## Performance measures
    recruit = "mod_recruit",
    recruit2 = "mod_recruit2",
    recruitmap = "mod_recruitment_map",
    recruitment_prediction = "mod_recruitment_prediction",
    completeness = "mod_completeness",
    retention = "mod_retention",
    consistency = "mod_consistency",
    timeliness = "mod_timeliness",
    queries = "mod_queries",
    ## Study management
    visits = "mod_visits",
    completed = "mod_completed",
    participant = "mod_participant",
    safetymgm = "mod_safetymgm",
    sae = "mod_sae",
    ae = "mod_ae",
    asr = "mod_asr",
    contacts = "mod_contacts"
  )
  return(mod)
}
