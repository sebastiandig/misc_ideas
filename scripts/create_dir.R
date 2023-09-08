# ============================================================================ #
# 
# 
# ---- Create Standard Directories for New Projects ----
# 
# 
# ============================================================================ #    

# will create a set directory if does not exists
# useful for new projects

##%######################################################%##
#                                                          #
####                Show Directory Tree                 ####
#                                                          #
##%######################################################%##
#' Show Directory Tree
#'
#' This is run when creating directories for the first time when running 
#' {.fun create_dir}
#'
#' @param .loc DESCRIPTION.
#' @param showtree DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#'
showTree <- function(.loc, showtree = TRUE, ...) {
  if (showtree) {
    cli::cli_text("Here is the {.strong project} structure:")
    fs::dir_tree(.loc, type = "directory", ...)
  }
  
  # ---- end of function showTree
}

##%######################################################%##
#                                                          #
####    Create Standard Directories for New Projects    ####
#                                                          #
##%######################################################%##
#' Create Standard Directories for New Projects
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .loc DESCRIPTION.
#' @param cust_dir DESCRIPTION.
#' @param showtree DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
create_dir <- function(
    .loc      = NULL, 
    cust_dir = NULL,
    showtree = FALSE, ...) {
  
  library(here)
  
  subDir_deflt <- c(
    here(.loc, "data", c("raw", "processed", "plots", "metadata")),
    here(.loc, "Rmd"),
    here(.loc, "scripts")
  )
  
  subDir <- c(subDir_deflt, cust_dir)
  subDir <- subDir[which(!fs::dir_exists(subDir))]
  
  fs::dir_create(path = subDir)
  
  if (!length(subDir) == 0) {
    cli::cli_alert_info("Creating {length(subDir)} director{?y/ies}.")
    cli::cli_ul(subDir, .close = TRUE)
    showtree <- TRUE
  } else {
    cli::cli_alert_info("No new directories created.")
  }
  
  showTree(.loc = .loc, showtree = showtree, ...)
  
  # ---- end of function create_dir
}

create_dir(here::here(),
           showtree = TRUE,
           recurse = 2)
