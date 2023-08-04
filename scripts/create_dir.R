# will create a set directory if does not exists
# useful for new projects

showTree <- function(.loc, showtree = TRUE, ...) {
  if (showtree) {
    cli::cli_text("Here is the {.strong project} structure:")
    fs::dir_tree(.loc, type = "directory", ...)
  }
}

create_dir <- function(
    .loc     = NULL, 
    cust_dir = NULL,
    showtree = FALSE, ...) {
  
  subDir_deflt <- c(
    here::here(.loc, "data", c("raw", "processed", "plots", "metadata")),
    here::here(.loc, "Rmd"),
    here::here(.loc, "scripts")
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
