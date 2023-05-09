# will create a set directory if does not exists
# useful for new projects

showTree <- function(showtree = TRUE) {
  if (showtree) {
    cli::cli_text("Here is the {.strong project} structure:")
    fs::dir_tree(type = "directory")
  }
}

subDir_deflt <- c(
    "data/raw",
    "data/processed",
    "data/plots",
    "data/metadata",
    "Rmd",
    "scripts"
)

cust_dir <- c(

)
subDir <- here::here(c(subDir_deflt, cust_dir))
subDir <- subDir[which(!fs::dir_exists(subDir))]

fs::dir_create(path = subDir)

if (!length(subDir) == 0) {
    cli::cli_alert_info("Creating {length(subDir)} director{?y/ies}.")
    cli::cli_ul(subDir, .close = T)
    showTree(showtree = TRUE)
    } else {
        cli::cli_alert_info("No new directories created.")
        }

rm(subDir_deflt, cust_dir, subDir)



