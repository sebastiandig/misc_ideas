# will create a set directory if does not exists
# useful for new projects

showTree <- function(showtree = TRUE) {
    treees <- data.frame(
        stringsAsFactors = FALSE,
        package = c(
            here::here(),"data","raw","processed","plots",
            "metadata", "Rmd", "scripts"
        ),
        dependences = I(
            list(
                c("data", "Rmd", "scripts"),
                c("metadata", "plots", "processed", "raw"),
                character(0), character(0), character(0), character(0),
                character(0), character(0))))
    if (showtree) {
        cli::cli_text("Here is the {.strong project} structure:")
        print(cli::tree(treees))
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



