# ============================================================================ #
#
#
# ---- Misc Functions ----
#
#
# ============================================================================ #    
# Note:
# Started from another project

##%######################################################%##
#                                                          #
####             Most Recently Created File             ####
#                                                          #
##%######################################################%##
#' Most Recently Created File
#'
#' This function should be used after an `fs::dir_ls` search with a specific 
#' file that may have multiple versions. When there are multiple matches to 
#' a search, this will take the most recent version of it.
#'
#' @param fpath The fs_path object created from `fs::dir_ls`
#' @param check Optionally check the most recent file. This can be set to 
#' either `TRUE` or `FALSE`.
#'
#' @return A vector of the most recent created file as `fs_path` object
#' @examples
#' # NA
#' 
last_mod <-  function(fpath, check = TRUE) {

    if (!check) return(fpath)
    
    ftime <- file.mtime(fpath) 
    
    return(fpath[which.max(ftime)]) 
    
    # ---- end of function last_mod
}

##%######################################################%##
#                                                          #
####      Base File Name and File Name Expression       ####
#                                                          #
##%######################################################%##
#' Base File Name and File Name Expression
#'
#' This function takes a location to save file and a base name to 
#' search for in either locally or the cloud
#'
#' @param loc Location to save aphia ID file
#' @param file_base Base name to search for
#' @param exts Extension to save file.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name. 
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#'                       - if help, will give a table of formats with examples
#'
#' @returns Returns a list of three:
#'          - file location  = file_loc
#'          - base file name = file_base,
#'          - file expression to be evaluated = file_expr
#' 
#' @details
#' The time stamp can be formated based on 
#' Code	Meaning	Code	Meaning
#'      %a - Abbreviated weekday	
#'      %A - Full weekday
#'      %b - Abbreviated month	
#'      %B - Full month
#'      %c - Locale-specific date and time	
#'      %d - Decimal date
#'      %H - Decimal hours (24 hour)	
#'      %I - Decimal hours (12 hour)
#'      %j - Decimal day of the year	
#'      %m - Decimal month
#'      %M - Decimal minute	
#'      %p - Locale-specific AM/PM
#'      %S - Decimal second	
#'      %U - Decimal week of the year (starting on Sunday)
#'      %w - Decimal Weekday (0=Sunday)	
#'      %W - Decimal week of the year (starting on Monday)
#'      %x - Locale-specific Date	
#'      %X - Locale-specific Time
#'      %y - 2-digit year	
#'      %Y - 4-digit year
#'      %z - Offset from GMT	
#'      %Z - Time zone (character)
#'      
#' @author Sebastian Di Geronimo (June, 2023)
#'      
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
file_expr <- function(loc = NULL, #loc       = here::here("data", "metadata", "aphia_id"),
                      file_base = "aphia_taxa",
                      exts      = "csv",
                      time_stamp_fmt = "%Y%m%d_%H%M%S") {
  
  # ---- libraries
  library(glue)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(here)
  library(rlang)
  library(magrittr)
  
  # ---- help for deciding time stamp format
  if (!is.null(time_stamp_fmt) && str_detect(time_stamp_fmt, "help")) {
    return(tribble(
      ~code, ~meaning,
      "%a",  "Abbreviated weekday", 
      "%A",  "Full weekday",
      "%b",  "Abbreviated month", 
      "%B",  "Full month", 
      "%c",  "Locale-specific date and time", 
      "%d",  "Decimal date", 
      "%H",  "Decimal hours (24 hour)", 
      "%I",  "Decimal hours (12 hour)", 
      "%j",  "Decimal day of the year",	
      "%m",  "Decimal month", 
      "%M",  "Decimal minute",	
      "%p",  "Locale-specific AM/PM", 
      "%S",  "Decimal second", 
      "%U",  "Decimal week of the year (starting on Sunday)", 
      "%w",  "Decimal Weekday (0=Sunday)", 
      "%W",  "Decimal week of the year (starting on Monday)", 
      "%x",  "Locale-specific Date", 
      "%X",  "Locale-specific Time", 
      "%y",  "2-digit year", 
      "%Y",  "4-digit year", 
      "%z",  "Offset from GMT",	
      "%Z",  "Time zone (character)", 
    ) %>%
      mutate(
        example = format(ymd_hms("2000-01-01 02:11:51"), code),
        example = sprintf("%s == `%s`", "2000-01-01 02:11:51", example)
      ))
  }
  
  # catch time stamp format if NULL  
  time_stamp_fmt <-
    tryCatch(
      {
        glue("_", format(Sys.time(), time_stamp_fmt))
        expr(glue("_", format(Sys.time(), !!time_stamp_fmt)))
      },
      error = function(e) {
        NULL
      }
    )
  
  # add period to extension
  exts <- glue(".{exts}")
  
  file_expr <-
    # create expression for the file name
    expr(
      here::here(
        !!loc,
        glue(
          !!file_base,
          !!time_stamp_fmt,
          !!exts,
          .null = ""
        )
      )
    )
  
  list(
    file_base = file_base,
    file_expr = file_expr,
    file_loc  = loc
  )
  
  # ---- end of function file_expr
}

##%######################################################%##
#                                                          #
####                   Save .csv File                   ####
#                                                          #
##%######################################################%##
#' Save .csv File
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .data The data.frame or tibble to be saved.
#' @param save_location Folder to save file. Will create folder if doesn't exist.
#' @param save_name Prefix name of file. Will be saved with `_<timestamp>.csv`
#' @param overwrite `TRUE` or `FALSE` to re-save file if exists, or keep current. 
#' @param verbose `TRUE` or `FALSE` to print location in script.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name.
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#' @param utf_8 Logical() `TRUE` or `FALSE` to use `readr::write_excel_csv` or 
#'              `readr::write_csv` to indicate to Excel the csv is UTF-8 
#'              encoded. 
#'
#' @return NULL, save file
#'
#' @author Sebastian Di Geronimo (June 02, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
save_csv <- function(
    .data         = NULL,        
    save_location = NULL,
    save_name     = NULL,
    overwrite     = FALSE,
    verbose       = TRUE,
    time_stamp_fmt = "%Y%m%d_%H%M%S",
    utf_8          = FALSE) {
  
  # ---- checking input parameters
  if (is.null(.data)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\".data\")}} is `NULL`.", 
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_location)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"save_location\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_name)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"save_name\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(overwrite)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"overwrite\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  
  # ---- file name
  data_f <- 
    file_expr(
      save_location,
      save_name,
      exts = "csv",
      time_stamp_fmt
    )
  
  if (any(class(data_f) %in% c("tbl_df", "tbl", "data.frame"))) {
    print(data_f)
    return(data_f)
  }
  
  data_f <- eval(data_f$file_expr)
  
  if (verbose) {
    cli::cli_h1("Base File Name: {.var {save_name}}")
    cli::cli_alert_info(
      c("File Information:\n",
        "Rows:      {nrow(.data)}\n",
        "Columns:   {ncol(.data)}\n",
        "Location:  {.file {save_location}}\n",
        "File Name: {.file {basename(data_f)}}")
    )
  }
  # ---- check if folder exists and create otherwise
  if (!dir_exists(save_location)) {
    
    if (verbose) 
      cli::cli_alert_info(
        "{col_green(\"Creating\")} folder location!")
    
    fs::dir_create(save_location)
  }
  
  # ---- check if need to create file
  file_loc <- 
    fs::dir_ls(
      save_location,
      regexp = save_name
    ) 
  
  create_f <- rlang::is_empty(file_loc)
  
  if (!create_f && !overwrite) {
    # return early if no need to create
    if (verbose)
      cli::cli_alert_info(
        "File exist and {.emph is not} being {col_green(\"overwritten\")}!\n"
      )
    return(invisible())
  }
  
  if (!create_f && overwrite && verbose) {
    cli::cli_alert_info(
      "File exist and {.emph is} being {col_red(\"overwritten\")}!")
  } else if (create_f && verbose) {
    cli::cli_alert_info("File does not exists, {col_green(\"creating\")}!")
  }
  
  
  # ---- saving file
  if (verbose) cli::cli_alert_info("Saving file!")
  
  if (utf_8) {
    # save with UTF-8 
    cli::cli_alert_info(
      c("Note: saving to indicate to excel as {.var UTF-8}.\n", 
        "Using: `{col_yellow(\"readr::write_excel_csv()\")}` ",
        "instead of `{col_red(\"readr::write_csv()\")}`"))
    readr::write_excel_csv(
      x    = .data,
      file = data_f,
      na   = ""
    )
  } else {
    readr::write_csv(
      x    = .data,
      file = data_f,
      na   = ""
    )
  }
  
  if (verbose) cli::cli_alert_success("Saved!\n\n")
  
  # ---- end of function save_csv
}

##%######################################################%##
#                                                          #
####                   Save gg Plots                    ####
#                                                          #
##%######################################################%##
#' Save ggplots
#' 
#' ggplot objects can be saved using "jpeg" or "svg" device. 
#'
#'
#' @param plt DESCRIPTION.
#' @param filename DESCRIPTION.
#' @param device DESCRIPTION.
#' @param height DESCRIPTION.
#' @param width DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
save_gg <- function(
    plt, 
    save_location  = NULL,
    save_name      = NULL,
    overwrite      = FALSE,
    verbose        = TRUE,
    time_stamp_fmt = "%Y%m%d_%H%M%S",
    device         = c("jpeg", "svg"), 
    height         = 15, 
    width          = 30, 
    ...
) {
  
  library(cli)
  library(cliExtras)
  library(fs)
  
  cli_quiet(!verbose)
  
  # ---- checking input parameters
  if (is.null(.data)) {
    cli_abort(
      c("Check input, {.var {col_yellow(\".data\")}} is `NULL`.", 
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_location)) {
    cli_abort(
      c("Check input, {.var {col_yellow(\"save_location\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_name)) {
    cli_abort(
      c("Check input, {.var {col_yellow(\"save_name\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(overwrite)) {
    cli_abort(
      c("Check input, {.var {col_yellow(\"overwrite\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  
  
  device <- match.arg(device)
  
  height <- if (is.null(height)) 3.71 else height
  
  # ---- file name
  data_f <- 
    file_expr(
      save_location,
      save_name,
      exts = device,
      time_stamp_fmt
    )

  data_f <- 
    data_f  %$% 
    eval(file_expr)

  cli_h1("ggplot Object Data Name: {.var {save_name}}")
  cli_alert_info(
    c(
      "Location:  {.file {save_location}}\n",
      "File Name: {.file {basename(data_f)}}")
  )
  
  # ---- check if folder exists and create otherwise
  if (!dir_exists(save_location)) {
    cli_alert_info("{col_green(\"Creating\")} folder location!")
    
    fs::dir_create(save_location)
  }
  
  # ---- check if need to create file
  create_f <- 
    fs::dir_ls(
      save_location,
      regexp = save_name
    )  %>%
    rlang::is_empty()
  
  if (!create_f && !overwrite) {
    # return early if no need to create
    cli_alert_info(
      "File exist and {.emph is not} being {col_green(\"overwritten\")}!\n"
    )
    return(invisible())
  }
  
  if (!create_f && overwrite ) { 
    cli_alert_info(
      "File exist and {.emph is} being {col_red(\"overwritten\")}!")
  } else if (create_f ) { 
    cli_alert_info("File does not exists, {col_green(\"creating\")}!")
  }
  
  
  # ---- saving file
  cli_alert_info("Saving file!")
  
  cowplot::save_plot(
    filename    = data_f,
    plot        = plt,
    base_height = height,
    base_width  = width,
    device      = device,
    ...
  )
  
  # ---- end of function save_gg
}

##%######################################################%##
#                                                          #
####             Source Files from a Folder             ####
#                                                          #
##%######################################################%##
#' Source Files from a Folder
#'
#' This function sources all selected files from a folder. 
#' 
#' You will be able to select which files to source. You can override this by 
#' setting `file_select = "all"` or `file_select` to a character vector of files 
#' to source. Setting the specific files will require the file extensions and 
#' correct spelling. This feature is most useful if you want to source the same
#' files after every time you restart your R session.
#'
#' @param path Path to folder.
#' @param pattern Pattern to match files.
#' @param file_select Files to source.
#'                    - `NULL` to select files from a list.
#'                    - `character` vector to select files from a list.
#'                    - `all` to source all files.
#' @param show_graphic logical, show graphic to select files.
#' @param envir Environment to source files.
#'
#' @author Sebastian Di Geronimo (November 02, 2023)
#'
#' @return `NULL`
#' @examples
#' 
#' # source a script from a folder to attach to the global environment
#' source_all(here::here("scripts"), pattern = "misc_functions", "all")
#' 
#' # Alternatively, create a new environment and attach the functions
#' new_env <- new.env()
#' 
#' # add the source files to the new environment
#' source_all(
#'   path        = here::here("scripts"), 
#'   pattern     = "misc_functions", 
#'   file_select = "all", 
#'   envir       = new_env)
#' attach(new_env)
#' 
#' # to remove new environment
#' detach(new_env)
#' 
source_all <- function(
    path,
    pattern      = NULL,
    file_select  = NULL,
    show_graphic = FALSE,
    envir        = parent.frame()) {
  
  files <- fs::dir_ls(path, regexp = pattern)

  # select script from a list
  if (is.null(file_select)) {
    file_select <-
      select.list(
        c(basename(files), "all"),
        multiple = TRUE,
        graphics = show_graphic
      )
  }

  if (any(file_select != "all")) {
    files <- here::here(path, file_select)
  }

  cat("From Folder: ", path, "\n",
    "Sourcing files:\n",
    sep = ""
  )
  
  for (file in files) {
    cat("-", basename(file), "\n")
    
    if (!fs::file_exists(file)) {
      cli::cli_alert_danger(
        c("File {.path {file}} does not exist. {col_red(\"Skipping file!\")}\n"))
      cli::cli_alert_info("Check path, spelling and file extension.")
      next
    }
    
    source(file, local = envir)
  }

  invisible()
  
  # ---- end of function source_all
}


##%######################################################%##
#                                                          #
####      Rendering Quarto File to Specific Folder      ####
#                                                          #
##%######################################################%##
#' Rendering Quarto File to Specific Folder
#'
#' The was created to render a quarto file to a specific folder. This is useful
#' because the default behavior is to render the file to the same folder as the
#' input file. This function will also create the folder if it does not exist.
#' 
#' This function is a wrapper around `quarto::render()` and was inspired from:
#' <https://stackoverflow.com/questions/73802144/how-do-i-change-the-default-output-location-of-the-quarto-document-in-rstudio>
#'
#' @param input_file The input `qmd` file to render
#' @param output_path The folder path to save files
#' @param file_ext The file extension to save the file as
#' @param ... Additional arguments passed to `quarto::render()`
#'
#' @return RETURN_DESCRIPTION
#'
#' @author shafee (September 22, 2022)
#' @author Sebastian Di Geronimo (December 11, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
#'
render_qmd <- function(input_file, output_path, file_ext, ...) {
  
  # libraries
  library(cli)
  library(fs)
  library(xfun)
  library(stringr)
  
  
  # check if input file is quarto file
  if (!stringr::str_detect(input_file, "\\.qmd$")) {
    cli::cli_alert_danger(
      c("Input file {.path {input_file}} is not a quarto file. ",
        "{col_red(\"Skipping file!\")}\n"))
    cli::cli_alert_info("Check path, spelling and file extension.")
    return(invisible())
  }
  
  # Extract input file name (without the file-extension)
  file_name <- xfun::sans_ext(input_file)
  cli::cli_h1("File Name: {.path {basename(input_file)}}")
  cli::cli_alert_info("Output Location: {.path {output_path}}")
  cli::cli_alert_info("Output File Extension: {.strong {file_ext}}")
  
  # render the input document, output file and folder will be in same directory 
  # as input file
  cli::cli_alert_info("Rendering quarto document")
  quarto::quarto_render(input = input_file, output_format = file_ext, ...)
  
  # create folder if doesnt exists
  fs::dir_create(output_path)
  
  # move output file
  file_name <- 
    fs::dir_ls(dirname(input_file), regexp = file_name, type = "file") |>
    stringr::str_subset("\\.qmd", negate = TRUE)
  cli::cli_alert_info("Moving file to output directory!")
  fs::file_move(file_name, output_path)
  
  # copy `<file name>_files` folder to the output directory
  dir_name <-  paste0(xfun::sans_ext(file_name), "_files")
  cli::cli_alert_info("Copying {.path {basename(dir_name)}} folder to output directory!")
  fs::dir_copy(dir_name, paste0(output_path, "/", basename(dir_name)), overwrite = TRUE) 
  
  cli::cli_alert_success("Done!")
}
