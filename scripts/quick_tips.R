# ============================================================================ #
# 
# 
# ---- R Tips and Tricks ----
# 
# 
# ============================================================================ #  

# ============================================================================ #
# ---- Save list of all libraries ----
# ============================================================================ #  
# save method
# # 1. using writelines into csv
# writeLines(pacman::p_lib(), 
#            glue:here::here("data",
#                            glue::glue("packages",
#                                       format(Sys.time(), '_%Y-%m-%d'),
#                                       ".rds"))
#            )

# 2. using saveRDS to save result, same output from pacman::p_lib
# fs::dir_create(here::here("data", "pkg_ls"))
# saveRDS(pacman::p_lib(), 
#         file = here::here("data", "pkg_ls",
#                     glue::glue("packages_pac",
#                          format(Sys.time(), '_%Y-%m-%d'),
#                          ".rds"))
#         )
# 
# # read method for saveRDS
# x <- 
#   (fs::dir_ls(here::here("data", "pkg_ls"),
#               regexp = "packages") |>
#   sort(decreasing = TRUE))[1] |>
#   readRDS()

# 3. 
# create directory to save 
fs::dir_create(here::here("data", "pkg_ls"))

x <- 
  # get all packages info with base R
  devtools::package_info("installed", include_base = TRUE) |>
  
  # convert to tibble for manipulation
  tibble::as_tibble() |>
  
  # select columns 
  dplyr::select(package, ondiskversion, source) |>
  
  # set where package was downloaded from
  # CRAN: <package_name>
  # GitHub/others: <user_name>/<package_name>
  dplyr::mutate(
    source_loc = dplyr::case_when(
      stringr::str_detect(source, "Git") ~ 
        stringr::str_extract(source, ".*\\((.*)@.*", group = 1),
        # stringr::str_extract(source, ".*\\((.*)\\).*", group = 1), # <-- includes version
      .default = package
    )
  )

# ex 
# acepack 1.4.1 CRAN (R 4.2.1)                                                           
# addinit 0.3.0 Github (dreamRs/addinit@c8d4bd1986b79a4c51eb1939b7af78c2dbb1425c) 

# source_loc would look like (everything after `@` is version):
# source_loc
# acepack
# dreamRs/addinit --or--
# dreamRs/addinit@c8d4bd1986b79a4c51eb1939b7af78c2dbb1425c <-- if want version


# save as date saved file, to not overwrite previous saves
saveRDS(x, 
        file = here::here(
          "data", "pkg_ls", # <-- folder to save file
          glue::glue("packages_dev", # <-- prefix 
                     format(Sys.time(), '_%Y%m%d'), # <-- add date
                     ".rds")) # <-- save as rds file for quick loading
)

# ============================================================================ #
# ---- Check Packages in Updated R to Previous R  ----
# ============================================================================ #  
# To be used after previous tip and updated R

# read method for saveRDS
# only useful if packages were moved, if not, you won't have any so use base R
# x <- 
#   (fs::dir_ls(here::here("data", "pkg_ls"),
#               regexp = "packages_dev") |>
#      sort(decreasing = TRUE))[1] |>
#   readRDS()

# base R if repeat above
x <- 
  # get pkg list
  list.files("./data/pkg_ls/", full.names = TRUE) |>
  
  # add time
  file.info(extra_cols = FALSE)

# sort and extract most recent and readRDS
x <- 
  (x[order(x$mtime, decreasing = TRUE),] |>
  rownames())[1] |>
  readRDS()

# find packages not included in current version of R that you had previously
y <- x[!x$package %in% .packages(TRUE),]

# download first from CRAN then GitHub
if (nrow(y) > 0) {
  try(install.packages(y[agrep("CRAN", y$source), ]))
  try(devtools::install_github(y[agrep("Git", y$source), ]))
} else {
  message("No packages to download")
  rm(x, y)
  }

# ============================================================================ #
# ---- Detect OS ----
# ============================================================================ #
# might be helpful when trying packages on other system

# using base R, this is the code using in pacman, so not necessary
Sys.info()[["sysname"]]

# using pacman
pacman::p_detectOS()

# ============================================================================ #
# ---- detect if using R interactive or not ----
# ============================================================================ #
# can be useful if running script in background and can't give input info
interactive()

# useful in functions or projects like:
# test if interactive, then stops executions if not in interactive mode
if (interactive()) stop("This must be in interactive mode to be used!")

# same as above instaed of calling `if` directly 
stopifnot(
  "This must be in interactive mode to be used!" = 
    interactive() == TRUE)

stopifnot(
  "This must be in interactive mode to be used!" = 
    interactive())


# ============================================================================ #
# ---- wrap code in {} `curly braces` ----  
# ============================================================================ #
# will run as block, instead of line by line
{
  print(c(1,2,3))
  plot(cars, main = "Stopping Distance versus Speed")
  lines(stats::lowess(cars))
}

# ============================================================================ #
# ---- Ghost Script location for font embedding ----
# ============================================================================ #  
# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs10.00.0/bin/gswin64c.exe")

# ============================================================================ #
# ---- make line with text as output to console ----
# ============================================================================ #
{
  header1 <- cli::rule(
    left = crayon::bold("Something important"),
    right = crayon::blurred("something else"))
  header2 <- cli::rule(center = crayon::yellow("Hello World"))
  print(header1)
  print(header2)
  
  rm(header1, header2)
}

# ============================================================================ #
# ---- print LaTeX in plots tab ----
# ============================================================================ #
# TODO: fix latex symbols that aren't rendering
# save old par
oldpar <- par(no.readonly = TRUE)
{ 
  library("latex2exp")
  # NOTE: for some reason, need to Global options -> General -> Graphics -> 
  # backend -> Cairo to get certain symbols to render in plot viewer pane
  
  # saving plot seems to have no issue
  # example latex
  ex   <-  c(
    paste0("$a = \\frac {a \\times(x \\cdot b^T)}",
           "{a \\times(b \\cdot b^T) + 10^{-100}}$"),
    paste0("$b = \\frac{b \\times (a^T \\cdot x + b_0)}",
           "{(a^T \\cdot(a \\cdot b) + b) + 10^{-100}}$"))
  x    <- c(0.5, 0.5) # set location of x on plot at middle
  y    <- c(0.75, 0.25) # set location of y on plot at 75% and 25% of 1
  plot.new() # start new plot
  par(mar = c(0, 0, 0, 0)) # set par for margins
  plot.window(xlim = c(-0.1, 1), ylim = c(0, 1)) # size plot window
  # print strings of LaTeX
  text(x, y, stringr::str_c("latex2exp::TeX(r\"(", ex, ")\")"), 
       pos = 3, cex = 0.7, family = "mono")
  # print LaTeX
  text(x, y, latex2exp::TeX(ex), pos = 1, cex = 0.75)
  
  # return old par
  par(oldpar)
  
 rm(ex, x, y, oldpar)
  pacman::p_unload("all")
  
}


# ============================================================================ #
# ---- attach() ----
# ============================================================================ #
# attach adds whatever you add to search path, like library or global environ
# this can be variables, functions, etc
# if you do an obj var, you can then search column names
# i.e. instead of data1$x1, it would be x1 after attach(data1)
{ 
  # Create example data
  data1 <- data.frame(x1 = c(1, 2, 3, 4, 5),	
                      x2 = c(6, 7, 8, 9, 0),
                      x3 = c(1, 2, 5, 4, 5))
  
  # Try to print x1
  tryCatch({print(x1)}, 
           error = function(e) {
             print("Does not print!")
             message(sprintf("Will look something like:\n%s", e[["message"]]))
             })
  # Error: object 'x1' not found

  # attach data
  attach(data1)										
  print(x1)
  print("Now prints fine")
  detach(data1)
  rm(data1)
}

# ============================================================================ #
# ---- with() ----
# ============================================================================ #  
# similar to attach, except you don't need to attach the variable to call its
# column names
# allows to use the information by name inside a variable
# this is sort of how dplyr works with `.` <dot notation>
{
  with(mtcars, mpg[cyl == 8  &  disp > 350])
  # is the same as, but nicer than
  mtcars$mpg[mtcars$cyl == 8  &  mtcars$disp > 350]
  
  with(data.frame(u    = c(5,10,15,20,30,40,60,80,100),
                  lot1 = c(118,58,42,35,27,25,21,19,18),
                  lot2 = c(69,35,26,21,18,16,13,12,12)),
       list(summary(glm(lot1 ~ log(u), family = Gamma)),
            summary(glm(lot2 ~ log(u), family = Gamma))))
  
  # instead of 
  x <-
    data.frame(u    = c(5,10,15,20,30,40,60,80,100),
               lot1 = c(118,58,42,35,27,25,21,19,18),
               lot2 = c(69,35,26,21,18,16,13,12,12))
  # dont need x in next lines
  summary(glm(lot1 ~ log(u), x, family = Gamma))
  summary(glm(lot2 ~ log(u), x, family = Gamma))
  
  dplyr::select(x, lot1) |> # still a data.frame
    dplyr::pull()           # now a vector
  x$lot1
  with(x, lot1)
  
  rm(x)
} 

# ============================================================================ #
# ---- read multiple sheets from excel ----
# ============================================================================ #  
# Need path, then read sheet, map over each sheet from the path, then conver to
# tibble. Can use left_join afterwards if wanted
# TODO: add filter for names if needed
{  
  library("magrittr")
  path <- readxl::readxl_example("datasets.xlsx")
  path <- path %>% 
    readxl::excel_sheets(.) %>% 
    purrr::set_names(., .) %>% 
    purrr::map(readxl::read_excel, 
               path = path,
               .name_repair = "unique_quiet" # another tip, removes "New names"
                                  # for read_csv - name_repair = "unique_quiet"
               ) %>%
    tibble::as_tibble_col(., column_name = "datasets")
  print(path)
  print(path$datasets$iris)
  print(path$datasets$mtcars)
  
  rm(path)
  pacman::p_unload("all")
}

# ============================================================================ #
# ---- repeat string concatenated ----
# ============================================================================ #  
# useful for many different representations of NA in read_xlsx(., na = c())
{
  library("magrittr")
  # repeats `-` and adds `-` * n 
  print(strrep("-", 1:20))
  
  na_skip <- c("NA", "Skipped", "skipped", "na", "n/a", "n./a", "n.a", "Flow", 
               "na",
               strrep("-", 1:20))
  
  print(na_skip)
  
  message("Before `na_skip`")
  here::here("data", "raw") %>%
    fs::dir_ls(.,
               regexp = "^[^~]+.test_na") %>%
    readxl::read_xlsx(.) %T>%
    print()
  
  message("After `na_skip`")
  here::here("data", "raw") %>%
    fs::dir_ls(.,
               regexp = "^[^~]+.test_na") %>%
    readxl::read_xlsx(., 
              na = na_skip) %T>%
    print()
  
  rm(na_skip)
  pacman::p_unload("all")
}


# ============================================================================ #
# ---- rescaling for plots ----
# ============================================================================ #  
# better implementation ggh4x
# ggh4x::help_secondary(data = .x, primary = , secondary = )
{
  scal <- function(.x, .y) {
    
    .x_range <- range(.x)
    .y_range <- range(.y)
    
    out <- (.y_range[2] - .y_range[1]) * (.x - .x_range[1]) / 
            (.x_range[2] - .x_range[1]) + .y_range[1]
    result <- list(c(.x_range, .y_range), out)
  }
}


# ============================================================================ #
# ---- anonymous function ----
# ============================================================================ #
# My understanding is its a way of creating a functions to be used in one 
# instance and never again. My experience is using it in `dplyr::summarise()` 
# within `dplyr::across()` (i.e. `dplyr::summarise(dplyr::across())`).
# 
# Three ways (anonymous or lambda):
# anonymous 1 (<4.1 R): ~ fun(.x)
# anonymous 2 (<4.1 R): function(x) fun(x)
# lambda (>4.1 R): \(x) fun(x) 
#         - This version is similar to Python using lambda notation, where the 
#           \(x) x is similar to Python: lambda x: x
#           
# ex lambda (R): 
# (\(x) x + 1)(2) 
# [1] 3
# 
# ex lambda (Python): 
# (lambda x: x + 1)(2)
# 3
# 
{
  dplyr::select(mtcars, 1) |>
    dplyr::summarise(
      dplyr::across(
        mpg, 
        .fns = list(
          avg    = mean, # <- normal
          anon   = function(.x) mean(.x, na.rm = TRUE), # <- anonymous  
          lambda = (\(x) mean(x, na.rm = TRUE)))) # <- lambda 
    )
  
  # this will get you a warning after dplyr 1.1.0
    dplyr::select(mtcars, 1) |>
    dplyr::summarise(
      dplyr::across(
        mpg,
        .fn = list(mean = mean,
                    sum = sum),
        na.rm = TRUE # <- this is the issue where its seen as `...` in `across`
                     # and needs to be place explicitly for the functions that 
                     # will be using it.
                     # 
                     # Annoying, but I understand that a function may not use
                     # `...` and cause issues later like in 
                     # `mean(x, ...)` vs. `sqrt(x)`
                     # across would mean(x, na.rm = T) and sqrt(x, na.rm = T)
                     # which will create and error
                     
        # Warning message:
        #   There was 1 warning in `dplyr::summarise()`.
        # ℹ In argument: `dplyr::across(everything(), mean, na.rm = TRUE)`.
        # Caused by warning:
        #   ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
        # Supply arguments directly to `.fns` through an anonymous function instead.
        # 
        # # Previously
        # across(a:b, mean, na.rm = TRUE)
        # 
        # # Now
        # across(a:b, \(x) mean(x, na.rm = TRUE))
        
        # fix:
        # .fn = list(
        #   # anon version
        #   mean_anon = ~ mean(.x, na.rm = TRUE),
        #   sum_anon = ~ sum(.x, na.rm = TRUE),
        #   sqrt_anon = ~ sqrt(sum(.x)),
        #   # lam version
        #   mean_lam = (\(y) mean(y, na.rm = TRUE)),
        #   sum_lam = (\(y) sum(y, na.rm = TRUE)),
        #   sqrt_lam = (\(y) sqrt(sum(y)))
        #   )
        )
    )
  
  
}


# ============================================================================ #
# ---- Using `...` in function call and extract them in order ----
# ============================================================================ #  
# I use this when making a function that uses the `tidyverse` variable names
# like in dplyr::select(.data = x, col1, col2), where col1 and col2 are not in
# quotes are would be the `...` in the function, i.e. dplyr::select(.data, ...).

# This is is helpful if you have an expected number of columns, but want to 
# limit them in some way. 

# x = data.frame or tibble
# new_col = new name for column, uses `:=` to set with embraces
# ... = two column names with or without quotes

test_fun <- function(x, new_col, ...) {
  
  # extract `...` as list of dots
  arg <- match.call(expand.dots = FALSE)$...
  
  # get length of ... args
  arg_len <- length(arg)
  
  print(arg_len)
  
  # needs to have more than 1 and less than 3
  if (arg_len < 2) stop("`arg` needs to have more than 1 column name")
  if (arg_len > 2) stop("`arg` needs to have less than 3 column name")
  
  # set cols 1 and 2 to the 2 `...` inputs
  col1 <- arg[[1]]
  col2 <- arg[[2]]
  
  # use `{{}}` embrace notation in a function call to substitute in
  dplyr::mutate(x, 
         add1 = {{ col1 }} + {{ col2 }},
         {{ new_col }} := {{ col1 }} + {{ col2 }},
         .keep = "used")
  
}


test_fun(x = dplyr::storms, "add2", year, month)

rm(test_fun)


# ============================================================================ #
# ---- Snippet for `case_when` using quasi-quotation ----
# ============================================================================ #  
# Not sure if this will be useful
 
# this would normally be a separate data.frame with the same names as the one 
# to be modified
name <- dplyr::starwars

(names(name)[2]) # <-- has quote around name
# [1] "height"
noquote(names(name)[2]) # <-- has no quote around name, will be used in mutate
# [1] height


dplyr::mutate(
  dplyr::starwars,
  .keep = "used",
  
  # quasi-quotation to get unquoted name from a vector 
  # i.e. `!! noquote(names(name)[2]) :=`
  !! noquote(names(name)[2]) :=  dplyr::case_when(
    mass > 75 ~ 1,
    
    # default is also from a name in a vector
    # i.e. !!{sym(names(name)[3])}, here it is `mass`
    .default =  !!{sym(names(name)[3])}
  )
)


# ============================================================================ #
# ---- Test if have Internet ----
# ============================================================================ #
# This is useful if you have an automated process and need internet to run it.
# I'm not sure what would happen if you didn't have internet when running a 
# script, but this at least will try it first and you can have it give a 
# custom error or stop, etc

# ---- check if have internet
# curl::has_internet()
if (curl::has_internet()) {
  message("Good to go!")
} else {
  "No Internet"
}

# ---- pingr::is_online(), from CRAN
pingr::is_online()

# ---- check specific URL has internet
try(is.character(RCurl::getURL("www.google.com"))) == TRUE
RCurl::url.exists("www.google.com") # <-- better 
!as.logical(system("ping -n 1 www.google.com"))
pingr::is_up("www.google.com")


# ============================================================================ #
# ---- Ignore Open Files ----
# ============================================================================ #  
fs::dir_ls(
  path = here::here("data", "raw"),
recurse = FALSE,
type    = "file",
# included ^[^~]* to not match ~, means that a file is opened
# regexp  = "^[^~]*\\.xlsx$"
regexp  = "\\.xlsx$"
)

fs::dir_ls(
  path = here::here("data", "raw"),
recurse = FALSE,
type    = "file",
# included ^[^~]* to not match ~, means that a file is opened
# regexp  = "^[^~]*\\.xlsx$"
regexp  = "^[^~]*\\.xlsx$"
)


# ============================================================================ #
# ---- Test if to save or not ----
# ============================================================================ #  
# choose to save or not
# when not set, it will ask to set if in interactive mode 
# else will set to FALSE
# 
# sv <- FALSE
# sv <- TRUE
sv <- NULL

if (is.null(sv) & interactive()) {
  sv <- menu(c("Save", "DO NOT SAVE"))
  sv <- ifelse(sv == 1, TRUE, FALSE)
} else if (is.null(sv) & !interactive()) {
  sv <- FALSE
}


# ============================================================================ #
# ---- Set option `continue` to something else ----
# ============================================================================ #  
# By default, the continue character is "+", which is not great if you want to 
# copy/paste code to a script. You can redefine this as " " leading to easier
# copy/paste. You can set it at the current runtime, project runtime or global
# runtime. 

# rstudio default
{
  old_continue <- getOption("continue") # whatever its currently set to
  options(continue = "+ ") # set default in case it's already changed
  getOption("continue") |> 
    print()
  # "+ "
}

# change default, but this won't save if restart R
{
  options(continue = " ")
  getOption("continue") |> 
    print()
  # " "

  options(old_continue)
  getOption("continue") |> print()
}


# to save for next time, save an ".Rprofile" file and 
# copy/paste options(continue = "<whatever>"). This can be done for either
# a single project or for all projects. 
# 
# NOTE: if you use on "user" level, but a "project" level ".Rprofile" exists, 
# you will need to add `source("<path to user level>/.Rprofile")` to use. When R
# starts up, it will only use one ".Rprofile" and it will be the one closest
# to the project.
# Take a look: ?Startup

# 1. project level 
  # This will only affect the current user's computer unless ".Rprofile" is 
  # controlled with git
  # usethis::edit_r_profile(scope = "project")
  # copy/paste options(continue = "<whatever>")

# 2. global level
  # This will only affect the current user's computer
  # usethis::edit_r_profile(scope = "user")
  # copy/paste options(continue = "<whatever>")

# ============================================================================ #
# ---- Set option `prompt` to something else ----
# ============================================================================ # 
# This options sets what is waiting in the console before you start typing.
# The default is "> ". You can change the default in the same way as "continue".

# rstudio default
{
  old_prompt <- getOption("prompt") # whatever its currently set to
  options(prompt = "> ") # set default in case it's already changed
  getOption("prompt") |>
    print()
  # "+ "
}

# change default, but this won't save if restart R
{
  options(prompt = " ")
  getOption("prompt") |>
    print()
  # " "
  options(old_prompt)
  getOption("continue") |> print()
}

# to save for next time, save an ".Rprofile" file and 
# copy/paste options(prompt = "<whatever>"). This can be done for either
# a single project or for all projects. 
# 
# NOTE: if you use on "user" level, but a "project" level ".Rprofile" exists, 
# you will need to add `source("<path to user level>/.Rprofile")` to use. When R
# starts up, it will only use one ".Rprofile" and it will be the one closest
# to the project.
# Take a look: ?Startup

# 1. project level 
  # This will only affect the current user's computer unless ".Rprofile" is 
  # controlled with git
  # usethis::edit_r_profile(scope = "project")
  # copy/paste options(prompt = "<whatever>")

# 2. global level
  # This will only affect the current user's computer
  # usethis::edit_r_profile(scope = "user")
  # copy/paste options(prompt = "<whatever>")  

# This more advanced examples is from: 
#   <https://lapsedgeographer.london/2020-11/custom-r-prompt/>
# 
# Here, when a project is git controlled, you may have the prompt be the branch
# that you are currently on and check if your branch is ahead
# Format:
#   "[HH:MM <location of project directory> @<branch name> <other characters>] >" 
#   Other characters:
#     - ⬆ = file has been committed and can be pushed
#     - M = a file has been modified
#     - ✘ = untracked files exists and can be commited
#     - O = other files have some status
#     TODO: can add to other files like D = deleted files, R = renamed, etc
#
# 
# .First <- function() {
#   
#   chg_prompt <- function(...) {
#     
#     # get project path
#     proj_path <- here::here()
#     
#     # get git branch
#     git_branch <-
#       suppressWarnings(
#         system(
#           "git rev-parse --abbrev-ref HEAD",
#           ignore.stderr = TRUE,
#           intern = TRUE
#         )
#       )
#     
#     git_msg <- ""
#     
#     # if branch exists
#     if (length(git_branch) != 0) {
#       # initialize message
#       git_msg <- paste0(" ", proj_path, " @", git_branch)
#       
#       # extract status
#       git_status <-
#         suppressWarnings(
#           system(
#             "git status -sb",
#             ignore.stderr = TRUE,
#             intern = TRUE
#           )
#         )
#       
#       # check if ahead
#       if (any(grepl("ahead", git_status))) {
#         git_msg <- paste(git_msg, "⬆︎")
#       }
#       
#       # check if modified files
#       if (any(grepl("^ M", git_status[-1]))) {
#         git_msg <- paste(git_msg, "︎M")
#       }
#       
#       # check if untracked
#       if (any(grepl("^??", git_status[-1]))) {
#         git_msg <- paste(git_msg, "✘")
#       }
#       
#       # check other
#       if (any(!grepl("ahead|^ M|^??", git_status[-1]))) {
#         git_msg <- paste(git_msg, "O")
#       }
#     }
#     
#     console_msg <-
#       paste0(
#         "[",
#         format(Sys.time(), "%H:%M"),
#         git_msg,
#         "] > "
#       )
#     
#     
#     options(prompt = console_msg)
#     
#     invisible(TRUE)
#     
#     # ---- end of function chg_prompt
#   }
#   
#   chg_prompt()
#   
#   addTaskCallback(chg_prompt)
#   
#   options(continue = " ")
# }



# ============================================================================ #
# ---- Excel Date and Time Conversion ----
# ============================================================================ # 
# added Sept 13, 2023
#  
# Convert excel storage of date to a date object
# i.e. 44639 to 2022-03-19
# 
# Convert excel storage of time to a time object
# 0.48263889 to 11:35:00 GMT
# 
# Combine date and time to date-time
# i.e. 2022-03-19 and 11:35:00 GMT to 2022-03-19 11:35:00 GMT/UTC
{
  library(magrittr)
  # 2022-03-19 11:35:00 GMT/UTC
  
  # 2022-03-19
  ex_date <- 
    44639 %T>% # from excel
    print() %>% 
    janitor::excel_numeric_to_date() %T>% 
    print()
  
  # 11:35:00 GMT
  ex_time <- 
    (0.48263889 + 1 ) %T>% # from excel
    print() %>% 
    janitor::excel_numeric_to_date(include_time = TRUE, tz = "GMT") %T>% 
    print() %>% 
    hms::as_hms() %T>% 
    print()
  
  
  lubridate::ymd_hms(glue::glue("{ex_date} {ex_time}"))
  pacman::p_unload(magrittr)
}

# ============================================================================ #
# ---- cliExtra ----
# ============================================================================ #  
# 2021-09-13
# may be an alternative to using if (verbose) print("message")
# bsically if set cliExtras::cli_quiet(quiet = TRUE) then no messages will be
# displayed when called from `cli`
# this does not affect `message()` or `print()`
test_fn <- function(verbose = FALSE) {
  cliExtras::cli_quiet(quiet = verbose)
  cli::cli_alert_info(
    "{.arg verbose} is {.val {verbose}}"
  )
  
  cli::cli_alert_info(
    "hi how are ya? {.kbd ENTER}"
  )
  
  message("message test")
  print("print test")
}


test_fn(FALSE)
test_fn(TRUE)

rm(test_fn)

# ============================================================================ #



# ============================================================================ #
# ---- Using .Renviron for Global Variables ----
# ============================================================================ #
# 2021-09-13
# From: <https://www.dartistics.com/renviron.html>
# Also good info: <https://rstats.wtf/r-startup.html>
# .Renviron is a file that can be used to store environment variables
# NOTE: make sure to add `.Renviron` to `.gitignore` when using Git

# To setup:
if (FALSE) {
  
  usethis::edit_r_environ("project")
  # use "user" if you want to set .Renvrion for all projects on your computer
  # and will only access them if using the `Sys.getenv()` function
  # usethis::edit_r_environ("user")
  
  # add the following line and a comment as to what it is:
  environment_parameter = "<key value pair>"
  # save and restart R
  
  # to use:
  Sys.getenv("environment_parameter")
  
  # OR this can be set as a variable in the global environment
  # NOTE: you may add this to `.Rprofile` to have it load automatically when
  # restarting R
  global_var <- Sys.getenv("environment_parameter")
  global_var
  
}


# ============================================================================ #



# ============================================================================ #
# ---- Change GitHub Language ----
# ============================================================================ #
# Dec 06, 2023
# Sometimes projects are labeled as `HTML` if using `Rmd` or `Qmd`
# A way around this is to create a `.gitattributes` file to ignore everything
# except `.R`
# Info from:
# <https://stackoverflow.com/questions/34713765/github-changes-repository-to-the-wrong-language>

if (FALSE) {
  # create file
  fs::file_create(here::here(".gitattributes"))
  
  # add code to file
  cat(
    "* linguist-vendored",
    # "*.<change the prog lang here> linguist-vendored=false", 
    "*.R linguist-vendored=false", 
    sep    = "\n",
    file   = here::here(".gitattributes"),
    append = TRUE
  )
  
}

# ============================================================================ #


# ============================================================================ #
# ---- Benchmark with Equal Results ----
# ============================================================================ #
# Dec 14, 2023
# `bench::mark()` requires results to be EQUAL
# will run:
bench::mark(
  # method 1: mean
  "mean" = {
    x <- 1:500
    sum(x) / length(x) # result 250.5
  },
  # method 2: median, that will not be equal
  "median" = {
    x <- 1:500
    median(x) # result 250
  }
)
# will not run:
bench::mark(
  # method 1: mean
  "mean" = {
    x <- 1:500
    sum(x) / length(x) # result 250.5
  },
  # method 2: median, that will not be equal
  "median" = {
    x <- 1:501
    median(x) # result 251
  }
)
# ============================================================================ #


# ============================================================================ #
# ---- Benchmark with Non-equal Results ----
# ============================================================================ #
# Dec 13, 2023
# I used this to test 2 saving methods to see speed difference.
# 
# using `microbenchmark::microbenchmark()`
# - Note: this is different than using `bench::mark()` because the results do 
#   not need to be equal unless added `check = "<flag>"`
# 

microbenchmark::microbenchmark(
  times   = 1000L,
  control = list(order = "inorder", warmup = 20),
  # check = "equal",      # check equality using `all.equal()`
  # check = "equivalent", # check equality using `all.equal()`
  # check = "identical",  # check equality using `identical()`
  
  # method 1: mean hard
  "mean-hard" = {
    x <- 1:500
    x <- sum(x) / length(x) # result 250.5
  },
  
  # method 2: mean easy
 "mean-easy" = {
    x2 <- 1:500
    mean(x2) # result 250.5
  },
 
  # method 3: median, NOTE: will not be equal
  "median" = {
    x3 <- 1:501
    median(x3) # result 251
  }
)

# Dec 14, 2023
# alternate using `bench::workout_expressions()`
# - this will time multiple expressions for ONE iteration, but need to be 
#   wrapped in `list()`
# - To add a name to the `exprs` result, either name each element in the list or 
#   give a vector to the `description` with each name 
{
  loop <- 10000
  bench::workout_expressions(
    list(
      # method 1A: mean
      "mean-hard-one-iter" = {
        set.seed(1)
        x2 <- runif(500)
        x2 <- sum(x2) / length(x2)
      },
      # method 1B: mean loop
      "mean-hard-loop" = {
        set.seed(1)
        for (i in seq(loop)) {
          x3 <- runif(500)
          x3 <- sum(x3) / length(x3)
        }
      },
      # method 2A: mean easy
      "mean-easy-one-iter" = {
        x4 <- runif(500)
        x4 <- mean(x4) # result 250.5
      },
      # method 2B: mean easy loop
      "mean-easy-loop" = {
        for (i in seq(loop)) {
          x5 <- runif(500)
          x5 <- mean(x5) # result 250.5
        }
      },
      # method 3: median
      "median" = {
        for (i in seq(loop)) {
          x6 <- runif(500)
          x6 <- median(x6)
        }
      }
    )
  ) |>
    print()
  rm(list = c("loop", "i", paste0("x", c(2:6))))
}
# ============================================================================ #

# ============================================================================ #
# ---- Export Variables from Function ----
# ============================================================================ #
# Dec 15, 2023
# WARNING: 
# ~~~~~~~~
# this is probably NOT recommended to do because will use the `<<-` or `assign()` 
# to export variables in a list to a named variables in the global environment.
# ~~~~~~~~~~~~~~~~~
#
# This was motivated to have something similar to *Python* or *MatLab* when 
# functions are run that you can assign the output to a variable.
# 
# This will export variables from a list to the global environment. The most 
# useful case is using a list output from a function as input.
# 
# NOTE: 
# - This will only go one level deep but, either rerun or maybe edit to go 
#   deeper. 
# 
if (FALSE) {
  create_vars_from_fun <- function(input_list, prefix = TRUE, overwrite = FALSE) {
    
    # check input is a list
    if (!is.list(input_list)) {
      stop("Input must be a list")
    }

    # check list is a named variable from the parent environment
    # if not, will set to "anon" = anonymous
    if (!exists(deparse(substitute(input_list)), envir = parent.frame())) {
      x_name <- "anon"
    } else {
      x_name <- deparse(substitute(input_list))
    }
    
    # check list names is not `NULL`
    var_names <- names(input_list)
    var_check <- var_names == "" # check if any list names are ""
    
    if (is.null(var_names)) {
      # all unnamed elements of list are given sequential numbers  
      var_names <- paste0("var_", seq_along(input_list)) 
    } else if (any(var_check)) {
      # unnamed elements of list are given position numbers
      var_names[which(var_check)] <- paste0("var_", which(var_names == ""))
    }
    
    # check prefix is a character or logical
    if (is.character(prefix)) {
      var_names <- paste(prefix, var_names, sep = "_") # custom name
    } else if (prefix) {
      var_names <- paste(x_name, var_names, sep = "_") # using input var name
    } else {
      warning("`prefix = FALSE`: variables create are the names of the the list (not recommended).")
      var_names <- var_names # no prefix
    }
    
    # check vars to be created don't already exist in the parent environment
    check_vars <- var_names %in% ls(envir = parent.frame())
    
    if (!overwrite && any(check_vars)) {
      var_names[which(check_vars)]
      stop(
        paste(
          "\b\bCannot create variables that already exist in global environment:",
          paste(var_names[which(check_vars)], collapse = "\n"),
          "\n------------------",
          "Set `overwrite = TRUE` to overwrite existing variables",
          "-- OR --\nChange the `prefix` to a new name",
          "------------------\n\n",
          sep = "\n"
        )
      )
    } else if (overwrite && any(check_vars)) {
      warning(
        paste(
          "\b\bOverwriting variables that already exist in global environment:",
          paste(var_names[which(check_vars)], collapse = "\n"),
          "\n------------------",
          "Set `overwrite = FALSE` to not overwrite existing variables",
          "-- OR --\nChange the `prefix` to a new name",
          "------------------\n",
          sep = "\n"
        )
      )
    }

    input_list <- setNames(input_list, var_names)

    # assign variables to global environment
    for (i in var_names) {
      assign(i, input_list[[i]], envir = parent.frame())
    }
  
    # alternative to `assign()` is to use `<<-` but this is not recommended
    # for (i in var_names) {
    #   do.call("<<-", list(i, input_list[[i]]))
    # }
     
    return(invisible())
    
    # ---- end of function create_vars_from_fun
    
  }

  # example
  # linear model using mtcars
  mod1 <- lm(mpg ~ wt, data = mtcars)
  names(mod1)
  create_vars_from_fun(mod1)
  create_vars_from_fun(mod1)
  create_vars_from_fun(mod1, overwrite = TRUE)
  create_vars_from_fun(mod1, "test")
  create_vars_from_fun(mod1, FALSE)
  
  name_var <- list(c(1, 2), c("foo", "bar"))
  create_vars_from_fun(name_var)
  create_vars_from_fun(list(c(1, 2), c("foo", "bar")))

  # clean up  
  rm(list = ls())
  
  test2 <- list(x = c(1,2), c("foo", "bar"))
  create_vars_from_fun(test2)
  create_vars_from_fun(list(x = c(1,2)))
}

# ============================================================================ #


# ============================================================================ #
# ---- Tip for `rstudioapi` to Modify Documents ----
# ============================================================================ #
# Dec 17, 2023
# this works best when inside a function
# 
# `rstudioapi::getActiveDocumentContext()` export: 
# `id`        = the file name ID used to identify which file to edit
# `path`      = the path to this file
# `contents`  = is all the lines within this document
# `selection` = the lines selected (i.e. the next line with text)
#     `range`   = the start and end range
#     `text`    = the text (idk what this means because it's always "")
# 
# The range defined is where the modification would take place, but needs to be
# structured. A quick way would be to use `document_position` within 
# `rstudioapi::document_range` to set the start and end based on some text
# 
if (FALSE) {

  test_fun <- function(text = "") {
    context <- rstudioapi::getActiveDocumentContext()

    lign_start <- min(grep("test_fun", context$contents))
    lign_end   <- max(grep("test_fun", context$contents))

    range <-
      rstudioapi::document_range(
        rstudioapi::document_position(lign_end + 1, 1),
        rstudioapi::document_position(lign_end + 1, 77777)
      )

    cat("\n\n--- The Contents of `getActiveDocumentContext` ---\n\n")
    print(context)
    cat("\n\n--- The structure of `getActiveDocumentContext` ---\n\n")
    print(str(context))
    cat("\n\n--- The location for modification ---\n\n")
    print(range)
    cat(
      "\n\n--- To Modify where the text ends up ---\n\n",
      "rstudioapi::document_range(",
      "\n  `rstudioapi::document_position(<edit row start>, <edit column start>)` <--- here",
      "\n  `rstudioapi::document_position(<edit row end >, <edit column end>)`    <--- here",
      "\n)"
    )

    rstudioapi::modifyRange(
      location = range,
      text     = paste("#", text),
      id       = context$id
    )
    # ---- end of function test_fun
  }

  test_fun("here is some text")
# here is some text
}
# ============================================================================ #


# ============================================================================ #
# ---- Read/Save Binary ----
# ============================================================================ #
# Dec 18, 2023
# This is to not add carriage returns like if write as text file. 

# Read Binary
# readr::read_file(<file path>) # read data as vector with length 1
# readr::read_lines(<file path>) # read data as vector with length = line #

# Save Binary
# readr::write_file(<object to save>, <file path to save>)

# alternate:
# `wb` = write binary
# save_file <- file(<new_file>, open = "wb") 
# cat(<object>, sep = "\n", file = save_file)
# close(save_file)

# ============================================================================ #


# ============================================================================ #
# ---- Expressions Tips ----
# ============================================================================ #
# Apr 29, 2024
# Some difficulties occur when using `expression()` to create nicely formatted 
# units
# Here are two examples to help with this.
#   1. Using strings to expressions
#     - this typically won't work if you send a string to an expression so can 
#       mitigate this using`parse()`, but need to remove any spaces
{
  ex1 <- 
  "degree*C alpha mg L^-1" |>
  stringr::str_replace_all( "\\s", "~") |>
  parse(text = _)
  print(ex1)
  cat("\n\n")
#   2. When using percent (%)
#     - this need an extra step where you quote the percent before passing to 
#       parse 
ex2 <- 
"%" |>
  stringr::str_replace_all( "\\s", "~") |>
  stringr::str_replace_all( "%", "'%'") |>
  parse(text = _) 
  print(ex2)
  
  plot(1:10, main = ex1)
  plot(1:20, main = ex2)
}
# ============================================================================ #


# ============================================================================ #
# ---- Set Date to Same Year for Plotting ----
# ============================================================================ #
# Jun 28, 2024
# https://stackoverflow.com/questions/48288429/plotting-a-time-series-in-ggplot-with-lines-grouped-by-year
{
  librarian::shelf(
    quiet = TRUE,
    librarian,
    dplyr, lubridate, ggplot2, purrr
  )
  
  # function used to set all date to same year
  same_year <- function(x, .year = 2000) {
    year(x) <- .year
    x
  }
  
  
  kobe_data <- 
    lubridate::lakers |>
    filter(player == "Kobe Bryant") |>
    mutate(
      .after = 1,
      date = ymd(date),
      year = year(date),
      date2 = map(
        .x = date,
        .f = same_year
      )
    ) |>
    tidyr::unnest(date2) |>
    mutate(point2 = cumsum(points), .by = year) |>
    print()
  
  # using `date2` column created to set to same year 
  (kobe_data |>
    ggplot() +
    geom_line(
      aes(x = date2, y = point2, color = as.character(year(date)))
    ) +
    labs(
      x = NULL,
      y = "Cumulative Points",
      color = "Year",
      title = "Using `purrr:map`"
    ) +
    scale_x_date(
      date_labels = "%b",
      date_breaks = "1 month"
    ) +
    theme_bw()
  ) |>
    print()
  
  # alternate version: use `same_year` function in `ggplot()` call
  (kobe_data |>
    ggplot() +
    geom_line(
      aes(x = same_year(date), y = point2, color = as.factor(year(date)))
    ) +
    labs(
      x = NULL,
      y = "Cumulative Points",
      color = "Year",
      title = "Using `same_year()`"
    ) +
    scale_x_date(
      date_labels = "%b",
      date_breaks = "1 month"
    ) +
    theme_bw()
  ) |>
    print()


  pacman::p_unload("all")
  rm(same_year, kobe_data)
}


# ============================================================================ #

# ============================================================================ #
# ---- Convert .Rmd or Qmd to .R file to use `prefixer::count_calls` ----
# ============================================================================ #
# Jul 11, 2024

# from <https://stackoverflow.com/questions/71183578/how-to-extract-all-code-from-an-rmarkdown-rmd-file>

# convert a `.Rmd` or `.qmd` file to `.R`
# *** NOTE: all chunks need to be uniquely named or blank ***

# knitr::purl(
#   input         = "<file name>.<Rmd or qmd>", 
#   output        = "<output file name>.R", 
#   documentation = 0)

# then use prefixer::count_calls
if (FALSE) {
  # here::here("scripts", "<output file name>.R") |>
  here::here("scripts", "quick_tips.R") |>
    prefixer::count_calls() |>
    tibble::as_tibble() |>
    dplyr::arrange(package) |>
    View()
}
# ============================================================================ #

