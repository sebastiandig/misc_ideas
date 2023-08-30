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

# 1. project level 
  # This will only affect the current user's computer unless ".Rprofile" is 
  # controlled with git
  usethis::edit_r_profile(scope = "project")
  
# 2. global level
  # This will only affect the current user's computer
  usethis::edit_r_profile(scope = "user")

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
  
  # 1. project level 
  # This will only affect the current user's computer unless ".Rprofile" is 
  # controlled with git
  usethis::edit_r_profile(scope = "project")
  
  # 2. global level
  # This will only affect the current user's computer
  usethis::edit_r_profile(scope = "user")
  
# This more advanced examples is from: 
#   <https://lapsedgeographer.london/2020-11/custom-r-prompt/>
# 
# Here, when a project is git controlled, you may have the prompt be the branch
# that you are currently on and check if your branch is ahead
#   
git_branch <- suppressWarnings(system("git rev-parse --abbrev-ref HEAD",
                                      ignore.stderr = TRUE, intern = TRUE))

if (length(git_branch) != 0) {
  git_msg <- paste0(" @", git_branch)
  git_status <- suppressWarnings(system("git status -s",
                                        ignore.stderr = TRUE, intern = TRUE))
  git_ahead <- suppressWarnings(system("git status -sb",
                                       ignore.stderr = TRUE, intern = TRUE))
  git_ahead_chk <- grepl("ahead", git_ahead)
  
  git_behind <- suppressWarnings(system("git status -sb",
                                       ignore.stderr = TRUE, intern = TRUE))
  if (any(git_ahead_chk)) {
    git_msg <- paste0(git_msg, " ⬆︎")
  }
  
  if (any(git_behind_chk)) {
    git_msg <- paste0(git_msg, " ↓")
  }
  
  if (length(git_status) != 0) {
    git_msg <- paste0(git_msg, " ✘")
  }


  } else {
    git_msg <- ""
    }

console_msg <- paste0("[",
                      format(Sys.time(), "%H:%M"),
                      git_msg,
                      "] > ")


options(prompt = console_msg)
  