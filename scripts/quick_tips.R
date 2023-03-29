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
fs::dir_create(here::here("data", "pkg_ls"))
saveRDS(pacman::p_lib(), 
        file = here::here("data", "pkg_ls",
                    glue::glue("packages",
                         format(Sys.time(), '_%Y-%m-%d'),
                         ".rds"))
        )

# read method for saveRDS
x <- 
  (fs::dir_ls(here::here("data", "pkg_ls"),
              regexp = "packages") |>
  sort(decreasing = TRUE))[1] |>
  readRDS()

x <- 
  devtools::package_info("installed", include_base = TRUE) |>
  tibble::as_tibble()

# ============================================================================ #
# ---- detect OS ----
# ============================================================================ #
# might be helpful when trying packages on other system
pacman::p_detectOS()

# ============================================================================ #
# ---- detect if using R interactive or not ----
# ============================================================================ #
# can be useful if running script in background and can't give input info
interactive()

# ============================================================================ #
# ---- wrap code in {} ----  
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
# this can be var, functions, etc
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
  
  with(data.frame(u = c(5,10,15,20,30,40,60,80,100),
                  lot1 = c(118,58,42,35,27,25,21,19,18),
                  lot2 = c(69,35,26,21,18,16,13,12,12)),
       list(summary(glm(lot1 ~ log(u), family = Gamma)),
            summary(glm(lot2 ~ log(u), family = Gamma))))
  
  # instead of 
  x <-
    data.frame(u = c(5,10,15,20,30,40,60,80,100),
               lot1 = c(118,58,42,35,27,25,21,19,18),
               lot2 = c(69,35,26,21,18,16,13,12,12))
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
# useful for na in read_xlsx(., na = c())
{
  library("magrittr")
  # repeats `-` and adds `-` * n 
  strrep("-", 1:20) 
  na_skip <- c("NA", "Skipped", "skipped", "na", "n/a", "n./a", "n.a", "Flow", "na",
               strrep("-", 1:20))
  print(strrep("-", 1:20) )
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