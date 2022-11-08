
# ---- wrap code in {} ----  
# will run as block, instead of line by line
{
  print(c(1,2,3))
  plot(cars, main = "Stopping Distance versus Speed")
  lines(stats::lowess(cars))
}

# ---- make line with text as output to console ----
{
  header1 <- cli::rule(
    left = crayon::bold("Something important"),
    right = crayon::blurred("something else"))
  header2 <- cli::rule(center = crayon::yellow("Hello World"))
  print(header1)
  print(header2)
}

# ---- print LaTeX in plots tab ----
# save old par
oldpar <- par(no.readonly = TRUE)
{ 
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
}


# ---- attach() ----
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
  detach(data1)
  rm(data1)
}

# ---- read multiple sheets from excel ----
# Need path, then read sheet, map over each sheet from the path, then conver to
# tibble. Can use left_join afterwards if wanted
# TODO: add filter for names if needed
{  
  library("magrittr")
  path <- readxl::readxl_example("datasets.xlsx")
  path <- path %>% 
    readxl::excel_sheets(.) %>% 
    purrr::set_names(., .) %>% 
    purrr::map(readxl::read_excel, path = path) %>%
    tibble::as_tibble_col(., column_name = "datasets")
  print(path)
  print(path$datasets$iris)
  print(path$datasets$mtcars)
  
  rm(path)
  detach("package:magrittr")
}
