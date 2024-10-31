if (!nzchar(system.file(package = "librarian"))) 
  install.packages("librarian")

librarian::shelf(
  quiet = TRUE,
  librarian, conflicted, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
  forcats, lubridate, glue, fs, magrittr, here,
  
  # additional packages, if from GitHub, include username
  oce
)

conflicts_prefer(
  dplyr::filter, 
  dplyr::select
  )


ctd_file <- rstudioapi::selectFile(path = "~/Box/ctd_data/")
# ctd_file <- rstudioapi::selectDirectory(path = "~/Box/ctd_data/")
ctd_file

# test <- read.ctd(ctd_file, type = "WOCE", station = "16")

test <- read_csv(ctd_file)

test_ctd <-
  as.ctd(
    salinity = test$sea_water_salinity,
    temperature = test$sea_water_temperature,
    pressure = test$depth,
    ship = "Walton Smith",
    station = "21/LK"
  )
test_ctd %>%
  plotScan()

test_ctd2 <- oceSetData(
  test_ctd,
  name = "fluorescence",
  value = test$chlorophyll_fluorescence,
)

plotProfile(test_ctd)
plotProfile(test_ctd2, "fluorescence")

ctd_load <- function(file) {
  
  test <- read_csv(file, show_col_types = FALSE)
  
  test_ctd <-
    as.ctd(
      salinity = test$sea_water_salinity,
      temperature = test$sea_water_temperature,
      pressure = test$depth,
      ship = "Walton Smith",
      station = "21/LK"
    )
  

  
  test_ctd2 <- oceSetData(
    test_ctd,
    name = "fluorescence",
    value = test$chlorophyll_fluorescence,
  )
  
    # ---- end of function ctd_load
}



ctd_file <- rstudioapi::selectDirectory(path = "~/Box/ctd_data/")

ctd <-
  ctd_file %>%
  dir_ls()  %T>% 
  print() %>%
  map(
    ctd_load
  )

for (i in seq(length(ctd))) {
# for (i in 6:10) {
  ctd[[i]] %>%
    plotScan()
  # plotProfile(ctd[[i]])
  # plotProfile(ctd[[i]], "fluorescence")
}
