
# check if package is installed, and install if not
is_inst <- function(pkg, prt = FALSE) {
  pkg <- as.data.frame(cbind(package = pkg, has = NA))
  for (i in 1:nrow(pkg)) {
    pkg[i, 2] <- nzchar(system.file(package = pkg[i, 1]))
  }
  if (prt) print(pkg)
  
  return(pkg)
}

packes <- c("bestglm", "fitdistrplus", "NADA", "survminer", "car", "Kendall",
            "nlme", "vegan", "cenGAM", "mgcv", "perm", "EnvStats", "multcomp", 
            "lattice", "rms", "npsurvSS", "reshape2", "Hmisc", "ggplot2", 
            "jmuOutlier", ## has permutation bases t-test
            "MXM", ## Has permutation based correlation matrix
            "gridExtra", "egg", "gtable", "ggplot2", "plyr", "scales", "readxl")

packes_has <- is_inst(packes, TRUE) 
packes_no  <- packes_has[packes_has$has == FALSE,]
install.packages(packes_no[,1])

print(is_inst(packes))

# check if same packages installed in 4.1 and 4.2
# v41 <- 
#   here::here("C:/", "Users", "spd19", "Documents", "R", "win-library", "4.1") |> 
#   fs::dir_ls() |>
#   basename() |>
#   tibble::tibble(package = _) |>
#   dplyr::mutate(old = "4.1")
# 
# v42 <- 
# here::here("C:/", "Users", "spd19", "AppData", "Local", "R", "win-library", "4.2") |> 
#   fs::dir_ls() |>
#   basename() |>
#   tibble::tibble(package = _) |>
#   dplyr::mutate(new = "4.2")
# 
# dplyr::left_join(
#   v42, v41
# ) |> View()
# 
# (Sys.getenv("PATH") |> stringr::str_split(";"))[[1]]  |> cat(sep = "\n")
