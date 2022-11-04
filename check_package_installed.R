
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