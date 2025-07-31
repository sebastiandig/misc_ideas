#' Download AVISO+ Data
#'
#' This function aids in downloading AVISO+ data from Copernicus. This requires
#' previous download and log in of `copernicusmarine.exe` from: 
#' <https://help.marine.copernicus.eu/en/articles/10750437-copernicus-marine-toolbox-executable-no-installation>
#'
#' @param months Months of data to download.
#' @param vars Variables to download
#' "adt", "err_sla", "err_ugosa", "err_vgosa", "flag_ice", "sla", 
#' "tpa_correction", "ugos", "ugosa", "vgos", "vgosa"
#' 
#' @param bounds Bounding box consisting of c(xmin, xmax, ymin, ymax).
#' @param path_copernicusmarine Path to `copernicusmarine.exe` file.
#' @param path_save Path for saved data.
#' @param download Logical to download or not
#' @param overwrite Logical to overwrite existing data 
#'
#' @return Nothing, side effect of download files separated into months
#' @examples
#' # ADD_EXAMPLES_HERE
aviso_download <- function(
    months,
    vars,
    bounds,
    path_copernicusmarine,
    path_save,
    download = FALSE,
    overwrite = FALSE) {
  month_list <- vector("list", length(months))
  names(month_list) <- format(months, "%B_%Y")

  for (i in seq(length(months))) {
    date_i <- months[i]
    date_range <- c(date_i, date_i + months(1) - days(1))

    message(paste("\n-----\n\nNew Month:", format(date_i, "%B %Y")))
    cat(as.character(date_range), sep = "\n")

    month_list[[i]] <- c(as.character(date_range), paste(date_range, collapse = "-"))

    prev_files <-
      dir_ls(path_save) %>%
      str_detect(paste(date_range, collapse = "-")) %>%
      any()

    if (prev_files & !overwrite) {
      message("Skipping because date range exists!")
      next
    }

    # set date range
    date_range <-
      date_range %>%
      as_datetime() %>%
      format_ISO8601()

    # set up command to download
    command <-
      paste(
        shQuote(path_copernicusmarine),
        "subset",
        "--dataset-id cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D",
        paste("--variable", vars, collapse = " "),
        "--start-datetime",    date_range[1],
        "--end-datetime",      date_range[2],
        "--minimum-longitude", bounds["xmin"],
        "--maximum-longitude", bounds["xmax"],
        "--minimum-latitude",  bounds["ymin"],
        "--maximum-latitude",  bounds["ymax"],
        "-o",
        path_save
      )

    if (download) {
      message("Downloading AVISO+ Date")

      # execute command to download files
      system(command)
    } else {
      message("Not downloading AVISO+ data")
    }
  }
}


#' Load AVISO+ Data
#'
#' FUNCTION_DESCRIPTION
#'
#' @param data_path Path to data
#' @param .by Number of days skip
#'
#' @return A list with a tibble and gridded data 
#' @examples
#' # ADD_EXAMPLES_HERE
aviso_load <- function(data_path, .by = 1) {
  seq_nc <- expression(seq(1, nc_len, by = .by))

  multi_day <- vector("list", length(data_path))

  for (j in seq(data_path)) {
    # open data and extract variables
    ssh_nc <- nc_open(data_path[j])

    attributes(ssh_nc$var) # show vars names
    attributes(ssh_nc$dim) # show vars names

    # determine number of time dimensions
    nc_len <- ssh_nc$dim$time$len
    dat_grid_temp <- vector("list", nc_len)

    # extract dates
    nc_date <- as_date(ncvar_get(ssh_nc, "time"), origin = as_date(ssh_nc$dim$time$units))

    for (i in eval(seq_nc)) {
      # extract variables
      adt  <- ncvar_get(ssh_nc, "adt")[, , i]
      vgos <- ncvar_get(ssh_nc, "vgos")[, , i]
      ugos <- ncvar_get(ssh_nc, "ugos")[, , i]
      lat  <- ncvar_get(ssh_nc, "latitude")
      lon  <- ncvar_get(ssh_nc, "longitude")

      # create grid data
      ssh_grid <-
        expand.grid(lon = lon, lat = lat) %>%
        mutate(
          adt     = as.vector(adt), # absolute dynamic topography
          vgos    = as.vector(vgos), # north geostrophic velocity
          ugos    = as.vector(ugos), # east geostrophic velocity
          vel_mag = sqrt(vgos^2 + ugos^2) # calculate current velocity
        )

      dat_grid_temp[[i]] <- ssh_grid
    }

    # set list names to date
    names(dat_grid_temp) <- nc_date

    # close `.nc` file
    nc_close(ssh_nc)

    dat_grid_temp

    multi_day[[j]] <-
      dat_grid_temp %>%
      Filter(Negate(is.null), .)
  }

  # flatten list from months to days
  dat_grid <-
    multi_day %>%
    list_flatten()

  # convert to tibble
  grid_tibble <-
    dat_grid %>%
    list_rbind(names_to = "date")
  
  return(
    list(
      "gridded" = dat_grid,
      "tibble"  = grid_tibble
      )
  )
}
