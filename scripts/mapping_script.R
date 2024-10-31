# NOTE: modified version of `map_files_dwnlod.R` script to send

##%######################################################%##
#                                                          #
####         Download World Shapefiles for Maps         ####
#                                                          #
##%######################################################%##
#' Download World Shapefiles for Maps
#'
#' This function will download topography, state lines, and coastline from
#' NOAA if it does not exist at the designated path.
#' 
#' The topography should be re-downloaded for new projects since it's only a 
#' subset of the world based on the extent given.
#' 
#' The land boundaries only need to be downloaded once, so specifying where it's 
#' located may be a better option. 
#' 
#' Location of data:
#' Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), 
#' (Ice Sheet Surface) from https://coastwatch.pfeg.noaa.gov/erddap/griddap/
#' 
#' Global Self-consistent, Hierarchical, High-resolution Geography Database
#' (GSHHG) from https://www.ngdc.noaa.gov/mgg/shorelines/
#'
#' @param path_land File directory to be used for coastline files. This may need 
#'                  to be created or already exists. This is where the land map 
#'                  will be downloaded to if it doesn't exists.
#' @param path_topo File directory to be used for topgraphy file. This may need 
#'                  to be created or already exists. This is where the topo map 
#'                  will be downloaded to if it doesn't exists. This will 
#'                  require a spatial extent to subset the data.
#' @param extent Spatial extent for the topography data.
#'               Format: exnt <- c(xmin = -82,    # West
#'                                 xmax = -80,    # East
#'                                 ymin = 24.25,  # South
#'                                 ymax = 25.75   # North
#'                                 ) 
#' @param file_suffix Suffix to end of topography name
#' @param .timeout Number of seconds to download world map before error timeout 
#'
#' @author Sebastian Di Geronimo (2023-01-11)
#' 
#' @return NULL, saves files
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
world_download <- function(
    path_land   = NULL,
    path_topo   = path_land,
    extent      = NULL,
    file_suffix = NULL,
    use_suffix  = NULL,
    .timeout    = 300) {
  # ========================================================================== #
  # ---- Load libraries
  # ========================================================================== #
  library("here")
  library("fs")
  library("rerddap")
  library("cli")
  library("rlang")
  library("magrittr")
  
  pre_timeout <- getOption('timeout')
  on.exit(options(timeout = pre_timeout))
  options(timeout = .timeout)
  # ========================================================================== #
  # ---- Download Topography to the Specified Path ---- #
  # ========================================================================== #
  cli_h1("Topography Data")
  
  if (is.null(path_topo)) {
    # ---- skip if path to topography is NULL
    cli_alert_warning(c(
      "Skipping Topography nc file because ",
      "{.var path_topo} = {.var NULL}.\n"
    ))
  } else {
    
    if (!is.null(use_suffix) 
        & isTRUE(str_detect(use_suffix, "(?i)extent"))
        & (!is.null(extent) | length(extent) != 4)) {
      file_suffix <- 
        extent %>%
        tibble(extent = .) %>%
        mutate(
          type = rep(c("lon","lat"), each = n()/2),
          label = case_when(
            str_detect(type, "lon") & exnt < 0 ~ glue("{extent}W"),
            str_detect(type, "lon") & exnt > 0 ~ glue("{extent}E"),
            str_detect(type, "lat") & exnt > 0 ~ glue("{extent}N"),
            str_detect(type, "lat") & exnt < 0 ~ glue("{extent}S")
          ),
          label = str_remove(label, "-") 
        ) %$% 
        str_c(label, collapse = "_") %>%
        str_replace_all("(^|\\.)", "_") 
    }
    
    topo_file <- here(
      path_topo,
      glue("etopo1{file_suffix}.nc",
           .null = ""
      )
    )
    
    if (file.exists(topo_file)) {
      # ---- skip if topography file exists
      cli_alert_info(c(
        "Topography data exists in {.file {path_topo}}.\n",
        "You may need to {.emph {col_red('check')}} the spatial",
        " extent."
      ))
    } else if (is.null(extent) | length(extent) != 4) {
      # ---- stop if extent is not correct
      cli_abort(c(
        "{.var extent} needs to have 4 values.",
        "x" = "You supplied {length(extent)} values.",
        "Format Example:",
        "exnt <- c(xmin = -82, xmax = -80, ymin = 24.25, ymax = 25.75)"
      ))
    } else {
      
      # ---- download topography data
      cli_alert_info(
        "Starting Topography Data Download to {.file {path_topo}}"
      )
      
      # create temp directory
      cli_alert_info("Creating a temporary folder in {.file {path_topo}}")
      dir_create(here(path_topo, "temp"))
      
      cli_alert_info(c(
        "Downloading Topography Data from:\n",
        "URL: {.url https://coastwatch.pfeg.noaa.gov/erddap/griddap/}\n",
        "Bounding Box:\n",
        sprintf("\tLongitude: % 7.2f, % 7.2f\n", extent[1], extent[2]),
        sprintf("\tLatitude:  % 7.2f, % 7.2f\n", extent[3], extent[4])
        # "\tLatitude:  {extent[3]}, {extent[4]} \n"
        
      ))
      
      # ERDDAP extract and save
      griddap(
        info("etopo180"),
        latitude  = extent[3:4],
        longitude = extent[1:2],
        stride    = c(1, 1),
        fields    = "altitude",
        store     = disk(here(path_topo, "temp"))
      )
      
      # Move and rename .nc file
      file_move(
        dir_ls(here(path_topo, "temp"), regexp = "\\.nc$"),
        topo_file
      )
      
      # delete temp folder
      cli_alert_info("Deleting the temporary folder in {.file {path_topo}}")
      dir_delete(here(path_topo, "temp"))
      
      cli_alert_success("Downloaded etopo1.nc")
    }
  }
  
  # ========================================================================== #
  # ---- Download GSHHS Coastline Shapefile ---- #
  # ========================================================================== #
  cli_h1("Coastline Data")
  
  if (is.null(path_land)) {
    
    cli_alert_warning(c(
      "Skipping Coastline shapefile because ",
      "{.var path_land} = {.var NULL}.\n"
    ))
    
  } else {
    
    # download GSHHS shapefile if not already downloaded
    coast <-
      dir_ls(
        path    = path_land,
        recurse = TRUE,
        regexp  = "GSHHS_h_L1.shp"
      )
    
    if (!rlang::is_empty(coast)) {
      cli_alert_info("Coastline shapefile exists in {.file {path_land}}.\n") 
      
    } else {
      
      dir_create(path_land)
      
      
      cli_alert_info(c(
        "GSHHS_h_L1 Coastline data doesn't exists in ",
        "{.file {path_land}}"
      ))
      cli_alert_info(c(
        "Downloading Coastline Shapefile\nURL:",
        "{.url https://www.ngdc.noaa.gov/mgg/shorelines/}"
      ))
      
      temp <- tempfile()
      download.file(
        "ftp://ftp.soest.hawaii.edu/gshhg/gshhg-shp-2.3.7.zip",
        temp,
        method = "libcurl",
        mode = "wb"
      )
      
      unzip(exdir = path_land, temp)
      # delete temp folder and files
      unlink(temp)
    }
  } 
  
  return(invisible(NULL))
  # ---- End of `world_download` Function ----
}

##%######################################################%##
#                                                          #
####                  Load Map Objects                  ####
#                                                          #
##%######################################################%##
#' Load Map Objects
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .map_coast Path location for "GSHHS_h_L1.shp" shapefile
#' @param .map_state Path location for "WDBII_border_h_L2.shp" shapefile. 
#'                   Should be the same as `.map_coast`. Set to `NULL` if 
#'                   want to ignore.
#' @param .map_bath Path location for "etopo1.nc"
#' @param .map_file Name for coastline file (default = "etopo1.nc")
#' @param .extent Spatial extent for the topography data.
#'                Format: exnt <- c(xmin = -82,    # West
#'                                  xmax = -80,    # East
#'                                  ymin = 24.25,  # South
#'                                  ymax = 25.75   # North
#'                                  ) 
#'
#' @return List
#'         - coast_topo = an sf object for coastline 
#'         - state      = an sf object for state lines
#'         - bathy      = a date.frame object for bathymetry
#' 
#' @author Sebastian Di Geronimo (2023-04-27)
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
load_map_obj <- function(
    .map_coast, 
    .map_state = .map_coast,
    .map_bath = .map_coast, 
    .map_file = "etopo1.nc",
    .extent) {
  
  # ---- libraries
  library("sf")
  library("terra")
  library("fs")
  library("cli")
  
  
  # fixes issue with reading raster file using decimal degrees
  sf::sf_use_s2(FALSE)
  
  coast_file_name <- "GSHHS_h_L1.shp"
  state_file_name <- "WDBII_border_h_L2.shp"
  
  # ---- coastline ---- #
  cli::cli_h1("Coastline")
  cli::cli_alert_info("Searching: {.file {(.map_coast)}}")
  coast_topo <-
    dir_ls(
      path    = .map_coast,
      recurse = TRUE,
      regexp  = coast_file_name
    ) 
  
  if (rlang::is_empty(coast_topo)) {
    cli::cli_alert_warning(
      c(
        "Skipping: Couldn't find ",
        "{.var {col_red(coast_file_name)}}"
      )
    )
    coast_topo <- NULL
  } else {
    cli::cli_alert_info("Loading: {.file {coast_file_name}}")
    
    coast_topo <-
      coast_topo %>%
      st_read(.) %>%
      st_crop(
        .,
        st_bbox(
          .extent
        )
      ) %>%
      suppressMessages() %>%
      suppressWarnings()
  }
  
  # ---- state lines ---- #
  cli::cli_h1("State Lines")
  if (rlang::is_empty(.map_state)) {
    cli::cli_alert_warning(
      c(
        "Skipping State Lines:\n", 
        "{.var {col_red(\".map_state\")}} = {.var NULL}"
      )
    )
    state <- NULL
  } else {
    cli::cli_alert_info("Searching: {.file {(.map_state)}}")
    state <-
      dir_ls(
        path    = .map_state,
        recurse = TRUE,
        regexp  = state_file_name
      )
    
    if (rlang::is_empty(state)) {
      cli::cli_alert_warning(
        c(
          "Skipping: Couldn't find ", 
          "{.var {col_red(state_file_name)}}"
        )
      )
      state <- NULL
      
    } else {
      cli::cli_alert_info("Loading: {.file {state_file_name}}")
      state <-
        state %>%
        st_read(.) %>%
        st_crop(
          .,
          st_bbox(
            .extent
          )
        ) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
  }
  # ---- bathymetry ---- #
  cli::cli_h1("Bathymetry")
  cli::cli_alert_info("Searching: {.file {(.map_bath)}}")
  bathy <-
    here(.map_bath) %>%
    dir_ls(regexp = .map_file)
  
  if (rlang::is_empty(bathy)) {
    cli::cli_alert_warning(
      c(
        "Skipping: Couldn't find ",
        "{.var {col_red(.map_file)}}")
    )
    bathy <- NULL
  } else {
    cli::cli_alert_info("Loading: {.file {(.map_file)}}")
    bathy <- 
      bathy %>%
      terra::rast() %>%
      as.data.frame(xy = TRUE)
    
  }
  
  return(
    list(
      coast_topo = coast_topo,
      state      = state,
      bathy      = bathy
    )
  )
  # ---- End of `load_map_obj` Function ----
}


##%######################################################%##
#                                                          #
####                   Base Map Plot                    ####
#                                                          #
##%######################################################%##
#' Base Map Plot 
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .topo Coastline sf object
#' @param .bathy Bathymetry sf object
#' @param .extnt Spatial extent for the topography data.
#'               Format: exnt <- c(xmin = -82,    # West
#'                                 xmax = -80,    # East
#'                                 ymin = 24.25,  # South
#'                                 ymax = 25.75   # North
#'                                 ) 
#' @param .breaks_z Contour lines as a vector
#' @param .breaks_y Breaks on latitude passed to `scale_y_continuous`
#' @param .breaks_x Breaks on longitude passed to `scale_x_continuous`
#'
#' @return gg object
#' 
#' @author Sebastian Di Geronimo (April 27, 2023)
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
base_map_plot <- function(
    .topo,
    .bathy,
    .extent,
    .breaks_z = c(100, 50, 25, 10, 0),
    .breaks_y = waiver(),
    .breaks_x = waiver()
) {
  
  # ---- libraries
  library("ggplot2")
  library("metR")
  
  # ---- plot 
  plt <- 
    ggplot() +
    geom_sf(data = .topo) +
    scale_x_continuous(expand = c(0, 0), breaks = .breaks_x) +
    scale_y_continuous(expand = c(0, 0), breaks = .breaks_y) +
    coord_sf(xlim = .extent[1:2], ylim = .extent[3:4]) +
    labs(
      x = NULL,
      y = NULL,
    )  +
    theme_bw() +
    theme(
      text = element_text(family = "serif", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  if (rlang::is_empty(.bathy)) return(plt)
  
  plt +
    metR::geom_contour2(
      data = .bathy,
      aes(
        x = x,
        y = y,
        z = -altitude
      ),
      col    = "grey70",
      breaks = .breaks_z
    )
  
  # ---- End of `base_map_plot` Function ---- #
}
