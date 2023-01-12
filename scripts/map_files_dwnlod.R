world_download <- function(path_land = NULL,
                           path_topo = path_land,
                           extent = NULL) {
# ============================================================================ #
#                                                                              # 
#               Download World Shapefiles for plotting                         #
#                                                                              #    
# ============================================================================ #
# ---- DESCRIPTION: ------
# This function will download topography, state lines, and coastline from
# NOAA if it does not exist at the designated path. 
# 
# The topography should be re-downloaded for new projects since it's only a 
# subset of the world based on the extent given.
# 
# The land boundaries only need to be downloaded once, so specifying where it's 
# located may be a better option. 
# 
# Location of data:
# Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), 
# (Ice Sheet Surface) from https://coastwatch.pfeg.noaa.gov/erddap/griddap/
#
# Global Self-consistent, Hierarchical, High-resolution Geography Database 
# (GSHHG) from https://www.ngdc.noaa.gov/mgg/shorelines/
#
# ---- INPUTS: -----------
# path_land = File directory to be used for coastline files. This may need to be 
#             created or already exists. This is where the land map will be 
#             downloaded to if it doesn't exists.
# path_topo = File directory to be used for topgraphy file. This may need to be 
#             created or already exists. This is where the topo map will be 
#             downloaded to if it doesn't exists. This will require a spatial 
#             extent to subset the data
# extent    = Spatial extent for the topography data.
#             Format: exnt <- c(xmin = -82,    # West
#                               xmax = -80,    # East
#                               ymin = 24.25,  # South
#                               ymax = 25.75   # North 
#                              ) 
#
# ---- OUTPUTS: ----------
# NA
#
# ---- NOTES: ------------
#
# ---- REFERENCES(s): ----
#
# ---- AUTHOR(s): --------
# Sebastian Di Geronimo (2023-01-11 14:55:54)

    # ============================================================================ #
    # ---- Load libraries ----
    # ============================================================================ #    
    library("here")
    library("fs")
    library("rerddap")
    library("cli")
    library("rlang")
    
    # ======================================================================== #
    # ---- Create directories for files ----
    # ======================================================================== #    
    fs::dir_create(path_land)
    fs::dir_create(path_topo)
    
    # ======================================================================== #
    # ---- Download topography for the specific location ----
    # ======================================================================== #    
    # TODO: add suffix to name to specify spatial location
    # space <- paste(as.character(round(extent)), sep = "_")
    # unite(as.character(round(extent)), col = "one", c(1:4))
    # topo_file <- here(path_topo,"etopo1_{space}.nc")
    
    topo_file <- here(path_topo,"etopo1.nc")
    if (length(extent) != 4) {
        cli_abort(c(
            "{.var extent} needs to have 4 values.",
            "x" = "You supplied {length(extent)} values.",
            "Format Example: 
            exnt <- c(xmin = -82,xmax = -80, ymin = 24.25, ymax = 25.75) "))
        }
    
    if (!file.exists(topo_file)) {
        
        cli_alert_info("Topography data doesn't exists in {.file {path_topo}}")
        cli_alert_info(c("Downloading Topography Data\n",
                         "Longitude: {exent[1]}, {exent[2]} \n",
                         "Latitude:  {exent[3]}, {exent[4]} \nURL: ",
                         "{.url https://coastwatch.pfeg.noaa.gov/erddap/griddap/}"))
        
        # ERDDAP extract and save
        griddap(
            info('etopo180'),
            latitude  = exent[3:4],
            longitude = exent[1:2],
            stride    = c(1, 1),
            fields    = 'altitude',
            store     = disk(path_topo)
        )
        
        # Rename file 
        file.rename(fs::dir_ls(path_topo, regexp = "\\.nc$"),
                    topo_file)
        
        cli_alert_success("Downloaded etopo1.nc")
    } else {
        cli_alert_info(c("Topography data exists in {.file {path_topo}}.\n",
                        "You may need to {.emph {col_red('check')}} the spatial",
                        " extent."))
    }
    
    # ======================================================================== #
    # ---- Download GSHHS Coastline Shapfile ----
    # ======================================================================== # 
    # download GSHHS shapefile if not already downloaded
    coast         <- fs::dir_ls(path = path_land,
                                recurse = TRUE ,
                                regexp = "GSHHS_h_L1.shp")
    
    if (rlang::is_empty(coast)) {
        cli_alert_info(c("GSHHS_h_L1 Coastline data doesn't exists in ", 
                        "{.file {path_land}}"))
        cli_alert_info(c("Downloading Coastline Shapefile\nURL:",
                       "{.url https://www.ngdc.noaa.gov/mgg/shorelines/}"))
        
        temp <- tempfile()
        download.file(
            "ftp://ftp.soest.hawaii.edu/gshhg/gshhg-shp-2.3.7.zip",
            temp,
            method = "libcurl",
            mode = "wb"
        )
        unzip(exdir = path_land, temp)
        unlink(temp)
    } else {
        cli_alert_info("Coastline shapefile exists in {.file {path_land}}.\n")
    }
}
