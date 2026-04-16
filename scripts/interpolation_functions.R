# ============================================================================ #
#
#
# ---- Interpolation Functions ---- #
#
#
# ============================================================================ #    

if (!nzchar(system.file(package = "librarian"))) 
  install.packages("librarian")

librarian::shelf(
  quiet = TRUE,
  librarian, conflicted, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
  forcats, lubridate, glue, fs, magrittr, here,
  
  # additional packages, if from GitHub, include username
  sf, ggspatial, terra, geodata, gstat, viridis, ggrepel, grid, gstat,
  patchwork # combine panels
)

conflicts_prefer(
  dplyr::filter,
  dplyr::select
)

# default: local IDW = tighter, less spread, more visually refined
idw_options <-
  list(
    "idp_pow"   = 2.0,
    "maxdist_m" = 120000,
    "nmin_nb"   = 3,
    "nmax_nb"   = 10
  )

# default: light 3x3 weighted kernel (gentle smoothing only)
smooth_kernel <-
  matrix(
    c(
      1, 2, 1,
      2, 4, 2,
      1, 2, 1
    ),
    nrow = 3, byrow = TRUE
  )
smooth_kernel <- smooth_kernel / sum(smooth_kernel)

##%######################################################%##
#                                                          #
####           Cruise Information Extraction            ####
#                                                          #
##%######################################################%##
#' Cruise Information Extraction
#'
#' This function is only used for CTD data produced by TAMU and binned to the
#' first 5 meters.
#' 
#' This is used to extract data points from a specific cruise and create a
#' convex hull around these points as an sf object. This also includes 
#' extracting titles and sub-titles for plotting.
#'
#' @param cruise_id Selected cruise ID 
#' @param dat A dataframe with cruise data 
#'
#' @return A list object
#'  - pts_utm          - dataframe of data points as sf object
#'  - support_poly_utm - convex hull around the data points
#'  - main_title       - Character string title of plot in format:
#'    `<cruise id> - Surface (0-5 m mean)` or
#'    `No rows matched cruise_id`
#'  - sub_title        - Character string subtitle of plot in format:
#'     `<date start> to <date end> | IDW + (UTM grid)`
#'  - chl_panel_title  - Character string one of three:
#'    * `Chlorophyll concentration`
#'    * `Chlorophyll fluorescence`
#'    * `Chlorophyll`
#' 
#' @author Mostafa Solimon (March 26, 2026)
#' @author Tylar Murray (March 26, 2026)
#' @author Sebastian Di Geronimo (March 26, 2026)
#' 
cruise_extract <- function(cruise_id, dat) {
  
  # ---- main title text ---- #
  main_title <- paste0(cruise_id, " — Surface (0–5 m mean)")
  
  df <-
    dat %>%
    filter(
      str_detect(cruise_file, cruise_id), # select cruise
      !is.na(station),
      station != "",
      !is.na(latitude),
      !is.na(longitude)
    )
  
  if (nrow(df) == 0) stop("No rows matched cruise_id: ", cruise_id)
  
  # ---- subtitle text ---- #
  date_vec <- df$date_raw
  date_min <- min(date_vec, na.rm = TRUE)
  date_max <- max(date_vec, na.rm = TRUE)
  sub_title <-
    if (all(!is.na(c(date_min, date_max)))) {
      paste(format(date_min, "%Y-%m-%d"), "to", format(date_max, "%Y-%m-%d"))
    } else {
      "Date range: NA"
    }
  
  sub_title <- paste0(sub_title, " | IDW + (UTM grid)")
  
  # ---- chlorophyll source MODE for THIS cruise (panel title only) ---- #
  chl_src_tbl <- df %>%
    filter(!is.na(chl_source), chl_source != "") %>%
    count(chl_source, name = "n") %>%
    arrange(desc(n))
  
  chl_src_mode <- if (nrow(chl_src_tbl) == 0) {
    "unknown"
  } else {
    chl_src_tbl$chl_source[1]
  }
  
  chl_panel_title <- 
    if (identical(chl_src_mode, "chlorophyll_concentration")) {
      "Chlorophyll concentration"
    } else if (identical(chl_src_mode, "chlorophyll_fluorescence")) {
      "Chlorophyll fluorescence"
    } else {
      "Chlorophyll"
    }
  
  
  # ---- station means (ONLY aggregation; no smoothing of source data) ---- #
  df_st <-
    df %>%
    summarise(
      .by = station,
      across(
        c(latitude, longitude, sal, temp, oxy, chl),
        \(.x) mean(.x, na.rm = TRUE)
      )
    ) %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  # convert dataframe of stations to sf object and project
  pts_utm <- 
    st_as_sf(df_st, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)  %>%
    st_transform(utm_epsg) %>%
    mutate(
      longitude = st_coordinates(.)[,1],
      latitude = st_coordinates(.)[,2]
    ) 
  
  # ---- TIGHT HARD SUPPORT MASK — computed once ---- #  
  # create a smaller, irregular bounding box based on convex hull or buffer (<3)
  if (nrow(pts_utm) >= 3) {
    # convex hull, needs at least 3 points
    support_poly_utm <- 
      pts_utm %>%
      st_union() %>%
      st_convex_hull() %>%
      st_buffer(8000)   # 8 km buffer
  } else {
    # buffer around each point
    support_poly_utm <- 
      pts_utm %>%
      st_buffer(30000) %>%
      st_union()
  }
  
  return(
    list(
      "pts_utm"          = pts_utm,
      "support_poly_utm" = support_poly_utm,
      "main_title"       = main_title,
      "sub_title"        = sub_title,
      "chl_panel_title"  = chl_panel_title
    )
  )
  # ---- end of function cruise_extract ---- #
}



##%######################################################%##
#                                                          #
####             Create A Blank Raster Grid             ####
#                                                          #
##%######################################################%##
#' Create A Blank Raster Grid
#'
#' This creates a blank gridded raster object. The buffer and grid resolution
#' are set to 10 km and 1 km, respectively. A higher resolution grid will take
#' longer to perform the interpolation.
#'
#' @param .support_poly sf object that is used as bounding box around 
#'                      interpolation
#' @param utm_epsg UTM EPSG for your area
#' @param grid_res_m Resolution of the grid in meters (default = 1000 m)
#' @param buffer_m A buffer around the convex hull in meters (default = 10000)
#'
#' @return Two raster objects:
#'   - grid_rast - Spatraster version
#'   - grid_sf   - sf object version
#'
#' @author Mostafa Solimon (March 26, 2026)
#' @author Tylar Murray (March 26, 2026)
#' @author Sebastian Di Geronimo (March 26, 2026)
#'
grid_blank <- function(
    .support_poly, 
    utm_epsg, 
    grid_res_m = 1000, 
    buffer_m   = 10000
    ) {
  
  # create blank raster grid with equal grid numbers
  grid_rast <- 
    # add buffer boundary around convex hull
    st_buffer(.support_poly, dist = buffer_m) %>%
    st_bbox() %>%
    {
      terra::rast(
        xmin       = .[["xmin"]],
        xmax       = .[["xmax"]],
        ymin       = .[["ymin"]],
        ymax       = .[["ymax"]],
        resolution = grid_res_m,
        crs        = paste0("EPSG:", utm_epsg)
      )
    } 
  
  # convert projected raster to sf object with selected resolution
  # this will be the sf object to map predicted values from interpolation (IDW)  
  grid_sf <- 
    as.data.frame(grid_rast, xy = TRUE, cells = TRUE, na.rm = FALSE) %>%
    st_as_sf(coords = c("x","y"), crs = utm_epsg)
  
  return(
    list(
      "grid_rast" = grid_rast,
      "grid_sf"   = grid_sf
    )
  )
  # ---- end of function grid_blank ---- #
}



##%######################################################%##
#                                                          #
####        Edge Opacity Of Interpolated Polygon        ####
#                                                          #
##%######################################################%##
#' Edge Opacity Of Interpolated Polygon
#'
#' This only affects the edge display of the interpolation where the edge of 
#' the polygon is progressively more opaque as the edges of the polygon are 
#' reached.
#'
#' @param .grid_rast Spatraster object as a blank grid 
#' @param .support_poly_utm sf object to mask the opacity grid
#' @param .edge_feather_m distance in meters to set when the opacity goes to 
#'                        zero (default = 5000 m)
#'
#' @return a Spatraster object with `alpha` values for ggplot to be used as 
#'        an additional layer for the interpolation (values: 0 - 1)
#'
#' @author Mostafa Solimon (March 26, 2026)
#' @author Tylar Murray (March 26, 2026)
#' @author Sebastian Di Geronimo (March 26, 2026)
#'
edge_opacity <- function(
    .grid_rast,
    .support_poly_utm,
    .edge_feather_m = 5000 # try 4000–7000 depending on taste
) {
  
  # convert convex hull to linestring
  support_boundary_line <- st_boundary(.support_poly_utm) %>%
    terra::vect()
  
  # calculate distance from convex hull (as linestring) then mask to within convex hull
  dist_to_edge_utm <-
    terra::distance(.grid_rast, support_boundary_line, names = "alpha") %>%
    terra::mask(terra::vect(.support_poly_utm))
  
  # divides distances by edge buffer then sets upper bounds to 1 by converting all values
  # above 1 to 1 and leaves all values below as is
  alpha_utm <-
    terra::clamp(dist_to_edge_utm / .edge_feather_m, lower = 0, upper = 1, values = TRUE)
  
  # ---- end of function edge_opacity ---- #
}




##%######################################################%##
#                                                          #
####             Inverse Distance Weighted              ####
####           Interpolation Wrapper Function           ####
#                                                          #
##%######################################################%##
#' Inverse Distance Weighted Interpolation Wrapper Function
#'
#' FUNCTION_DESCRIPTION
#'
#' @param col_name the column name for the values to perform the interoplation
#' @param .df_st an sf object with the columns "station", "latitude", 
#'               "longitude" and "geometry"
#' @param .alpha_utm Spatraster object containing the opacity (alpha) values
#' @param .smooth_kernel a 3 x 3 smoothing kernel matrix object
#' @param .grid_sf sf object of the blank grid
#' @param .grid_rast Spatraster object of the blank grid
#' @param .support_poly_utm sf object to mask the interpolated values outside 
#'                          the data area
#' @param .land_mask an sf object for landmask boundary
#' @param idw_opts a list object for `gstat::idw()` parameters:
#'         - idp_pow
#'         - maxdist_m
#'         - nmin_nb
#'         - nmax_nb
#'
#' @return a list:
#'   - dd - a formatted dataframe for plotting
#'   - r_pred_water -  a Spatraster object containing the interpolated 
#'     values and opacity
#'   - plot_df = a dataframe of each cell with interpolated values and opacity
#'
#' @author Mostafa Solimon (March 26, 2026)
#' @author Tylar Murray (March 26, 2026)
#' @author Sebastian Di Geronimo (March 26, 2026)
#'
idw_func <- function(
    col_name,
    .df_st            = pts_utm,  
    .alpha_utm        = alpha_utm,
    .smooth_kernel    = smooth_kernel,
    .grid_sf          = grid_sf,
    .grid_rast        = grid_rast,
    .support_poly_utm = support_poly_utm,
    # .land_mask        = florida_sf, # <-- should be optional land mask
    .land_mask        = NULL, # <-- should be optional land mask
    idw_opts          = idw_options
) {
  
  # keep columns needed for IDW interpolation and plotting, converts selected
  # parameter name to val, removes NA 
  dd <- 
    .df_st %>%
    mutate(
      .keep = "none",
      station,
      latitude,
      longitude,
      val = .data[[col_name]],
      geometry
    ) %>%
    filter(!is.na(val))
  
  # ---- skip plotting if less than 5 points ---- #
  if (nrow(dd) < 5) {
    return(
      list(
        "dd"           = dd,
        "r_pred_water" = NULL,
        "plot_df"      = NULL
      )
    )
  }
  
  # ---- IDW interpolation as sf object ---- #
  # uses options set before
  idw_sf_utm <- 
    gstat::idw(
      formula   = val ~ 1,
      locations = dd,
      newdata   = .grid_sf,
      idp       = idw_opts$idp_pow,
      maxdist   = idw_opts$maxdist_m,
      nmin      = idw_opts$nmin_nb,
      nmax      = idw_opts$nmax_nb
    )
  
  # replace empty grid with IDW values
  terra::values(.grid_rast) <- as.numeric(idw_sf_utm$var1.pred)
  
  # ---- LIGHT DISPLAY SMOOTHING ONLY ---- #
  r_pred_water <-
    # mask data outside convex hull
    terra::mask(.grid_rast, terra::vect(.support_poly_utm)) %>%
    
    # smooths over values with weighting of 3x3 kernel
    terra::focal(
      w         = .smooth_kernel,
      fun       = mean,
      na.policy = "omit",
      na.rm     = TRUE,
      fillvalue = NA
    ) %>%
    
    # combine IDW and alpha (opacity)
    c(., .alpha_utm) %>%
    
    # mask again so smoothing does not bleed outside supported footprint
    # of convex hull
    terra::mask(terra::vect(.support_poly_utm)) # <-- may not be necessary
  
  # mask landmass
  if (!is.null(.land_mask)) {
    r_pred_water <- 
      r_pred_water %>%
      
      # mask landmass
      terra::mask(terra::vect(.land_mask), inverse = TRUE) 
  }    
  
  # convert into data frame
  plot_df <- 
    r_pred_water %>%
    as.data.frame(xy = TRUE, na.rm = FALSE) %>%
    rename("pred" = focal_mean)  %>%
    mutate(alpha = ifelse(is.na(alpha), 0, alpha)) %>%
    filter(!is.na(pred), alpha > 0)
  
  
  return(
    list(
      "dd"           = dd,
      "r_pred_water" = r_pred_water,
      "plot_df"      = plot_df
    )
  )
  # ---- end of function idw_func ---- #
}


##%######################################################%##
#                                                          #
#### Create a `gg` Object to Display the Interpolation  ####
#                                                          #
##%######################################################%##
#' Create a `gg` Object to Display the Interpolation
#'
#' FUNCTION_DESCRIPTION
#'
#' @param var_title Title for the variable being displayed
#' @param .dd an sf object with the data point
#' @param .r_pred_water Spatraster object of the interpolated grid
#' @param .plot_df a dataframe of the interpolated cells
#' @param legend_units unites to be displayed on the legend
#' @param .legend_limits the limites on the legend
#' @param .land_mask an sf object for landmask boundary
#' @param .bb_utm sf object of the bounding box for the map area
#'
#' @return gg object of the interpolated object
#' 
#' @author Mostafa Solimon (March 26, 2026)
#' @author Tylar Murray (March 26, 2026)
#' @author Sebastian Di Geronimo (March 26, 2026)
#'
make_panel <- function(
    var_title, 
    .dd,
    .r_pred_water  = r_pred_water,
    .plot_df       = plot_df,
    legend_units, 
    .legend_limits = NULL, 
    .land_mask, 
    .bb_utm
) {
  # ---- skip plotting if less than 5 points ---- #
  if (nrow(.dd) < 5) {
    return(
      ggplot() +
        theme_void() +
        labs(title = paste0(var_title, " (too few stations)")) +
        theme(plot.title = element_text(face = "bold", size = 12))
    )
  }
  
  # ---- contours ---- #
  contour_sf <- NULL
  
  rng <- range(.plot_df$pred, na.rm = TRUE)
  
  if (all(is.finite(rng)) && diff(rng) > 0) {
    brks <- pretty(rng, n = 6)
    brks <- brks[brks > rng[1] & brks < rng[2]]
    
    if (length(brks) > 0) {
      contour_sf <- try(
        terra::as.contour(.r_pred_water, levels = brks),
        silent = TRUE
      )
      # if error, convert to sf
      if (!inherits(contour_sf, "try-error")) {
        contour_sf <- try(sf::st_as_sf(contour_sf), silent = TRUE)
      }
      
      # if error on conversion, set to NULL
      if (inherits(contour_sf, "try-error")) contour_sf <- NULL
    }
  }
  
  if (is.null(.legend_limits)) .legend_limits <- c(NA, NA)
  
  # ---- plot ---- #
  p <- 
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#cfe8f3") +
    geom_raster(data = .plot_df, aes(x = x, y = y, fill = pred, alpha = alpha)) +
    geom_sf(data = .land_mask, fill = "#e8e6df", color = "grey35", linewidth = 0.55) +
    scale_fill_viridis_c(
      name = legend_units, option = "C", direction = 1, na.value = NA, 
      limits = .legend_limits, oob = scales::squish
    ) +
    scale_alpha_identity() +
    
    # add contours if possible
    {
      if (!is.null(contour_sf) && nrow(contour_sf) > 0) {
        geom_sf(
          data      = contour_sf,
          color     = "white",
          linewidth = 0.22,
          alpha     = 0.45
        )
      }
    } +
    
    # plot data points and labels
    geom_sf(data = .dd, color = "black", size = 0.55, alpha = 0.95) +
    ggrepel::geom_text_repel(
      data = .dd,
      aes(x = longitude, y = latitude, label = station),
      seed               = 1,
      size               = 2.0,
      color              = "black",
      box.padding        = 0.18,
      point.padding      = 0.10,
      min.segment.length = 0,
      segment.color      = "grey35",
      segment.size       = 0.30,
      segment.curvature  = 0.0,
      arrow              = grid::arrow(length = unit(0.006, "npc"), type = "closed"),
      max.overlaps       = Inf
    ) +
    
    # add arrow and scale bar
    annotation_north_arrow(
      location    = "tr",
      which_north = "true",
      pad_x       = unit(0.10, "cm"),
      pad_y       = unit(0.10, "cm"),
      height      = unit(0.85, "cm"),
      width       = unit(0.85, "cm"),
      style       = north_arrow_orienteering(
        line_col = "grey20",
        fill     = c("grey20", "white"),
        text_col = "grey20"
      )
    ) +
    annotation_scale(
      location   = "bl",
      width_hint = 0.22,
      pad_x      = unit(0.12, "cm"),
      pad_y      = unit(0.12, "cm"),
      text_cex   = 0.55,
      line_width = 0.6
    ) +
    
    # limits
    coord_sf(xlim = .bb_utm[c(1,3)], ylim = .bb_utm[c(2,4)], expand = FALSE) +
    
    # labels
    labs(title = var_title, x = NULL, y = NULL) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#cfe8f3"),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      plot.title       = element_text(face = "bold", size = 12)
    )
  
  p
  
  # ---- end of function make_panel ---- #
}


