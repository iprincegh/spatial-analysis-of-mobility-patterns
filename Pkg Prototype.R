#' Visualize mobility flows at multiple spatial and temporal scales
#'
#' @param od_data Origin-destination data (with time column for animation)
#' @param zones Spatial features of zones (sf object)
#' @param focus_area Optional area to zoom in on (character vector of zone IDs)
#' @param output_type Visualization type: "heatmap", "flowmap", "animation"
#' @param flow_threshold Minimum flow count to display (default = 100)
#' @param temporal_resolution "hour", "day", or "week" for animations
#' @param animation_speed Playback speed multiplier (default = 1.0)
#' @param disease_params Optional list with transmission parameters (R0, serial_interval)
#' 
#' @return A ggplot object (heatmap) or htmlwidget (flowmap/animation)
#'
#' @examples
#' # Static heatmap
#' visualize_mobility(madrid_od, madrid_zones, output_type = "heatmap")
#' 
#' # Animated flowmap with disease parameters
#' visualize_mobility(madrid_od_time, madrid_zones, 
#'                   output_type = "animation",
#'                   disease_params = list(R0 = 2.5, serial_interval = 5))
visualize_mobility <- function(od_data, zones, focus_area = NULL, 
                               output_type = "heatmap", flow_threshold = 100,
                               temporal_resolution = "hour", animation_speed = 1.0,
                               disease_params = NULL) {
  
  # Validate input
  if (!output_type %in% c("heatmap", "flowmap", "animation")) {
    stop("Invalid output_type. Choose 'heatmap', 'flowmap', or 'animation'")
  }
  
  # Filter data to focus area
  if (!is.null(focus_area)) {
    zones <- zones %>% filter(id %in% focus_area)
    od_data <- od_data %>% 
      filter(origin %in% focus_area, dest %in% focus_area)
  }
  
  # Process spatial data
  centroids <- st_centroid(st_transform(zones, 4326))
  
  # Visualization logic
  if (output_type == "heatmap") {
    # Heatmap generation code
    # ... (implementation from earlier) ...
    
  } else {
    # Prepare flowmap data
    locations <- st_coordinates(centroids) %>% 
      as.data.frame() %>% 
      mutate(id = zones$id) %>% 
      rename(lon = X, lat = Y)
    
    # Filter flows
    flows_filtered <- od_data %>% 
      filter(count >= flow_threshold)
    
    # Generate visualization
    flowmapblue(
      locations = locations,
      flows = flows_filtered,
      clustering = FALSE,
      darkMode = TRUE,
      animation = (output_type == "animation")
    )
  }
}