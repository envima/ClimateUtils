#' Voronoi
#'
#' Calculates a voronoi using the terra package
#'
#' @param x To your liking:
#'
#' - `terra`: SpatVector. A terra SpatVector with spatial points.
#'
#' - `sf`: Multipolygon. Sf object with spatial points.
#'
#' - `path`: Character. A path to a vector object with spatial points that can be read by `terra::vect`.
#' @param envelope Same as x. Polygon object containing the Area of Interest.
#' @param values character. Column Name of your data values.
#' @param filename character. Path object. Name of the output file.
#' @param main character. Title for your voronoi.
#' @param labels Row, Column or Vector of labels in x.
#' @param statistics logical. If `TRUE`, basic data statistics will be plotted
#'                   next to the diagramm. Default = `FALSE`.
#' @param delaunay logical. If `TRUE`, a delauney diagramm will be plotted in
#'                 the background. Default = `FALSE`.
#'
#' @return SpatVector. A terra SpatVector with voronoi polygons.
#' @seealso terra, sf
#'
#' @name voronoi
#' @export voronoi
#'
#' @examples
#' # load example data
#' \dontrun{
#' nc1 <- st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) %>%
#'   summarise()
#' df <- data.frame(plotID = 1:20,
#'                  Temperature = runif(n=20, min=5, max=20))
#' df <- dplyr::mutate(df, st_sample(nc, 20)) %>%
#'   st_as_sf()
#'
#' # create Voronoi at "WorkingDirectory/Voronoi.pdf"
#' voronoi(x = df,
#'         envelope = nc,
#'         values = "Temperature",
#'         filename = "Voronoi.pdf",
#'         main = "Voronoi",
#'         labels = df$plotID,
#'         statistics = TRUE,
#'         delaunay = TRUE)
#' }

voronoi <- function(x,
                    envelope = NULL,
                    values,
                    filename,
                    main = NULL,
                    labels = NULL,
                    statistics = FALSE,
                    delaunay = FALSE){
# add values to x
  x$ClimUtils <- values

# change to terra object
  data <- terra::vect(x)

# Bring everything into same crs
  data <- terra::project(data, "EPSG:32632")

# Calculate Voronoi
  v <- terra::voronoi(data)

# Optional: Calculate Delauney
  if(isTRUE(delaunay)){
    d <- terra::delaunay(data)
  }

# Optional: Crop to envelope
  if (!is.null(envelope)){
    aoi <- terra::vect(envelope)
    aoi <- terra::project(aoi, "EPSG:32632")
    v <- terra::crop(v, aoi)
    if(isTRUE(delaunay)){
      d <- terra::crop(d, aoi)
    }
  }
# Create PDF
  pdf(filename,
      width = 11, height = 7.5,
      bg = "white", colormodel = "cmyk",
      paper = "a4r") # Start of PDF-File
  terra::plot(v,
              "ClimUtils",
              type = "continuous",
              lwd = 2,
              col = grey.colors(length(v$ClimUtils)),
              main = main,
              background = "beige")
  if(exists("d")){
    terra::lines(d,
                 lwd=2,
                 col = "darkred",
                 alpha = 0.1)
  }
  terra::points(data, col = "yellow")
  if(!is.null(labels)){
    terra::text(data,
                labels,
#                halo = TRUE,
                cex = 0.1)
  }
  if(isTRUE(statistics)){
    legend("topright",
           xpd = TRUE,
           inset = c(-0.15, 0),
           legend = c(paste0("Mean: ", round(mean(data$ClimUtils), 3)),
                      paste0("Min: ", round(min(data$ClimUtils), 3)),
                      paste0("Max: ", round(max(data$ClimUtils), 3)),
                      paste0("SD: ", round(sd(data$ClimUtils), 3)),
                      paste0("Median: ", round(median(data$ClimUtils), 3))
                      ),
           title = "Value Statistics"
           )
  }
  dev.off() # End of PDF-File
}
