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
#' @param crop_envelope Logical. If there is an envelope, should it be used to crop the scene? FALSE will add the envelope to the plot instead.
#' @param col color palete. Default = [grDevices::grey.colors()]
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
#' nc <- st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) %>%
#'   summarise()
#' df <- data.frame(plotID = 1:20,
#'                  Temperature = runif(n=20, min=5, max=20))
#' p <- dplyr::mutate(df, st_sample(nc, 20)) %>%
#'   st_as_sf()
#'
#' # create Voronoi at "WorkingDirectory/Voronoi.pdf"
#' voronoi(x = p,
#'         envelope = nc,
#'         values = p$Temperature,
#'         filename = "Voronoi.pdf",
#'         main = "Voronoi",
#'         labels = p$plotID,
#'         statistics = TRUE,
#'         delaunay = TRUE)
#' }

voronoi <- function(x,
                    envelope = NULL,
                    crop_envelope = TRUE,
                    values,
                    col = grDevices::grey.colors(length(values)),
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

  extent <- terra::ext(data)
  range.xy <- c(extent[2] - extent[1],
                extent[4] - extent[3])
  boundary <- c(extent[1] - 0.1*range.xy[1],
                extent[2] + 0.1*range.xy[1],
                extent[3] - 0.1*range.xy[2],
                extent[4] + 0.1*range.xy[2])

# Calculate Voronoi
  v <- terra::voronoi(data, bnd = boundary)

# Optional: Calculate Delauney
  if(isTRUE(delaunay)){
    d <- terra::delaunay(data)
  }

# Optional: Crop to envelope
  if (!is.null(envelope)){
    aoi <- terra::vect(envelope)
    aoi <- terra::project(aoi, "EPSG:32632")
    if (isTRUE(crop_envelope)){
      v <- terra::crop(v, aoi)
      if(isTRUE(delaunay)){
        d <- terra::crop(d, aoi)
      }
    }
  }
# Create PDF
  grDevices::pdf(filename,
                 width = 11, height = 7.5,
                 bg = "white", colormodel = "cmyk",
                 paper = "a4r") # Start of PDF-File
  terra::plot(v,
              "ClimUtils",
              type = "continuous",
              lwd = 2,
              col = col,
              main = main,
              background = "beige")
  if(exists("aoi") & isFALSE(crop_envelope)){
    terra::lines(aoi,
                 lwd = 2,
                 col = "darkgreen",
                 alpha = .4)
  }
  if(exists("d")){
    terra::lines(d,
                 lwd = 2,
                 col = "darkred",
                 alpha = .1)
  }
  terra::points(data, col = "yellow")
  if(!is.null(labels)){
    terra::text(data,
                labels,
#                halo = TRUE,
                cex = 0.1)
  }
  if(isTRUE(statistics)){
    graphics::legend("topright",
                     xpd = TRUE,
                     inset = c(-0.15, 0),
                     legend = c(paste0("Mean: ", round(mean(data$ClimUtils), 3)),
                                paste0("Min: ", round(min(data$ClimUtils), 3)),
                                paste0("Max: ", round(max(data$ClimUtils), 3)),
                                paste0("SD: ", round(stats::sd(data$ClimUtils), 3)),
                                paste0("Median: ", round(stats::median(data$ClimUtils), 3))
                                ),
                     title = "Value Statistics"
                     )
  }
  grDevices::dev.off() # End of PDF-File
}
