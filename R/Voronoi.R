#' Voronoi
#'
#' Description
#'
#' @param data list. List of SpatVectors containing spatial points.
#'
#' @return
#' @seealso
#'
#' @name
#' @export
#'
#' @examples
#'
build_voronoi <- function(data, aoi = NULL, outpath){
  v_df <- data.frame(matrix(ncol = 2,
                     nrow = length(data))
#             row.names = unlist(modellist) # Find a way to name each voronoi
             )
  colnames(v_df) <- c("ID", "path")
  v_df$ID <- 1:length(data)

  for(i in 1:length(data)){
    voi <- terra::voronoi(data[i])
    if(!is.null(aoi)){
      voi <- terra::crop(voi, aoi)
    }
    terra::writeVector(voi, file.path(outpath, paste0("Voronoi_", i, ".tif")))
    v_df$path[i] <- paste0("Voronoi_", i, ".tif")
  }
  return(v_df)
}

#' Voronoi
#'
#' Calculates a voronoi using the terra package
#'
#' @param data Depending on method:
#' - "terra": SpatVector. A terra SpatVector with spatial points.
#' - "sf": Multipolygon. Sf multiploygon object with spatial points.
#'
#' @param method character. "sf" or "terra". Which package should be used for
#'               calculating the voronoi diagram?
#'
#' @return SpatVector. A terra SpatVector with voronoi polygons.
#' @seealso terra, sf
#'
#' @name voronoi
#' @export voronoi
#'
#' @examples
#' # extracted from terra package
#' wkt <- c("MULTIPOLYGON ( ((40 40, 20 45, 45 30, 40 40)),
#'         ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))",
#'         "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))")
#' x <- vect(wkt)
#'
#' v <- voronoi(x, "terra")
#'
#' plot(v, lwd=2, col=rainbow(15))
#'
voronoi <- function(data, method){
  if(method == "terra"){
    voi <- terra::voronoi(data) # sf checken
    return(voi)
  }
  if(method == "sf"){
    voi <- sf::st_voronoi(data)
    return(voi)
  }
}
