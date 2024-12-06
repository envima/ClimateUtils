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
voronoi <- function(data){
  voi <- terra::voronoi(data) # sf checken
  return(voi)
}
