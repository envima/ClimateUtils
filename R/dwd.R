#' Search DWD-Stations
#'
#' Search n-count of nearby DWD stations from a point in space.
#'
#' @param x sf_point object with one point.
#' @param number_of_stations numeric. Number of stations one wants to collect.
#' @param mindate character. Latest date you want to have in the stations. Format: "YYYY-MM-DD"
#' @param res,var,per character. Restrictions for dataset type as documented in rdwd::selectDWD(). Each can be a vector of entries.
#' @param update_rdwd logical. Use updateRdwd() before execution? Important if one wants the most recent stations from DWD.
#'
#' @return dataframe. A data frame containing all nearby stations defined in number_of_stations
#' @seealso
#'
#' @name find_dwd_stations
#' @export find_dwd_stations
#'
#' @examples
#'
#' # create sf_point from central Germany
#' g <- sf::st_sfc(sf::st_point(x = c(51.163375, 10.447683)), crs = "EPSG:4326")
#' p <- sf::st_sf(ID = "central_germany", geom = g)
#'
#' # find 20 nearest dwd stations from central germany that are running til today
#' dataframe <- find_dwd_stations(p,
#'                                number_of_stations = 20,
#'                                mindate = "2024-06-30",
#'                                res = "hourly",
#'                                var = "air_temperature",
#'                                per = "recent",
#'                                update_rdwd = TRUE)
#' utils::head(dataframe)


find_dwd_stations <- function(x,
                              number_of_stations,
                              mindate,
                              res,
                              var,
                              per,
                              update_rdwd = FALSE){
  # first transform x into WGS 84 (required for rdwd)
  point <- sf::st_transform(x, "EPSG:4326")

  # check rdwd version
  a <- installed.packages()[which(row.names(installed.packages()) == "rdwd"), 3]
  b <- available.packages()[which(row.names(available.packages()) == "rdwd"), 2]

  if (utils::compareVersion(a, b) == 1){
    cat("You are using the newest version of `rdwd`.")
  } else {  # Up to date Condition
    if (utils::compareVersion(a, b) == 0){
      ifelse(isFALSE(check_for_updates),
             cat("Your version of `rdwd` is up to date with CRAN. There might be a newer version when running `rdwd::updateRdwd()`."),
             rdwd::updateRdwd())
    } else {
      ifelse(isFALSE(check_for_updates),
             cat("Your version of `rdwd` is outdated. Setting argument `update_rdwd` to `TRUE` is recommended."),
             rdwd::updateRdwd())
    } # Outdated Version Condition
  } # CRAN-Version Condition
  remove(a, b)

  # find nearby stations
  rad <- 1
  stations <- 0

  while(number_of_stations > stations){
    try(
      dataframe <- rdwd::nearbyStations(lat = sf::st_geometry(point)[[1]][1],
                                 lon = sf::st_geometry(point)[[1]][2],
                                 radius = rad,
                                 mindate = as.Date(mindate),
                                 res = res,
                                 var = var,
                                 per = per,
                                 quiet = TRUE),
      silent = TRUE
    )

    if(exists("dataframe")){
      stations <- nrow(dataframe) - 1

      cat(sprintf("\rNumber of stations: %d   Radius (in km): %.1f", stations, rad))

      if(stations >= number_of_stations){
        cat("\n")
      }
    }

    rad <- rad + 0.5
  }

  return(dataframe)
}

