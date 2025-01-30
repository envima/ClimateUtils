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
#' @seealso [load_dwd_data], [rdwd::selectDWD()]
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
    cat("You are using the newest version of `rdwd`.\n")
  } else {  # Up to date Condition
    if (utils::compareVersion(a, b) == 0){
      ifelse(isFALSE(check_for_updates),
             cat("Your version of `rdwd` is up to date with CRAN. There might be a newer version when running `rdwd::updateRdwd()`.\n"),
             rdwd::updateRdwd())
    } else {
      ifelse(isFALSE(check_for_updates),
             cat("Your version of `rdwd` is outdated. Setting argument `update_rdwd` to `TRUE` is recommended.\n"),
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
  dataframe <- dataframe[-1,] # remove first row with your input variables
  return(dataframe)
}


#' Load DWD-Stations
#'
#' Search n-count of nearby DWD stations from a point in space.
#'
#' @param dataframe dataframe returned by [find_dwd_data].
#' @param dir character. Directory where DWD_raw_files should be stored. Default = `tempdir()`.
#' @param res,var,per character. Restrictions for dataset type as documented in rdwd::selectDWD(). Each can be a vector of entries. Per default taken from dataframe.
#' @param start,end character. Start and End of Timeseries one wants to load. Format = `"yyyy-mm-dd"`
#'
#' @return dataframe containing all stations loaded after one another.
#' @seealso [find_dwd_data], [rdwd::selectDWD()]
#'
#' @name load_dwd_data
#' @importFrom magrittr %>%
#'
#' @export load_dwd_data
#'
#' @examples
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
#'
#' # download and filter all these stations to desired structure
#' dwd_raw <- load_dwd_data(dataframe,
#'                          per = c("recent", "historical"),
#'                          start = "2022-08-01",
#'                          end = "2024-07-31")
#' utils::head(dwd_raw)

load_dwd_data <- function(dataframe,
                          dir = tempdir(),
                          res = NULL,
                          var = NULL,
                          per = NULL,
                          start,
                          end){


  for (i in unique(dataframe$Stations_id)){

    # define res, var and per
    res <- ifelse(is.null(res),
                  dataframe[which(dataframe$Stations_id == i),]$res,
                  res)
    var <- ifelse(is.null(var),
                  dataframe[which(dataframe$Stations_id == i),]$var,
                  var)
    per <- ifelse(is.null(per),
                  dataframe[which(dataframe$Stations_id == i),]$per,
                  per)

    # select right station
    ftp <- rdwd::selectDWD(id = i,
                           res = res,
                           var = var,
                           per = per)

    # download and filter station data
    dwd <- rdwd::dataDWD(ftp,
                         dir = dir,
                         read = TRUE,
                         sleep = ifelse(length(unique(dataframe$Stations_id)) > 50, 20, 0),
                         quiet = TRUE)

    if(isFALSE(is.data.frame(dwd))){
      dwd <- do.call("rbind", c(dwd, make.row.names = FALSE))
    }

    dwd <- dwd[,c(1:2, 4:(length(var) + 3))] %>%
      dplyr::filter(dplyr::between(MESS_DATUM, as.Date(start), as.Date(end)))

    if(!exists("dwd_full")){
      dwd_full <- dwd
    } else {
      dwd_full <- rbind(dwd_full, dwd)
    } # end if condition
  } # end for loop

  names(dwd_full) <- c("plotID", "datetime", var)
  dwd_full$datetime <- format(dwd_full$datetime, "%Y-%m-%dT%H")

  return(dwd_full)
}
