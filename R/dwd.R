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
#' @seealso [load_dwd_data]{load_dwd_data}, [rdwd::selectDWD()]{selectDWD}
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
  a <- utils::installed.packages()[which(
        row.names(utils::installed.packages()) == "rdwd"), 3]
  b <- utils::available.packages(
    contriburl = utils::contrib.url(
      c(CRAN = "https://cloud.r-project.org")))[which(
        row.names(utils::available.packages(
          contriburl = utils::contrib.url(
            c(CRAN = "https://cloud.r-project.org")))) == "rdwd"), 2]

  if (utils::compareVersion(a, b) == 1){
    cat("You are using the newest version of `rdwd`.\n")
  } else {  # Up to date Condition
    if (utils::compareVersion(a, b) == 0){
      ifelse(isFALSE(update_rdwd),
             cat("Your version of `rdwd` is up to date with CRAN. There might be a newer version when running `rdwd::updateRdwd()`.\n"),
             rdwd::updateRdwd())
    } else {
      ifelse(isFALSE(update_rdwd),
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
#' @param dataframe dataframe returned by [find_dwd_stations].
#' @param dir character. Directory where DWD_raw_files should be stored. Default = `tempdir()`.
#' @param res,var,per character. Restrictions for dataset type as documented in rdwd::selectDWD(). Each can be a vector of entries. Per default taken from dataframe.
#' @param start,end character. Start and End of Timeseries one wants to load. Format = `"yyyy-mm-dd"`
#'
#' @return dataframe containing all stations loaded after one another.
#' @seealso [find_dwd_stations]{find_dwd_stations}, [rdwd::selectDWD()]{selectDWD}
#'
#' @name load_dwd_data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
    if(is.null(res)){
      res <- dataframe[which(dataframe$Stations_id == i),]$res
    }
    if(is.null(var)){
      var <- dataframe[which(dataframe$Stations_id == i),]$var
    }
    if(is.null(per)){
      per <- dataframe[which(dataframe$Stations_id == i),]$per
    }

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
      dplyr::filter(dplyr::between(.data$MESS_DATUM, as.Date(start), as.Date(end)))

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

#' Aggregate DWD data
#'
#' Aggregate downloaded DWD data to what the user likes.
#'
#' @param dataframe dataframe returned by [load_dwd_data]{load_dwd_data} or in the same format.
#' @param aggregation_of_time character. To what time span should data be aggregated? Options: "hour", "day", "week", "month", "year"
#' @param min_entries integer. Minimum number of entries for a single time step in the aggregation to be valid.
#'
#' @return dataframe containing all aggregated stations loaded after one another.
#' @seealso [load_dwd_data]{load_dwd_data}
#'
#' @name aggregate_dwd_data
#'
#' @export aggregate_dwd_data
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
#'
#' # aggregate data to monthly means
#' dwd_aggregated <- aggregate_dwd_data(dwd_raw,
#'                                      aggregation_of_time = "monthly")
#'
#' # show data
#' utils::head(dwd_aggregated)

aggregate_dwd_data <- function(dataframe,
                               aggregation_of_time,
                               min_entries = NULL){

#  table(data[which(data$plotID == 896),]$datetime)

  # aggregate datetime
  if (aggregation_of_time == "hour"){
    data <- transform(dataframe, datetime = substring(datetime, 1, 13))
  }
  if (aggregation_of_time == "day"){
    data <- transform(dataframe, datetime = substring(datetime, 1, 10))

  }
  if (aggregation_of_time == "week"){
    data <- transform(dataframe, datetime = substring(datetime, 1, 10))
    data$datetime <- lubridate::floor_date(as.Date(data$datetime), "week")
  }
  if (aggregation_of_time == "month"){
    data <- transform(dataframe, datetime = substring(datetime, 1, 7))
  }
  if (aggregation_of_time == "year"){
    data <- transform(dataframe, datetime = substring(datetime, 1, 4))
  }

  nr_of_dates <- length(unique(data$plotID)) * length(unique(data$datetime))
  foo <- as.formula(paste0(colnames(data)[3], " ~ datetime"))

  # aggregate time
  result <- data.frame(matrix(ncol = 3, nrow = nr_of_dates))
  colnames(result) <- colnames(dataframe)
  counter <- 1

  for (i in unique(data$plotID)){
    data_agg <- stats::aggregate(foo,
                                 data[which(data$plotID == i),],
                                 mean)
    result[c(counter:(nrow(data_agg) + counter - 1)), 1] <- i
    result[c(counter:(nrow(data_agg) + counter - 1)), 2:3] <- data_agg
    counter <- counter + nrow(data_agg)
  }

  return(result)
}
