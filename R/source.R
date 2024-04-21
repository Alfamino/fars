#' Read the Fatality Analysis Reporting System data
#'
#' The \code{fars_read} function takes only one argument \code{filename}
#' the name of the csv file that contains the Fatality Analysis Reporting
#' System data \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}.
#' It uses \code{readr::read_csv} to load data so other formats are also supported.
#'
#' @param filename character string with a file name and filepath to the file
#'    containing data
#'
#' @return if the file is succesfully loaded it returns \code{data.frame} object
#'    with a given data, otherwise is "file does not exists" occur.
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' \dontrun{fars_read("data.csv")
#' fars_read("accident_2013.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
  #tibble::as_tibble(data)
}

#' File name generator for FARS data
#'
#' The function helps generating a proper name for a FARS data set just by
#' giving the year number.
#'
#' @param year integer of string of the year of the downloaded dataset.
#'    The year will be used in string \code{accident_X.csv.bz2} where
#'    \code{X} will be replaced by the given integer/
#'
#' @return returns a name of the desired dataset
#'
#' @examples
#' make_filename("2024")
#' make_filename(2024)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reading several FARS datasets
#'
#' The function is designed to automaticaly load the data from user specific years.
#' If the specified years would have mistake a "invalid year" warrning occurs.
#'
#' @note You must include tidyr package \code{library(tidyr)}.
#' The data files should be named "accident_X.csv.bz2" and be in the same directory.
#'
#' @param years a vector containig a years from which the data will be collected.
#'
#' @return The function returns \code{data frame} containig all specified years.
#'    If the year would be provided wrong the function returns \code{NULL}.
#'
#' @import tidyr
#'
#' @examples
#' \dontrun{fars_read_years( c(2013 ))
#' fars_read_years( c(2013,2014 ))
#' fars_read_years( c('2013','2014'))}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
    dat <- fars_read(file)
    dplyr::mutate(dat, year = year) %>%
      dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Quick summarization of FARS data
#'
#' The function takes user specified years and summarize the in the data frame format.
#'
#' @note You must include tidyr and dplyr packages.
#' The data files should be named "accident_X.csv.bz2" and be in the same directory.
#'
#' @param years a vector containing the years of data. Data files should be in the
#'     same directory.
#'
#' @return A summarization data frame or a \code{NULL}.
#'
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' \dontrun{fars_summarize_years( c('2014') )
#' fars_summarize_years( c(2013,2014) )}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Detaild summarization of FARS data
#'
#' The function makes a graphical representation of the FARS data on the
#' state map.
#'
#' @param state.num the state numer that should be considered. If the number of the state
#'    would be wrong the arror would should up.
#' @param year a vector containing the list of the years
#'
#' @return Returns a graphical representation of the data.
#'
#' @note You must include tydyr and dplyr packages.
#' The data files should be named "accident_X.csv.bz2" and be in the same directory.
#'
#' @import dplyr
#' @import tidyr
#' @import maps
#' @import graphics
#'
#' @examples
#' \dontrun{fars_map_state(13 , c(2013) )}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
