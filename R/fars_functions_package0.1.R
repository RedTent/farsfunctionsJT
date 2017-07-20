#' Read a datafile from the Fatality Analysis Reporting System
#'
#' @description This function reads a csv datafile. Though designed speficially to read FARS data it can be used to read all kinds of csv files. It uses the \code{readr::read_csv}) function
#'
#' @param filename A character string with the filename. The file should be in csv-format. Zipped csv-files can also be read
#'
#' @return Returns a tibble with all the data from the csv file
#'
#' @importFrom readr read_csv
#'
#' @details If the file doesn't exists it will throw an error.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2"))
#' fars_read(make_filename(year=2015))
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Helper function to create a characterstring with the name of a FARS datafile of specific year
#' @description Helper function to create a characterstring with the name of a FARS datafile of specific year
#' @param year , the year of which you want to load the FARS data. Should be an integer. The function will try to coerce this to an integer
#'
#' @return returns a character string of the filename to be loaded, can be used together with the fars_read() function
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_filename(year=2015)
#' make_filename(2015)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Helper function to create a summary of FARS data
#'
#' @description This function is a helper function for the fars_summarize_years It reads the FARS data for
#' the years that are provided and makes a list with a tibble for each year that was supplied.
#' Each tibble in the list contains the months and years of the observations
#'
#' @param years , a vector with the years of FARS you want read from file. The years will be coerced to integers
#'
#' @return function returns a list with a tibble for each year that was supplied. Each tibble in the list
#' contains the months and years of the observations
#'
#' @details make sure that the files for the years that are provided in the years argument do exist.
#' The function will create a warning if there isn't a file for the year that is provided
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- fars_read_years(c(2013,2014,2015))
#' }
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

#' Function to create a summary of FARS data
#'
#' @description This function creates a summary of FARS datafor the selected years. The summary consists of the count the monthly observation per year.
#'
#' @param years , a vector with the years of FARS you want to summarize. The years will be coerced to integers
#'
#' @return returns a tibble with the number of FARS observation per month for the provided years
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' #' fars_summarize_years(c(2013,2014,2015))
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                tidyr::spread(year, n)
}



#' Create a state map with FARS accident locations in a specific year
#'
#' @description This function creates a map for the state that is selected. On the map all the locations of accidents of the selected year are plotted.
#'
#' @param state.num Integer number indicating the state you want to plot
#' @param year the year of which you want to view the FARS data. The year will be coerced to an integer
#'
#' @return This function creates a map with the FARS-locations plotted. Returns a nULL
#' @export
#'
#' @details Make sure that the \code{state.num} argument refers to a state that exists.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_map_state(state.num=5,year = 2015)
#' }
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


# testline
