#' Loads a CSV file
#'
#' @description
#' The function loads a CSV file, defined by \code{filename} argument and 
#' returns a 
#' The function loads a CSV file defined by \code{filename} argument and returns
#' a tibble (Modern form of a data.frame).
#' CHECK: If path is not valid the function will end with an error.
#'
#' @param filename Path to the CSV file (character)
#'
#' @return The function returns a tibble (data.frame).
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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

#' Creates a filename
#'
#' @description
#' The function creates a filename for a .csv.bz2 file using the \code{year} as 
#' argument in a form "accident_<year>.csv.bz2".
#' Requires numerical (or integer) input (Error if not).
#'
#' @param year Numerical (force to integer) input indicating a year of a data set.
#'
#' @return Returns a character string in a format "accident_<year>.csv.bz2" that
#' can be used as a file name
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year from accident files
#'
#' @description
#' The function takes a vector (or list) of years and returns a list of data
#' frames with month and year columns based on data "accident_<year>.csv.bz2
#' files. 
#' The files need to be located in the working directory.
#'
#' @param years Vector (or list) of years in numeric format.
#'
#' @return Returns a list of data frames (tibble) "accident_<year>.csv.bz2" data
#' Returns NULL and a warning if the file does not exist.
#'
#' @importFrom dplyr %>% mutate select
#'
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

#' Counts number of accidents per month and year
#'
#' @description
#' Using the list of years, this function calculates the number of accidents per 
#' month. The accident files must have in the working directory, years can be
#' provided as list or vector.
#'
#' @param years A vector or list of years (numeric or integer) that will be
#' searched in the data
#'
#' @return Returns a pivot tibble (data frame) with months in rows and selected
#' years in columns containing the number of accidents. Warning for each year 
#' that does not exist in the datasets. Error (no results) if a different than
#'  numeric or integer input is presented.
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the accidents on a US state map
#'
#' This function requires a state number and year (numeric -or integer- both), 
#' and plots the accidents in a map. The state number must be exist in FARS 
#' dataset (error if not) and data file must exist too.
#'
#' @param state.num The number of a state in the US as used in the FARS data
#' sets. Must be numeric or integer.
#' @param year The year of analysis (numeric or integer)
#'
#' @return Returns a plot of the accidents based on the \code{state.num} and
#' \code{year} inputs. Error if not exists.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
