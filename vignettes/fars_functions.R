#' Reading FARS data
#'
#' The function reads FARS (Fatality Analysis Reporting System) data from a csv file.
#' If the file does not exist, an error message is shown.
#' @param filename character string, ath to the csv file
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return Returns a tibble with the lodaded FARS data.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Creating file name
#'
#' The function generates the filename for a FARS data file, based on the input year.
#'
#' @param year integer, The year for which the filename is generated
#'
#' @return A character string representing the generated filename. Output is the file name of the form accident_year.csv.bz2, where year is specified by the user
#'
#' @examples
#' make_filename(2019)
#' \dontrun{
#' make_filename(2019)
#' }
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reading FARS data for multiple years
#'
#' The function reads FARS data for multiple years and combines them into a tibble.
#' If the year is not valid, the warning message is shown.
#'
#' @param years integer, a vector of years
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @return returns a tibble object with FARS data for specified years by month. Imported file is imported via function fars_read. If the year is not existing in the data
#' then warning gets created stating "invalid year"
#'
#' @examples
#' far_read(fars_read_years)
#' \dontrun{
#' far_read(fars_read_years)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat,  year = "YEAR") %>%
        dplyr::select("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarizing FARS data for multiple years
#'
#' The function summarizes FARS data for multiple years, counting accidents per month.
#'
#' @param years integer, vector of years
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#' @return A tibble summarizing the number of accidents per month for each year
#'
#' @examples
#' fars_summarize_years(2013)
#' \dontrun{
#' fars_summarize_years(2013)
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by("year", "MONTH") %>%
    dplyr::summarize(n = "n()") %>%
    tidyr::spread("year", "n")
}
#' Mapping FARS data for a specific state and year
#'
#' The function shows FARS data on a map for a specific state and year.
#' If the state number is invalid, the error message is shown.
#'
#' @param year integer, the year for which the data is plotted
#' @param state.num A number between 1 and 56 representing the states in USA
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return A plot of the state and dots representing the places where the accidents happened. If state.num representing state number is not valid, we stop. Furthermore,
#' if for one state there is not data, this means there are no accidents
#'
#' @examples
#' fars_map_state(1,2013)
#' \dontrun{
#' fars_map_state(1,2013)
#' #' }

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
    }$
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
      maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                xlim = range(LONGITUD, na.rm = TRUE))
      graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}


