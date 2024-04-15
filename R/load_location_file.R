#' Load Location File
#'
#' Loads a CHDC location file with a flag for any duplicated settlement names
#'
#' @param path path to a .csv file with CHDC location data.
#'
#' @export
#'
load_location_file <- function(path) {
  file <- purrr::map_df(path, \(x) read.csv(x))
  dup <- create_duplicate_list(file)

  out <- file |>
    mark_all_duplicates(dup)
}

create_duplicate_list <- function(dat) {
  out <- dplyr::filter(dat, type == "node") |>
    dplyr::mutate(duplicated = duplicated(clean_string(title))) |>
    dplyr::filter(duplicated == TRUE) |>
    dplyr::select(title) |>
    unlist()
  out
}

mark_all_duplicates <- function(dat, dup) {
  out <- dplyr::mutate(dat, duplicated = ifelse(
    type == "node" & clean_string(dat$title) %in% clean_string(dup),
    TRUE, FALSE
  ))
  out
}
