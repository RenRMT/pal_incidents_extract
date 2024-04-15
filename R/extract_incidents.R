#' Extract incidents from PDF report
#'
#' @param dat a character vector. Expects output from `read_pdf_file()`
#' @sec_areas a dataframe containing the security areas & governorate headers from the report
#'
#' @export

extract_incidents <- function(dat, sec_areas) {
  dat <- pre_clean_loaded_pdf(dat)
  file <- purrr::map_df(1:nrow(sec_areas), \(x) {
    dplyr::bind_cols(
      governorate = sec_areas[x, "governorate"],
      report = select_incident_range(
        dat,
        sec_areas[x, "governorate"],
        sec_areas[x, "end_at"]
      ) |>
        extract_incident_description()
    )
  })

  out <- dplyr::left_join(
    dplyr::select(sec_areas, !end_at),
    file,
    by = "governorate"
  ) |>
    dplyr::filter(!is.na(report))
}

select_incident_range <- function(dat, start_at, end_at) {
  stringr::str_extract(
    dat,
    paste0(
      "(?<=",
      gsub(")", "\\)", gsub("(", "\\(", gsub("^Gaza Gov", "The Gaza Gov", start_at), fixed = TRUE), fixed = TRUE),
      ")[\\S\\s]+(?=",
      gsub(")", "\\)", gsub("(", "\\(", gsub("^Gaza Gov", "The Gaza Gov", end_at), fixed = TRUE), fixed = TRUE),
      ")"
    )
  )
}

extract_incident_description <- function(dat) {
  out <- dat |>
    stringr::str_extract_all("(?<=•)[^•]+(?=\\n)") |>
    unlist() |>
    stringr::str_remove_all("\\n") |>
    stringr::str_replace_all("\\s{2,}", " ") |>
    trimws()
  out
}

# some minor cleaning for the loaded PDF file, there's no good other place to do this
pre_clean_loaded_pdf <- function(dat) {
  dat |>
    gsub("Israel, the occupied Palestinian territories, and Jerusalem/Division of Regional Operations\\n", "", x = _) |>
    gsub("UNDSS Jerusalem\\n", "", x = _) |>
    gsub("\nGaza Governorate", "\nThe Gaza Governorate", x = _)
}
