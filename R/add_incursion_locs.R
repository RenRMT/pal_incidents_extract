extract_incursion_locs <- function(vec) {
  vec <- tolower(vec)
  found <- dplyr::coalesce(
    stringr::str_extract(vec, "(?<=incursions )[\\s\\S]+(?= are still ongoing.)"),
    stringr::str_extract(vec, "(?<=incursion )[\\s\\S]+(?= is still ongoing.)")
  ) |>
    gsub(" and ", " , ", x = _) |>
    stringr::str_remove_all("\\b[nsew]{1,2}\\b|\\bof\\b|\\barea\\b|\\bin\\b") |>
    stringr::str_remove("(?<=hamad city)[^,]+(?=,)") |>
    stringr::str_remove("(?<=al qarara)[^,]+(?=,)") |>
    strsplit(split = ",")
}

add_incursion_locs <- function(vec) {
  dplyr::case_when(
    stringr::str_detect(vec, "incursion") ~ extract_incursion_locs(vec),
    .default = NA
    )
}

expand_incursion_locs = function(dat, vec) {
  dplyr::mutate(dat, incursion_location = add_incursion_locs(dat[[vec]])) |> 
    tidyr::unnest(cols = incursion_location, keep_empty = TRUE)
}
