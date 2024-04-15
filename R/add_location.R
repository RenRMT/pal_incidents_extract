add_location = function(dat, vec, locs, type = c("id", "lat", "lng")) {
  locs = dplyr::coalesce(
    replace_names(trimws(dat$incursion_location)),
    extract_location_isr(vec),
    extract_location_lbn(vec),
    extract_location_wb(vec),
    extract_location_gaza(vec)
  )
  out = purrr::map_chr(locs, \(x) get_location(x, locations, type))
  out
}

extract_location_wb <- function(vec) {
  vec <- tolower(vec)
  loc <- dplyr::coalesce(
    stringr::str_extract(vec, "( in |near |vicinity of )[\\w\\s'-]+( village| town| camp| checkpoint| area)[, ]*[nsew]{1,2} of [:alpha:]+\\b"),
    stringr::str_extract(vec, "( in |near |vicinity of )[\\w'-]+ city\\b")
  )
  found <- stringr::str_extract(loc, "(?<=( in |ear | of ))[\\w\\s'-]+(?=( village| town| camp| checkpoint| city| area))")
  out = stringr::str_replace_all(
    found, c(
      "refugee" = "camp"  
    )
  )
  out
}

extract_location_gaza <- function(vec) {
  vec <- tolower(vec)
  found <- dplyr::case_when(
    stringr::str_detect(vec, "(?<=coast of )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=off the coast of )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?<=and buildings, in )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=and buildings, in )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?<=fence [nsew] of )[\\w\\s'-]+(?=\\,)") ~ stringr::str_extract(vec, "(?<=fence [nsew] of )[\\w\\s'-]+(?=\\,)"),
    stringr::str_detect(vec, "(?<=fence [nsew] of )[\\w\\s'-]+(?=\\,)") ~ stringr::str_extract(vec, "(?<=fence [nsew] of )[\\w\\s'-]+(?=\\,)"),
    stringr::str_detect(vec, "(?<=, in )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=, in )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "rafah crossing") ~ stringr::str_extract(vec, "rafah crossing"),
    stringr::str_detect(vec, "(?<=house in )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=house in )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?<=vehicle in )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=vehicle in )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?<=target in )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=target in )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?=in )[\\w\\s'-]+(?= neighborhood)") ~ stringr::str_extract(vec, "(?=in )[\\w\\s'-]+(?=neighborhood)"),
    stringr::str_detect(vec, "(?<=[nsew] of )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=[nsew] of )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "(?<=[nsew]{2} of )[\\w\\s'-]+(?=\\.)") ~ stringr::str_extract(vec, "(?<=[nsew] of )[\\w\\s'-]+(?=\\.)"),
    stringr::str_detect(vec, "the middle governorate") ~ "-N/A- Location"
  )
  
  out <- replace_names(found)
  out
}

extract_location_isr = function(vec) {
  vec = tolower(vec)
  found = dplyr::case_when(
    stringr::str_detect(vec, "towards israel[\\.]*$") ~ "Israel - unknown location",
    .default = NA
  )
  found
}

extract_location_lbn = function(vec) {
  vec = tolower(vec)
  found = dplyr::case_when(
    stringr::str_detect(vec, "(?<=in )[\\w\\s'-]+(?= (village|town) in southern lebanon)") ~ 
      stringr::str_extract(vec, "(?<=in )[\\w\\s'-]+(?= (village|town) in southern lebanon)"),
    stringr::str_detect(vec, "(?<=targeted )[\\w\\s'-]+(?= (village|town) in southern lebanon)") ~ 
      stringr::str_extract(vec, "(?<=targeted )[\\w\\s'-]+(?= (village|town) in southern lebanon)"),
    .default = NA
  )
  out = replace_names(found)
  out
}

get_location <- function(vec, dat, type = c("id", "lat", "lng")) {
  if (clean_string(vec) %in% clean_string(dplyr::filter(dat, duplicated == FALSE, type == "node")$title)) {
    out <- dplyr::filter(dat, duplicated == FALSE, type == "node", clean_string(title) == clean_string(vec)) |>
      dplyr::mutate(
        lat = stringr::str_extract(latlng, "(?<=\\{\"lat\":).+(?=, )"),
        lng = stringr::str_extract(latlng, "(?<=\"lng\":).+(?=\\})")
      ) |>
      dplyr::select(!!type) |> 
      unlist()
  } else {
    out <- NA
  }
  out
}
