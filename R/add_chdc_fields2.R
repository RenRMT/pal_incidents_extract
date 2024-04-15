add_incident_id <- function(dat) {
  timestamp = round(as.numeric(Sys.time()))
  (timestamp + 1):(timestamp + nrow(dat)) |>
    as.hexmode() |>
    gsub("^.{1}", "a", x = _)
}

add_slug <- function(vec, narrative) {
  dplyr::case_when(
    stringr::str_detect(tolower(narrative), "fired from .{0,10}lebanon") ~ "isr",
    stringr::str_detect(tolower(narrative), "towards israel") ~ "isr",
    vec == "1.    SECURITY AREA WEST BANK" ~ "pal",
    vec == "2. SECURITY AREA JERUSALEM:" &
      stringr::str_detect(tolower(narrative), "old city|east jerusalem|israeli security forces")~ "pal",
    vec == "2. SECURITY AREA JERUSALEM:" ~ "isr",
    vec == "3. SECURITY AREA NORTHERN ISRAEL:" ~ "isr",
    vec == "4. SECURITY AREA ISRAEL CENTRAL AND SOUTH:" ~ "isr",
    vec == "5. SECURITY AREA GAZA:" ~ "pal",
    vec == "SIGNIFICANT SECURITY INCIDENTS IN THE REGION" ~ "lbn",
    .default = NA
  )
}

add_datetime <- function(vec) {
  dt <- vec |>
    stringr::str_extract("[:digit:]{1,2} [:alpha:]{3,4} [:digit:]{2}[, ]*[:digit:]*") |>
    gsub(",", "", x = _)
  out <- ifelse(nchar(dt) < 12, paste0(dt, "0000"), dt) |>
    lubridate::parse_date_time("d b y HM") |>
    format("%d/%m/%Y %H:%M")
  # substr(start = 1, stop = 16)
  out
}

add_funcloc <- function(vec) {
  dplyr::case_when(
    stringr::str_detect(tolower(vec), "checkpoint") ~ "location-relative:vehicle-check-point-vcp",
    stringr::str_detect(tolower(vec), "check point") ~ "location-relative:vehicle-check-point-vcp",
    stringr::str_detect(tolower(vec), "search operation") ~ "location-relative:private-residence",
    stringr::str_detect(tolower(vec), "house") ~ "location-relative:private-residence",
    stringr::str_detect(tolower(vec), "school") ~ "location-relative:education-facility",
    stringr::str_detect(tolower(vec), "university") ~ "location-relative:education-facility",
    stringr::str_detect(tolower(vec), "agricultural") ~ "location-relative:farmland",
    stringr::str_detect(tolower(vec), "camp") ~ "location-relative:refugee-idp-camp",
    stringr::str_detect(tolower(vec), "military site") ~ "location-relative:minor-outpost-or-camp-any-actor",
    stringr::str_detect(tolower(vec), "towards israel") ~ "location-relative:lrna",
    stringr::str_detect(tolower(vec), "off the coast") ~ "location-relative:lrna",
    stringr::str_detect(tolower(vec), "at the security fence") ~ "location-relative:lrna",
    stringr::str_detect(tolower(vec), "travelling") ~ "location-relative:road-roadside",
    stringr::str_detect(tolower(vec), "impacted idf post") ~ "location-relative:minor-outpost-or-camp-any-actor",
    stringr::str_detect(tolower(vec), "fired from southern lebanon") ~ "location-relative:international-border",
    .default = NA
  )
}


add_act <- function(vec) {
  dplyr::case_when(
    # West Bank
    stringr::str_detect(tolower(vec), "search operation") &
      stringr::str_detect(tolower(vec), "no arrest") ~ "actsnew:confine-cordonsearch",
    stringr::str_detect(tolower(vec), "confiscated") ~ "actsnew:theft-armed-robbery-firearm",
    stringr::str_detect(tolower(vec), "demolished") ~ "actsnew:demolition-vehicle",
    stringr::str_detect(tolower(vec), "assaulted") ~ "actsnew:attack-hand-assault",
    stringr::str_detect(tolower(vec), "arrested") ~ "actsnew:confine-arrest",
    # Gaza - ISR
    stringr::str_detect(tolower(vec), "air raid") &
      stringr::str_detect(tolower(vec), "missile") ~ "actsnew:attack-combination-co4",
    stringr::str_detect(tolower(vec), "air raid") ~ "actsnew:attack-platforms-airstrike",
    stringr::str_detect(tolower(vec), "airstrike") ~ "actsnew:attack-platforms-airstrike",
    stringr::str_detect(tolower(vec), "air strike") ~ "actsnew:attack-platforms-airstrike",
    stringr::str_detect(tolower(vec), "missile") ~ "actsnew:attack-artillery-rockets",
    stringr::str_detect(tolower(vec), "tank shells") ~ "actsnew:attack-platforms-fighting vehicle",
    stringr::str_detect(tolower(vec), "shells") &
      stringr::str_detect(tolower(vec), "off the coast") ~ "actsnew:attack-platforms-warship",
    stringr::str_detect(tolower(vec), "incursion") ~ "actsnew:attack-small-arms",
    # Gaza - PAL
    stringr::str_detect(tolower(vec), "rockets") ~ "actsnew:attack-artillery-rockets",
    stringr::str_detect(tolower(vec), "mortar shell") ~ "actsnew:attack-light-weapons",
    # Israel
    stringr::str_detect(tolower(vec), "demonstrated") ~ "actsnew:rule-peaceful-demonstration",
    stringr::str_detect(tolower(vec), "protest") ~ "actsnew:rule-peaceful-demonstration",
    stringr::str_detect(tolower(vec), "stabbed") ~ "actsnew:attack-hand-armed-assault",
    stringr::str_detect(tolower(vec), " shot | shots ") ~ "actsnew:attack-small-arms",
    # other
    stringr::str_detect(tolower(vec), "threw stones") ~ "actsnew:attack-hand-armed-assault",
    .default = NA
  )
}

add_confidential_note <- function(date = Sys.Date()) {
  out <- paste0("UNDSS Report ", gsub("-", "/", x = date))
  out
}

add_clean_narrative_second_pass = function(slug, sec_area, vec) {
  dplyr::case_when(
    stringr::str_detect(sec_area, "GAZA") ~ stringr::str_replace_all(vec, "Palestinian militants", "Al-Qassam Brigades (Hamas)"),
    .default = vec
  )

}

# create incident df ------------------------------------------------------

create_incident_df <- function(incidents, date, locations) {
  out <- tibble::as_tibble(incidents) |>
    dplyr::mutate(
      slug = add_slug(sec_area, report),
      narrative = add_clean_narrative(report),
      date_time = add_datetime(report),
      location = add_location(incidents, report, locations, "id"),
      lat = add_location(incidents, report, locations, "lat"),
      lng = add_location(incidents, report, locations, "lng"),
      func_location = add_funcloc(report),
      actor1 = add_actor1(slug, sec_area, narrative),
      actor2 = add_actor2(slug, sec_area, narrative),
      act = add_act(report),
      conf_note = add_confidential_note(date = date),
      incident_id = paste(slug, add_incident_id(incidents), sep = ":"),
      narrative = add_clean_narrative_second_pass(slug, sec_area, narrative)
    ) |> 
    dplyr::select(-incursion_location)
}
