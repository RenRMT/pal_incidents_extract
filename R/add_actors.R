add_actor1 <- function(slug, sec_area, vec) {
  actor1 <- dplyr::case_when(
    slug == "isr" & stringr::str_detect(tolower(vec), "^israeli air force") ~ "actors:israel-airforce",
    (slug == "pal" | slug == "lbn") & stringr::str_detect(tolower(vec), "^israeli air force") ~ "actors:israeli-airforce",
    slug == "isr" & stringr::str_detect(tolower(vec), "^israeli defense forces") ~ "actors:israel-armed-forces",
    (slug == "pal" | slug == "lbn") & stringr::str_detect(tolower(vec), "^israeli army") ~ "actors:israeli-army",
    slug == "isr" & stringr::str_detect(tolower(vec), "^israeli security forces") ~ "actors:israel-police",
    (slug == "pal" | slug == "lbn") & stringr::str_detect(tolower(vec), "^israeli security forces") ~ "actors:israeli-police",
    (slug == "pal" | slug == "lbn") & stringr::str_detect(tolower(vec), "^israeli navy") ~ "actors:israeli-navy",
    slug == "isr" & stringr::str_detect(tolower(vec), "^palestinian militants") ~ "actors:al-qassam-brigades-(hamas)",
    slug == "pal" & stringr::str_detect(sec_area, "GAZA") & stringr::str_detect(tolower(vec), "^palestinian militants") ~ "actors:al-qassam-brigades",
    slug == "pal" & stringr::str_detect(sec_area, "WEST BANK") & stringr::str_detect(tolower(vec), "^palestinian militants") ~ "actors:other-general-public",
    slug == "isr" & stringr::str_detect(tolower(vec), "fired from .{0,10}lebanon") ~ "actors:hezbollah_oag",
    stringr::str_detect(tolower(vec), "^palestinians and israeli security forces clashed") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "^armed palestinians") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "^israeli settlers") ~ "actors:israeli-settlers",
    stringr::str_detect(tolower(vec), "israelis demonstrated") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "palestinians demonstrated") ~ "actors:other-general-public",
    .default = NA
  )
}

add_actor2 <- function(slug, sec_area, vec) {
  dplyr::case_when(
    stringr::str_detect(tolower(vec), "conducted a search") &
      stringr::str_detect(tolower(vec), "no arrest") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "shells towards palestinian territories") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "shells off the coast of") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "targeting a house") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "assaulted a palestinian") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "of a palestinian") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "at israeli settlers") ~ "actors:israeli-settlers",
    stringr::str_detect(tolower(vec), "targeting a vehicle") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "targeting a group") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "various targets, including houses and buildings") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "towards israel\\.") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "arrested .{0,6}palestinian") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "palestinian casualties were reported") &
      !stringr::str_detect(tolower(vec), "militants") ~ "actors:other-general-public",
    stringr::str_detect(tolower(vec), "at israeli security forces") ~ "actors:israeli-police",
    stringr::str_detect(tolower(vec), "at an israeli security forces") ~ "actors:israeli-police",
    slug == "lbn" & stringr::str_detect(tolower(vec), "belonging to hezbollah") ~ "actors:hezbollah",
    slug == "lbn" & stringr::str_detect(tolower(vec), "military site and infrastructure") ~ "actors:hezbollah",
    slug == "pal" & stringr::str_detect(sec_area, "GAZA") & stringr::str_detect(tolower(vec), "palestinian militants") ~ "actors:al-qassam-brigades",
    .default = NA
  )
}
