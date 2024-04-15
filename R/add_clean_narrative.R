add_clean_narrative <- function(vec) {
  vec |>
    add_terminology() |>
    replace_compass() |>
    replace_acronyms() |>
    replace_terminology() |>
    remove_date_time()
}

add_terminology <- function(vec) {
  out <- dplyr::case_when(
    stringr::str_detect(vec, "[:alpha:]{4}[ ]*[:digit:]{2} [:alpha:]{3} [:digit:]{2}[, ]*") ~ paste(vec, "This incident took place over the course of 24 hours.", sep = " "),
    stringr::str_detect(vec, "^[:digit:]{2} [:alpha:]{3} [:digit:]{2}[, ]*") ~ paste(vec, "This incident took place over the course of 24 hours.", sep = " "),
    .default = vec
  )
}

replace_compass <- function(vec) {
  vec |>
    stringr::str_replace_all(c(
      "\\bE\\b" = "east",
      "\\bN\\b" = "north",
      "\\bW\\b" = "west",
      "\\bS\\b" = "south",
      "\\bNE\\b" = "north-east",
      "\\bNW\\b" = "north-west",
      "\\bSE\\b" = "south-east",
      "\\bSW\\b" = "south-west"
    ))
}

replace_acronyms <- function(vec) {
  vec |>
    stringr::str_replace_all(
      c(
        "\\bCEDAW\\b" = "Convention on the Elimination of all Forms of Discrimination Against Women",
        "\\bDFLP\\b" = "Democratic Front for the Liberation of Palestine",
        "\\bDSR\\b" = "Daily Situation Report",
        "\\bEA\\b" = "Emergency Appeal (EA)",
        "\\bEOD\\b" = "Explosive Ordnance Disposal",
        "\\bGFO\\b" = "Gaza Field Office (UNRWA)",
        "\\bIAF\\b" = "Israeli Air Force",
        "\\bICRC\\b" = "International Committee of the Red Cross",
        "\\bIDF\\b" = "Israeli Defense Forces",
        "\\bIED\\b" = "Improvised Explosive Device",
        "\\bIL\\b" = "Israeli",
        "\\bIDPs\\b" = "internally displaced persons",
        "\\bISF\\b" = "Israeli Security Forces",
        "\\bLGBTI\\b" = "Lesbian, Gay, Bisexual, Transgender, and Intersex",
        "\\bMK\\b" = "Member of the Knesset",
        "\\bNGO\\b" = "Non-Governmental Organization",
        "\\bPA\\b" = "Palestinian Authority",
        "\\bPBIED\\b" = "Person-borne Improvised Explosive Device",
        "\\bPFLP\\b" = "Popular Front for the Liberation of Palestine",
        "\\bPIJ\\b" = "Palestinian Islamic Jihad",
        "\\bPL\\b" = "Palestinian",
        "\\bPLC\\b" = "Palestinian Legislative Council",
        "\\bPLO\\b" = "Palestine Liberation Organization",
        "\\bPM\\b" = "Prime Minister",
        "\\bPRC\\b" = "Popular Resistance Committee",
        "\\bPRS\\b" = "Palestinian refugees from Syria",
        "\\bRSSP\\b" = "Relief and Social Services Program",
        "\\bPSF\\b" = "Palestinian Security Forces",
        "\\bRCIED\\b" = "Remote-controlled Improvised Explosive Device",
        "\\bUAV\\b" = "Unmanned Aerial Vehicle",
        "\\bUO\\b" = "Ultra-Orthodox Members",
        "\\bUVIED\\b" = "Under Vehicle Improvised Explosive Device",
        "\\bUXO\\b" = "Unexploded Ordnance",
        "\\bVBIED\\b" = "Vehicle-borne Improvised Explosive Device"
      )
    )
}

replace_terminology <- function(vec) {
  vec |>
    stringr::str_replace_all(
      c(
        "\"Hezbollah\"" = "Hezbollah",
        "“Hezbollah”" = "Hezbollah",
        "air raid" = "airstrike",
        "air strike" = "airstrike",
        "Israeli Air Force" = "Israeli Air Force (IDF)",
        # assumption IDF = Army if not further specified
        "Israeli Defense Forces" = "Israeli Army (IDF)",
        "Israeli Navy" = "Israeli Navy (IDF)",
        "live bullet" = "gunshot",
        "live rounds" = "shots",
        "numbers of" = "multiple",
        "pipe bomb" = "IED",
        "quadcopter drone" = "drone",
        "Ultra-Orthodox" = "Haredi"
      )
    )
}

remove_date_time <- function(vec) {
  vec |>
    stringr::str_remove_all("[:alpha:]{0,4} [:digit:]{2} [:alpha:]{3} [:digit:]{2}[, ]*") |>
    stringr::str_remove_all("[:alpha:]{2} [:digit:]{2} [:alpha:]{3} [:digit:]{2}[, ]*") |>
    stringr::str_remove_all("[:alpha:]{0,4}[ ]*[:digit:]{2} [:alpha:]{3} [:digit:]{2}[, ]*") |>
    stringr::str_remove_all("[:digit:]{4} hrs[, ]*")
}
