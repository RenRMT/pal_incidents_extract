# Helpers for loading PDF files -------------------------------------------
# Gets the path to the latest UNDSS report from the specified folder
get_latest_file <- function(folder) {
  file <- sort(
    list.files(
      folder,
      pattern = ".pdf"
    ),
    decreasing = TRUE
  )[1]
}

# Extract the date from a given UNDSS report filename
get_file_date <- function(file) {
  stringr::str_extract(file, "\\d{8}") |>
    lubridate::as_date()
}

# Helpers for string detection/comparison ---------------------------------
clean_string <- function(string) {
  string |>
    stringr::str_remove_all("[:punct:]") |>
    # whitespace
    stringr::str_remove_all("\\s+") |>
    tolower()
}

# General helpers ---------------------------------------------------------
#save latest report to txt file
save_latest = function(file, path = getwd()) {
  writeLines(file, path)
}
# read latest report from txt file
read_latest = function(path = getwd()) {
  readLines(path)
}

# changing the spelling of certain names from UNDSS convention to CHDC convention
replace_names = function(vec) {
  out = vec |> 
    stringr::str_replace_all(
      c(
        "beit lahia" = "beit lahiya",
        "jabalia" = "jabalya",
        "beit hanoun" = "beit hanun",
        "an nuseirat" = "an nuseirat camp",
        "the middle governorate" = "-N/A- Location",
        "gaza city" = "-N/A- urban sub-division-gaza",
        "as-shuja’iyah" = "ash shuja'iyeh - ijdeedeh",
        "tal al hawa" = "tal el hawa",
        "beach camp" = "al shati camp",
        "shiek radwan" = "ash sheikh radwan",
        "at-toffah" = "at tuffah",
        "az-zawaida" = "az zawayda",
        "al-mosadar" = "al musaddar",
        "an- nuseirat" = "an nuseirat camp",
        "bani suhaila" = "bani suheila",
        "al fukhari" = "al fukhkhari",
        "maa’n" = "ma'n",
        "maa’" = "ma'n",
        "al mahattah" = "al mahatta",
        "al katebeh" = "al kateebeh",
        "hamad city" = "hamad",
        "ayta ash shab" = 	"aayta ech-chaab",
        "odaisseh" = 	"aadaysseh marjaayoun",
        "kafra"	= "kafra bant jbayl",
        "mays al-jabal" = 	"meiss ej-jabal",
        "kafr kila" = "kfar kila",
        "elmah" = "aalma ech-chaab"
      )
    )
    out
}
