## code to prepare `openSchemes` dataset goes here

# Edit in "data-raw/openSchemes.csv"
openSchemes <- readr::read_csv("data-raw/openSchemes.csv")

# write object
usethis::use_data(openSchemes, overwrite = TRUE)
