# Mock data for testing the data visualizations
library(dplyr)

data_recruitments <- readr::read_csv("data-raw/data-recruitments.csv")
data_assessments  <- readr::read_csv("data-raw/data-assessments.csv") |>
  # only include full years from BL to Y6
  filter(!stringr::str_detect(event, "\\.")) |>
  filter(event != "6 Year")

usethis::use_data(data_recruitments, overwrite = TRUE)
usethis::use_data(data_assessments,  overwrite = TRUE)
