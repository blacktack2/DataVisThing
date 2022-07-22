inst_packages <- installed.packages()

reinstall <- function(package) {
  if (package %in% inst_packages[, 1]) {
    update.packages(package)
  } else {
    install.packages(package)
  }
}

print("Updating Libraries...")

reinstall("shiny")
reinstall("shinyjs")
reinstall("leaflet")

reinstall("jsonlite")
reinstall("anytime")
reinstall("dplyr")
reinstall("ggplot2")
reinstall("tidyr")
reinstall("purrr")
reinstall("tibble")

library("shiny")

print("Initializing App...")

options(shiny.host = "0.0.0.0")
options(shiny.port = 8000)

runApp("data-vis-app")
