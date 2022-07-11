inst_packages <- installed.packages()

reinstall <- function(package) {
  if (package %in% inst_packages[, 1]) {
    update.packages(package)
  } else {
    install.packages(package)
  }
}

reinstall("shiny")
reinstall("shinyjs")
reinstall("leaflet")

reinstall("jsonlite")
reinstall("anytime")
reinstall("dplyr")
reinstall("ggplot2")
reinstall("tidyr")

library("shiny")

options(shiny.host = "0.0.0.0")
options(shiny.port = 8000)

runApp("data-vis-app")

#ggplot(data=light, aes(x=timestamp, y=light, group=1)) +
#  geom_line() +
#  scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
#  theme(axis.text.x = element_text(angle = 90))

#ggplot(data=sound, aes(x=timestamp, y=f, group=1)) +
#  geom_line() +
#  scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
#  theme(axis.text.x = element_text(angle = 90))


