library("shiny")
library("shinyjs")
library("leaflet")

library("jsonlite")
library("anytime")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")
library("purrr")
library("tibble")

print("Loading Data...")

# Load and combine all json datasets into a single dataframe
path = "../cleaned"
dataFiles <- list.files(path, "*.json", full.names = TRUE)

print(paste("-Reading from files:", toString(dataFiles)))

df_raw <- dataFiles %>%
  map_df(~fromJSON(file.path(path, .), flatten=TRUE))

print(paste("-Raw data read with", ncol(df_raw), "columns:", toString(names(df_raw))))

initial_columns = c(
  timestamp=NA, type=NA,
  payload.light=NA,
  payload.x=NA, payload.y=NA, payload.z=NA, payload.f=NA,
  payload.type=NA, payload.name=NA, payload.addr=NA, payload.rssi=NA,
  payload.lat=NA, payload.lon=NA
)

df_raw <- df_raw %>% add_column(!!!initial_columns[setdiff(names(initial_columns), names(df_raw))])

print(paste("-Added any missing columns:", toString(names(df_raw))))

print("Cleaning Data...")

print("-Renaming columns...")

df <- df_raw %>%
  rename(
    timestamp=timestamp, dtype=type,
    light=payload.light,
    x=payload.x, y=payload.y, z=payload.z, f=payload.f,
    wtype=payload.type, name=payload.name, addr=payload.addr, rssi=payload.rssi,
    lat=payload.lat, lon=payload.lon
    )

print(paste("--Columns renamed to:", toString(names(df))))
print("-Casting data to correct types...")

df$timestamp <- anytime(df$timestamp)

print("--Timestamps converted from unix epoch to datetime")

# Remove all columns with only NA and the type column
clean_columns <- function(df) {
  cleaned <- Filter(function(x)!all(is.na(x)), df)
  cleaned <- subset(cleaned, select = -c(dtype))
  return(cleaned)
}

# Split data into it's different types
light    <- subset(df, dtype == "light"   )[c("timestamp", "light")]
movement <- subset(df, dtype == "movement")[c("timestamp", "x", "y", "z", "f")]
wireless <- subset(df, dtype == "wireless")[c("timestamp", "wtype", "name", "addr", "rssi")]
sound    <- subset(df, dtype == "sound"   )[c("timestamp", "f")]
gps      <- subset(df, dtype == "gps"     )[c("timestamp", "lat", "lon")]
# light    <- clean_columns(subset(df, dtype == "light"))
# movement <- clean_columns(subset(df, dtype == "movement"))
# wireless <- clean_columns(subset(df, dtype == "wireless"))
# sound    <- clean_columns(subset(df, dtype == "sound"))
# gps      <- clean_columns(subset(df, dtype == "gps"))

print("Creating UI...")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Vis"),
  
  fluidRow(
    tabsetPanel(
      id = "plotSelect",
      tabPanel("Light",    plotOutput("lightPlot")),
      tabPanel("Movement", plotOutput("movementPlot")),
      tabPanel("Wireless", plotOutput("wirelessPlot")),
      tabPanel("Sound",    plotOutput("soundPlot")),
      tabPanel("GPS",      leafletOutput("gpsPlot"))
    )
  ),
  fluidRow(
    id = "wirelessParams",
    style = "background-color: #bbbbbb",
    
    selectInput("wirelessConns", "Connections", unique(wireless$name)),
    checkboxInput("wirelessAll", "Show All", value = TRUE)
  ),
  fluidRow(
    style = "background-color: #ddddff",
    tagList(a("GitHub page", href="https://github.com/blacktack2/DataVisThing"))
  )
)

print("Creating Server...")

server <- function(input, output, session) {
  
  plotSelection <- reactiveValues(current = "")
  
  observeEvent(input$plotSelect, {
    if (input$plotSelect == "Wireless") {
      show("wirelessParams")
    } else {
      hide("wirelessParams")
    }
  })
  
  observeEvent(input$wirelessAll, {
    if (input$wirelessAll) {
      disable("wirelessConns")
    } else {
      enable("wirelessConns")
    }
  })
  
  output$lightPlot <- renderPlot({
    ggplot(data=light, aes(x=timestamp, y=light, group=1)) +
      geom_line() +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
      theme(axis.text.x = element_text(angle = 90))
  })
  output$movementPlot <- renderPlot({
    ggplot(data=movement, aes(x=timestamp, y=f)) +
      geom_line()
  })
  output$wirelessPlot <- renderPlot({
    if (input$wirelessAll) {
      data <- group_by(wireless, name)
      ggplot(data=data, aes(x=timestamp, y=rssi, color=name)) +
        {if(nrow(data) > 1) geom_line() else geom_point()}
    } else {
      data <- subset(wireless, name == input$wirelessConns)
      ggplot(data=data, aes(x=timestamp, y=rssi)) +
        {if(nrow(data) > 1) geom_line() else geom_point()}
    }
  })
  output$soundPlot <- renderPlot({
    ggplot(data=sound, aes(x=timestamp, y=f, group=1)) +
      geom_line() +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
      theme(axis.text.x = element_text(angle = 90)) +
      ylab("Frequency (Hz) ((this might be wrong))")
  })
  output$gpsPlot <- renderLeaflet({
    leaflet(data = gps) %>%
      addTiles() %>%
      addMarkers(popup = ~as.character(timestamp), label = ~as.character(timestamp))
  })
}

shinyApp(ui = ui, server = server)

