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

# Global convenience variables for the data
get_bounds <- function(collection) {
  return(if (length(collection) < 1) c(0, 0) else c(min(collection), max(collection)))
}

lightTSBounds    <- get_bounds(light$timestamp)
movementTSBounds <- get_bounds(movement$timestamp)
wirelessTSBounds <- get_bounds(wireless$timestamp)
soundTSBounds    <- get_bounds(sound$timestamp)
gpsTSBounds      <- get_bounds(gps$timestamp)

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
  conditionalPanel(
    condition = "plotSelect == 'Light'",
    fluidRow(
      style = "background-color: #bbbbbb",
      
      sliderInput("lightDateRange", "Date range:",
                  min = lightTSBounds[1], max = lightTSBounds[2],
                  value = lightTSBounds,
                  timeFormat = "%Y-%m-%d %H:%M:%S")
    )
  ),
  conditionalPanel(
    condition = "plotSelect == 'Movement'",
    fluidRow(
      id = "movementParams",
      style = "background-color: #bbbbbb",
      
      sliderInput("movementDateRange", "Date range:",
                  min = movementTSBounds[1], max = movementTSBounds[2],
                  value = movementTSBounds,
                  timeFormat = "%Y-%m-%d %H:%M:%S")
    )
  ),
  conditionalPanel(
    condition = "plotSelect == 'Wireless'",
    fluidRow(
      id = "wirelessParams",
      style = "background-color: #bbbbbb",
      
      sliderInput("wirelessDateRange", "Date range:",
                  min = wirelessTSBounds[1], max = wirelessTSBounds[2],
                  value = wirelessTSBounds,
                  timeFormat = "%Y-%m-%d %H:%M:%S"),
      
      selectInput("wirelessConns", "Connections", unique(wireless$name)),
      checkboxInput("wirelessAll", "Show All", value = TRUE)
    )
  ),
  conditionalPanel(
    condition = "plotSelect == 'Sound'",
    fluidRow(
      id = "soundParams",
      style = "background-color: #bbbbbb",
      
      sliderInput("soundDateRange", "Date range:",
                  min = soundTSBounds[1], max = soundTSBounds[2],
                  value = soundTSBounds,
                  timeFormat = "%Y-%m-%d %H:%M")
    )
  ),
  conditionalPanel(
    condition = "plotSelect == 'GPS'",
    fluidRow(
      id = "gpsParams",
      style = "background-color: #bbbbbb",
      
      sliderInput("gpsDateRange", "Date range:",
                  min = gpsTSBounds[1], max = gpsTSBounds[2],
                  value = gpsTSBounds,
                  timeFormat = "%Y-%m-%d %H:%M:%S")
    )
  ),
  fluidRow(
    style = "background-color: #ddddff",
    tagList(a("GitHub page", href="https://github.com/blacktack2/DataVisThing"))
  )
)

print("Creating Server...")

server <- function(input, output, session) {
  
  observeEvent(input$wirelessAll, {
    if (input$wirelessAll) {
      disable("wirelessConns")
    } else {
      enable("wirelessConns")
    }
  })
  
  output$lightPlot <- renderPlot({
    line_graph(light, input$lightDateRange, aes(x = timestamp, y = light), FALSE)
  })
  output$movementPlot <- renderPlot({
    line_graph(movement, input$movementDateRange, aes(x = timestamp, y = f), FALSE)
  })
  output$wirelessPlot <- renderPlot({
    data <- NULL
    color <- NULL 
    if (input$wirelessAll) {
      data <- group_by(wireless, name)
      color <- wireless$name
    } else {
      data <- subset(wireless, name == input$wirelessConns)
    }
    
    line_graph(data, input$wirelessDateRange, aes(x = timestamp, y = rssi, color = color), (nrow(data) == 1))
  })
  output$soundPlot <- renderPlot({
    line_graph(sound, input$soundDateRange, aes(x = timestamp, y = f), FALSE) +
      ylab("Frequency (Hz)")
  })
  
  output$gpsPlot <- renderLeaflet({
    # Base map - Markers can be found in observer below
    bounds <- if (nrow(gps) > 1) c(min(gps$lon), min(gps$lat), max(gps$lon), max(gps$lat)) else c(-180, -90, 180, 90)
    leaflet() %>%
      addTiles() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  })
  
  observe({
    if (input$plotSelect == "GPS") {
      data <- subset(gps, as.POSIXct.Date(timestamp) %between%
                          as.POSIXct.Date(input$gpsDateRange, "UTC"))

      if (nrow(data) > 0) {
        leafletProxy("gpsPlot") %>%
          clearMarkers() %>%
          addMarkers(~lon, ~lat, data=data,
                     popup = ~as.character(timestamp),
                     label = ~as.character(timestamp))
      } else {
        leafletProxy("gpsPlot") %>%
          clearMarkers()
      }
    }
  })
  
  line_graph <- function(data, range, aes, as_point) {
    return(ggplot(data = data, aes) +
      {if(as_point) geom_point() else geom_line()} +
      scale_x_datetime(limits = range,
                       breaks = seq(range[1], range[2], length.out = 8)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)))
  }
}

print("Starting App...")

shinyApp(ui = ui, server = server)

