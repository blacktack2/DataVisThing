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

print("Defining Constants...")

graphHeight <- "400px"
inputAreaColor <- "#bbbbbb"

dateRangeSliderIDs <- c() # Populated in plotRow during creation of UI

print("Defining functions...")

plotRow <- function(idPrefix, name, ..., plotType = plotOutput) {
  toggleID <- sprintf("%sToggle"   , idPrefix)
  sliderID <- sprintf("%sDateRange", idPrefix)
  plotID   <- sprintf("%sPlot"     , idPrefix)
  
  dateRangeSliderIDs <<- append(dateRangeSliderIDs, sliderID)
  
  return(
    fluidRow(
      fluidRow(
        style = "background-color: #888888; padding-left: 20px; border: 1px solid #666666",
        checkboxInput(toggleID, name, value = TRUE),
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 1", toggleID),
        fluidRow(
          column(
            3,
            style = sprintf(
              "background-color: %s;
              padding-left: 20px;
              height: %s;
              overflow-x: hidden;
              overflow-y: scroll",
              inputAreaColor, graphHeight
            ),
            sliderInput(sliderID, "Date range:",
                        min = dataTSBounds[1], max = dataTSBounds[2],
                        value = dataTSBounds,
                        timeFormat = "%Y-%m-%d %H:%M:%S",
                        width="100%"),
            ...
          ),
          column(
            9,
            plotType(plotID, height = graphHeight)
          )
        )
      )
    )
  )
}

inlineCheckbox <- function(checkbox) {
  return(
    span(style = "display: inline-block; width: inherit", checkbox)
  )
}

# Remove all columns with only NA and the type column
clean_columns <- function(df) {
  cleaned <- Filter(function(x)!all(is.na(x)), df)
  cleaned <- subset(cleaned, select = -c(dtype))
  return(cleaned)
}

get_bounds <- function(collection) {
  return(if (length(collection) < 1) c(0, 0) else c(min(collection), max(collection)))
}

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

# Split data into it's different types
light    <- subset(df, dtype == "light"   )[c("timestamp", "light")]
movement <- subset(df, dtype == "movement")[c("timestamp", "x", "y", "z", "f")]
wireless <- subset(df, dtype == "wireless")[c("timestamp", "wtype", "name", "addr", "rssi")]
sound    <- subset(df, dtype == "sound"   )[c("timestamp", "f")]
gps      <- subset(df, dtype == "gps"     )[c("timestamp", "lat", "lon")]

# Global convenience variables for the data
lightTSBounds    <- get_bounds(light$timestamp)
movementTSBounds <- get_bounds(movement$timestamp)
wirelessTSBounds <- get_bounds(wireless$timestamp)
soundTSBounds    <- get_bounds(sound$timestamp)
gpsTSBounds      <- get_bounds(gps$timestamp)

dataTSBounds     <- get_bounds(df$timestamp)

wirelessNames    <- unique(wireless$name)
wirelessNameColors <- setNames(topo.colors(length(wirelessNames)), wirelessNames)

print("Creating UI...")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Vis"),
  
  fluidRow(
    style = sprintf(
      "background-color: %s;
      padding-left: 20px;",
      inputAreaColor
    ),
    sliderInput(
      "globalDateRange", "Date Range (All):",
      min = dataTSBounds[1], max = dataTSBounds[2],
      value = dataTSBounds,
      timeFormat = "%Y-%m-%d %H:%M:%S",
      width="100%")
  ),
  
  plotRow("light", "Light"),
  plotRow("movement", "Movement"),
  plotRow("wireless", "Wireless",
          inlineCheckbox(checkboxInput("wirelessAll", "Select All", value = TRUE)),
          inlineCheckbox(checkboxInput("wirelessNone", "Deselect All", value = FALSE)),
          checkboxGroupInput("wirelessConns", "Connections:",
                             selected = wirelessNames,
                             inline = TRUE,
                             choiceNames = lapply(
                               wirelessNames,
                               function (x) span(x, style = paste0("color: ", wirelessNameColors[x]))
                             ),
                             choiceValues = wirelessNames)),
  plotRow("sound", "Sound"),
  plotRow("gps", "GPS", plotType = leafletOutput),
  
  fluidRow(
    style = "background-color: #ddddff",
    tagList(a("GitHub page", href="https://github.com/blacktack2/DataVisThing"))
  )
)

print("Creating Server...")

server <- function(input, output, session) {
  
  observeEvent(input$globalDateRange, {
    for (slider in dateRangeSliderIDs) {
      updateSliderInput(session, slider, value = input$globalDateRange)
    }
  })
  
  observeEvent(input$wirelessAll, {
    updateCheckboxInput(session, "wirelessAll", value = TRUE)
    updateCheckboxGroupInput(session, "wirelessConns", selected = wirelessNames)
  }, ignoreInit = TRUE)
  observeEvent(input$wirelessNone, {
    updateCheckboxInput(session, "wirelessNone", value = FALSE)
    updateCheckboxGroupInput(session, "wirelessConns", selected = vector(mode = "list", length = 0))
  }, ignoreInit = TRUE)
  
  output$lightPlot <- renderPlot({
    data <- subset(light, as.POSIXct.Date(timestamp) %between%
                     as.POSIXct.Date(input$lightDateRange, "UTC"))
    
    ggplot(data = data, aes(x = timestamp, y = light)) +
      geom_line_point_(data) +
      scale_x_datetime_(light, input$lightDateRange) +
      theme_()
  })
  output$movementPlot <- renderPlot({
    data <- subset(movement, as.POSIXct.Date(timestamp) %between%
                     as.POSIXct.Date(input$movementDateRange, "UTC"))
    
    ggplot(data = data, aes(x = timestamp, y = f)) +
      geom_line_point_(data) +
      scale_x_datetime_(movement, input$movementDateRange) +
      theme_()
  })
  output$wirelessPlot <- renderPlot({
    data <- subset(wireless, name %in% input$wirelessConns)
    data <- subset(data, as.POSIXct.Date(timestamp) %between%
                     as.POSIXct.Date(input$wirelessDateRange, "UTC"))
    
    ggplot(data = data, aes(x = timestamp, y = rssi, group = name, color = name)) +
      geom_line_point_(data) +
      scale_x_datetime_(data, input$wirelessDateRange) +
      theme_() +
      scale_color_manual(values = wirelessNameColors)
  })
  output$soundPlot <- renderPlot({
    data <- subset(sound, as.POSIXct.Date(timestamp) %between%
                     as.POSIXct.Date(input$soundDateRange, "UTC"))
    
    ggplot(data = data, aes(x = timestamp, y = f)) +
      geom_line_point_(data) +
      scale_x_datetime_(sound, input$soundDateRange) +
      theme_() +
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
    if (input$gpsToggle == 1) {
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
  
  geom_line_point_ <- function(data) {
    return ({if(nrow(data) == 1) geom_point(show.legend = FALSE) else geom_line(show.legend = FALSE)})
  }
  scale_x_datetime_ <- function(data, range) {
    return (scale_x_datetime(limits = range,
                             breaks = seq(range[1], range[2], length.out = 8)))
  }
  theme_ <- function() {
    return (theme(axis.text.x = element_text(angle = 30, hjust = 1)))
  }
}

print("Starting App...")

shinyApp(ui = ui, server = server)

