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

normalPalette <- topo.colors
dayColorHigh  <- topo.colors(2)[1]
dayColorLow   <- topo.colors(2)[2]

dateRangeSliderIDs <- c() # Populated in plotRow during creation of UI

print("Defining functions...")

plotRow <- function(idPrefix, name, ..., plotType = plotOutput) {
  toggleID   <- sprintf("%sToggle"   , idPrefix)
  dateTimeID <- sprintf("%sDateTimeRange", idPrefix)
  plotID     <- sprintf("%sPlot"     , idPrefix)
  dateID     <- sprintf("%sDateRange" , idPrefix)
  
  dateRangeSliderIDs <<- append(dateRangeSliderIDs, dateTimeID)
  
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
            class = "input-area graph-input",
            sliderInput(dateTimeID, "Date range:",
                        min = dataTSBounds[1], max = dataTSBounds[2],
                        value = dataTSBounds,
                        timeFormat = "%Y-%m-%d %H:%M:%S",
                        width="100%", step = 1),
            conditionalPanel(
              condition = "input.dateMode == 1",
              sliderInput(dateID, "Date range:",
                          min = dataDateBounds[1], max = dataDateBounds[2],
                          value = range(dataDateBounds),
                          timeFormat = "%Y-%m-%d",
                          width = "100%", step = 1)
            ),
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

groupCheckbox <- function(idPrefix, values, colors) {
  allID   <- sprintf("%sSelectAll" , idPrefix)
  noneID  <- sprintf("%sSelectNone", idPrefix)
  checkID <- sprintf("%sGroupBox"  , idPrefix)
  return (
    fluidRow(
      inlineCheckbox(checkboxInput(allID , "Select All"  , value = TRUE)),
      inlineCheckbox(checkboxInput(noneID, "Deselect All", value = FALSE)),
      checkboxGroupInput(checkID, "Connections:",
                         selected = values,
                         inline = TRUE,
                         choiceNames = lapply(
                           values,
                           function (x) span(x, style = paste0("color: ", colors[x]))
                         ),
                         choiceValues = values)
    )
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
print("--Converting timestamps from unix epoch to datetime...")

df$timestamp <- anytime(df$timestamp)

print("--Splitting data into separate types...")

# Split data into it's different types
light    <- subset(df, dtype == "light"   )[c("timestamp", "light")]
movement <- subset(df, dtype == "movement")[c("timestamp", "x", "y", "z", "f")]
wireless <- subset(df, dtype == "wireless")[c("timestamp", "wtype", "name", "addr", "rssi")]
sound    <- subset(df, dtype == "sound"   )[c("timestamp", "f")]
gps      <- subset(df, dtype == "gps"     )[c("timestamp", "lat", "lon")]

# Extract dates and times into their own columns
df$date       <- as.Date(df$timestamp)
df$time       <- as.POSIXct(sprintf("1970-01-01 %s", format(df$timestamp, "%H:%M:%s")))

print("--Separating dates and times into their own columns...")

light$date    <- as.Date(light$timestamp)
light$time    <- as.POSIXct(sprintf("1970-01-01 %s", format(light$timestamp, "%H:%M:%s")))
movement$date <- as.Date(movement$timestamp)
movement$time <- as.POSIXct(sprintf("1970-01-01 %s", format(movement$timestamp, "%H:%M:%s")))
wireless$date <- as.Date(wireless$timestamp)
wireless$time <- as.POSIXct(sprintf("1970-01-01 %s", format(wireless$timestamp, "%H:%M:%s")))
sound$date    <- as.Date(sound$timestamp)
sound$time    <- as.POSIXct(sprintf("1970-01-01 %s", format(sound$timestamp, "%H:%M:%s")))
gps$date      <- as.Date(gps$timestamp)
gps$time      <- as.POSIXct(sprintf("1970-01-01 %s", format(gps$timestamp, "%H:%M:%s")))

print("--Creating general global variables")

# Global convenience variables for the data
uniqueDates   <- unique(df$date)
wirelessNames <- unique(wireless$name)

lightTSBounds    <- get_bounds(light$timestamp)
movementTSBounds <- get_bounds(movement$timestamp)
wirelessTSBounds <- get_bounds(wireless$timestamp)
soundTSBounds    <- get_bounds(sound$timestamp)
gpsTSBounds      <- get_bounds(gps$timestamp)

dataTSBounds     <- get_bounds(df$timestamp)
dataDateBounds   <- get_bounds(uniqueDates)

dateColors         <- setNames(normalPalette(length(uniqueDates  )), uniqueDates)
wirelessNameColors <- setNames(normalPalette(length(wirelessNames)), wirelessNames)

print("Creating UI...")

ui <- fluidPage(
  tags$head(includeCSS("../www/app.css")),
  useShinyjs(),
  titlePanel("Data Vis"),
  
  fluidRow(
    class = "input-area",
    checkboxInput("dateMode", "Enable Day Mode:", value = FALSE),
    sliderInput(
      "globalDateRange", "Date Range (All):",
      min = dataTSBounds[1], max = dataTSBounds[2],
      value = dataTSBounds,
      timeFormat = "%Y-%m-%d %H:%M:%S",
      width="100%", step = 1)
  ),
  
  plotRow("light", "Light"),
  plotRow("movement", "Movement"),
  plotRow("wireless", "Wireless",
          conditionalPanel(
            condition = "input.dateMode == 0",
            groupCheckbox("wireless", wirelessNames, wirelessNameColors)
            )),
  plotRow("sound", "Sound"),
  plotRow("gps", "GPS", plotType = leafletOutput),
  
  fluidRow(
    style = "background-color: #ddddff",
    tagList(a("GitHub page", href="https://github.com/blacktack2/DataVisThing"))
  )
)

print("Creating Server...")

server <- function(input, output, session) {
  
  currentDateSliderFormat <- "%H:%M:%S"
  
  observeEvent(input$globalDateRange, {
    for (slider in dateRangeSliderIDs) {
      updateSliderInput(session, slider, value = input$globalDateRange,
                        timeFormat = currentDateSliderFormat)
    }
  })
  
  observeEvent(input$dateMode, {
    if (input$dateMode) {
      currentDateSliderFormat <- "%H:%M:%S"
      updateSliderInput(session, "globalDateRange", "Time Range (All)",
                        min = as.POSIXct("1970-01-01 00:00"), max = as.POSIXct("1970-01-01 23:59:59"),
                        value = c(as.POSIXct("1970-01-01 00:00"), as.POSIXct("1970-01-01 23:59:59")),
                        timeFormat = currentDateSliderFormat)
      for (slider in dateRangeSliderIDs) {
        updateSliderInput(session, slider, "Time Range",
                          min = as.POSIXct("1970-01-01 00:00"), max = as.POSIXct("1970-01-01 23:59:59"),
                          value = c(as.POSIXct("1970-01-01 00:00"), as.POSIXct("1970-01-01 23:59:59")),
                          timeFormat = currentDateSliderFormat)
      }
    } else {
      currentDateSliderFormat <- "%Y-%m-%d %H:%M:%S"
      updateSliderInput(session, "globalDateRange", "Date Range (All)",
                        min = dataTSBounds[1], max = dataTSBounds[2],
                        value = dataTSBounds,
                        timeFormat = currentDateSliderFormat)
      for (slider in dateRangeSliderIDs) {
        updateSliderInput(session, slider, "Date Range",
                          min = dataTSBounds[1], max = dataTSBounds[2],
                          value = dataTSBounds,
                          timeFormat = currentDateSliderFormat)
      }
    }
  })
  
  observeEvent(input$wirelessSelectAll, {
    updateCheckboxInput(session, "wirelessSelectAll", value = TRUE)
    updateCheckboxGroupInput(session, "wirelessGroupBox", selected = wirelessNames)
  }, ignoreInit = TRUE)
  observeEvent(input$wirelessSelectNone, {
    print(input$wirelessGroupBox)
    updateCheckboxInput(session, "wirelessSelectNone", value = FALSE)
    updateCheckboxGroupInput(session, "wirelessGroupBox", selected = vector(mode = "list", length = 0))
  }, ignoreInit = TRUE)
  
  output$lightPlot <- renderPlot({
    if (input$dateMode) {
      day_plot(light, "light", input$lightDateTimeRange, input$lightDateRange)
    } else {
      normal_plot(light, "light", input$lightDateTimeRange)
    }
  })
  output$movementPlot <- renderPlot({
    if (input$dateMode) {
      day_plot(movement, "f", input$movementDateTimeRange, input$movementDateRange)
    } else {
      normal_plot(movement, "f", input$movementDateTimeRange)
    }
  })
  output$wirelessPlot <- renderPlot({
    data <- subset(wireless, name %in% input$wirelessGroupBox)
    if (input$dateMode) {
      day_plot(data, "rssi", input$wirelessDateTimeRange, input$wirelessDateRange)
    } else {
      normal_plot(data, "rssi", input$wirelessDateTimeRange, group = "name", color = "name") +
        scale_color_manual(values = wirelessNameColors)
    }
  })
  output$soundPlot <- renderPlot({
    if (input$dateMode) {
      day_plot(sound, "f", input$soundDateTimeRange, input$soundDateRange)
    } else {
      normal_plot(sound, "f", input$soundDateTimeRange)
    }
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
      if (input$dateMode == 1) {
        data <- subset(gps, as.POSIXct.Date(time) %between%
                            as.POSIXct.Date(input$gpsDateTimeRange, "UTC"))
        data <- subset(data, as.POSIXct.Date(date) %between%
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
      } else {
        data <- subset(gps, as.POSIXct.Date(timestamp) %between%
                            as.POSIXct.Date(input$gpsDateTimeRange, "UTC"))
  
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
    }
  })
  
  normal_plot <- function(df, dataCol, dateTimeRange, group = NULL, color = NULL) {
    data <- subset(df, as.POSIXct.Date(timestamp) %between%
                       as.POSIXct.Date(dateTimeRange, "UTC"))
    
    ggplot(data = data, aes_string(x = "timestamp", y = dataCol, group = group, color = color)) +
      geom_line_point_(data) +
      scale_x_datetime_(dateTimeRange) +
      theme_() +
      expand_limits(x = dateTimeRange)
  }
  day_plot <- function(df, dataCol, timeRange, dateRange) {
    data <- subset(df, as.POSIXct.Date(time) %between%
                       as.POSIXct.Date(timeRange, "UTC"))
    data <- subset(data, as.POSIXct.Date(date) %between%
                         as.POSIXct.Date(dateRange, "UTC"))

    return (
      ggplot(data = data, aes_string(x = "time", y = dataCol, group = "date", color = "date")) +
        geom_line_point_(data) +
        scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
        theme_() +
        expand_limits(x = timeRange) +
        scale_color_gradient(low = dayColorLow, high = dayColorHigh)
      )
  }
  
  geom_line_point_ <- function(data) {
    return ({if(nrow(data) == 1) geom_point(show.legend = FALSE) else geom_line(show.legend = FALSE)})
  }
  scale_x_datetime_ <- function(range) {
    return (
      scale_x_datetime(limits = range,
                             breaks = seq(range[1], range[2], length.out = 8))
      )
  }
  theme_ <- function() {
    return (theme(axis.text.x = element_text(angle = 30, hjust = 1)))
  }
}

print("Starting App...")

shinyApp(ui = ui, server = server)

