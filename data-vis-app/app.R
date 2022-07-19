library("shiny")
library("shinyjs")
library("leaflet")

library("jsonlite")
library("anytime")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")

# Load data from json file 'file' into a flat dataframe
load_data <- function(file) {
  return(fromJSON(file, simplifyDataFrame = TRUE, flatten = TRUE));
}

print("Loading Data...")

# Load and combine all json datasets into a single dataframe
dataFiles <- list.files("../cleaned", "*.json", full.names = TRUE)

print(paste("-Reading from files:", toString(dataFiles)))

df_raw <- bind_rows(lapply(dataFiles, load_data))

print(paste("-Raw data read with", ncol(df_raw), "columns:", toString(names(df_raw))))

print("Cleaning Data...")

print("-Renaming columns...")

df <- df_raw %>% rename(timestamp=timestamp, dtype=type,
                    light=payload.light,
                    x=payload.x, y=payload.y, z=payload.z, f=payload.f,
                    wtype=payload.type, name=payload.name, addr=payload.addr, rssi=payload.rssi,
                    lat=payload.lat, lon=payload.lon)

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
light    <- clean_columns(subset(df, dtype == "light"))
movement <- clean_columns(subset(df, dtype == "movement"))
wireless <- clean_columns(subset(df, dtype == "wireless"))
sound    <- clean_columns(subset(df, dtype == "sound"))
gps      <- clean_columns(subset(df, dtype == "gps"))

print("Creating UI...")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Vis"),
  
  fluidRow(
    column(4,
      style = "background-color: #bbbbbb",
      fluidRow(
        actionButton("light", "Light"),
        actionButton("movement", "Movement"),
        actionButton("wireless", "Wireless"),
        actionButton("sound", "Sound"),
        actionButton("gps", "GPS")
      ),
      fluidRow(
        id = "wirelessParams",
        
        selectInput("wirelessConns", "Connections", unique(wireless$name)),
        checkboxInput("wirelessAll", "Show All", value = TRUE)
      )
    ),
    
    column(8,
      plotOutput(outputId = "graph"),
      leafletOutput(outputId = "mapGraph"),
    )
  ),
  fluidRow(
    style = "background-color: #ddddff",
    tagList(a("GitHub page", href="https://github.com/blacktack2/DataVisThing"))
  )
)

print("Creating Server...")

server <- function(input, output, session) {
  
  plotSelection <- reactiveValues(current = "")
  
  observeEvent(input$light, {
    setCurrent("light")
  }, ignoreNULL = FALSE) # ignoreNULL set so this function is called on init
  observeEvent(input$movement, {
    setCurrent("movement")
  })
  observeEvent(input$wireless, {
    setCurrent("wireless", FALSE, TRUE)
  })
  observeEvent(input$sound, {
    setCurrent("sound")
  })
  observeEvent(input$gps, {
    setCurrent("gps", TRUE)
  })
  
  observeEvent(input$wirelessAll, {
    if (input$wirelessAll) {
      disable("wirelessConns")
    } else {
      enable("wirelessConns")
    }
  })
  
  output$graph <- renderPlot({
    switch(
      plotSelection$current,
      "light"    = lightGraph(),
      "movement" = movementGraph(),
      "wireless" = wirelessGraph(),
      "sound"    = soundGraph(),
      "gps"      = NULL
    )
  })
  output$mapGraph <- renderLeaflet({
    switch(
      plotSelection$current,
      "light"    = NULL,
      "movement" = NULL,
      "wireless" = NULL,
      "sound"    = NULL,
      "gps"      = gpsGraph()
    )
  })
  
  # Switch between different graph views
  setCurrent <- function(newCurrent, mapGraph = FALSE, connectionRow = FALSE) {
    enable(plotSelection$current)
    disable(newCurrent)
    plotSelection$current = newCurrent
    
    if (mapGraph) {
      hide("graph")
      show("mapGraph")
    } else {
      show("graph")
      hide("mapGraph")
    }
    
    if (connectionRow) {
      show("wirelessParams")
    } else {
      hide("wirelessParams")
    }
  }
  
  lightGraph <- function() {
    ggplot(data=light, aes(x=timestamp, y=light, group=1)) +
      geom_line() +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
      theme(axis.text.x = element_text(angle = 90))
  }
  movementGraph <- function() {
    ggplot(data=movement, aes(x=timestamp, y=f)) +
      geom_line()
  }
  wirelessGraph <- function() {
    if (input$wirelessAll) {
      data <- group_by(wireless, name)
      ggplot(data=data, aes(x=timestamp, y=rssi, color=name)) +
        {if(nrow(data) > 1) geom_line() else geom_point()}
    } else {
      data <- subset(wireless, name == input$wirelessConns)
      ggplot(data=data, aes(x=timestamp, y=rssi)) +
        {if(nrow(data) > 1) geom_line() else geom_point()}
    }
  }
  soundGraph <- function() {
    ggplot(data=sound, aes(x=timestamp, y=f, group=1)) +
      geom_line() +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%Y %H:%M") +
      theme(axis.text.x = element_text(angle = 90)) +
      ylab("Frequency (Hz) ((this might be wrong))")
  }
  gpsGraph <- function() {
    leaflet(data = gps) %>%
      addTiles() %>%
      addMarkers(popup = ~as.character(timestamp), label = ~as.character(timestamp))
  }
}

shinyApp(ui = ui, server = server)

