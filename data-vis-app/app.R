library("shiny")
library("shinyjs")
library("leaflet")

library("jsonlite")
library("anytime")
library("dplyr")
library("ggplot2")
#library("sf")
library("tidyr")


# Load data from json file 'file' into a flat dataframe
load_data <- function(file) {
  return(fromJSON(file, simplifyDataFrame = TRUE, flatten = TRUE));
}

# Load and combine all json datasets into a single dataframe
dataFiles <- list.files("cleaned", "*.json", full.names = TRUE)

print(paste("Reading data from:", dataFiles[1]))
df_raw <- load_data(dataFiles[1])
for (i in 2:length(dataFiles)) {
  print(paste("Reading data from:", dataFiles[i]))
  bind_rows(df_raw, load_data(dataFiles[i]))
}

df_raw2 <- load_data("../cleaned/1656633600.json")
df_raw2 <- bind_rows(df_raw, load_data("../cleaned/1656720000.json"))
df_raw2 <- bind_rows(df_raw, load_data("../cleaned/1656806400.json"))
df_raw2 <- bind_rows(df_raw, load_data("../cleaned/1656892800.json"))
df_raw2 <- bind_rows(df_raw, load_data("../cleaned/1656979200.json"))

print(summary(df_raw))
print(summary(df_raw2))

# Parse data and rename headers
df <- as.data.frame(df_raw %>% unnest(c())) # Flatten nested structure (payload)
print(names(df))
#quit()
names(df) <- c("timestamp", "dtype", "light", "x", "y", "z", "f", "wtype", "name", "addr", "rssi", "lat", "lon")
df$timestamp <- anytime(df$timestamp)

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


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Vis"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        actionButton("light", "Light"),
        actionButton("movement", "Movement"),
        actionButton("wireless", "Wireless"),
        actionButton("sound", "Sound"),
        actionButton("gps", "GPS")
      ),
      fluidRow(id = "wirelessParams",
        selectInput("wirelessConns", "Connections", unique(wireless$name)),
        checkboxInput("wirelessAll", "Show All", value = TRUE)
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "graph"),
      leafletOutput(outputId = "mapGraph")
    )
  )
)

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

