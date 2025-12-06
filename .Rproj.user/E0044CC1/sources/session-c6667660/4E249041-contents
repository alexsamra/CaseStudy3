#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(cluster)
library(readr)

# UI
ui <- page_sidebar(
  
  titlePanel("Case Study 3 - K-Means Spotify Clustering"),
  
  # Clusters
  sidebar = sidebar(
    sliderInput(
      inputId = "k",
      label = "Number of clusters:",
      min = 2,
      max = 10,
      value = 3
    ),
    
    # Features
    checkboxGroupInput(
      inputId = "features",
      label = "Select numeric features for clustering:",
      choices = c(
        "bpm",
        "danceability_pct",
        "energy_pct",
        "valence_pct",
        "acousticness_pct",
        "instrumentalness_pct",
        "liveness_pct",
        "speechiness_pct"
      ),
      selected = c(
        "bpm",
        "danceability_pct",
        "energy_pct",
        "valence_pct",
        "acousticness_pct",
        "instrumentalness_pct",
        "liveness_pct",
        "speechiness_pct"
      )
    )
  ),
  
  # Display plots
  mainPanel(
    plotOutput("clusterPlot"),
    tableOutput("clusterCenters"),
    tableOutput("clusterSizes"),
    tableOutput("clusterSongs")
  )
)

# Server
server <- function(input, output) {
  
  # Load Spotify data
  df <- read_csv("spotify-2023.csv", locale = locale(encoding = "latin1"))
  
  # Fix column names
  names(df) <- make.names(names(df))
  names(df) <- gsub("danceability..", "danceability_pct", names(df))
  names(df) <- gsub("energy..", "energy_pct", names(df))
  names(df) <- gsub("valence..", "valence_pct", names(df))
  names(df) <- gsub("acousticness..", "acousticness_pct", names(df))
  names(df) <- gsub("instrumentalness..", "instrumentalness_pct", names(df))
  names(df) <- gsub("liveness..", "liveness_pct", names(df))
  names(df) <- gsub("speechiness..", "speechiness_pct", names(df))
  
  # Scale data
  scaled_data <- reactive({
    req(input$features)
    data <- df[, input$features]
    scale(data)
  })
  
  # Run k-means
  km <- reactive({
    kmeans(scaled_data(), centers = input$k)
  })
  
  # plot data
  output$clusterPlot <- renderPlot({
    req(km())
    
    clusplot(
      scaled_data(),
      km()$cluster,
      color = TRUE,
      shade = FALSE,
      labels = 0,
      lines = 0,
      main = "k-Means Cluster Analysis"
    )
  })
  
  # Show cluster centers
  output$clusterCenters <- renderTable({
    round(km()$centers, 3)
  })
  
  # Show cluster sizes
  output$clusterSizes <- renderTable({
    table(km()$cluster)
  })
  
  # Show cluster songs
  output$clusterSongs <- renderTable({
    req(km())
    
    results <- data.frame(
      Track = df$track_name,
      Cluster = km()$cluster
    )
    
    results[order(results$Cluster), ]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
