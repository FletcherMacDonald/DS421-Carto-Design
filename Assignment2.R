library(shiny)
library(mapgl)
library(here)
library(sf)
library(viridis)
library(tidyverse)

# Load data
county_data <- st_read(here("data", "county_data.gpkg"), layer = "county_data")

# Get unique languages
language_choices <- county_data %>%
  st_drop_geometry() %>%
  pull(language) %>%
  unique() %>%
  sort()

ui <- fluidPage(
  titlePanel("Language Speakers by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Choose a language:", choices = language_choices)
    ),
    mainPanel(
      maplibreOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    req(input$language)
    
    filtered_data <- county_data %>%
      filter(language == input$language) %>%
      mutate(speakers = as.numeric(speakers))
    
    color_scale <- interpolate_palette(
      data = filtered_data,
      column = "speakers",
      method = "quantile",
      n = 5,
      palette = viridisLite::viridis
    )
    
    legend_labels <- get_legend_labels(color_scale, digits = 0)
    
    maplibre(style = carto_style("positron")) %>%
      fit_bounds(filtered_data) %>%
      add_fill_layer(
        id = "language_speakers",
        source = filtered_data,
        fill_color = color_scale$expression,
        fill_opacity = 0.8
      ) %>%
      add_legend(
        legend_title = paste("Speakers of", input$language),
        values = legend_labels,
        colors = get_legend_colors(color_scale),
        type = "continuous"
      )
  })
}

shinyApp(ui, server)


