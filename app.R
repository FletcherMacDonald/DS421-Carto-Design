library(shiny)
library(mapgl)
library(sf)




www_path <- file.path(Sys.getenv("HOME"), "Documents", "DS421-Carto-Design", "www")

addResourcePath("custom_www", www_path)


journey <- data.frame(
  section_id = c("sec1", "sec2", "sec3", "sec4", "sec5"),
  name = c(
    "Keiraville Public School",
    "Holy Spirit College, Bellambi",
    "Newington College, Sydney",
    "Lubbock Christian University, Texas",
    "Chaminade University, Hawaii"
  ),
  lon = c(150.8769, 150.9151078048384, 151.1631793820517, -101.8811, -157.80777759152016),
  lat = c(-34.4110, -34.364249647475, -33.89777003577011, 33.5701, 21.290298013001976),
  years = c("2008–2015", "2016–2018", "2019–2021", "2022–2024", "2024–present"),
  image = c("Keiraville.png", "holy.png", "newo.png", "lcu.png", "cham.png"),
  stringsAsFactors = FALSE
)


for (img in journey$image) {
  if (!file.exists(file.path(www_path, img))) {
    warning(paste("Image file not found:", file.path(www_path, img)))
  }
}

journey_sf <- st_as_sf(journey, coords = c("lon", "lat"), crs = 4326)


coords <- rbind(
  st_coordinates(journey_sf[1:3, ]), 
  c(165, -20), 
  c(-150, 0), 
  st_coordinates(journey_sf[4, ]), 
  c(-115, 30), 
  c(-135, 25), 
  st_coordinates(journey_sf[5, ]) 
)
path_sf <- st_sf(geometry = st_sfc(st_linestring(coords), crs = 4326))


cat("Path coordinates:\n")
print(coords)


ui <- fluidPage(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
  
  verbatimTextOutput("debug"),
  story_map(
    map_id = "map",
    font_family = "Poppins",
    sections = list(
      "sec1" = story_section(
        title = journey$name[1],
        content = list(
          p(journey$years[1]),
          img(src = file.path("custom_www", journey$image[1]), width = "300px")
        )
      ),
      "sec2" = story_section(
        title = journey$name[2],
        content = list(
          p(journey$years[2]),
          img(src = file.path("custom_www", journey$image[2]), width = "300px")
        )
      ),
      "sec3" = story_section(
        title = journey$name[3],
        content = list(
          p(journey$years[3]),
          img(src = file.path("custom_www", journey$image[3]), width = "300px")
        )
      ),
      "sec4" = story_section(
        title = journey$name[4],
        content = list(
          p(journey$years[4]),
          img(src = file.path("custom_www", journey$image[4]), width = "300px")
        )
      ),
      "sec5" = story_section(
        title = journey$name[5],
        content = list(
          p(journey$years[5]),
          img(src = file.path("custom_www", journey$image[5]), width = "300px")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  output$debug <- renderPrint({
    list.files(www_path, pattern = "\\.(png|jpg|jpeg|webp|svg)$", full.names = TRUE)
  })
  
  output$map <- renderMapboxgl({
    mapboxgl(
      scrollZoom = FALSE,
      center = c(journey$lon[1], journey$lat[1]),
      zoom = 12
    ) |> 
      add_source("path", path_sf) |>
      add_line_layer(
        id = "path_layer",
        source = "path",
        line_color = "#333333", # Dark grey
        line_width = 3
      )
  })
  
  for (i in seq_len(nrow(journey))) {
    local({
      ii <- i
      on_section("map", journey$section_id[ii], {
        marker_sf <- journey_sf[ii, ]
        
        proxy <- mapboxgl_proxy("map") |>
          clear_layer("marker_layer") |>
          add_source("marker", marker_sf) |>
          add_circle_layer(
            id = "marker_layer",
            source = "marker",
            circle_color = "#333333", # Dark grey
            circle_radius = 8,
            circle_opacity = 0.9,
            popup = HTML(paste0(
              "<h3>", journey$name[ii], "</h3>",
              "<p>", journey$years[ii], "</p>",
              "<img src='custom_www/", journey$image[ii], "' style='max-width: 200px; max-height: 200px;'>"
            ))
          ) |>
          fly_to(
            center = c(journey$lon[ii], journey$lat[ii]),
            zoom = ifelse(ii == 5, 15, ifelse(ii == 4, 10, 14)), # Zoom 15 for Hawaii, 10 for Texas, 14 for Australia
            pitch = 45,
            bearing = 0
          )
      })
    })
  }
}

shinyApp(ui, server)