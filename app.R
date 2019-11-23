library(shiny)
library(curl)
library(sf)
library(tibble)
library(here)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(showtext)
library(colorspace)
library(stringr)
library(readr)
library(purrr)
library(shinycssloaders)

# Add custom fonts
font_add_google("Hanalei", "Hanalei")
font_add_google("Hanalei Fill", "Hanalei Fill")
# automatically use showtext to render text
showtext_auto()

### Helper functions 
# Capitalize first letter
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1, 1)), substring(c, 2),
        sep = "", collapse = " "
  )
}

### Color palettes

## hunting
hunting_colors <- c(
  "Closed" = "grey64",
  "Mammals & Birds" = "#D95B42",
  "Birds Only" = "#F2E191",
  "Mammals Only" = "#34273B"
) # used @katiejolly nationalparkcolors
hunting_vec <- c("Closed",
                 "Mammals & Birds",
                 "Birds Only",
                 "Mammals Only")

## Second Tab - State Maps
state_df <- read.csv(here::here("data/ocean_state_basemap_paths.csv"))

state_layers <- tibble(
  state_layer = c("none", "Explosives Dumping", "Whale Sanctuaries", "Small Boat Harbor", "Fishing Aggregators", "Surfing Spots", "Shipwrecks")
)

### load the different layers
#### dumping
state_explosive_geo <- sf::read_sf(here::here("data/shapefiles/oceans/dumping/explosive_dumping.shp"))


#### whale sanctuary
state_whale_geo <- sf::read_sf(here::here("data/shapefiles/oceans/whales/state_whale_sanctuary.shp"))
state_whale_geo$AREA_NAME <- str_replace_all(state_whale_geo$AREA_NAME, pattern = "KAUAI", replacement = "Kauai")
state_whale_geo$AREA_NAME <- str_replace_all(state_whale_geo$AREA_NAME, pattern = "NORTH OAHU", replacement = "Oahu")
state_whale_geo$AREA_NAME <- str_replace_all(state_whale_geo$AREA_NAME, pattern = "SOUTH OAHU", replacement = "Oahu")
state_whale_geo$AREA_NAME <- str_replace_all(state_whale_geo$AREA_NAME, pattern = "BIG ISLAND", replacement = "Hawaii")


#### boating
state_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/state_boating.shp"))
state_boating_geo %>% filter(facility == "Small Boat Harbor") -> state_boating_geo

#### fishing
state_fishing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/fishing/fish_aggregators.shp"))

#### wrecks
state_wrecks_geo <- sf::read_sf(here::here("data/shapefiles/oceans/wrecks/wrecks.shp"))

## Third Tab - Ocean Maps (Zoom)
island_ocean_df <- read.csv(here::here("data/ocean_island_basemap_paths.csv"))

island_ocean_layers <- tibble(
  island_ocean_layer = c("none", "Whale Sanctuaries", "Small Boat Harbor", "Fishing Aggregators", "Surfing Spots")
)

#### surfing
hawaii_surfing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/surfing/hawaii_surfing.shp"))
hawaii_surfing_geo$island <- rep("Hawaii", nrow(hawaii_surfing_geo))
maui_surfing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/surfing/maui_surfing.shp"))
maui_surfing_geo$island <- rep("Maui", nrow(maui_surfing_geo))
oahu_surfing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/surfing/oahu_surfing.shp"))
oahu_surfing_geo$island <- rep("Oahu", nrow(oahu_surfing_geo))
kauai_surfing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/surfing/kauai_surfing.shp"))
kauai_surfing_geo$island <- rep("Kauai", nrow(kauai_surfing_geo))
state_surfing_geo <- rbind(hawaii_surfing_geo, maui_surfing_geo, oahu_surfing_geo, kauai_surfing_geo)

island_whale_geo <- state_whale_geo
island_whale_geo$AREA_NAME <- str_replace_all(island_whale_geo$AREA_NAME, pattern = "Oahu", replacement = "Oahu & Maui")
island_whale_geo$AREA_NAME <- str_replace_all(island_whale_geo$AREA_NAME, pattern = "LANAI", replacement = "Oahu & Maui")
island_boating_geo <- state_boating_geo
island_boating_geo$island <- str_replace_all(island_boating_geo$island, pattern = "Oahu", replacement = "Oahu & Maui")
island_boating_geo$island <- str_replace_all(island_boating_geo$island, pattern = "Maui", replacement = "Oahu & Maui")
island_surfing_geo <- state_surfing_geo
island_surfing_geo$island <- str_replace_all(island_surfing_geo$island, pattern = "Oahu", replacement = "Oahu & Maui")
island_surfing_geo$island <- str_replace_all(island_surfing_geo$island, pattern = "Maui", replacement = "Oahu & Maui")
island_fishing_geo <- state_fishing_geo
island_fishing_geo$island <- str_replace_all(island_fishing_geo$island, pattern = "Oahu", replacement = "Oahu & Maui")
island_fishing_geo$island <- str_replace_all(island_fishing_geo$island, pattern = "Maui", replacement = "Oahu & Maui")

## Fourth Tab - Island Maps

island_df <- read.csv(here::here("data/island_basemap_paths.csv"))

island_layers <- tibble(
  island_layer = c("none", "Golf Courses", "Hunting Zones", "Hotels", "Banks")
)


### load the different layers
#### hotels
hotels_geo <- sf::read_sf(here::here("data/shapefiles/locations/hotels/Hotels.shp"))
hotels_geo$island %>%
  str_remove_all(., pattern = "'") %>%
  tolower() -> hotels_geo$island
hotels_geo$island <- purrr::map_chr(hotels_geo$island, CapStr) # Capitalize the irst letter of each row

#### banks
banks_geo <- sf::read_sf(here::here("data/shapefiles/locations/banks/banks_credit_unions.shp"))

#### golf courses
golf_geo <- sf::read_sf(here::here("data/shapefiles/locations/golfCourses/golf_courses.shp"))

#### hunting zones
hunting_geo <- sf::read_sf(here::here("data/shapefiles/locations/hunting_areas/hunting_areas.shp"))
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Safety Zone", replacement = "Closed")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "No Hunting", replacement = "Closed")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Closed \\(NO HUNTING\\)", replacement = "Closed")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "CLOSED", replacement = "Closed")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Hunting Area \\(Mammal ONLY\\)", replacement = "Mammals Only")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Hunting Area \\(Mammal and Bird\\)", replacement = "Mammals & Birds")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Hunting Area \\(Bird ONLY\\)", replacement = "Birds Only")
hunting_geo$Status <- str_replace_all(hunting_geo$Status, pattern = "Hunting Area", replacement = "Mammals & Birds")

road_df <- read.csv(here::here("data/road_basemap_paths.csv"))

road_layers <- tibble(
  road_layer = c("none", "Banks", "Fire Stations", "Hospitals", "Hotels", "Precincts")
)

#### firestations
fire_geo <- sf::read_sf(here::here("data/shapefiles/locations/firestations/firestations_state.shp"))
#### hospitals
hawaii_hospitals_geo <- sf::read_sf(here::here("data/shapefiles/locations/hospitals/hawaii_hospitals.shp"))
hawaii_hospitals_geo$island <- rep("Hawaii", nrow(hawaii_hospitals_geo))
maui_hospitals_geo <- sf::read_sf(here::here("data/shapefiles/locations/hospitals/maui_hospitals.shp"))
maui_hospitals_geo$island <- rep("Hawaii", nrow(maui_hospitals_geo))
oahu_hospitals_geo <- sf::read_sf(here::here("data/shapefiles/locations/hospitals/oahu_hospitals.shp"))
oahu_hospitals_geo$island <- rep("Hawaii", nrow(oahu_hospitals_geo))
kauai_hospitals_geo <- sf::read_sf(here::here("data/shapefiles/locations/hospitals/kauai_hospitals.shp"))
kauai_hospitals_geo$island <- rep("Hawaii", nrow(kauai_hospitals_geo))
hospitals_geo <- rbind(hawaii_hospitals_geo, maui_hospitals_geo, oahu_hospitals_geo, kauai_hospitals_geo)
#### precints
precincts_geo <- sf::read_sf(here::here("data/shapefiles/locations/police/policestations_state.shp"))

# load latitude/longitude coordinates for cities
cities_geolocation <- read_csv(here::here("data/cities_geolocation.csv"))

rayshader_layers <- tibble(
  rayshader_layer = c("Hawaii", "Maui", "Oahu", "Kauai")
)

## Section 2 ____________________________________________________
# set up the user interface
ui <- navbarPage("hinuhinu",
                 tabPanel("Intro", includeCSS("style.css"),
                          fluidPage(
                            HTML('<meta name="viewport" content="width=1024">'),
                            
                            h1("The Hawaiian Islands"),
                            br(),
                            p(strong(em("\"Eddie Would Go...\""), "Eddie Aikau - Polynesian Voyaging Society")),
                            br(),
                            p("There has been a rich history of map making in Hawaii ever since Polynesians rowed in on their outriggers."),
                            p("The European discovery of Hawaii occurred on January 18, 1778, when English ships under the command of Captain James Cook sighted the islands of Oahu an Kauai.", 
                              "Cook was conducting one of the great exploratory voyages of history and ", 
                              a("mapmaking was an integral part of his work.", href = "https://www.storyofhawaiimuseum.com/the-story-of-hawaii/")), 
                            p("Today with new technological tools and ", a("open data ", href = "https://en.wikipedia.org/wiki/Open_data"), "stunning maps can be created at our fingertips easier then ever."),
                            p("Play with this interactive tool and find out!"),
                            br(),
                            br(),
                            div(img(src = "intro_figure.png", height = 420, width = 1000), style="text-align: center;"),
                            br(),
                            br(),
                            br(),
                            div(p(strong("Built by"), a("Matt.0", href = "https://twitter.com/mattoldach"), "using the power of Rstudio and Shiny."), 
                                p(strong("Sources:"), a("State of Hawaii Office of Planning", href = "https://planning.hawaii.gov/gis/download-gis-data/"), "for shapefiles,", a("EarthWorks", href = "https://earthworks.stanford.edu/catalog/stanford-qh711pf3383"), "for rasters"),
                                style="text-align: right;")
                          )
                 ),
                 tabPanel("State Maps",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Island:"),
                                                              htmlOutput("state_basemap_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Elevation Raster:"),
                                                              htmlOutput("state_relief_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("state_layer_selector")),
                                                    radioButtons(inputId = "plot1", label = "Select the file type to download", choices = list("png", "pdf")),
                                                    downloadButton(outputId = "down1", label = "Download the plot")
                                                  ),
                                                  
                                                  mainPanel(
                                                    plotOutput("plot1") %>% withSpinner(color = "#ad1d28")
                                                  )
                          )
                          )
                 ),
                 tabPanel("Island Maps: Ocean",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Island:"),
                                                              htmlOutput("island_ocean_basemap_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Elevation Raster:"),
                                                              htmlOutput("island_ocean_relief_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("island_ocean_layer_selector")),
                                                    radioButtons(inputId = "plot2", label = "Select the file type to download", choices = list("png", "pdf")),
                                                    downloadButton(outputId = "down2", label = "Download the plot")
                                                  ),
                                                  
                                                  mainPanel(
                                                    plotOutput("plot2") %>% withSpinner(color = "#ad1d28")
                                                  )
                          )
                          )
                 ),
                 tabPanel("Island Maps: Terrestrial",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Island:"),
                                                              htmlOutput("island_basemap_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Elevation Raster:"),
                                                              htmlOutput("island_relief_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("island_layer_selector")),
                                                    radioButtons(inputId = "plot3", label = "Select the file type to download", choices = list("png", "pdf")),
                                                    downloadButton(outputId = "down3", label = "Download the plot")
                                                  ),
                                                  
                                                  mainPanel(
                                                    plotOutput("plot3") %>% withSpinner(color = "#ad1d28")
                                                  )
                          )
                          )
                 ),
                 tabPanel("Road Networks",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Choose Island:"),
                                                              htmlOutput("road_basemap_selector")),
                                                    wellPanel(style = "background: #E5C595",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("road_layer_selector")),
                                                    radioButtons(inputId = "plot4", label = "Select the file type to download", choices = list("png", "pdf")),
                                                    downloadButton(outputId = "down4", label = "Download the plot")
                                                  ),
                                                  
                                                  mainPanel(
                                                    plotOutput("plot4") %>% withSpinner(color = "#ad1d28")
                                                  )
                          )
                          )
                 ),
                 tabPanel("Rayshader",
                          fluidPage(
                            # embed_url("https://youtu.be/jZ7D5kv8U8I")
                            HTML('<iframe width="560" height="315" src="https://youtu.be/5KEe-DKcimI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            HTML('<iframe width="560" height="315" src="https://youtu.be/QaOlK-aczLk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            HTML('<iframe width="560" height="315" src="https://youtu.be/p0Va_dlpRzQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            HTML('<iframe width="560" height="315" src="https://youtu.be/Z1b0eLrJkJ8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            HTML('<iframe width="560" height="315" src="https://youtu.be/sPjy694ClGg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            HTML('<iframe width="560" height="315" src="https://youtu.be/faCiHnL76Fw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                          )
                 ),
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 )
)


## Section 3 ____________________________________________________
# server controls what is displayed by the user interface
server <- shinyServer(function(input, output) {
  
  # creates logic behind ui outputs ** pay attention to letter case in names
  
  ## Second Tab
  output$state_basemap_selector <- renderUI({ # creates basemap select box object called in ui
    selectInput(
      inputId = "state_basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(state_df$location)),
      # calls unique values from the basemap column in the previously created table
      selected = "State Map"
    ) # default choice (not required)
  })
  
  output$state_relief_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- state_df[state_df$location == input$state_basemap, "relief"]
    # creates a reactive list of available reliefs based on the state_basemap selection made
    
    selectInput(
      inputId = "state_relief", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$state_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- state_layers
    # creates a reactive list of available reliefs based on the state_basemap selection made
    
    selectInput(
      inputId = "state_layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot1 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter state_basemaps
    state_df %>%
      filter(location == input$state_basemap & relief == input$state_relief) -> state_df
    
    # load the state_basemap for four main islands
    state_plot <- readRDS(here::here("data/basemaps/ocean_state", state_df$filename))
    
    if (input$state_layer == "Explosives Dumping") {
      state_plot + geom_sf(data = state_explosive_geo, color = darken("#F98866"), fill = "#F98866")
    } else if(input$state_layer == "Whale Sanctuaries"){
      state_plot + geom_sf(data = state_whale_geo, color = darken("#756bb1"), fill = "#756bb1")
    } else if(input$state_layer == "Small Boat Harbor"){
      state_plot + geom_sf(data = state_boating_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 18, size = 2, alpha = 0.5)
    } else if(input$state_layer == "Fishing Aggregators"){
      state_plot + geom_sf(data = state_fishing_geo, color = darken("#7C000C"), fill = "#7C000C", size = 2, alpha = 0.5)
    } else if(input$state_layer == "Surfing Spots"){
      state_plot + geom_sf(data = state_surfing_geo, color = darken("#B35234"), fill = "#B35234", shape = 16, size = 2, alpha = 0.5)
    } else if(input$state_layer == "Shipwrecks"){
      state_plot + geom_sf(data = state_wrecks_geo, color = darken("#963484"), fill = "#963484", shape = 25, size = 2, alpha = 0.5)
    } else {
      state_plot
    }
  }, bg="#f5f5f2", execOnResize=T, height = 1000, units = "px")
  
  output$down1 <- downloadHandler(
    filename =  function() {
      paste("StateMap", input$plot1, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$plot1 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      # filter state_basemaps
      state_df %>%
        filter(location == input$state_basemap & relief == input$state_relief) -> state_df
      
      # load the state_basemap for four main islands
      state_plot <- readRDS(here::here("data/basemaps/ocean_state", state_df$filename))
      
      if (input$state_layer == "Explosives Dumping") {
        print(state_plot + geom_sf(data = state_explosive_geo, color = darken("#F98866"), fill = "#F98866"))
      } else if(input$state_layer == "Whale Sanctuaries"){
        print(state_plot + geom_sf(data = state_whale_geo, color = darken("#756bb1"), fill = "#756bb1"))
      } else if(input$state_layer == "Small Boat Harbor"){
        print(state_plot + geom_sf(data = state_boating_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 18, size = 2, alpha = 0.5))
      } else if(input$state_layer == "Fishing Aggregators"){
        print(state_plot + geom_sf(data = state_fishing_geo, color = darken("#7C000C"), fill = "#7C000C", size = 2, alpha = 0.5))
      } else if(input$state_layer == "Surfing Spots"){
        print(state_plot + geom_sf(data = state_surfing_geo, color = darken("#B35234"), fill = "#B35234", shape = 16, size = 2, alpha = 0.5))
      } else if(input$state_layer == "Shipwrecks"){
        print(state_plot + geom_sf(data = state_wrecks_geo, color = darken("#963484"), fill = "#963484", shape = 25, size = 2, alpha = 0.5))
      } else {
        print(state_plot)
      }
      dev.off()
    } 
  )
  
  ## Third Tab
  
  output$island_ocean_basemap_selector <- renderUI({ # creates island_basemap select box object called in ui
    selectInput(
      inputId = "island_ocean_basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(island_ocean_df$location)),
      # calls unique values from the island_basemap column in the previously created table
      selected = "Hawaii"
    ) # default choice (not required)
  })
  
  output$island_ocean_relief_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_ocean_df[island_ocean_df$location == input$island_ocean_basemap, "relief"]
    # creates a reactive list of available reliefs based on the island_basemap selection made
    
    selectInput(
      inputId = "island_ocean_relief", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$island_ocean_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_ocean_layers
    # creates a reactive list of available reliefs based on the island_basemap selection made
    
    selectInput(
      inputId = "island_ocean_layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot2 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter state_basemaps
    island_ocean_df %>%
      filter(location == input$island_ocean_basemap & relief == input$island_ocean_relief) -> island_ocean_df
    
    # filter all shapefiles
    island_whale_geo %>% filter(AREA_NAME == input$island_ocean_basemap) -> island_whale_geo
    island_boating_geo %>% filter(island == input$island_ocean_basemap) -> island_boating_geo
    island_fishing_geo %>% filter(island == input$island_ocean_basemap) -> island_fishing_geo
    island_surfing_geo %>% filter(island == input$island_ocean_basemap) -> island_surfing_geo
    
    # load the state_basemap for four main islands
    island_ocean_plot <- readRDS(here::here("data/basemaps/ocean_island", island_ocean_df$filename))
    if (input$island_ocean_layer == "Whale Sanctuaries"){
      island_ocean_plot + geom_sf(data = island_whale_geo, color = darken("#756bb1"), fill = "#756bb1")
    } else if(input$island_ocean_layer == "Small Boat Harbor"){
      island_ocean_plot + geom_sf(data = island_boating_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 25, size = 4, alpha = 0.5)
    } else if(input$island_ocean_layer == "Fishing Aggregators"){
      island_ocean_plot + geom_sf(data = island_fishing_geo, color = darken("#7C000C"), fill = "#7C000C", size = 2, alpha = 0.5)
    } else if(input$island_ocean_layer == "Surfing Spots"){
      island_ocean_plot + geom_sf(data = island_surfing_geo, color = darken("#B35234"), fill = "#B35234", shape = 16, size = 2, alpha = 0.5)
    } else {
      island_ocean_plot
    }
  }, bg="#f5f5f2", execOnResize=T, height = 1000, units = "px")
  
  output$down2 <- downloadHandler(
    filename =  function() {
      paste("OceanMap", input$plot2, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$plot2 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      island_ocean_df %>%
        filter(location == input$island_ocean_basemap & relief == input$island_ocean_relief) -> island_ocean_df
      # load the state_basemap for four main islands
      island_ocean_plot <- readRDS(here::here("data/basemaps/ocean_island", island_ocean_df$filename))
      
      # filter all shapefiles
      state_whale_geo %>% filter(AREA_NAME == input$island_ocean_basemap) -> state_whale_geo
      state_boating_geo %>% filter(island == input$island_ocean_basemap) -> state_boating_geo
      state_fishing_geo %>% filter(island == input$island_ocean_basemap) -> state_fishing_geo
      
      if (input$island_ocean_layer == "Whale Sanctuaries"){
        print(island_ocean_plot + geom_sf(data = state_whale_geo, color = darken("#756bb1"), fill = "#756bb1"))
      } else if(input$island_ocean_layer == "Small Boat Harbor"){
        print(island_ocean_plot + geom_sf(data = state_boating_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 18, size = 4, alpha = 0.5))
      } else if(input$island_ocean_layer == "Fishing Aggregators"){
        print(island_ocean_plot + geom_sf(data = state_fishing_geo, color = darken("#7C000C"), fill = "#7C000C", size = 2, alpha = 0.5))
      } else if(input$island_ocean_layer == "Surfing Spots"){
        print(island_ocean_plot + geom_sf(data = state_surfing_geo, color = darken("#B35234"), fill = "#B35234", shape = 16, size = 2, alpha = 0.5))
      } else if(input$island_ocean_layer == "Shipwrecks"){
        print(island_ocean_plot + geom_sf(data = state_wrecks_geo, color = darken("#963484"), fill = "#963484", shape = 25, size = 2, alpha = 0.5))
      } else {
        print(island_ocean_plot)
      }
      
      dev.off()
    } 
  )
  
  ## Fourth Tab
  
  output$island_basemap_selector <- renderUI({ # creates island_basemap select box object called in ui
    selectInput(
      inputId = "island_basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(island_df$location)),
      # calls unique values from the island_basemap column in the previously created table
      selected = "Hawaii"
    ) # default choice (not required)
  })
  
  output$island_relief_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_df[island_df$location == input$island_basemap, "relief"]
    # creates a reactive list of available reliefs based on the island_basemap selection made
    
    selectInput(
      inputId = "island_relief", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$island_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_layers
    # creates a reactive list of available reliefs based on the island_basemap selection made
    
    selectInput(
      inputId = "island_layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot3 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter island_basemaps
    island_df %>%
      filter(location == input$island_basemap & relief == input$island_relief) -> island_df
    # filter all shapefiles
    hotels_geo %>% filter(island == input$island_basemap) -> hotels_geo
    banks_geo %>% filter(island == input$island_basemap) -> banks_geo
    golf_geo %>% filter(island == input$island_basemap) -> golf_geo
    hunting_geo %>% filter(Island == input$island_basemap) -> hunting_geo
    
    # load the basemap for four main islands
    island_plot <- readRDS(here::here("data/basemaps/land", island_df$filename))
    if (input$island_layer == "Hotels") {
      island_plot + geom_sf(data = hotels_geo, color = darken("#963484"), fill = "#963484", shape = 21, size = 2.5, alpha = 0.5)
    } else if(input$island_layer == "Banks"){
      island_plot + geom_sf(data = banks_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 21, size = 2.5, alpha = 0.5)
    } else if(input$island_layer == "Golf Courses"){
      island_plot + geom_sf(data = golf_geo, color = darken("#F98866"), fill = "#F98866", size = 2)  
    } else if(input$island_layer == "Hunting Zones"){
      # to-do - create a script to source with palletes!
      island_plot + 
        geom_sf(data = hunting_geo, aes(fill = Status), alpha = 0.5) +
        scale_fill_manual(values = hunting_colors)
    } else {
      island_plot
    }
  }, bg="#f5f5f2", execOnResize=T, height = 1000, units = "px")
  
  output$down3 <- downloadHandler(
    filename =  function() {
      paste("IslandMap", input$plot3, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$plot3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      island_df %>%
        filter(location == input$island_basemap & relief == input$island_relief) -> island_df
      # filter all shapefiles
      hotels_geo %>% filter(island == input$island_basemap) -> hotels_geo
      banks_geo %>% filter(island == input$island_basemap) -> banks_geo
      golf_geo %>% filter(island == input$island_basemap) -> golf_geo
      hunting_geo %>% filter(Island == input$island_basemap) -> hunting_geo
      
      # load the basemap for four main islands
      island_plot <- readRDS(here::here("data/basemaps/land", island_df$filename))
      if (input$island_layer == "Hotels") {
        print(island_plot + geom_sf(data = hotels_geo, color = darken("#963484"), fill = "#963484", shape = 21, size = 2.5, alpha = 0.5))
      } else if(input$island_layer == "Banks"){
        print(island_plot + geom_sf(data = banks_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 21, size = 2.5, alpha = 0.5))
      } else if(input$island_layer == "Golf Courses"){
        print(island_plot + geom_sf(data = golf_geo, color = darken("#F98866"), fill = "#F98866", size = 2))
      } else if(input$island_layer == "Hunting Zones"){
        # to-do - create a script to source with palletes!
        print(island_plot + geom_sf(data = hunting_geo, aes(fill = Status), alpha = 0.5) + scale_fill_manual(values = hunting_colors))
      } else {
        print(island_plot)
      }
      
      dev.off()
    } 
  )
  
  ## Fifth tab
  
  output$road_basemap_selector <- renderUI({ # creates island_basemap select box object called in ui
    selectInput(
      inputId = "road_basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(road_df$location)),
      # calls unique values from the island_basemap column in the previously created table
      selected = "Hawaiian Paradise Park "
    ) # default choice (not required)
  })
  
  output$road_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- road_layers
    # creates a reactive list of available reliefs based on the island_basemap selection made
    
    selectInput(
      inputId = "road_layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot4 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter island_basemaps
    road_df %>%
      filter(location == input$road_basemap) -> road_df
    # filter all shapefiles
    city <- cities_geolocation %>% filter(city == input$road_basemap)
    pt <- data.frame(lat = city$lat, long = city$lon)
    pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
    circle <- st_buffer(pt, dist = 24140.2)
    
    # subset shapefiles to fit city
    banks_circle <- circle %>% st_transform(st_crs(banks_geo))
    banks <- st_intersection(banks_circle, banks_geo)
    fire_circle <- circle %>% st_transform(st_crs(fire_geo))
    firestations <- st_intersection(fire_circle, fire_geo)
    hospitals_circle <- circle %>% st_transform(st_crs(hospitals_geo))
    hospitals <- st_intersection(hospitals_circle, hospitals_geo)
    hotels_circle <- circle %>% st_transform(st_crs(hotels_geo))
    hotels <- st_intersection(hotels_circle, hotels_geo)
    precincts_circle <- circle %>% st_transform(st_crs(precincts_geo))
    precincts <- st_intersection(precincts_circle, precincts_geo)
    
    # load the basemap for four main islands
    road_plot <- readRDS(here::here("data/basemaps/roads", road_df$filename))
    if (input$road_layer == "Banks") {
      road_plot + geom_sf(data = banks, color = darken("#963484"), fill = "#963484", shape = 19, size = 3, alpha = 0.7)
    } else if(input$road_layer == "Firestations"){
      road_plot + geom_sf(data = firestations, color = darken("#CE2029"), fill = "#CE2029", shape = 19, size = 3, alpha = 0.7)
    } else if(input$road_layer == "Hospitals"){
      road_plot + geom_sf(data = hospitals, color = darken("#78ACA8"), fill = "#78ACA8", shape = 19, size = 4, alpha = 0.7)
    } else if(input$road_layer == "Hotels"){
      road_plot + geom_sf(data = hotels, color = darken("#E77A5B"), fill = "#E77A5B", shape = 19, size = 3, alpha = 0.7)
    } else if(input$road_layer == "Precincts"){
      road_plot + geom_sf(data = precincts, color = darken("#20425B"), fill = "#20425B", shape = 19, size = 3, alpha = 0.7)
    } else {
      road_plot
    }
  }, bg="#f5f5f2", execOnResize=T, height = 1000, units = "px")
  
  output$down4 <- downloadHandler(
    filename =  function() {
      paste("RoadMap", input$plot4, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$plot4 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      road_df %>%
        filter(location == input$road_basemap) -> road_df
      # filter all shapefiles
      city <- cities_geolocation %>% filter(city == input$road_basemap)
      pt <- data.frame(lat = city$lat, long = city$lon)
      pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
      circle <- st_buffer(pt, dist = 24140.2)
      
      # subset shapefiles to fit city
      banks_circle <- circle %>% st_transform(st_crs(banks_geo))
      banks <- st_intersection(banks_circle, banks_geo)
      fire_circle <- circle %>% st_transform(st_crs(fire_geo))
      firestations <- st_intersection(fire_circle, fire_geo)
      hospitals_circle <- circle %>% st_transform(st_crs(hospitals_geo))
      hospitals <- st_intersection(hospitals_circle, hospitals_geo)
      hotels_circle <- circle %>% st_transform(st_crs(hotels_geo))
      hotels <- st_intersection(hotels_circle, hotels_geo)
      precincts_circle <- circle %>% st_transform(st_crs(precincts_geo))
      precincts <- st_intersection(precincts_circle, precincts_geo)
      
      # load the basemap for four main islands
      road_plot <- readRDS(here::here("data/basemaps/roads", road_df$filename))
      if (input$road_layer == "Banks") {
        print(road_plot + geom_sf(data = banks, color = darken("#963484"), fill = "#963484", shape = 19, size = 3.3, alpha = 0.7))
      } else if(input$road_layer == "Firestations"){
        print(road_plot + geom_sf(data = firestations, color = darken("#CE2029"), fill = "#CE2029", shape = 19, size = 3.3, alpha = 0.7))
      } else if(input$road_layer == "Hospitals"){
        print(road_plot + geom_sf(data = hospitals, color = darken("#78ACA8"), fill = "#78ACA8", shape = 19, size = 3.3, alpha = 0.7))
      } else if(input$road_layer == "Hotels"){
        print(road_plot + geom_sf(data = hotels, color = darken("#E77A5B"), fill = "#E77A5B", shape = 19, size = 3.3, alpha = 0.7))
      } else if(input$road_layer == "Precincts"){
        # to-do - create a script to source with palletes!
        print(road_plot + geom_sf(data = precincts, color = darken("#20425B"), fill = "#20425B", shape = 19, size = 3.3, alpha = 0.7))
      } else {
        print(road_plot)
      }
      
      dev.off()
    } 
  )
  
}) # close the shinyServer

## Section 4____________________________________________________
shinyApp(ui = ui, server = server) # need this if combining ui and server into one file.