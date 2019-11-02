#* save the following code in a file named app.R *
library(shiny)
library(shinyWidgets)
library(sf)
library(tibble)
library(here)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(showtext)
library(colorspace) # for darken
library(stringr)

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

## Second Tab - Ocean Maps
ocean_state_df <- read.csv(here::here("data/ocean_state_basemap_paths.csv"))

ocean_state_layers <- tibble(
  layer = c("none", "Explosives Dumping", "Whale Sanctuaries", "Boating", "Fishing Aggregators", "Surfing Spots", "Shipwrecks")
)

### load the different layers
#### dumping
state_explosive_geo <- sf::read_sf(here::here("data/shapefiles/oceans/dumping/explosive_dumping.shp"))

#### whale sanctuary
state_whale_geo <- sf::read_sf(here::here("data/shapefiles/oceans/whales/state_whale_sanctuary.shp"))

#### boating
state_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/state_boating.shp"))

#### fishing
state_fishing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/fishing/fish_aggregators.shp"))

#### surfing
state_surfing_geo <- sf::read_sf(here::here("data/shapefiles/oceans/surfing/state_surfing.shp"))

#### wrecks
state_wrecks_geo <- sf::read_sf(here::here("data/shapefiles/oceans/wrecks/wrecks.shp"))

# ## Third Tab - Ocean Maps (Zoom)
# 
# hawaii_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/hawaii_boating.shp"))
# maui_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/maui_boating.shp"))
# oahu_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/oahu_boating.shp"))
# islands_boating_geo <- rbind(oahu_boating_geo, maui_boating_geo)
# kauai_boating_geo <- sf::read_sf(here::here("data/shapefiles/oceans/boating/kauai_boating.shp"))

## Fourth Tab - Island Maps

island_df <- read.csv(here::here("data/island_basemap_paths.csv"))

island_layers <- tibble(
  layer = c("none", "Hotels", "Banks", "Golf Courses", "Hunting Zones")
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

## Section 2 ____________________________________________________
# set up the user interface
ui <- navbarPage("hinuhinu",
                 tabPanel("Intro", includeCSS("style.css"),
                          fluidPage(h1("The Hawaiian Islands"),
                                    br(),
                                    p(strong(em("Eddie Would Go"), "Eddie Aikau, Hawaiian Hero")),
                                    br(),
                                    p("The European discovery of Hawaii occurred on January 18, 1778, when English ships under the command of Captain James Cook sighted the islands of Oahu an Kauai.", 
                                      "Cook was conducting one of the great exploratory voyages of history and ", 
                                      a("mapmaking was an integral part of his work.", href = "https://www.storyofhawaiimuseum.com/the-story-of-hawaii/")), 
                                    p("Today with new technological tools and ", a("open data ", href = "https://en.wikipedia.org/wiki/Open_data"), "we can easily map out weather, tides, and other activities on land and sea."),
                                    p("Using advanced computer techiques, scientists, planners and residents can visualize the impace of development throughout Hawaii"),
                                    p("Play with this interactive tool and find out!"),
                                    br(),
                                    br(),
                                    div(img(src = "is-work.png", height = 420, width = 1000), style="text-align: center;"),
                                    br(),
                                    br(),
                                    br(),
                                    div(p(strong("Built by"), a("Matt.0", href = "https://twitter.com/mattoldach"), "using the power of Rstudio and Shiny."), 
                                        p(strong("Sources:"), a("State of Hawaii Office of Planning", href = "https://planning.hawaii.gov/gis/download-gis-data/"), "for shapefiles,", a("EarthWorks", href = "https://earthworks.stanford.edu/catalog/stanford-qh711pf3383"), "for rasters"),
                                        style="text-align: right;"),
                                    setBackgroundColor("#F5F5F2")
                          )
                 ),
                 tabPanel("State Maps",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Choose Island:"),
                                                              htmlOutput("ocean_state_basemap_selector"),
                                                              br()),
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Choose Elevation Raster:"),
                                                              htmlOutput("ocean_state_relief_selector"),
                                                              br()),
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("ocean_state_layer_selector"),
                                                              br())
                                                  ),

                                                  mainPanel(
                                                    plotOutput("plot1")
                                                  )
                          )
                 )
                 ),
                 tabPanel("Island Maps",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel( # designates location of following items
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Choose Island:"),
                                                              htmlOutput("island_basemap_selector"),
                                                              br()),
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Choose Elevation Raster:"),
                                                              htmlOutput("island_relief_selector"),
                                                              br()),
                                                    wellPanel(style = "background: #F5F5F2",
                                                              h4("Overlay GIS Layer:"),
                                                              htmlOutput("island_layer_selector"),
                                                              br())
                                                  ),
                                                  
                                                  mainPanel(
                                                    plotOutput("plot2")
                                                  )
                          )
                          )
                 ),
                 tabPanel("Ocean Maps: Islands"),
                 tabPanel("Slide Show")
                 
)



## Section 3 ____________________________________________________
# server controls what is displayed by the user interface
server <- shinyServer(function(input, output) {
  
  # creates logic behind ui outputs ** pay attention to letter case in names
  
  ## Second Tab
  output$ocean_state_basemap_selector <- renderUI({ # creates basemap select box object called in ui
    selectInput(
      inputId = "basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(ocean_state_df$location)),
      # calls unique values from the basemap column in the previously created table
      selected = "State Map"
    ) # default choice (not required)
  })
  
  output$ocean_state_relief_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- ocean_state_df[ocean_state_df$location == input$basemap, "relief"]
    # creates a reactive list of available reliefs based on the basemap selection made
    
    selectInput(
      inputId = "relief", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$ocean_state_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- ocean_state_layers
    # creates a reactive list of available reliefs based on the basemap selection made
    
    selectInput(
      inputId = "layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot1 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter basemaps
    ocean_state_df %>%
      filter(location == input$basemap & relief == input$relief) -> ocean_state_df
    
    # load the basemap for four main islands
    state_plot <- readRDS(here::here("data/basemaps/ocean_state", ocean_state_df$filename))
    if (input$layer == "Explosives Dumping") {
      state_plot + geom_sf(data = state_explosive_geo, color = darken("#F98866"), fill = "#F98866")
    } else if(input$layer == "Whale Sanctuaries"){
      state_plot + geom_sf(data = state_whale_geo, color = darken("#756bb1"), fill = "#756bb1")
    } else if(input$layer == "Boating"){
      state_plot + geom_sf(data = state_boating_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 18, size = 2, alpha = 0.5)
    } else if(input$layer == "Fishing Aggregators"){
      state_plot + geom_sf(data = state_fishing_geo, color = darken("#7C000C"), fill = "#7C000C", size = 2, alpha = 0.5)
    } else if(input$layer == "Surfing Spots"){
      state_plot + geom_sf(data = state_surfing_geo, color = darken("#B35234"), fill = "#B35234", shape = 16, size = 2, alpha = 0.5)
    } else if(input$layer == "Shipwrecks"){
      state_plot + geom_sf(data = state_wrecks_geo, color = darken("#963484"), fill = "#963484", shape = 25, size = 2, alpha = 0.5)
    } else {
      state_plot
    }
  })

  
  ## Third Tab
  
  output$island_basemap_selector <- renderUI({ # creates basemap select box object called in ui
    selectInput(
      inputId = "basemap", # name of input
      label = "", # label displayed in ui
      choices = as.character(unique(island_df$location)),
      # calls unique values from the basemap column in the previously created table
      selected = "Hawaii"
    ) # default choice (not required)
  })
  
  output$island_relief_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_df[island_df$location == input$basemap, "relief"]
    # creates a reactive list of available reliefs based on the basemap selection made
    
    selectInput(
      inputId = "relief", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$island_layer_selector <- renderUI({ # creates relief select box object called in ui
    
    data_available <- island_layers
    # creates a reactive list of available reliefs based on the basemap selection made
    
    selectInput(
      inputId = "layer", # name of input
      label = "", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })
  
  output$plot2 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter basemaps
    island_df %>%
      filter(location == input$basemap & relief == input$relief) -> island_df
    # filter all shapefiles
    hotels_geo %>% filter(island == input$basemap) -> hotels_geo
    banks_geo %>% filter(island == input$basemap) -> banks_geo
    golf_geo %>% filter(island == input$basemap) -> golf_geo
    hunting_geo %>% filter(Island == input$basemap) -> hunting_geo
    
    # load the basemap for four main islands
    a_plot <- readRDS(here::here("data/basemaps/land", island_df$filename))
    if (input$layer == "Hotels") {
      a_plot + geom_sf(data = hotels_geo, color = darken("#963484"), fill = "#963484", shape = 21, size = 2, alpha = 0.5)
    } else if(input$layer == "Banks"){
      a_plot + geom_sf(data = banks_geo, color = darken("#9E391A"), fill = "#9E391A", shape = 21, size = 2, alpha = 0.5)
    } else if(input$layer == "Golf Courses"){
      a_plot + geom_sf(data = golf_geo, color = darken("#F98866"), fill = "#F98866", size = 2, alpha = 0.5)  
    } else if(input$layer == "Hunting Zones"){
      # to-do - create a script to source with palletes!
      a_plot + 
        geom_sf(data = hunting_geo, aes(fill = hunting_vec), alpha = 0.5) +
        scale_fill_manual(values = hunting_colors)
    } else {
      a_plot
    }
  })
}) # close the shinyServer

## Section 4____________________________________________________
shinyApp(ui = ui, server = server) # need this if combining ui and server into one file.