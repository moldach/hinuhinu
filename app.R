#* save the following code in a file named app.R *
library(shiny)
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



## Third TAB - Island Land Maps 

df <- read.csv(here::here("data/island_basemap_paths.csv"))

layer_df <- tibble(
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
ui <- shinyUI(
  fluidPage( # allows layout to fill browser window
    titlePanel(""),
    # adds a title to page and browser tab
    #-use "title = 'tab name'" to name browser tab
    sidebarPanel( # designates location of following items
      htmlOutput("basemap_selector"), # add selectinput boxs
      htmlOutput("relief_selector"), # from objects created in server,
      htmlOutput("layer_selector") # from objects created in server
    ),

    mainPanel(
      plotOutput("plot1") # put plot item in main area
    )
  )
)


## Section 3 ____________________________________________________
# server controls what is displayed by the user interface
server <- shinyServer(function(input, output) {

  # df <- read.csv(here::here("test_paths.csv"))
  # creates logic behind ui outputs ** pay attention to letter case in names

  output$basemap_selector <- renderUI({ # creates basemap select box object called in ui
    selectInput(
      inputId = "basemap", # name of input
      label = "Choose Island:", # label displayed in ui
      choices = as.character(unique(df$location)),
      # calls unique values from the basemap column in the previously created table
      selected = "Hawaii"
    ) # default choice (not required)
  })

  output$relief_selector <- renderUI({ # creates relief select box object called in ui

    data_available <- df[df$location == input$basemap, "relief"]
    # creates a reactive list of available reliefs based on the basemap selection made

    selectInput(
      inputId = "relief", # name of input
      label = "Add Elevation Detail:", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })

  output$layer_selector <- renderUI({ # creates relief select box object called in ui

    data_available <- layer_df
    # creates a reactive list of available reliefs based on the basemap selection made

    selectInput(
      inputId = "layer", # name of input
      label = "Select GIS Layer:", # label displayed in ui
      choices = unique(data_available), # calls list of available reliefs
      selected = unique(data_available)[1]
    )
  })

  output$plot1 <- renderPlot({ # creates a the plot to go in the mainPanel
    # filter basemaps
    df %>%
      filter(location == input$basemap & relief == input$relief) -> df
    # filter all shapefiles
    hotels_geo %>% filter(island == input$basemap) -> hotels_geo
    banks_geo %>% filter(island == input$basemap) -> banks_geo
    golf_geo %>% filter(island == input$basemap) -> golf_geo
    hunting_geo %>% filter(Island == input$basemap) -> hunting_geo
    
    # load the basemap for four main islands
    a_plot <- readRDS(here::here("data/basemaps/land", df$filename))
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