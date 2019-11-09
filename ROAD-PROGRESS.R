grob <- readRDS(here::here("data/basemaps/roads/Hilo_road_network_grob_basemap.rds"))

# load latitude/longitude coordinates for cities
cities_geolocation <- read_csv(here::here("data/cities_geolocation.csv"))
j <- "Hilo"

# do
city <- cities_geolocation %>% filter(city == j)
pt <- data.frame(lat = city$lat, long = city$lon)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
circle <- st_buffer(pt, dist = 24140.2)


# subset shapefiles to fit city
banks_circle <- circle %>% st_transform(st_crs(banks_geo))
banks <- st_intersection(banks_circle, banks_geo)

hospitals_circle <- circle %>% st_transform(st_crs(hospitals_geo))
hospitals <- st_intersection(hospitals_circle, hospitals_geo)

hotels_circle <- circle %>% st_transform(st_crs(hotels_geo))
hotels <- st_intersection(hotels_circle, hotels_geo)

precincts_circle <- circle %>% st_transform(st_crs(precincts_geo))
precincts <- st_intersection(precincts_circle, precincts_geo)


# banks
grob + geom_sf(data = banks, color = darken("#963484"), fill = "#963484", shape = 19, size = 2.5, alpha = 0.6)

# hospitals
grob + geom_sf(data = hospitals, color = darken("#78ACA8"), fill = "#78ACA8", shape = 19, size = 2.5, alpha = 0.6)

# hotels
grob + geom_sf(data = hotels, color = darken("#E77A5B"), fill = "#E77A5B", shape = 19, size = 2.5, alpha = 0.6)

# precincts
grob + geom_sf(data = precincts, color = darken("#20425B"), fill = "#20425B", shape = 19, size = 2.5, alpha = 0.6)