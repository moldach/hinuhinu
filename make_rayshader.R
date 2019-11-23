library(leaflet)
library(geoviz)
library(rayshader)

source("helper_funcs.R")

### Hawaii
lat <- 19.617688
lon <- -155.497077
square_km <- 48

# Hawaii
bbox <- list(
        p1 = list(long = -154.6747, lat = 18.6333),
        p2 = list(long = -156.1075, lat = 20.64573)
)

dem <- mapzen_dem(lat, lon, square_km, max_tiles) # Whats the biggest max_tiles can be???

elev_matrix <- matrix(
        raster::extract(dem, raster::extent(dem), buffer = 1000), ## Make this higher
        nrow = ncol(dem),
        ncol = nrow(dem)
)

ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)

# fetch overlay image
image_size <- define_image_size(bbox, major_dim = 9000)
overlay_file <- "hawaii_overlap.png"
get_arcgis_map_image(bbox,
                     map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height,
                     sr_bbox = 4326
)
overlay_img <- png::readPNG("hawaii_overlap.png")

# plot
rgl::clear3d()
elev_matrix %>%
        sphere_shade(sunangle = 270, texture = "imhof4") %>%
        add_water(detect_water(elev_matrix), color = "imhof4") %>%
        add_overlay(overlay_img, alphalayer = 0.8) %>%
        add_shadow(ray_shade(elev_matrix, zscale = 3, maxsearch = 300), 0.5) %>%
        plot_3d(elev_matrix,
                solid = T,
                zscale = raster_zscale(dem) / 3,
                water = T,
                wateralpha = 0.4,
                watercolor = "lightblue",
                waterlinecolor = "white",
                waterlinealpha = 0.5,
                windowsize = c(1200, 1000),
                fov = 0, theta = 135, zoom = 0.75, phi = 45
        )

render_movie(
        filename = "hawaii.mp4", type = "custom",
        frames = 1080,
        phi = 30 + 15 * sin(1:1080 * pi / 540),
        theta = -45 - 1:1080,
        zoom = zoom_values
)

### Maui
lat = 20.785700
lon = -156.259204
square_km = 22

# Maui
bbox <- list(
        p1 = list(long = -156.8037, lat = 20.29737),
        p2 = list(long = -155.7351, lat = 21.29577)
)

dem <- mapzen_dem(lat, lon, square_km, max_tiles) # Whats the biggest max_tiles can be???

elev_matrix <- matrix(
        raster::extract(dem, raster::extent(dem), buffer = 1000), ## Make this higher
        nrow = ncol(dem),
        ncol = nrow(dem)
)

ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)

# fetch overlay image
image_size <- define_image_size(bbox, major_dim = 9000)
overlay_file <- "maui_overlap.png"
get_arcgis_map_image(bbox,
                     map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height,
                     sr_bbox = 4326
)
overlay_img <- png::readPNG("maui_overlap.png")

# plot
rgl::clear3d()
elev_matrix %>%
        sphere_shade(sunangle = 270, texture = "imhof4") %>%
        add_water(detect_water(elev_matrix), color = "imhof4") %>%
        add_overlay(overlay_img, alphalayer = 0.8) %>%
        add_shadow(ray_shade(elev_matrix, zscale = 3, maxsearch = 300), 0.5) %>%
        plot_3d(elev_matrix,
                solid = T,
                zscale = raster_zscale(dem) / 3,
                water = T,
                wateralpha = 0.4,
                watercolor = "lightblue",
                waterlinecolor = "white",
                waterlinealpha = 0.5,
                windowsize = c(1200, 1000),
                fov = 0, theta = 135, zoom = 0.75, phi = 45
        )

render_movie(
        filename = "maui.mp4", type = "custom",
        frames = 1080,
        phi = 30 + 15 * sin(1:1080 * pi / 540),
        theta = -45 - 1:1080,
        zoom = zoom_values
)

### Oahu
lat = 21.466841
lon = -157.918327
square_km = 34

### Oahu
bbox <- list(
        p1 = list(long = -158.5615, lat = 21.94945),
        p2 = list(long = -157.4929, lat = 20.95489)
)

dem <- mapzen_dem(lat, lon, square_km, max_tiles) # Whats the biggest max_tiles can be???

elev_matrix <- matrix(
        raster::extract(dem, raster::extent(dem), buffer = 1000), ## Make this higher
        nrow = ncol(dem),
        ncol = nrow(dem)
)

ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)

# fetch overlay image
image_size <- define_image_size(bbox, major_dim = 9000)
overlay_file <- "oahu_overlap.png"
get_arcgis_map_image(bbox,
                     map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height,
                     sr_bbox = 4326
)
overlay_img <- png::readPNG("oahu_overlap.png")

# plot
rgl::clear3d()
elev_matrix %>%
        sphere_shade(sunangle = 270, texture = "imhof4") %>%
        add_water(detect_water(elev_matrix), color = "imhof4") %>%
        add_overlay(overlay_img, alphalayer = 0.8) %>%
        add_shadow(ray_shade(elev_matrix, zscale = 3, maxsearch = 300), 0.5) %>%
        plot_3d(elev_matrix,
                solid = T,
                zscale = raster_zscale(dem) / 3,
                water = T,
                wateralpha = 0.4,
                watercolor = "lightblue",
                waterlinecolor = "white",
                waterlinealpha = 0.5,
                windowsize = c(1200, 1000),
                fov = 0, theta = 135, zoom = 0.75, phi = 45
        )

render_movie(
        filename = "oahu.mp4", type = "custom",
        frames = 1080,
        phi = 30 + 15 * sin(1:1080 * pi / 540),
        theta = -45 - 1:1080,
        zoom = zoom_values
)

### Kauai
lat = 22.069423
lon = -159.498984
square_km = 22

# Kauai
bbox <- list(
        p1 = list(long = -159.7886, lat = 21.777),
        p2 = list(long = -159.2541, lat = 22.27195)
)

dem <- mapzen_dem(lat, lon, square_km, max_tiles) # Whats the biggest max_tiles can be???

elev_matrix <- matrix(
        raster::extract(dem, raster::extent(dem), buffer = 1000), ## Make this higher
        nrow = ncol(dem),
        ncol = nrow(dem)
)

ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)

# fetch overlay image
image_size <- define_image_size(bbox, major_dim = 9000)
overlay_file <- "kauai_overlap.png"
get_arcgis_map_image(bbox,
                     map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height,
                     sr_bbox = 4326
)
overlay_img <- png::readPNG("kauai_overlap.png")

# plot
rgl::clear3d()
elev_matrix %>%
        sphere_shade(sunangle = 270, texture = "imhof4") %>%
        add_water(detect_water(elev_matrix), color = "imhof4") %>%
        add_overlay(overlay_img, alphalayer = 0.8) %>%
        add_shadow(ray_shade(elev_matrix, zscale = 3, maxsearch = 300), 0.5) %>%
        plot_3d(elev_matrix,
                solid = T,
                zscale = raster_zscale(dem) / 3,
                water = T,
                wateralpha = 0.4,
                watercolor = "lightblue",
                waterlinecolor = "white",
                waterlinealpha = 0.5,
                windowsize = c(1200, 1000),
                fov = 0, theta = 135, zoom = 0.75, phi = 45
        )

render_movie(
        filename = "kauai.mp4", type = "custom",
        frames = 1080,
        phi = 30 + 15 * sin(1:1080 * pi / 540),
        theta = -45 - 1:1080,
        zoom = zoom_values
)