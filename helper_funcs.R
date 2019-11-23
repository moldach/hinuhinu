ease_function = function(beginning, end, steepness = 1, length.out = 540) {
        single = (end) + (beginning - end) * 1/(1 + exp(seq(-10, 10, length.out = length.out)/(1/steepness)))
        single
}

zoom_values = c(ease_function(1,0.3), ease_function(0.3,1))

define_image_size <- function(bbox, major_dim = 400) {
        # calculate aspect ration (width/height) from lat/long bounding box
        aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
        # define dimensions
        img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
        img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
        size_str <- paste(img_width, img_height, sep = ",")
        list(height = img_height, width = img_width, size = size_str)
}

get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
        require(httr)
        require(glue) 
        require(jsonlite)
        
        url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
        
        # define JSON query parameter
        web_map_param <- list(
                baseMap = list(
                        baseMapLayers = list(
                                list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                                                map_type = map_type)))
                        )
                ),
                exportOptions = list(
                        outputSize = c(width, height)
                ),
                mapOptions = list(
                        extent = list(
                                spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
                                xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
                                xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
                                ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
                                ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
                        )
                )
        )
        
        res <- GET(
                url, 
                query = list(
                        f = "json",
                        Format = "PNG32",
                        Layout_Template = "MAP_ONLY",
                        Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
        )
        
        if (status_code(res) == 200) {
                body <- content(res, type = "application/json")
                message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
                if (is.null(file)) 
                        file <- tempfile("overlay_img", fileext = ".png")
                
                img_res <- GET(body$results[[1]]$value$url)
                img_bin <- content(img_res, "raw")
                writeBin(img_bin, file)
                message(paste("image saved to file:", file))
        } else {
                message(res)
        }
        invisible(file)
}

## Define global variables
max_tiles = 500
