library(terra)
library(dplyr)
library(sf)
library(ggplot2)
library(rayshader)
library(MetBrewer)
library(XML)
library(magick)
library(glue)
library(scales)
library(ggplot2)

zugspitze <- c(47.419857, 10.983788)
ex_zugspitze <- ext(zugspitze[2]-0.2, zugspitze[2]+0.2, zugspitze[1]-0.2, zugspitze[1]+0.2)
ex <- ext(11.83, 11.9,47.65, 47.74)
Germany <- giscoR::gisco_get_countries(country = "Germany") %>% st_crop(., ex_zugspitze)
Austria <- giscoR::gisco_get_countries(country = c("Germany", "Austria")) %>% st_crop(., ex_zugspitze)
el_Austria <- elevatr::get_elev_raster(Austria, z = 12, clip = "locations")

elevation <- elevatr::get_elev_raster(Germany,z = 12, clip = "locations")
plot(el_Austria)

matrix <- raster_to_matrix(elevation)
# matrix %>% 
#   sphere_shade(texture="desert") %>% 
#   add_shadow(ray_shade(matrix), 0.5) %>%
#   #add_shadow(ambient_shade(matrix), 0) %>% # add lights
#   add_water(detect_water(matrix), color = "desert") %>%
#   plot_map()

# import gpx file
import_hike <- function(link){
  gpx <- htmlTreeParse(file = link, useInternalNodes = TRUE)
  coords <- xpathSApply(doc = gpx, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx, path = "//trkpt/ele", fun = xmlValue)
  hike.sf <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation)) %>% 
    st_as_sf(., coords = c("lon", "lat"))
  return(hike.sf)
}

hike1 <- import_hike("data/hike1")
hike2 <- import_hike('data/hike2')
hike3 <- import_hike('data/hike3')
hike4 <- import_hike('data/hike4')
hike5 <- import_hike('data/hike5')

link <- "Data/gpx/Höllental.gpx"
hike_höllental <- import_hike(link)
komoot_link <- "https://www.komoot.de/tour/56376593"
# add points of interests
Brecherspitz <- c(47.675049, 11.864671)

# Dynaimcally set window height and width based on object size
w <- nrow(matrix)
h <- ncol(matrix)

# Scale the dimensions so we can use them as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# Limit ratio so that the shorter side is at least .75 of longer side
if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# add hikes to map
matrix %>% 
  height_shade() %>%
  add_water(detect_water(matrix), color = "desert") %>%
  add_shadow(ray_shade(matrix), 0.5) %>%
  plot_3d(matrix,
          solid=FALSE , 
          zscale = 10, 
          fov = 0, theta = 30, zoom = 0.8, 
          phi = 45, 
          windowsize = c(1000*wr, 1000*hr)
  )

#Pass in the extent of the underlying raster (stored in an attribute for the montereybay
#dataset) and the latitudes, longitudes, and altitudes of the track.
render_path(extent = ex_zugspitze, 
            lat = st_coordinates(hike_höllental)[,2], long = st_coordinates(hike_höllental)[,1], 
            color="white", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)

render_label(matrix, x = 220, y = 120, z = 1500, zscale = 10,
             text = "Zugspitze", textsize = 2*wr, linewidth = 2)

render_path(extent = ex, 
            lat = st_coordinates(hike2)[,2], long = st_coordinates(hike2)[,1], 
            color="red", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)
render_path(extent = ex, 
            lat = st_coordinates(hike3)[,2], long = st_coordinates(hike3)[,1], 
            color="green", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)
render_path(extent = ex, 
            lat = st_coordinates(hike4)[,2], long = st_coordinates(hike4)[,1], 
            color="blue", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)
render_path(extent = ex, 
            lat = st_coordinates(hike5)[,2], long = st_coordinates(hike5)[,1], 
            color="black", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)
# render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
#                 scale_length = c(0.33,1))
# render_compass(position = "E", compass_radius=40)
render_label(matrix, x = 220, y = 120, z = 1500, zscale = 10,
             text = "Schliersee", textsize = 2*wr, linewidth = 2)
render_label(matrix, x = 380, y = 510, z = 1000, zscale = 10,
             text = "Spitzingsee", textsize = 2*wr, linewidth = 2)
render_label(matrix, x = 230, y = 440, z = 1300, zscale = 10,
             textsize = 2*wr, linewidth = 2, text = "Brecherspitz")

# render
render_snapshot()

render_highquality(
  "s0.4_hikes_Schliersee_RGB.png",
  parallel=T, # using parallel processing, which speeds things up
  samples = 300,
  light = F, # we are using environmental light
  environment_light = "data/phalzer_forest_01_4k.hdr", 
  interactive = FALSE, # because I don’t want to accidentally screw up the scene while it’s rendering by accidentally interacting with it
  intensity_env = 1.75,
  rotate_env = 180, # light will come from SSW instead NNE
  width = round(6000 * wr), 
  height = round(6000 * hr))

# add annotations
