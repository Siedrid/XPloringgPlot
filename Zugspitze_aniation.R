# Zugspitze

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
library(reshape)
library(gpx)

load("Data/Rayshader_animate/el_mat.RData")

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


zugspitze <- c(47.419857, 10.983788)
ex_zugspitze <- ext(zugspitze[2]-0.1, zugspitze[2]+0.1, zugspitze[1]-0.2, zugspitze[1]+0.2)
Austria <- giscoR::gisco_get_countries(country = c("Germany", "Austria")) %>% st_crop(., ex_zugspitze)
el_Austria <- elevatr::get_elev_raster(Austria, z = 12, clip = "locations")
writeRaster(el_Austria, "Data/Rayshader_animate/DEM_Zugspitze.tif")
el_Austria <- terra::rast("Data/Rayshader_animate/DEM_Zugspitze.tif")
plot(el_Austria)

matrix <- raster_to_matrix(el_Austria)


link <- "Data/gpx/Höllental.gpx"
hike_höllental <- import_hike(link)
komoot_link <- "https://www.komoot.de/tour/56376593"
# add points of interests


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

render_path(extent = ex_zugspitze, 
            lat = st_coordinates(hike_höllental)[,2], long = st_coordinates(hike_höllental)[,1], 
            color="white", antialias=TRUE, heightmap=matrix, zscale=10, linewidth=2)

render_label(matrix, x = 220, y = 120, z = 1500, zscale = 10,
             text = "Zugspitze", textsize = 2*wr, linewidth = 2)

# Animation with ggplot

gpx_file_loc <- system.file("Data/gpx/Höllental.gpx")
gpx_zugspitze <- read_gpx(link)
hike_zugspitze <- gpx_zugspitze$tracks[[1]]

#df <- get_elevdata_long(el_mat)
rst_df <- cbind.data.frame(
  crds(el_Austria, na.rm = F),
  values(el_Austria))

my_plot <- ggplot()+
  geom_raster(data = rst_df, aes(x = x, y=y, fill = DEM_Zugspitze))+
  scale_alpha(range = c(1,0), na.value = 0)+
  scale_x_continuous("Longitude") +
  
  scale_y_continuous("Latitude", expand = c(0,0)) +
  scale_fill_gradientn("Elevation",
                       colours = terrain.colors(10))+
  geom_line(data = hike_zugspitze, aes(x= Longitude, y= Latitude))

plot_gg(my_plot, shadow_intensity = 0.7, width = 5, height = 5,
        multicore = TRUE, scale = 350, raytrace = TRUE)

# with Animation ----

# calculate rel Speed
hike_zugspitze$rel_speed <- c(0,
               # Calculate a distance by sqrt((long1 - long2) ^ 2 + (lat1 - lat2)^2) distance
               apply(diff(as.matrix(hike_zugspitze[, c("Longitude", "Latitude")])), 1, function(x){
                 sqrt(x[1] ^ 2 + x[2] ^ 2)
               }) / 
                 # Devide by time in seconds
                 diff(as.matrix(hike_zugspitze$Time)))

video_indeces <- get_video_indeces(time_data = hike_zugspitze$Time,
                                   number_of_screens = 10)
zoom_scale <- 0.5 + 0.5 * 1/(1 + exp(seq(-5, 5, length.out = length(video_indeces))))
#theta_angles <- rev(30 - 50 * 1/(1 + exp(seq(-5, 6, length.out = length(video_indeces)))))
theta_angles <- seq(140, 100, length.out = length(video_indeces))
for (video_index in 1:length(video_indeces)) {
  
  if (video_index == 1) {
    message("First step takes longer due to shadow calculation.")
  }
  
  vid_indx <- video_indeces[video_index]
  
  my_plot <- ggplot() +
    geom_raster(data = rst_df, aes(x = x, y=y, fill = DEM_Zugspitze))+
    
    scale_y_continuous("Latitude", expand = c(0,0)) +
    scale_fill_gradientn("Elevation", colours = colorspace::terrain_hcl(12)) +
    scale_x_continuous(paste0("Longitude | ", hike_zugspitze$Time[vid_indx]), expand = c(0,0)) +
    coord_fixed() +
    geom_line(data = hike_zugspitze[1:vid_indx, ],
      aes(x = Longitude, y = Latitude, color = rel_speed)) +
    scale_color_viridis_c(option = "A")+
    guides(colour=FALSE)
  
  
  shadow_mat <- plot_gg(my_plot, shadow_intensity = 0.7, width = 5, height = 5, multicore = TRUE, scale = 350,
                        zoom = zoom_scale[video_index],
                        theta = theta_angles[video_index],
                        phi = 60, windowsize = c(800, 800), reduce_size = 0.5,
                        saved_shadow_matrix = if (video_index == 1) {
                          NULL
                        } else {
                          shadow_mat
                        },
                        save_shadow_matrix = TRUE, raytrace = TRUE)
  render_snapshot(filename = file.path("Out/Snapshots/", paste0("Zugspitze_", video_index, ".png")), clear = TRUE)
}

video_files <- list.files("Out/Snapshots/", pattern = "Zugspitze", full.names = T)

if (make_gif) {
  images <- magick::image_read(video_files) 
  animation <- magick::image_animate(images, fps = 1, optimize = TRUE)
  magick::image_write(animation, "Out/Hike_zugspitze_v1.gif")
  return(paste0(output_file_loc, ".gif"))
}
