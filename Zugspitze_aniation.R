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

get_video_indeces <- function(time_data = c(), number_of_screens = 8) {
  stopifnot(length(time_data) > 20)
  time_distance <- max(time_data) - min(time_data)
  avg_time_step <- time_distance/length(time_data)
  
  index_from <- 1
  index_to <- 2 
  all_indeces <- c(index_from)
  
  while(index_to < length(time_data) && index_to > index_from) {
    while(
      
      if(index_to >= length(time_data)){
        FALSE
      } else {
        (time_data[index_to] - time_data[index_from]) < time_distance/number_of_screens
      }
    ) {
      index_to <- index_to + 1
    }
    all_indeces <- c(all_indeces, index_to)
    index_from <- index_to
    index_to <- index_to + 1
  }
  return(all_indeces)
}


zugspitze <- c(47.419857, 10.983788)
ex_zugspitze <- ext(zugspitze[2]-0.05, zugspitze[2]+0.1, zugspitze[1]-0.05, zugspitze[1]+0.05)
Austria <- giscoR::gisco_get_countries(country = c("Germany", "Austria")) %>% st_crop(., ex_zugspitze)
el_Austria <- elevatr::get_elev_raster(Austria, z = 12, clip = "locations")
plot(el_Austria)
writeRaster(el_Austria, "Data/Rayshader_animate/DEM_Zugspitze_05.tif")


el_Austria <- terra::rast("Data/Rayshader_animate/DEM_Zugspitze_05.tif")
plot(el_Austria)

matrix <- raster_to_matrix(el_Austria)


link <- "Data/gpx/Höllental.gpx"
hike_höllental <- import_hike(link)
komoot_link <- "https://www.komoot.de/tour/56376593"


# Animation with ggplot

gpx_zugspitze <- read_gpx(link)
hike_zugspitze <- gpx_zugspitze$tracks[[1]]

#df <- get_elevdata_long(el_mat)
rst_df <- cbind.data.frame(
  crds(el_Austria, na.rm = F),
  values(el_Austria))

my_plot <- ggplot()+
  geom_raster(data = rst_df, aes(x = x, y=y, fill = DEM_Zugspitze_05))+
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
number_of_scenes = 25
hike_zugspitze$rel_speed <- c(0,
               # Calculate a distance by sqrt((long1 - long2) ^ 2 + (lat1 - lat2)^2) distance
               apply(diff(as.matrix(hike_zugspitze[, c("Longitude", "Latitude")])), 1, function(x){
                 sqrt(x[1] ^ 2 + x[2] ^ 2)
               }) / 
                 # Devide by time in seconds
                 diff(as.matrix(hike_zugspitze$Time)))

video_indeces <- get_video_indeces(time_data = hike_zugspitze$Time,
                                   number_of_screens = number_of_scenes)
zoom_scale <- 0.5 + 0.5 * 1/(1 + exp(seq(-5, 5, length.out = length(video_indeces))))
#theta_angles <- rev(30 - 50 * 1/(1 + exp(seq(-5, 6, length.out = length(video_indeces)))))
theta_angles <- seq(140, 100, length.out = length(video_indeces))

for (video_index in 1:length(video_indeces)) {
  
  if (video_index == 1) {
    message("First step takes longer due to shadow calculation.")
  }
  
  vid_indx <- video_indeces[video_index]
  
  my_plot <- ggplot() +
    geom_raster(data = rst_df, aes(x = x, y=y, fill = DEM_Zugspitze_05))+
    
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
  render_snapshot(filename = file.path("Out/Snapshots_v2/", paste0("Zugspitze_", sprintf("%02d",video_index), ".png")), clear = TRUE)
}

video_files <- list.files("Out/Snapshots_v2/", pattern = "Zugspitze", full.names = T)

if (make_gif) {
  images <- magick::image_read(video_files) 
  animation <- magick::image_animate(images, fps = 1, optimize = TRUE)
  magick::image_write(animation, "Out/Hike_zugspitze_v2.gif")
  return(paste0(output_file_loc, ".gif"))
}
