library(RCurl)
library(sf)
library(ggplot2)
library(ggspatial)
library(basemaps)
df_steigerwald <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
df.sf <- st_as_sf(x = df_steigerwald, coords = c("x", "y"), crs = "+init=epsg:32632")
#st_write(df.sf, "steigerwaldv3.gpkg")

head(df_steigerwald)
summary(df_steigerwald)
names(df_steigerwald)

# Bubble Plot, LCname is coded through circle size
ggplot()+
  geom_sf(data = df.sf, aes(color = L7.ndvi, size = LCname))+
  annotation_scale(location = 'bl')+
  annotation_north_arrow(location = 'bl', pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"))+
  ggtitle("Spatial distribution of NDVI and Land Cover classes")

# same, but one symbol per LC class
ggplot()+
  geom_sf(data = df.sf, aes(color = L7.ndvi, pch = LCname))+
  annotation_scale(location = 'bl')+
  annotation_north_arrow(location = 'bl', pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"))+
  ggtitle("Spatial distribution of NDVI and Land Cover classes")

# just some grouping and summarizing :)
df_LC_ndvi <- df_steigerwald %>% group_by(LCname) %>% summarise(n = n(), NDVI = mean(L7.ndvi, na.rm = T))

ggplot(data = df_LC_ndvi, aes(x= LCname, y =NDVI))+
  geom_bar(stat = "identity")




