## Tutorium

# Task 2:
# wordcloud

library(wordcloud)
wordcloud::wordcloud(words = names(lang_count), freq = lang_count, min.freq = 1)


# Matrixes etc -----
'Indexing dataframes etc.'

fruits <- matrix(c(1,2,1,3,1,1,3,1,2), ncol = 3)
fruits[4] # single brackets return values in order of columns
fruits[[2]]
as.data.frame(fruits)[[2]]

# Working with Datasets -----

energy_prod <- read.csv("Data/day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")
names(energy_prod)
head(energy_prod)
summary(energy_prod)
unique(energy_prod$ProductionTypeName)

library(ggplot2)
# Clean data

energy_prod <- energy_prod[!energy_prod$ActualGenerationOutput > energy_prod$InstalledGenCapacity *4, ]
#nrow(energy_prod)

ggplot(data = energy_prod, aes(x = ProductionTypeName)) +
  geom_bar()

g <- ggplot(data = energy_prod, aes(x = InstalledGenCapacity, y=ActualGenerationOutput))
g + geom_point()

plants <- unique(energy_prod$GenerationUnitEIC)

barplot()
group_by(energy_prod, GenerationUnitEIC, ProductionTypeName)
energy_prod %>% group_by(ProductionTypeName) %>% summarize(Sum = sum(GenerationUnitEIC))

df_agg_type <- aggregate(energy_prod$InstalledGenCapacity,
                         by = list(energy_prod$ProductionTypeName),
                         FUN = sum)

colnames(df_agg_type) <- c("ProductionTypeName", "InstalledGenCapacity_sum")
df_agg_type$InstalledGenCapacity_sum <- df_agg_type$InstalledGenCapacity_sum/1000

df_agg_type <- df_agg_type[order(df_agg_type$InstalledGenCapacity_sum),] # sort the dataframe by indexing with order

# sort for plotting in the right, y is a character, and ggplot converts it to factor
df_agg_type$ProductionTypeName <- factor(df_agg_type$ProductionTypeName, levels = unique(df_agg_type$ProductionTypeName))

ggplot(data = df_agg_type, aes(x= df_agg_type$ProductionTypeName, y = df_agg_type$InstalledGenCapacity_sum, fill = df_agg_type$ProductionTypeName))+
  geom_bar(stat = "identity", show.legend = F)+
  #scale_color_manual(name = "Legend", values = unique(df_agg_type$ProductionTypeName))+
  coord_flip()+
  xlab("Installed Capacity")+
  ylab("Production Type")

# Tasks

df_agg_cap <- aggregate(energy_prod$InstalledGenCapacity,
                         by = list(energy_prod$ProductionTypeName),
                         FUN = sum)
df_agg_prod <- aggregate(energy_prod$ActualGenerationOutput,
                         by = list(energy_prod$ProductionTypeName),
                         FUN = sum)

colnames(df_agg_cap) <- c("ProductionTypeName", "InstalledGenCapacity_sum")
colnames(df_agg_prod) <- c('ProductionTypeName', 'ActualProduction')

merged_df <- merge(df_agg_cap, df_agg_prod, by = "ProductionTypeName")

ggplot(data = energy_prod, aes(x = InstalledGenCapacity, y = ActualGenerationOutput, color = ProductionTypeName))+
  geom_point()

ts_prod <- aggregate(energy_prod$InstalledGenCapacity,
                     by= list(energy_prod$DateTime),
                     FUN = sum)
colnames(ts_prod) <- c('DateTime', 'ProductionCapacity')
ggplot(data = ts_prod)+
  geom_point(aes(x = DateTime, y = ProductionCapacity))

# Spatial Plotting
library(sf)
library(ggspatial)
library(RColorBrewer)
library(terra)
library(ggspatial)
library(raster)
library(ggplot2)
europe <- st_read("Data/ne_10m_admin_0_countries.shp")

ctr <- europe$WB_A2

energy_prod$MapCodev2 <- energy_prod$MapCode

energy_prod$MapCodev2[grep("DE_", energy_prod$MapCodev2)] <- "DE"
energy_prod$MapCodev2 <- gsub("NIE", "GB", energy_prod$MapCodev2)

# Aggregating the dataframe
gen_out <- aggregate(energy_prod[c('ActualGenerationOutput', 'ActualConsumption', 'InstalledGenCapacity')], 
                     by = list(energy_prod$MapCodev2), FUN = sum, na.rm = T)
colnames(gen_out)[1] <- "ctr_code"
# Checking if there are still missing countries
gen_out$ctr_code[!gen_out$ctr_code %in% europe$WB_A2]
europe[europe$NAME_EN == "Norway",]$WB_A2 <- "NO"
st_write(europe, "Data/admin_ctr_cleared.gpkg")

euro_geom <- europe[c("NAME_LONG", "WB_A2", "geometry")]

box <- st_bbox(c(xmin = -30, xmax = 33, ymin = 30, ymax = 81), crs = st_crs(4326))

merged_df <- merge(europe, gen_out, by.x = "WB_A2", by.y = "ctr_code") %>% st_crop(box) %>% st_transform(st_crs(3035))

ggplot(data = merged_df, aes(fill = ActualGenerationOutput))+
  geom_sf()+
  ggtitle("Generation Output per Country")+
  #coord_sf(xlim = c(2521500,6754500), ylim=c(900500,4516500), expand = F)+  
  #annotation_scale(location = 'bl')+
  annotation_north_arrow(location = 'bl', pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"))+
  scale_fill_gradientn("Generated Energy [GW]", colours = brewer.pal(9, 'YlOrBr'))+
  theme(legend.position = 'bottom', legend.key.width = unit(0.5, "in"))


rnaturalearth::ne_download(scale = 50, type = "MSR_50")
dem <- rast("data/PRISMA_SR_50M/PRIMSA_SR_50M.tif") %>% crop(box) %>% project("epsg:3035")
crop(dem, )
ggplot()+
  geom_stars(data = st_as_stars(dem))

rst_df <- cbind.data.frame(
  crds(dem, na.rm = F),
  values(dem))


ggplot()+
  geom_raster(data = rst_df, aes(x = x, y=y, alpha = value))+
  scale_alpha(range = c(1,0), na.value = 0)
