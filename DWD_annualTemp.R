# ###########
# download and display temperature and precipitation for Germany
# created just for teaching purpose - not for scientific analysis! 100% accuracy not ensured
# learning goal: download data, convert them, analyse spatio-temporal data and display them
# ###########
# 
# idea triggered by these news articles:
# https://projekte.sueddeutsche.de/artikel/wissen/bilanz-des-sommers-und-der-hitzewelle-2018-e547928/
# https://projekte.sueddeutsche.de/artikel/panorama/duerre-in-deutschland-e407144/
# https://www.nytimes.com/interactive/2018/08/30/climate/how-much-hotter-is-your-hometown.html
# 
# idea to replicate the news infos by using the weather data of the german weather service
# 
# Sept./Oct. 2018, written, adapted, modified and commented by 
# Marius Philipp (EAGLE MSc. student, student assistant Aug/Sept. 2018) and Martin Wegmann
# 
# to-do and ideas:
# make it more generic to select years, months and datasets
# loop through months and download all
# analyse trend and map it
# create animations w/ gganimate and tweenr of time series (maps and line plot), mean temp map and transition to year x map
# adapt some ideas from this weather in Japan R script: https://ryo-n7.github.io/2018-10-04-visualize-weather-in-japan/



################################
### Download from ftp server ###
################################

# Define http and use the getURL from the RCurl package to list the data
# In this example I chose the monthly mean temperature, but it can be switched to e.g. precipitation etc.
# All that needs to be done is change the link to the desired data files
#http <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/monthly/air_temperature_mean/08_Aug/"
http <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_mean/08_Aug/"

# Data for monthly precitpiation in August can be found on the ftp as well:
# ftp://opendata.dwd.de/climate_environment ...

# List resulting datasets of given url
# activate library to fetch url infos
library(RCurl)
result <- getURL(http, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)

# Split string into pieces by identifying certain pattern that seperates the individual filenames
library(tidyverse)

result_tidy <- str_split(result, "\n|\r\n")  # sometimes \r needed
result_tidy <- result_tidy[[1]]

# Reorder data frame to alphabetically decreasing file names
result_tidy <- sort(result_tidy, decreasing = F)

# Delete first entry which is empty because of the previously applied pattern
result_tidy <- result_tidy[2:length(result_tidy)]

# Data can already be subsetted to desired years e.g. 1961-2018
# 1: 1881
# 80: 1980
# 138: 2018
result_tidy <- result_tidy[c(seq(1,138, by=1))]

# Define output directory of downloads
# create one if not yet there, warning if it exists
dir.create("Data/DWDdata/")
out_dir <- "Data/DWDdata/"

# loop for downloading all files listed in the ftp-server if they do not yet exist in the folder
for (i in 1:length(result_tidy)) {
  if(file.exists(paste0(out_dir, result_tidy[i]))){
    print(paste0(result_tidy[i], sep=" ", "file already exists"))
  }
  else
  {
    download.file(paste0(http, result_tidy[i]), paste0(out_dir, result_tidy[i]))
  }
}


############################
### Read ASCII-Grid-File ###
############################

# ## Define file names and directory
mypath <- "Data/DWDdata/"

# just grep all "temp" (= temperature) file, instead of "precipitation"
# check the names in the folder which pattern is appropriate
temp <- grep("*temp*", list.files(path = mypath, pattern="*.gz$"), value=T)

filenames <- paste0(mypath, temp)

# read all ascii files and convert them into a raster stack
# activate relevant raster packages
library(sp)
library(raster)
library(terra)
library(sf)

for (i in 1:length(filenames)){
  if (i == 1){
    # for the first run define our final raster file ...
    current_ascii <- read.asciigrid(filenames[i])
    # remove the raster in case it already exists to avoid duplicate entries
    rm(my_raster)
    my_raster <- raster(current_ascii)
  } else {
    # ... and fill it with each additional run with another layer
    current_ascii <- read.asciigrid(filenames[i])
    current_raster <- raster(current_ascii)
    my_raster <- stack(my_raster, current_raster)
    # Delete all variables except for the raster stack "my_raster"
    rm(i, current_ascii, current_raster)
  }
}

my_crs <- "+init=epsg:31467"
my_raster@crs <- sp::CRS(my_crs)
writeRaster(my_raster, filename = "Data/DWD_Temp.tif")
rm(my_raster)

# Start from here:
library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggspatial)
library(tidyterra)
library(stars)
library(RColorBrewer)
library(ggplot2)

my_raster <- terra::rast("Data/DWD_Temp.tif")
# optional to check the structure
# my_raster

# Change names of raster layers
# adapt sequence in case you subsetted the data before
layer_names <- c(paste0("Year_", seq(1881, 2018, by=1)))
names(my_raster) <- layer_names

# Subset Raster-Stack into old dates and new date
# select range of historical data to subset

# time-series data, to use for temporal aggregation
# define the first and last year to grab from the time series 
rasterHist <- my_raster[[grep("1961", layer_names):grep("2017", layer_names)]]

# year for comparison to long term statistics
rasterComp <- my_raster$Year_2018

# Add Coordinate Reference System to rasterstack
# information extracted from DWD webpage
# ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/monthly/air_temperature_mean/DESCRIPTION_gridsgermany_monthly_air_temperature_mean_en.pdf


# for temperature only!
# do NOT use for precipitation or other datasets
# Divide by 10 to get values in C as described in the description pdf on the ftp server:
# ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/monthly/air_temperature_mean/
# DESCRIPTION_gridsgermany_monthly_air_temperature_mean_en.pdf
rasterHist <- rasterHist/10
rasterComp <- rasterComp/10

# Calculate mean temperature between 1961 and 1990
rasterHist_mean <- mean(rasterHist)

maxVal <- max(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)
minVal <- min(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)


p1 <- ggplot()+
  geom_stars(data = rasterHist_mean %>% st_as_stars())+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temperature", na.value = NA, limits=c(minVal,maxVal))+
  # , guide = F
  labs(x="",y="")+
  ggtitle("Mean Temperatures August 1881-2017")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")
p1


p2 <- ggplot()+
  geom_stars(data = rasterComp %>% st_as_stars())+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temperature", na.value = NA, limits=c(minVal,maxVal))+
  labs(x="",y="")+
  ggtitle("Temperature August 2018")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")

pdf("Out/DWD_script/August_mean_vs_2018.pdf", width = 14, height = 8)
grid.arrange(p1, p2, ncol=2)
dev.off()

# side-by-side plots, same height, just one legend

# Create Spatial Raster
st <- terra::rast(list(rasterHist_mean, rasterComp))
names(st) <- c("Mean Temperature August 1990 to 2017", "Temperature August 2018")

ggplot()+
  geom_spatraster(data = st)+
  facet_wrap(~lyr, ncol = 2)+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temperature", na.value = NA, limits=c(minVal,maxVal))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  labs(x="",y="")+
  ggtitle("Differences in Temperatures: August")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  xlab("")+
  ylab("")+
  coord_equal()

# compute difference of historical and raster to compare with
raster_diff <- rasterComp - rasterHist_mean

# Create Difference Map
p3 <- ggplot()+
  geom_stars(data = raster_diff %>% st_as_stars())+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="temp. diff.", na.value = NA)+
  labs(x="",y="")+
  ggtitle("Temperature Differences")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")


pdf("Out/DWD_script/August_mean_vs_2018_vs_diff.pdf", width = 20, height = 8)
grid.arrange(p1, p2, p3, ncol=3)
dev.off()



#################################
### Create a time Series plot ###
#################################

# Defide raster by 10 to get ?C values
# my_raster <- my_raster/10

# Define dataframe and fill it with the dates
my_years <- c(seq(1881, 2018, by=1))
my_mat <- matrix(data = NA, nrow = length(my_years), ncol = 2)
my_mat[,1] <- my_years
my_df <- data.frame(my_mat)
names(my_df) <- c("Year", "Mean_Temp")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years)){
  current_layer <- my_raster[[i]]
  current_mean <- mean(current_layer[,,1], na.rm=T)
  my_df[i,2] <- current_mean/10
  rm(current_layer, current_mean, i)
}

# optional: check data frame
# my_df

# Plot resulting dataframe and perform a regression analysis to display a trend line
pdf("Out/DWD_script/timeseries_mean_temp.pdf",width=15,height=8)
ggplot(my_df, aes(x=Year, y=Mean_Temp))+
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Mean Temperature Across Germany in August", 
       x="Year", y="Mean Temperature in °C") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# #########
# split by region and see what the differences are
# #########

# download boundary data
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
#rnaturalearth::ne_download(scale = 50, type = "GADM", country = "DEU")
bnd <- raster::getData("GADM", country='DEU', level=1)
#ne_countries(type = "GADM", country = "Germany", returnclass = "sf", type = "tiny_countries")

sf_bnd <- as(bnd, "sf")
bnd_trans <- st_transform(sf_bnd,31467)

masked <- mask(rasterHist_mean, bnd_trans[bnd_trans$NAME_1 == "Bayern",])

i = 1
bnd_trans$mean_val <- c()
for (b in 1:nrow(bnd_trans)){
  bdl <- bnd_trans$NAME_1[b]
  m <- mask(rasterHist_mean, bnd_trans[bnd_trans$NAME_1 == bdl,])
  bnd_trans$mean_val[b] <- mean(m[,,1], na.rm = T)
}
pdf("Out/DWD_script/Mean_per_Bundesland.pdf",width=15,height=8)
ggplot(data = bnd_trans, aes(fill = bnd_trans$mean_val))+
  geom_sf(color = NA)+
  scale_fill_gradientn("Mean Temperature [degC]", colours = brewer.pal(9, 'YlOrBr'))
dev.off()

# visual check

# crop and mask the data
bnd.utm <- spTransform(bnd, CRS(proj4string("+init=epsg:31467")))
bnd.utm.by <- bnd_trans[bnd_trans$NAME_1=="Bayern",]
my_raster.by <- crop(my_raster, bnd.utm.by)
my_raster.by <- mask(my_raster.by, bnd.utm.by)

# visual check
plot(my_raster.by,1)

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years)){
  current_layer <- my_raster.by[[i]]
  current_mean <- mean(current_layer[,,1], na.rm=T)
  my_df[i,2] <- current_mean/10
  rm(current_layer, current_mean, i)
}

# check data frame structure/content
my_df

# Plot resulting dataframe and perform a regression analysis to display a trend line
pdf("Out/DWD_script/timeseries_mean_temp_BY.pdf",width=15,height=8)
ggplot(my_df, aes(x=Year, y=Mean_Temp))+
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Mean Temperature Across Bavaria in August", 
       x="Year", y="Mean Temperature in °C") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()