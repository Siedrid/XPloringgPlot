library(sf)
library(ggplot2)
library(RColorBrewer)

setwd("~/Eagle/Intro_to_Programming/ggplot/Data/")
df <- read.csv(file = "P_Data_Extract_From_Gender_Statistics/5fec3b22-d19c-446c-bcf6-b4106e7ee00d_Data.csv")

# Read Shapefile of Africa
africa_shp <- "Africa_countries.gpkg"
africa <- sf::st_read(africa_shp)

df_savetravel <- df[df$Series.Code == df$Series.Code[1],]
df_savetravel <- df_savetravel[, 3:length(df_savetravel)]
head(df_savetravel)

# Change Country names, which are not spelled the same in both datasets
df_savetravel$Country.Name[!df_savetravel$Country.Name %in% africa$NAME_LONG] <- 
  c("eSwatini", "Democratic Republic of the Congo", "Republic of the Congo", "Egypt", "The Gambia", "Côte d'Ivoire", "São Tomé and Principe")
# Merge two Dataframes
merged_df <- merge(africa, df_savetravel, by.x = "NAME_LONG", by.y = "Country.Name")

sel_df <- merged_df[c(1, length(merged_df)-1, length(merged_df))]

y <- gsub("X", "", strsplit(colnames(sel_df)[2], ".", fixed = T)[[1]][1])

ggplot(data = sel_df$geometry, aes(fill = sel_df$X2021..YR2021.))+
  geom_sf()+
  scale_fill_manual(values = c("0" = '#E26928', "1" = '#24789D'), 
                    labels = c('No', 'Yes'), name = '')+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle(y, subtitle = 'A woman can travel the same way outside her home as a man.')+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", 
                                        linewidth = 0.5), panel.background = element_rect(fill = "white"),
        legend.position = "right", legend.key.width = unit(0.1, "inches"))

sel_df <- merged_df[c(1, 16:35)]

# Acess to Drugs ----

df_drugs_fem <- df[df$Series.Code == "SH.HIV.ARTC.FE.ZS",]

df_drugs_fem$Country.Name[!df_drugs_fem$Country.Name %in% africa$NAME_LONG] <- 
  c("eSwatini", "Democratic Republic of the Congo", "Republic of the Congo", "Egypt", "The Gambia", "Côte d'Ivoire", "São Tomé and Principe")

merged_df_fem <- merge(africa, df_drugs_fem, by.x = "NAME_LONG", by.y = "Country.Name")
df_drugs_fem <- merged_df_fem[c(1, 18:length(merged_df_fem))]

# function of colnames
plot_data <- function(df, year_col){
  y_str <- gsub("X", "", strsplit(year_col, ".", fixed = T)[[1]][1])
  ggplot(data = df, aes(fill = as.integer(df[[year_col]])))+
    geom_sf()+
    scale_fill_gradientn(name = 'Precent', colours = brewer.pal(9, 'Blues'), na.value = NA, limits = c(0,100))+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle(y_str, subtitle = 'Access to drugs by Females')+
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", 
                                          linewidth = 0.5), panel.background = element_rect(fill = "white"),
          legend.position = "right", legend.key.width = unit(0.1, "inches"))+
    geom_sf_label(aes(label = df[[year_col]]))
}

my_plots <- lapply(colnames(df_drugs_fem)[2:length(df_drugs_fem)-1], plot_data, df = df_drugs_fem)

my_plots[[10]]

# Males
df_drugs_male <- df[df$Series.Code == "SH.HIV.ARTC.MA.ZS",]

df_drugs_male$Country.Name[!df_drugs_male$Country.Name %in% africa$NAME_LONG] <- 
  c("eSwatini", "Democratic Republic of the Congo", "Republic of the Congo", "Egypt", "The Gambia", "Côte d'Ivoire", "São Tomé and Principe")

merged_df_male <- merge(africa, df_drugs_male, by.x = "NAME_LONG", by.y = "Country.Name")
df_drugs_male <- merged_df[c(1, 18:length(merged_df_male))]
