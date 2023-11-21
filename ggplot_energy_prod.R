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

plot(energy_prod$DateTime, energy_prod$ActualConsumption)

boxplot(energy_prod$DateTime, energy_prod$ActualConsumption)
barplot(energy_prod$DateTime, energy_prod$ActualConsumption)
hist

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

df_agg_type <- df_agg_type[order(df_agg_type$InstalledGenCapacity_sum),]
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
