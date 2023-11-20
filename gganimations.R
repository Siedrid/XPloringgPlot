library(RCurl)
library(transformr)
library(ggplot2)
library(gganimate)

setwd("~/Eagle/Intro_to_Programming/ggplot/Data/")
df <- read.csv(file = "EAGLE_course_ggplot - Form responses 1.csv")

df1 <- data.frame(df[,c(2:6,10,14)], semester = 1, courses = 11)
df2 <- data.frame(df[, c(2:5, 7, 11,15)], semester = 2, courses = 15)
df3 <- data.frame(df[,c(2:5, 8, 12, 16)], semester = 3, courses = 2)
df4 <- data.frame(df[,c(2:5, 9, 13, 17)], semester = 4, courses = 3)

df.names <- c("eyecol", "haircol", "glasses", "sex", "eoExp", "progExp", "praesExp", "semester")

names(df1) <- df.names
names(df2) <- df.names
names(df3) <- df.names
names(df4) <- df.names

df <- rbind(df1,df2,df3,df4)

ggplot(data = df, aes(y=X2003..YR2003., x = X2004..YR2004., color = Country.Name))+
  geom_point()

p <- ggplot(data = df, aes(y=eoExp, x = progExp, color = eyecol, size = sex))+
  geom_point(alpha=.8)

p + transition_time(semester)+
  ease_aes('linear')+
  shadow_wake(wake_length =0.1, alpha = F)+
  enter_fade()+
  exit_fade()


library(tidyverse)
africa_df <- read.csv(file = "P_Data_Extract_From_Gender_Statistics/5fec3b22-d19c-446c-bcf6-b4106e7ee00d_Data.csv")
df_drugs_fem <- africa_df[africa_df$Series.Code == "SH.HIV.ARTC.FE.ZS",]

df_drugs_fem <- df_drugs_fem[c(3:length(df_drugs_fem))]
df_long <- gather(df_drugs_fem, key = "key", value = "value", -Country.Name, -Country.Code)
get_years <- sapply(strsplit(df_long$key, ".", fixed = T), function(x) x[1])
df_long$key <- gsub("X", "", get_years)
df_long$key <- as.numeric(df_long$key)
head(df_long)

df_long$key <- for (i in 1:length(df_long$key)){
  gsub("X", "", strsplit(df_long$key[i], ".", fixed = T)[[1]][1])}

df_long$value <- as.numeric(df_long$value)

g <- ggplot(data=df_long, aes(x = Country.Code, y = value, color =Country.Code))+
  geom_bar(stat = 'identity')

g + transition_time(key)+
  ease_aes('linear')
g + transition_states(
  frame,
  transition_length = 2,
  state_length = 1
) +
  ease_aes('sine-in-out')  

