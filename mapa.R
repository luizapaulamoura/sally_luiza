# Covid por bairro
install.packages("lealeft")

library(gridExtra)
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(ggthemes)
library(ggplot2)
library(lealeft)


#dados de 2020
df <- read_csv("dados/covid_bairro - covid_bairro.csv")

view(df)

df$LATITUDE[45] <- -11.865360

df$LATITUDE <- as.numeric(df$LATITUDE)
df$LONGITUDE <- as.numeric(df$LONGITUDE)

str(df)

df %>%
  drop_na(Latitude, Longitude)%>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-38.4368023,-12.9144042,10) %>%
  addCircles(lng=~LONGITUDE,
             lat=~LATITUDE, popup = ~`CASOS_CONFIRMADOS`)
