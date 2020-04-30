# data preperation
library(tidyverse)
library(ggplot2)
library(GGally)

setwd("/Users/nils-frederikschulze/Desktop/R-Projekt")
df <- read.csv("StateGDP.csv")
df <- as_tibble(df)

#GDP Information exthrahieren
df <- df %>% filter(Description == "Real GDP (millions of chained 2012 dollars)")
#relevanten Zeitraum extrahieren
relevantVar <- c("GeoName", "X2017.Q1", "X2017.Q2", "X2017.Q3", "X2017.Q4","X2018.Q1", "X2018.Q2", "X2018.Q3", "X2018.Q4", "X2019.Q1", "X2019.Q2", "X2019.Q3", "X2019.Q4")
df <- df[relevantVar]

# Date-Var fuer neuen Dataframe
time <- c("31.03.2017", "30.06.2017", "30.09.2017", "31.12.2017", "31.03.2018", "30.06.2018", "30.09.2018", "31.12.2018", "31.03.2019", "30.06.2019", "30.09.2019", "31.12.2019")
time <- as.Date(time, format = "%d.%m.%Y")


# red states
# Auswahl festlegen
red_states <- sort(c("Texas", "Alabama", "Oklahoma", "Kansas", "Arkansas"))
blue_states <- sort(c("California", "New York", "Massachusetts", "Illinois","New Jersey"))
swing_states <- sort(c("Florida", "Nevada", "Ohio", "Michigan", "Pennsylvania"))


# Subset "blue states"
blue_GDP_df <- GDP_data_prep(blue_states)

# Subset "red states"
red_GDP_df <- GDP_data_prep(red_states)

# Subset "swing states"
swing_GDP_df <- GDP_data_prep(swing_states)

all_states <- merge(merge(blue_GDP_df, red_GDP_df), swing_GDP_df)
test <- merge(red_GDP_df, temp)
