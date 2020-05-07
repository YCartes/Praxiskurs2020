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

# Subset "blue states"
blue_GDP_df <- GDP_data_prep(blue_states)

# Subset "red states"
red_GDP_df <- GDP_data_prep(red_states)

# Subset "swing states"
swing_GDP_df <- GDP_data_prep(swing_states)

temp1 <- merge(red_GDP_df, merge(blue_GDP_df, swing_GDP_df))


#Grafiken
temp3 <- merge(temp1, temp2)
temp4 <- names(temp3)
temp4 <- temp4[2:31]
f <- temp3[temp4]
f <- cor(f)[16:30,1:15]
state_cor <-diag(f)
attribute <-  c("red","red","red","red","red","blue","blue","blue","blue","blue", "swing","swing","swing","swing","swing")
state_cor_df <- data_frame(all_states, state_cor, attribute)
names(state_cor_df) <- c("State", "Cor", "Group")
state_cor_df$Group <- factor(state_cor_df$Group, levels = c('red', 'blue', 'swing'))
ggplot(data = state_cor_df, aes(x = all_states, y = state_cor, fill = Group)) + geom_bar(stat= "identity", position = "dodge") + scale_fill_manual("Legend", values = c("red" = "darkred", "blue" = "steelblue", "swing" = "purple"))
