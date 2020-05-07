library(tidyverse)
library(ggplot2)
library(GGally)
setwd("/Users/nils-frederikschulze/Desktop/R-Projekt")
load(file = "EnvGroup1")

#_________________Funktionen_________________
#____________________________________________
adp <- function(x){
  x <- x[1:72]
  # Daten im Format (Approve, Disapprove, Approve, ...), daher interessieren wir uns nur für jeden 2ten Wert
  x <- x[seq(1,length(x),2)]
  # Daten in monatlicher Angabe, Durchschnitt von jeweils drei Monaten gesucht (Quartalsdurchschnitt)
  x <- .colMeans(x, 3, length(x) / 3)
  return(x)
}

GDP_data_prep <- function(states){
  states_df <- df %>% filter(GeoName %in% states)
  states_df <- as.data.frame(t(matrix(as.numeric(t(t(states_df)))[6:65],5, 12)))
  states_df$Date <- time
  states_df <- states_df[,c(6,1,2,3,4,5)]
  names(states_df)[2:6] <- paste(states, "_GDP", sep = "")
  return(states_df)
}

#_________________Approval-Data_________________
#_______________________________________________
# red states 
Alabama <- c(62, 26, 65, 29, 62, 32, 63, 33, 56, 38, 54, 41, 58, 38, 56, 38, 59, 36, 60, 35, 60, 35, 60, 35, 63, 33, 61, 35, 63, 33, 62, 34, 63, 33, 63, 33, 63, 33, 63, 33, 62, 34, 62, 34, 62, 34, 58, 37, 58, 38, 61, 35, 61, 35, 61, 35, 61, 34, 60, 36, 61, 35, 60, 36, 59, 37, 59, 38, 59, 37, 59, 37, 60, 37, 62, 34)
Arkansas <- c(59, 29, 59, 34, 60, 35, 56, 39, 54, 41, 53, 41, 55, 39, 54, 40, 54, 40, 54, 41, 51, 43, 53, 42, 53, 40, 52, 40, 55, 40, 54, 42, 54, 41, 55, 41, 55, 41, 56, 40, 54, 42, 55, 40, 55, 41, 53, 42, 53, 43, 51, 44, 52, 44, 53, 43, 52, 44, 51, 45, 53, 44, 53, 44, 50, 46, 51, 45, 52, 44, 53, 43, 55, 42, 54, 43) 
Kansas <- c(56, 32, 56, 38, 59, 35, 57, 38, 54, 41, 54, 41, 51, 44, 47, 47, 50, 45, 50, 45, 48, 47, 47, 48, 50, 45, 51, 45, 48, 47, 52, 44, 52, 44, 51, 45, 53, 43, 52, 45, 50, 46, 52, 44, 50, 46, 49, 47, 49, 48, 51, 45, 52, 44, 49, 47, 48, 47, 50, 47, 51, 46, 50, 47, 50, 46, 51, 45, 50, 47, 52, 45, 52, 45, 51, 47)
Oklahoma <- c(61, 27, 61, 33, 64, 31, 61, 32, 58, 37, 58, 37, 55, 40, 54, 40, 56, 39, 55, 39, 55, 40, 54, 41, 55, 40, 55, 40, 53, 43, 53, 43, 53, 42, 56, 40, 55, 41, 53, 44, 57, 39, 56, 40, 56, 39, 54, 41, 53, 43, 52, 44, 53, 43, 54, 43, 52, 43, 54, 42, 56, 40, 55, 41, 54, 43, 52, 44, 53, 44, 52, 44, 56, 41, 56, 42)
Texas <- c(54, 34, 56, 37, 59, 36, 56, 40, 54, 41, 51, 44, 52, 44, 49, 45, 51, 44, 48, 46, 48, 47, 49, 46, 51, 44, 51, 44, 50, 46, 51, 45, 50, 45, 53, 43, 52, 44, 50, 45, 51, 45, 51, 44, 51, 44, 49, 45, 48, 48, 49, 46, 50, 46, 50, 46, 49, 46, 50, 46, 51, 45, 50, 46, 49, 47, 49, 47, 49, 46, 48, 48)

# blue states
California <- c(42, 48, 43, 51, 45, 50, 44, 52, 41, 54, 40, 54, 38, 55, 33, 56, 36, 58, 34, 61, 35, 60, 35, 60, 36, 59, 34, 61, 34, 62, 35, 61, 37, 58, 37, 59, 36, 59, 35, 61, 35, 61, 36, 60, 35, 61, 33, 62, 33, 63, 33, 63, 34, 62, 34, 63, 34, 62, 33, 62, 33, 63, 33, 63, 34, 62, 32, 64, 34, 62, 34, 62, 33, 63, 34, 62)
Illinois <- c(49, 40, 47, 46, 46, 47, 53, 42, 45, 51, 39, 56, 38, 57, 31, 54, 37, 58, 38, 58, 35, 59, 36, 59, 37, 58, 38, 59, 36, 60, 37, 59, 37, 59, 38, 58, 38, 58, 38, 58, 37, 59, 38, 57, 38, 58, 37, 58, 37, 60, 38, 58, 37, 58, 37, 59, 38, 58, 38, 58, 39, 57, 38, 59, 37, 59, 36, 60, 38, 58, 37, 59, 39, 58, 39, 57)
Massachusetts <- c(43, 47, 42, 52, 42, 54, 40, 56, 37, 59, 35, 60, 33, 63, 31, 64, 32, 63, 32, 64, 32, 64, 32, 64, 32, 64, 34, 63, 33, 64, 32, 65, 35, 61, 36, 61, 35, 62, 36, 61, 35, 62, 35, 61, 36, 60, 33, 62, 33, 64, 33, 63, 34, 63, 34, 62, 35, 62, 35, 62, 34, 64, 33, 64, 33, 64, 32, 65, 34, 63, 33, 64, 34, 63, 36, 62)
New_Jersey <- c(46, 44, 47, 47, 47, 48, 45, 51, 43, 54, 40, 54, 39, 57, 38, 57, 38, 57, 39, 57, 38, 58, 39, 57, 40, 56, 41, 55, 38, 58, 41, 55, 40, 56, 42, 54, 42, 54, 41, 55, 39, 57, 41, 55, 42, 55, 39, 57, 39, 58, 40, 56, 40, 56, 40, 57, 39, 57, 41, 56, 41, 56, 39, 58, 40, 57, 40, 57, 41, 56, 41, 56, 42, 55, 42, 54)
New_York <- c(49, 41, 49, 45, 50, 45, 51, 45, 45, 51, 42, 54, 42, 53, 35, 59, 37, 58, 36, 60, 38, 58, 38, 58, 39, 57, 37, 59, 36, 60, 37, 59, 37, 58, 38, 58, 38, 58, 39, 57, 37, 59, 38, 58, 37, 59, 36, 59, 36, 60, 37, 59, 37, 59, 36, 60, 37, 59, 36, 61, 36, 60, 36, 61, 37, 60, 36, 61, 36, 61, 36, 61, 37, 59, 38, 58)

# swing states
Florida <- c(56 ,  34 ,  56 ,  38 ,  56 ,  39 ,  56 ,  40 ,  51 ,  44 ,  48 ,  47 ,  47 ,  49 ,  47 ,  48 ,  48 ,  47 ,  49 ,  47 ,  47 ,  48 ,  49 ,  47 ,  50 ,  45 ,  49 ,  47 ,  48 ,  48 ,  48 ,  48 ,  50 ,  45 ,  51 ,  45 ,  50 ,  46 ,  49 ,  47 ,  49 ,  47 ,  49 ,  47 ,  49 ,  47 ,  46 ,  49 ,  46 ,  50 ,  47 ,  49 ,  47 ,  49 ,  47 ,  49 ,  48 ,  48 ,  47 ,  50 ,  48 ,  49 ,  48 ,  48 ,  48 ,  49 ,  47 ,  49 ,  49 ,  48 ,  48 ,  49 ,  49 ,  48 ,  50 ,  47 )
Michigan <- c(48 ,  40 ,  49 ,  44 ,  50 ,  44 ,  46 ,  49 ,  45 ,  50 ,  40 ,  55 ,  43 ,  53 ,  39 ,  55 ,  40 ,  55 ,  40 ,  55 ,  40 ,  54 ,  41 ,  53 ,  42 ,  52 ,  43 ,  53 ,  41 ,  55 ,  42 ,  54 ,  43 ,  52 ,  44 ,  52 ,  42 ,  53 ,  42 ,  53 ,  41 ,  54 ,  42 ,  53 ,  43 ,  52 ,  41 ,  53 ,  40 ,  55 ,  40 ,  55 ,  42 ,  53 ,  43 ,  53 ,  42 ,  54 ,  40 ,  55 ,  42 ,  54 ,  42 ,  55 ,  43 ,  53 ,  42 ,  55 ,  41 ,  55 ,  40 ,  55 ,  42 ,  54 ,  43 ,  53)
Nevada <- c(49 ,  39 ,  55 ,  38 ,  52 ,  43 ,  51 ,  45 ,  51 ,  44 ,  47 ,  48 ,  48 ,  48 ,  44 ,  50 ,  44 ,  51 ,  42 ,  54 ,  44 ,  51 ,  44 ,  51 ,  47 ,  48 ,  44 ,  52 ,  45 ,  51 ,  45 ,  50 ,  47 ,  49 ,  44 ,  52 ,  45 ,  51 ,  46 ,  50 ,  44 ,  52 ,  44 ,  51 ,  45 ,  50 ,  42 ,  53 ,  42 ,  55 ,  43 ,  53 ,  43 ,  53 ,  45 ,  51 ,  42 ,  53 ,  43 ,  53 ,  43 ,  54 ,  42 ,  55 ,  43 ,  54 ,  41 ,  55 ,  45 ,  52 ,  42 ,  54 ,  41 ,  56 ,  43 ,  54) 
Ohio <- c(51 ,  37 ,  51 ,  42 ,  53 ,  42 ,  53 ,  43 ,  46 ,  48 ,  47 ,  48 ,  48 ,  48 ,  43 ,  51 ,  46 ,  49 ,  44 ,  51 ,  44 ,  51 ,  44 ,  52 ,  46 ,  50 ,  49 ,  47 ,  46 ,  50 ,  45 ,  51 ,  46 ,  50 ,  47 ,  49 ,  48 ,  48 ,  45 ,  50 ,  46 ,  49 ,  48 ,  47 ,  48 ,  48 ,  46 ,  49 ,  45 ,  51 ,  45 ,  50 ,  45 ,  51 ,  46 ,  50 ,  46 ,  50 ,  45 ,  51 ,  45 ,  51 ,  46 ,  51 ,  46 ,  51 ,  46 ,  51 ,  46 ,  51 ,  46 ,  50 ,  47 ,  49 ,  48 ,  49 )
Pennsylvania <- c(49 ,  39 ,  51 ,  43 ,  51 ,  43 ,  51 ,  44 ,  48 ,  48 ,  45 ,  49 ,  46 ,  50 ,  42 ,  52 ,  45 ,  51 ,  44 ,  51 ,  43 ,  52 ,  45 ,  51 ,  46 ,  49 ,  46 ,  50 ,  44 ,  52 ,  45 ,  51 ,  46 ,  50 ,  46 ,  51 ,  45 ,  51 ,  45 ,  51 ,  44 ,  52 ,  45 ,  51 ,  46 ,  51 ,  45 ,  51 ,  43 ,  53 ,  45 ,  52 ,  45 ,  52 ,  45 ,  52 ,  45 ,  52 ,  44 ,  53 ,  45 ,  52 ,  44 ,  53 ,  45 ,  52 ,  44 ,  53 ,  45 ,  52 ,  46 ,  51 ,  48 ,  49 ,  48 ,  49)


#_________________GDP-Data-Preperation________________
#_____________________________________________________
df <- as_tibble(read.csv("StateGDP.csv"))

red_states <- sort(c("Texas", "Alabama", "Oklahoma", "Kansas", "Arkansas"))
blue_states <- sort(c("California", "New York", "Massachusetts", "Illinois","New Jersey"))
swing_states <- sort(c("Florida", "Nevada", "Ohio", "Michigan", "Pennsylvania"))
all_states <- c(red_states, blue_states, swing_states)


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
# alle Staaten zusammengefasst nach GDP von 2017Q1 bis 2019Q4 
all_State_GDP <- merge(red_GDP_df, merge(blue_GDP_df, swing_GDP_df))



#________________Approval-Data-Preperation________________
#_________________________________________________________
red_Appr_df <- data_frame(time, adp(Alabama), adp(Arkansas), adp(Kansas), adp(Oklahoma), adp(Texas))
names(red_Appr_df) <- c("Date", red_states)
blue_Appr_df <- data_frame(time, adp(California), adp(Illinois), adp(Massachusetts), adp(New_Jersey), adp(New_York))
names(blue_Appr_df) <- c("Date", blue_states)
swing_Appr_df <- data_frame(time, adp(Florida), adp(Michigan), adp(Nevada), adp(Ohio), adp(Pennsylvania))
names(swing_Appr_df) <- c("Date", swing_states)
all_State_Approval <- merge(red_Appr_df, merge(blue_Appr_df, swing_Appr_df))


#________________Zusammenfuegen von GDP und Approval Daten________________
#_________________________________________________________________________
temp3 <- merge(all_State_GDP, all_State_Approval)
temp4 <- names(temp3)
temp4 <- temp4[2:31]
f <- temp3[temp4]
f <- cor(f)[16:30,1:15]
state_cor <-diag(f)
attribute <-  c("red","red","red","red","red","blue","blue","blue","blue","blue", "swing","swing","swing","swing","swing")
state_cor_df <- data_frame(all_states, state_cor, attribute)
names(state_cor_df) <- c("State", "Correlation", "Group")


#________________Analyse________________
#_______________________________________

# Barplot Korrelationen
States <- factor(state_cor_df$State, levels = c(red_states, blue_states, swing_states))
ggplot(data = state_cor_df, aes(x = States, y = Correlation, fill = Group)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual("State", values = c("red" = "darkred", "blue" = "steelblue", "swing" = "#9999CC")) + coord_flip() + xlab("")


# GDP Heatmap Plot
GDP_Matrix <- melt(all_State_GDP[names(all_State_GDP)[2:16]])
GDP_Matrix$Date <- all_State_GDP$Date
names(GDP_Matrix) <- c("State", "GDP", "Date")
ggplot(data = GDP_Matrix, aes(x=State, y=Date, fill=GDP)) + geom_tile()


save.image(file = "EnvGroup1")
