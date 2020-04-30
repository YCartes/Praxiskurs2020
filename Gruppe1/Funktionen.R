adp <- function(x){
  x <- x[1:72]
  # Daten im Format (Approve, Disapprove, Approve, ...), daher interessieren wir uns nur für jeden 2ten Wert
  x <- x[seq(1,length(x),2)]
  # Daten in monatlicher Angabe, daher Durchschnitt von jeweils drei Monaten gesucht
  x <- .colMeans(x, 3, length(x) / 3)
  return(x)
}

GDP_data_prep <- function(states){
  states_df <- df %>% filter(GeoName %in% states)
  states_df <- as.data.frame(t(matrix(as.numeric(t(t(states_df)))[6:65],5, 12)))
  states_df$Date <- time
  states_df <- states_df[, c(6,1,2,3,4,5)]
  names(states_df)[2:6] <- paste(states, "_GDP", sep = "")
  return(states_df)
}




