library(dplyr)

Data <- read.csv('Projektdaten.csv',sep = ";")
Data <- as_tibble(Data)


Approval <- Data %>% select(c(1,seq(2,76,2)))
names(Approval)
Approval <- Approval[1:52,]
Disapproval <- Data %>% select(c(seq(1,77,2)))
names(Disapproval) <- names(Approval)
Disapproval <- Disapproval[1:52,]

# Varianz

VarApp <- apply(Approval[-1,-1],1,var)
VarDisapp <- apply(Disapproval[-1,-1],1,var)
States <- Approval[2:52,1]



StatesApp <- data.frame(States,VarApp)
colnames(StatesApp) <- c("Staat","Varianz")
StatesDisapp <- data.frame(States,VarDisapp)
colnames(StatesDisapp) <- c("Staat","Varianz")
StatesApp <- as_tibble(StatesApp)
StatesDisapp <- as_tibble(StatesDisapp)

StatesAppSortMax <- StatesApp %>% arrange(desc(Varianz))
StatesDisappSortMax <- StatesDisapp %>% arrange(desc(Varianz))

StatesAppSortMin <- StatesApp %>% arrange(Varianz)
StatesDisappSortMin <- StatesDisapp %>% arrange(Varianz)


# Differenz

MaxApp <- as.numeric(apply(Approval[-1,-1],1,max))
MinApp <- as.numeric(apply(Approval[-1,-1],1,min))
DifferenzApp <- MaxApp - MinApp
StatesAppDiff <- data.frame(States,DifferenzApp)
colnames(StatesAppDiff) <- c("Staat","Differenz")
StatesAppDiff <- as_tibble(StatesAppDiff)
StatesAppDiffSorted <- StatesAppDiff %>% arrange(desc(Differenz))


#SwingStates

StatesAppDiffSorted %>% filter(StatesAppDiffSorted$Staat %in% c("Colorado","Florida","Iowa","Michigan","Minnesota","Nevada","New Hampshire","North Carolina","Ohio","Pennsylvania","Virginia","Wisconsin"))
StatesAppVarMax <- StatesAppSortMax
StatesAppVarMin <- StatesAppSortMin
StatesAppVarMax %>% filter(StatesAppVarMax$Staat %in% c("Colorado","Florida","Iowa","Michigan","Minnesota","Nevada","New Hampshire","North Carolina","Ohio","Pennsylvania","Virginia","Wisconsin"))

# sichern
write.csv(Approval,"Approval.csv")
write.csv(Disapproval,"Disapproval.csv")
write.csv(StatesAppVarMax, "StatesAppVarMax.csv")
StatesAppVarMin <- StatesAppSortMin
write.csv(StatesAppVarMin, "StatesAppVarMin.csv")
write.csv(StatesAppDiffSorted, "StatesAppDiffSorted.csv")
