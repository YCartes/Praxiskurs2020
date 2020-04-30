library(dplyr)
library(ggplot2)

setwd("C:/Users/felix/Desktop/Felix/R/Praxiskurs/Projekt")
  
        #polls<-read.csv("C:/Users/felix/Desktop/Felix/R/Praxiskurs/Projekt/Trump.csv",header=FALSE,sep=";",dec=".")
          #names(polls)<-c("Quelle","Datum","Stichprobe","Favorable","Unfavorable","Spread")
          #FOX News,CNN, CBS News
          #polls %>% filter(Quelle=="FOX News" |Quelle=="CNN" | Quelle=="CBS News" | Quelle=="Economist/YouGov") %>% group_by(Quelle) %>% summarize(Favorable=mean(Favorable,na.rm=TRUE))
          #anzahl_test<-polls %>% group_by(Quelle) %>% summarize(N=n())
          #anzahl_test
          #polls %>% filter(Quelle=="Economist/YouGov") %>% summarize(Fav=mean(Favorable,na.rm=TRUE))
          #polls %>% filter(Quelle=="FOX News" |Quelle=="CNN" | Quelle=="CBS News" | Quelle=="Economist/YouGov")

df<-read.csv("C:/Users/felix/Desktop/Felix/R/Praxiskurs/Projekt/approval_polllist.csv",header=TRUE,sep=",",dec=".")
         #df1<-read.csv("C:/Users/felix/Desktop/Felix/R/Praxiskurs/Projekt/approval_topline.csv",header=TRUE,sep=",",dec=".")
arrange(df %>% group_by(pollster) %>% summarize(N=n()),desc(N))

#filtern
df_filter<-df %>% filter(df$pollster=="Ipsos" | df$pollster=="YouGov" | df$pollster=="Rasmussen Reports/Pulse Opinion Research" | df$pollster=="CNN/SSRS" | df$pollster=="Fox News" | df$pollster=="NBC News/Wall Street Journal" | df$pollster=="ABC News/Washington Post" | df$pollster=="CBS News")
df_neu<-df_filter[c("subgroup","startdate","pollster","samplesize","population","approve","disapprove","adjusted_approve","adjusted_disapprove")]
df_neu$startdate<-as.Date(df_neu$startdate,"%m/%d/%Y")

save(df_neu, file="data.rda")

#Welche Zeiträume?
df_newsag<- df_neu %>% filter(df_neu$pollster=="ABC News/Washington Post"|df_neu$pollster=="CNN/SSRS"|df_neu$pollster=="Fox News"|df_neu$pollster=="NBC News/Wall Street Journal")


ggplot(df_newsag,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=60)+
  scale_fill_brewer(palette = "Set1")


df_marktforscher <- df_neu %>% filter(df_neu$pollster=="Ipsos"|df_neu$pollster=="Rasmussen Reports/Pulse Opinion Research"|df_neu$pollster=="YouGov")

ggplot(df_marktforscher,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=60)


#Ergebnis:CBS muss raus!
#Nächste Frage:optimaler Zeitraum?

df_neu<- df_neu %>% filter(df_neu$pollster!="CBS News")

#Zeitraum von 60 Tagen:
df_1<-subset(df_neu, startdate > "2017-01-20" & startdate < "2017-03-19" )
df_2<-subset(df_neu, startdate > "2017-03-20" & startdate < "2017-05-19" )
df_3<-subset(df_neu, startdate > "2017-05-20" & startdate < "2017-07-19" )
df_4<-subset(df_neu, startdate > "2017-07-20" & startdate < "2017-09-19" )
df_5<-subset(df_neu, startdate > "2017-09-20" & startdate < "2017-11-19" )
df_6<-subset(df_neu, startdate > "2017-11-20" & startdate < "2018-01-19" )
df_7<-subset(df_neu, startdate > "2018-01-20" & startdate < "2018-03-19" )
df_8<-subset(df_neu, startdate > "2018-03-20" & startdate < "2018-05-19" )
df_9<-subset(df_neu, startdate > "2018-05-20" & startdate < "2018-07-19" )
df_10<-subset(df_neu, startdate > "2018-07-20" & startdate < "2018-09-19" )
df_11<-subset(df_neu, startdate > "2018-09-20" & startdate < "2018-11-19" )
df_12<-subset(df_neu, startdate > "2018-11-20" & startdate < "2019-01-19" )
df_13<-subset(df_neu, startdate > "2019-01-20" & startdate < "2019-03-19" )
df_14<-subset(df_neu, startdate > "2019-03-20" & startdate < "2019-05-19" )
df_15<-subset(df_neu, startdate > "2019-05-20" & startdate < "2019-07-19" )
df_16<-subset(df_neu, startdate > "2019-07-20" & startdate < "2019-09-19" )
df_17<-subset(df_neu, startdate > "2019-09-20" & startdate < "2019-11-19" )
df_18<-subset(df_neu, startdate > "2019-11-20" & startdate < "2020-01-19" )
df_19<-subset(df_neu, startdate > "2020-01-20" & startdate < "2020-03-19" )
df_20<-subset(df_neu, startdate > "2020-03-20" & startdate < "2020-05-19" )



df__1<-subset(df_newsag, startdate > "2017-01-20" & startdate < "2017-03-19" )
df__2<-subset(df_newsag, startdate > "2017-03-20" & startdate < "2017-05-19" )
df__3<-subset(df_newsag, startdate > "2017-05-20" & startdate < "2017-07-19" )
df__4<-subset(df_newsag, startdate > "2017-07-20" & startdate < "2017-09-19" )
df__5<-subset(df_newsag, startdate > "2017-09-20" & startdate < "2017-11-19" )
df__6<-subset(df_newsag, startdate > "2017-11-20" & startdate < "2018-01-19" )
df__7<-subset(df_newsag, startdate > "2018-01-20" & startdate < "2018-03-19" )
df__8<-subset(df_newsag, startdate > "2018-03-20" & startdate < "2018-05-19" )
df__9<-subset(df_newsag, startdate > "2018-05-20" & startdate < "2018-07-19" )
df__10<-subset(df_newsag, startdate > "2018-07-20" & startdate < "2018-09-19" )
df__11<-subset(df_newsag, startdate > "2018-09-20" & startdate < "2018-11-19" )
df__12<-subset(df_newsag, startdate > "2018-11-20" & startdate < "2019-01-19" )
df__13<-subset(df_newsag, startdate > "2019-01-20" & startdate < "2019-03-19" )
df__14<-subset(df_newsag, startdate > "2019-03-20" & startdate < "2019-05-19" )
df__15<-subset(df_newsag, startdate > "2019-05-20" & startdate < "2019-07-19" )
df__16<-subset(df_newsag, startdate > "2019-07-20" & startdate < "2019-09-19" )
df__17<-subset(df_newsag, startdate > "2019-09-20" & startdate < "2019-11-19" )
df__18<-subset(df_newsag, startdate > "2019-11-20" & startdate < "2020-01-19" )
df__19<-subset(df_newsag, startdate > "2020-01-20" & startdate < "2020-03-19" )
df__20<-subset(df_newsag, startdate > "2020-03-20" & startdate < "2020-05-19" )


#20 nicht benutzen!!!
rm(df_20)
rm(df__20)


#nochmal überprüfen, ob gleich viele Werte in den Intervallen
ggplot(df__1,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=5)

ggplot(df__2,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=5)

ggplot(df__3,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=5)

ggplot(df__4,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=5)

ggplot(df__5,aes(x=startdate,fill=pollster))+
  geom_histogram(binwidth=5)


#Deskriptive Statistik

mean<-rep(0,19)

for i in 1:19 {
  mean(i)<-mean
}
mean(df_11$approve)
mean(filter(df_1, df_1$pollster=="Fox News")$approve)

#https://www.adfontesmedia.com/interactive-media-bias-chart/?v=402f03a963ba
#https://stat.ethz.ch/R-manual/R-devel/RHOME/library/base/html/get.html
