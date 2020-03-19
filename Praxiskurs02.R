# Praxiskurs Statistik mit R
# 02. Datenaufbereitung & Descriptive Statistik
# 05.03.2020

# Lernziele:
# Daten einlesen (fortgeschritten)
# Struktur und Typen der Daten untersuchen
# Daten aufbereiten
# Grundlegende statistische Eigenschaften der Daten explorieren
# Einfache Grafiken


# ----------------------Loesungsvorschlag fuer Blatt 02------------------------
# Code wird thematisch geordnet, Reihenfolge entspricht NICHT der der Aufgaben


# Daten einlesen

# Aufgabe 1.1 und 1.2 siehe Loesungsvorschlag fuer Blatt 01
silber <- read.table(file="silber.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(silber)
oel <- read.table(file="oel.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(oel)
kaffee <- read.table(file="kaffee.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(kaffee)
milch <- read.table(file="milch.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(milch)
osaft <- read.table(file="osaft.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(osaft)

gold <- read.table(file="gold.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(gold)
kakao <- read.table(file="kakao.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE)
str(kakao)


# Was man an der Ausgabe von str() ablesen kann

str(eisenerz)
# 5 Variablen, automatisch benannt als "V1",..., "V5" (Systemsprache Englisch)
# 431 Faelle
# Erste Spalte ist von der Klasse "factor", die anderen vier "numeric"
# Die Rohstoffdatensaetze haben unterschiedlich viele Faelle!
# Die Preise in Datensaetzen "gold" und "kakao" werden als factors eingelesen, weil die Werte einen Punkt als Tausendertrennzeichen enthalten!


# Ueber die Klasse "factor"

# Die Klasse "factor" ist fuer (ungeordnete oder geordnete) kategoriale Daten (z.B. Geschlecht, Schulnote,...) gedacht
# Die gesamte Spalte V1 ist ein Faktor und jede einzigartige Auspraegung ist ein factor level, hier ist also jeder Eintrag ein factor level, da sie sich nicht ueberlappen (vgl. Stock.Levels in books)
# Alle factor levels ermitteln
levels(eisenerz$V1)
# Anzahl der levels
nlevels(eisenerz$V1)
# Die Zahlen nach ":" sind die Indizes der levels in dem Vektor, den levels() zurueckgibt. Ein Beispiel:
bsp <- data.frame(obst=c("orange", "birne", "apfel", "banane"),
                  herkunft=c("spanien", "deutschland", "deutschland", "gahna"),
                  preis=c(2.99, 1.79, 1.99, 3.19))
levels(bsp$obst)
levels(bsp$herkunft)


# Zeilen und Spalten selegieren

# 10. bis 20. Zeilen selegieren
eisenerz[10:20,]
# 1. und 2. Spalte selegieren
eisenerz[,1:2]
# oder mit Namen
eisenerz[,c("V1", "V2")]
# Abrufen funktioniert wie bei Listen, vgl.
class(eisenerz[1])
class(eisenerz[[1]])
class(eisenerz[2])
class(eisenerz[[2]])


# Spalten/Merkmale umbenennen und factor levels umkodieren

# Spalten umbenennen
names(eisenerz)[1:2] <- c("Datum", "Eisenerzpreise")
str(eisenerz)
# Factor levels umkodieren
levels(bsp$herkunft) <- c("D", "G", "S")
bsp


# Komplexere Transformation von factors

# Eine Funktion schreiben, die character in numric umwandeln, wobei die Trennzeichen korrigiert werden 
transf <- function(x) as.numeric(gsub("\\,", "\\.",gsub("\\.", "", x)))
# Die Funktion auf all Spalten bis auf die erste anwenden 
gold.num <- data.frame(apply(gold[,2:5], MARGIN=2, FUN=transf))
# Das Datum in ein von R auswertbares Standardformat umwandeln
datum <- as.Date(gold$V1, format="%d.%m.%Y")
# Transformierte Daten zusammenfuegen
gold.num <- cbind(datum, gold.num)
# Uebepruefen, ob die Transformation stimmt
str(gold.num)
gold.num[289:324,]
gold[289:324,]
# Das Gleiche auf die Kakao-Daten durchfuehren
kakao.num <- data.frame(apply(kakao[,2:5], MARGIN=2, FUN=transf))
datum <- as.Date(kakao$V1, format="%d.%m.%Y")
kakao.num <- cbind(datum, kakao.num)
str(kakao.num)
kakao.num[156:168,]
kakao[156:168,]


# Deskriptive Statistik

# Nuetzliche R build-in Funktionen
mean
median
var
sd
max
min
rang
quantile(x= , probs= )
IQR # interquartile range
mad(x= , constant= ) # median absolute deviation
# Bitte beachtet, dass NA (Not Available; ~ missing value) per Default mitgerechnet wird. Um NAs auszuschliessen, muss das Argument na.rm auf TRUE gestellt wird. Vgl. die Ergebnisse von
apply(kakao.num[,2:5], MARGIN=2, FUN=mean)
apply(kakao.num[,2:5], MARGIN=2, FUN=mean, na.rm=TRUE)
apply(kakao.num[,2:5], MARGIN=2, FUN=var)
apply(kakao.num[,2:5], MARGIN=2, FUN=var, na.rm=TRUE)

# Einfachste Art, bestimmte Kenngroessen von allen Merkmalen auf einmal zu ermitteln
summary(kakao.num)
summary(gold.num)
summary(silber)
summary(oel)
summary(kaffee)
summary(milch)
summary(osaft)
summary(eisenerz)
# In vielen R-Packeten sind Funktionen aehnlicher Art vorhanden, z.B. psych


# Korrelationsmatrix berechnen
cor(kakao.num[,2:5], use='complete.obs')
# Achtung! NAs in Daten fuehren beim Default-Wert von use ("everything") zu NA. Siehe ?cor, details
cor(kakao.num[,2:5])
# Sehr hohe positive Korrelationen in allen Datensaetzen (>.98)


# Einfache Visualisierung

# 1. Boxplot: Visualisierung von Median, IQR, Maximum und Minimum
boxplot(kakao.num[,2:5])
# Was fuer Bedeutung haben die einzelne Elemente eines Boxplot?

# 2. Histogramm: absolute oder relative Haeufigkeitsverteilung 
# Einfache Histogramm (absolute vs. relative Haeufikeit) 
hist(kakao.num$V2, breaks=50)
hist(kakao.num$V2, freq=FALSE, breaks=50)
# Ein anderes Histogramm drauf plotten
hist(kakao.num$V5, freq=FALSE, breaks=50, border='green', add=TRUE)
# Wie sind die Saeulen zu interpretieren?

# Eine (oder mehrere) Variable(n) gegen eine andere plotten

# 3. Liniendiagramm: die zeitliche Entwicklung einer (oder mehrerer) Variable(n)
plot(kakao.num$datum, kakao.num$V2, type='l')
# Weitere Linien auf den Plot hinzufuegen
lines(kakao.num$datum, kakao.num$V3, col='blue')
lines(kakao.num$datum, kakao.num$V4, col='red')
lines(kakao.num$datum, kakao.num$V5, col='green')
# Legende hinzufuegen
legend('topright', legend=names(kakao.num)[-1], lty=1, col=c('black', 'blue', 'red', 'green'), bty='n')
# Oder alles auf einmal. Beachtet, dass die x-Achsenbeschriftung manuell "repariert" werden muss
matplot(kakao.num$datum, kakao.num[,2:5], type='l', xaxt='n')
axis.Date(side=1, kakao.num$V1, format="%m.%Y")
# Passende Legende hinzufuegen
legend('topright', legend=names(kakao.num)[-1], lty=1:ncol(kakao.num[,2:5]), col=seq_len(ncol(kakao.num[,2:5])), bty='n')

# 4. Scatter plot: Wie eine Variable in Abhaengigkeit von einer anderen variiert
plot(kakao.num$V2, kakao.num$V3)
# Weitere Punktwolken hinzufuegen
points(kakao.num$V2, kakao.num$V4, col='blue')
points(kakao.num$V2, kakao.num$V5, col='red')
# Oder alles auf einmal
matplot(kakao.num$V2, kakao.num[,3:5], pch=1, col = c('black', 'blue', 'red'))
# Legende hinzufuegen
legend('bottomright', legend=names(kakao.num)[3:5], col=c('black', 'blue', 'red'), pch=1, bty='n')
# Was man aus dem Plot ablesen kann: V3, V4, V5 liegen nahe auf der Diagonale gegen V2. V3 weicht von V2 ab nach oben und unten, V4 meist nach oben, V5 meist nach unten.


# Von Prof. Schlather programmierte Funktionen zu einer besseren Uebersicht, siehe Vorlesungskript, S. 63
# Dabei wurde fortgeschrittene Technik verwendet. Komplettes Verstaendnis nicht verlangt
panel.hist <- function(x, ...) {
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- cor(x, y, use="complete.obs")
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex * abs(r))
}

# Auf alle Datensaetze anwenden
pairs(kakao.num[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(gold.num[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(silber[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(oel[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(kaffee[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(milch[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(osaft[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)

pairs(eisenerz[, 2:5],
      diag.panel=panel.hist,
      lower.panel=panel.smooth,upper.panel=panel.cor)
