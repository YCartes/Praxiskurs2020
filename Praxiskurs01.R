# Praxiskurs Statistik mit R
# 01. Basics
# 27.02.2020

# Lernziele:
# Workspace-Management
# Variablen im Workspace manipulieren
# Daten einlesen, zusammenfuegen und speichern
# Struktur der Daten explorieren
# Relevante R-Objekte kennenlernen


# ----------------------Loesungsvorschlag fuer Blatt 01------------------------
# Code wird thematisch geordnet, Reihenfolge entspricht NICHT der der Aufgaben

# Navigation

# Pfad des working directory anzeigen lassen 
getwd()
# Pfad des working directory aendern
# (achte auf den absoluten vs. relativen Pfad)
# Windows:
setwd("C:/Users/yiqi/Praxiskurs2020")  # anderer Schraegstrich als kopiert!
# oder
setwd("C:\\Users\\yiqi\\Praxiskurs2020")
# Mac (bitte pruefen, da ich selbst keinen Mac habe):
setwd("/Users/yiqi/Praxiskurs2020")
# Linux:
setwd("/home/yiqi/Praxiskurs2020")
# home directory (den "default" Pfad) anzeigen bei einem relativen Pfad
path.expand("~")


# Workspace-Management
# Workspace-Dateien kennzeichnen sich durch die Dateiendung ".Rdata"
# Wichtig: "Hygiene" des Workspace halten, version control,
# sowie Ueberschuss und Ueberschreibung der Variablen vermeiden!
# Best practice: R projects verwenden, wenn ueber laengere Zeit hinweg an einem Projekt arbeiten, 
# siehe z.B. https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects

# Workspace laden
load(file = "/home/yiqi/Praxiskurs2020/Sitzung01.Rdata")
# oder gegeben der working directory "/home/yiqi/Praxiskurs2020"
load(file = "booksales.Rdata" )
# Workspace speichern
save.image(file = "Sitzung01.Rdata")
# Objekte im Workspace auflisten
ls()
# Vergleiche: Dateien im working directory auflisten
list.files()
# base type eines Objekts ("base type" hat jedes R-Objekt)
typeof(object_name)
# class eines S3/S4-Objekts (s. objektorientierte Programmierung)
class(object_name)
# Alle Objekte im Workspace loeschen 
rm(list = ())
# Argument "list" spezifiziert, welche Variablen geloescht werden sollen


# Dataframe in R
# Datafreme ist im Wesentlichen eine Liste mit Elementen von der gleichen Laenge
# Gut fuer Datenanalyse, schlecht fuer intensives Rechnen/Programmieren
# siehe auch list, matrix, array

# Dataframe erzeugen
teilnehmer <- data.frame(Name=c("Student A", "Student B"), Studiengang=c("Wirtschaftsmathematik", "Wirtschaftsmathematik"), Fachsemester=c(4,6))
# Dataframe in Datenfile abspeichern
write.csv(teilnehmer, "teilnehmer.csv")


# Daten einlesen
# Uebliche Formate fuer externe Daten:
# Datenfile (.csv), Excel-File (.xls, .xlsx), Textfile (.txt, .dat)
# SPSS, Stata, SAS, PDF & co.: spezielle packages oder zuerst in .csv/.txt 
# Die Funktionen zum Dateneinlesen geben ein data.frame-Objekt zurueck

# Comma seperated values files einlesen
books <- read.csv(file = "booksales.csv")
# achte auf die Argumente header = TRUE/FALSE, sep= , dec= .
# moegliche Trennzeichen: ",", ";", ":" und "\t" (Tabulator). Default: sep=",".

# Tipp: Excel-Files in .csv-File umzuwandeln und dann mit read.csv() einzulesen
# Excel-Files koennen zwar i.d.R mit read.table() oder Funktionen aus speziellen
# Paketen (z.B. xlsx, readxl) einlesen, sie breiten aber haeufig Probleme
# (z.B. die Excel-Files in wolfe.zip)


# Struktur der Daten explorieren
str(books)
names(books)
attributes(books)
head(books)
tail(books)
summary(books)


# Textfiles (.txt, .dat) einlesen
eisenerz <- read.table(file="eisenerz.txt", sep="\t", dec=",", header=FALSE, na.strings="NA", fill=TRUE, quote='')
# Beachte: missing values, deutsches Dezimaltrennzeichen, separator, header, ...
# Nuetzlicher Blog "Using R: common errors in table import"
# https://www.r-bloggers.com/using-r-common-errors-in-table-import/
# read.table()  header=TRUE/FALSE, sep= , dec= , fill=TRUE

# Wenn die Daten in den Textfiles nicht strukturiert sind wie eine Tabelle,
# kann man scan() verwenden mit file= , what= , header=TRUE/FALSE, sep= , skip= , fill=TRUE/FALSE
# Funktioniert NICHT gut bei strukturierten Daten!
eisenerz1 <- scan(file="eisenerz.txt", sep="\t", dec=",", na.strings="NA", fill=TRUE)
eisenerz2 <- scan(file="eisenerz.txt", what = character(), sep="\t", dec=",", na.strings="NA", fill=TRUE)

# Allgemein: Es gibt zahlreiche packages, die speziallen Faelle behandeln
