getwd()
setwd("D:/DANIEL/Programming for Data Science with R/R Programming/Project 2")

wash <- read.csv("washington.csv")
ny <- read.csv("new-york-city.csv")
chi <- read.csv("chicago.csv")

names(chi)
names(wash)
names(ny)

str(chi)
str(wash)
str(ny)



Gender <- c(rep(" ", 300000))
Birth.Year <- c(rep(" ", 300000))

#add empty columns for Gender and Birth.Year to the washington dataset
wash <- cbind(wash, Gender, Birth.Year)
#wash$Gender <- Gender
head(wash, 3)

ny$city <- "New York"
chi$city <- "Chicago"
wash$city <- "Washington"

#merge all datasets into one
cities <- rbind(chi, ny, wash)

#1 What are the counts for each user type?
#=============================================================================================
by(cities$User.Type, cities$city, summary)


library(ggplot2)
ggplot(data = cities, aes(User.Type)) +
  geom_bar() +
  facet_wrap(~city, ncol = 3) +
  ggtitle("Counts of User Types for Each City") +
  xlab("User Types") +
  ylab("Count of User Type")


#2 What is the most common start station?
#=============================================================================================
summary(ny$Start.Station)

#
ssn <- data.frame(table(ny$Start.Station))
ny.freq <- ssn[which.max(ssn$Freq), ]
ny.freq[1, 1]
ny.freq$Var1

#
ss.chi <- data.frame(table(chi$Start.Station))
chi.freq <- ss.chi[which.max(ss.chi$Freq), ]
chi.freq[1, 1]
chi.freq$Var1

#
ss.wash <- data.frame(table(wash$Start.Station))
wash.freq <- ss.wash[which.max(ss.wash$Freq), ]
wash.freq[1, 1]
wash.freq$Var1
#
get.freq.SS <- function(p = wash){
  ss.p <- data.frame(table(p$Start.Station))
  p.freq <- ss.p[which.max(ss.p$Freq), ]
  p.freq[1, 1]
  return(p.freq[1, 1])
}

get.freq.SS(ny)
get.freq.SS(wash)
get.freq.SS(chi)
#
ss.all <- rbind(ss.wash, ss.chi, ssn)

n <- c("Streeter Dr & Grand Ave","Pershing Square North","Columbus Circle / Union Station")

ss.all2 <- subset(ss.all, Var1 == "Streeter Dr & Grand Ave" | Var1 == "Pershing Square North" | Var1 == "Columbus Circle / Union Station")

#
cities2 <- subset(cities, Start.Station == "Streeter Dr & Grand Ave" | Start.Station == "Pershing Square North" | Start.Station == "Columbus Circle / Union Station")
ggplot(data = cities2, aes(cities2$Start.Station))  +
  geom_bar() +
  ggtitle("Frequencies for the most common Stations in each city") +
  xlab("Start Station") +
  ylab("Frequency")


#What is the most common month?
#============================================================================================
cities
names(cities)
head(cities$Start.Time, 1)

cities$Start.Time2 <- as.Date(cities$Start.Time)
class(cities$Start.Time2)

cities$Months <- months.POSIXt(cities$Start.Time2)
names(cities)
#
library(magrittr)

cities$Months <- cities$Start.Time %>%
                  as.Date() %>%
                  months.Date()

#
ggplot(cities, aes(Months)) +
  geom_bar() +
  ggtitle("Monthly Number of Rides by City") +
  ylab("Number of Rides") +
  facet_wrap(~city, ncol = 2)

MostCommonMonth <- function(city = Chicago ,tt = " "){
  cities <- subset(cities, cities$city == city)
  df = data.frame(table(cities$Month))
  df[which.max(df$Freq), ]
  print(tt)
  return(df[which.max(df$Freq), ][1, 1])
}

MostCommonMonth(city = "New York", tt = "New York's most common Month")
MostCommonMonth(city = "Washington", tt = "Washington's most common Month")
MostCommonMonth(city = "Chicago", tt = "Chicago's most common Month")
