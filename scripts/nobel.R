
library(jsonlite)
library(dplyr)
require(stringr)

u <- "http://api.nobelprize.org/v1/laureate.json"
nobel <- fromJSON(u)

# table with one row per prize
prizes <- rbind_all(nobel$laureates$prizes)[1:2]


# count multiple winners
cnt <- sapply(nobel$laureates$prizes, function(x) nrow(x))
prizes$gender <- rep(nobel$laureates$gender, cnt)
prizes$born <- rep(nobel$laureates$born, cnt)

# add new 2016 one's birthdays 
# 907 Yoshinori Ohsumi medicine February 9, 1945 https://en.wikipedia.org/wiki/Yoshinori_Ohsumi
prizes$born[907] <- "1945-02-09"
# http://starsunfolded.com/michael-kosterlitz/
prizes$born[908] <- "1942-01-01"
# https://en.wikipedia.org/wiki/Duncan_Haldane
prizes$born[909] <- "1951-09-14"
# https://en.wikipedia.org/wiki/David_J._Thouless
prizes$born[910] <- "1934-09-21"
#https://en.wikipedia.org/wiki/Fraser_Stoddart
prizes$born[911] <- "1942-05-24"
#https://en.wikipedia.org/wiki/Jean-Pierre_Sauvage
prizes$born[912] <- "1944-10-21"
#https://en.wikipedia.org/wiki/Ben_Feringa
prizes$born[913] <- "1951-05-18"
#https://en.wikipedia.org/wiki/Juan_Manuel_Santos
prizes$born[914] <- "1951-08-10"
#https://en.wikipedia.org/wiki/Oliver_Hart_(economist)
prizes$born[915] <- "1948-10-09"
#https://en.wikipedia.org/wiki/Bengt_R._Holmstr%C3%B6m
prizes$born[916] <- "1949-04-18"

prizes$age <- as.numeric(as.Date(paste(prizes$year, "12-31", sep = "-"), "%Y-%m-%d") - as.Date(prizes$born, "%Y-%m-%d"))/365
prizes$country <- rep(nobel$laureates$bornCountryCode, cnt)

#for some reason there are 6 empty rows in the data. remove
prizes <- prizes[!is.na(prizes$year),]
prizes$year <- as.numeric(prizes$year)
# remove organizations as well
prizes %>%
  filter(gender != "org") -> prizes
rm(cnt, nobel, u)


lexp <- read.csv("https://ourworldindata.org/grapher/life-expectancy-globally-since-1770.csv?country=ALL")
#rename stupid column name
names(lexp)[3] <- "lexp"
# keep only world data on year and lexp
# pad missing years and join back
lexp %>% 
filter(Country == "World") %>%
select(Year, lexp ) %>%
full_join(data.frame(Year = seq(min(lexp$Year), 2016))) %>%
arrange(Year) %>%
mutate(lexp.interpolated = zoo::na.spline(lexp)) %>%
select(Year, lexp.interpolated) %>%
right_join(prizes, by = c("Year" = "year"))-> full.lexp

rm(prizes, lexp)


span = 2/3
degree = 2
FunPlotNorm <- function(data, color){
x <- data$Year
y <- data$age
scatter.smooth(x, y, pch = 19, 
axes = FALSE,
col = color[1], lpars = list(col = color[2], lwd = 3),
xlab = "Year", ylab = "Age", main = deparse(substitute(data)),
ylim = c(0,100), xlim = c(1900, 2020))
lines(c(1900,2020), c(20,20), lty=2, lwd=0.5)
lines(c(1900,2020), c(40,40), lty=2, lwd=0.5)
lines(c(1900,2020), c(60,60), lty=2, lwd=0.5)
lines(c(1900,2020), c(80,80), lty=2, lwd=0.5)
lines(c(1900,2020), c(100,100), lty=2, lwd=0.5)
axis(1, las=2, at = seq(1900, 2020, 20),labels = c(1900, 1920, 1940, 1960, 1980, 2000,""))

}

FunPlotRelative <- function(data, color){
x <- data$Year
y <- data$age/data$lexp.interpolated
scatter.smooth(x, y, pch = 19, 
axes = FALSE,
col = color[1], lpars = list(col = color[2], lwd = 3),
xlab = "Year", ylab = "Age", main = deparse(substitute(data)),
ylim = c(0,2.2), xlim = c(1900, 2020))
lines(c(1900,2020), c(0.5,0.5), lty=2, lwd=0.5)
lines(c(1900,2020), c(1,1), lty=2, lwd=0.5)
lines(c(1900,2020), c(1.5,1.5), lty=2, lwd=0.5)
lines(c(1900,2020), c(2,2), lty=2, lwd=0.5)
lines(c(1900,2020), c(100,100), lty=2, lwd=0.5)
axis(1, las=2, at = seq(1900, 2020, 20),labels = c(1900, 1920, 1940, 1960, 1980, 2000,""))
}

physics <- full.lexp[full.lexp$category == "physics",]
chemistry <- full.lexp[full.lexp$category == "chemistry",]
medicine <- full.lexp[full.lexp$category == "medicine",]
economics <- full.lexp[full.lexp$category == "economics",]
literature <- full.lexp[full.lexp$category == "literature",]
peace <- full.lexp[full.lexp$category == "peace",]

layout(mat=matrix(1:7, c(1,7)), widths = c(1,3,3,3,3,3,3))
par(mar = c(3,2.4,1,0))
plot(1,1, ylim= c(0,100), type = "n", axes = FALSE, xlab = "", ylab = "")
axis(2, las=2)
par(mar = c(3,0,1,0))

FunPlotNorm(physics, c("darkolivegreen3", "darkolivegreen4"))   
FunPlotNorm(chemistry, c("plum2", "plum4"))    
FunPlotNorm(medicine, c("skyblue", "skyblue3"))   
FunPlotNorm(economics, c("orange1", "orange3"))  
FunPlotNorm(literature, c("gray70", "gray40"))  
FunPlotNorm(peace, c("mediumpurple1", "mediumpurple4"))  






layout(mat=matrix(1:7, c(1,7)), widths = c(1,3,3,3,3,3,3))
par(mar = c(3,2.4,1,0))
plot(1,1, ylim= c(0,2.2), type = "n", axes = FALSE, xlab = "", ylab = "")
axis(2, las=2)
par(mar = c(3,0,1,0))
FunPlotRelative(physics, c("darkolivegreen3", "darkolivegreen4"))             
FunPlotRelative(chemistry, c("plum2", "plum4"))             
FunPlotRelative(medicine, c("skyblue", "skyblue3"))             

FunPlotRelative(economics, c("orange1", "orange3"))             
FunPlotRelative(literature, c("gray70", "gray40"))             
FunPlotRelative(peace, c("mediumpurple1", "mediumpurple4")) 

