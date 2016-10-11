
# preliminaries
library(jsonlite)
library(dplyr)

nobel <- fromJSON("http://api.nobelprize.org/v1/laureate.json")

cnt <- sapply(nobel$laureates$prizes, function(x) nrow(x))
multi <- which(cnt > 1)
nobel.multi.df <- cbind(bind_rows(nobel$laureates[multi,])[rep(1:length(cnt[multi]),cnt[multi]),1:2],
                        bind_rows(nobel$laureates$prizes[multi])[1:2])
nobel.multi.df

nobel$laureates[is.na(nobel$laureates$firstname),]


#merge into final df
nobel.df <- cbind(bind_rows(nobel$laureates)[rep(1:length(cnt),cnt),c(1:4,12)],
                  bind_rows(nobel$laureates$prizes)[1:2])
# remove weird six rows
nobel.df <- nobel.df[!is.na(nobel.df$firstname),]

# add new 2016 one's birthdays 
# https://en.wikipedia.org/wiki/Yoshinori_Ohsumi
# http://starsunfolded.com/michael-kosterlitz/ (only one not precise, assume bday before 10.10.)
# https://en.wikipedia.org/wiki/Duncan_Haldane
# https://en.wikipedia.org/wiki/David_J._Thouless
# https://en.wikipedia.org/wiki/Fraser_Stoddart
# https://en.wikipedia.org/wiki/Jean-Pierre_Sauvage
# https://en.wikipedia.org/wiki/Ben_Feringa
# https://en.wikipedia.org/wiki/Juan_Manuel_Santos
# https://en.wikipedia.org/wiki/Oliver_Hart_(economist)
# https://en.wikipedia.org/wiki/Bengt_R._Holmstr%C3%B6m

nobel.df %>% 
  mutate(born = ifelse(surname == "Ohsumi" & born =="0000-00-00","1945-02-09", born)) %>%
  mutate(born = ifelse(surname == "Kosterlitz" & born =="0000-00-00", "1942-01-01", born)) %>%
  mutate(born = ifelse(surname == "Haldane" & born =="0000-00-00", "1951-09-14", born)) %>%
  mutate(born = ifelse(surname == "Thouless" & born =="0000-00-00", "1934-09-21", born)) %>%
  mutate(born = ifelse(surname == "Stoddart" & born =="0000-00-00", "1942-05-24", born)) %>%
  mutate(born = ifelse(surname == "Sauvage" & born =="0000-00-00", "1944-10-21", born)) %>%
  mutate(born = ifelse(surname == "Feringa" & born =="0000-00-00", "1951-05-18", born)) %>%
  mutate(born = ifelse(surname == "Santos" & born =="0000-00-00", "1951-08-10", born)) %>%
  mutate(born = ifelse(surname == "Hart" & born =="0000-00-00", "1948-10-09", born)) %>%
  mutate(born = ifelse(surname == "Holmström" & born =="0000-00-00", "1949-04-18", born)) ->
  nobel.df


nobel.df %>%
  mutate(age = as.numeric(as.Date(paste(year, "12-31", sep = "-"), 
                                  "%Y-%m-%d") - as.Date(born,"%Y-%m-%d"))/365,
         year = as.numeric(year)) %>%
  filter(gender != "org") -> nobel.df

rm(cnt, nobel, nobel.multi.df)

lexp <- read.csv("https://ourworldindata.org/grapher/life-expectancy-globally-since-1770.csv?country=ALL")
#rename stupid column name
names(lexp)[3] <- "lexp"
# keep only world data on year and lexp
# pad missing years and join back
# spline for interpolated lexp

lexp %>% 
  filter(Country == "World") %>%
  select(Year, lexp ) %>%
  full_join(data.frame(Year = seq(min(lexp$Year), 2016))) %>%
  arrange(Year) %>%
  mutate(lexp.interpolated = zoo::na.spline(lexp)) %>%
  select(Year, lexp.interpolated) %>%
  right_join(nobel.df, by = c("Year" = "year"))-> full.lexp

rm(nobel.df, lexp)


# you can adjust span and degree for the smoothing curve, defaults are 2/3 and 1
span = 2/3
degree = 1
FunPlot <- function(data, color, norm = TRUE){
  x <- data$Year
  y <- data$age
  y.grids <- seq(0,100,20)
  ylim <- 100
  if (!norm) {y.grids <- y.grids/40
  y <- data$age/data$lexp.interpolated
  ylim <- 2.3
  }
  scatter.smooth(x, y, pch = 19, 
                 axes = FALSE, span = span, degree= degree,
                 col = color[1], lpars = list(col = color[2], lwd = 3),
                 xlab = "Year", ylab = "Age", main = deparse(substitute(data)),
                 ylim = c(0,ylim), xlim = c(1900, 2020))
  for (i in y.grids){
    lines(c(1900,2020), c(i,i), lty=2, lwd=0.5)
  }
  axis(1, las=2, at = seq(1900, 2020, 20),labels = c(1900, 1920, 1940, 1960, 1980, 2000,""))
}

# and neat little tables for each category
physics <- full.lexp[full.lexp$category == "physics",]
chemistry <- full.lexp[full.lexp$category == "chemistry",]
medicine <- full.lexp[full.lexp$category == "medicine",]
economics <- full.lexp[full.lexp$category == "economics",]
literature <- full.lexp[full.lexp$category == "literature",]
peace <- full.lexp[full.lexp$category == "peace",]



layout(mat=matrix(1:7, c(1,7)), widths = c(1,3,3,3,3,3,3))
par(mar = c(3,2.2,1,0))
plot(1,1, ylim= c(0,100), type = "n", axes = FALSE, xlab = "", ylab = "")
axis(2, las=2)
par(mar = c(3,0,1,0))
FunPlot(physics, c("darkolivegreen3", "darkolivegreen4"))   
FunPlot(chemistry, c("plum2", "plum4"))    
FunPlot(medicine, c("skyblue", "skyblue3"))   
FunPlot(economics, c("orange1", "orange3"))  
FunPlot(literature, c("gray70", "gray40"))  
FunPlot(peace, c("mediumpurple1", "mediumpurple4"))  

layout(mat=matrix(1:7, c(1,7)), widths = c(1,3,3,3,3,3,3))
par(mar = c(3,2.2,1,0))
plot(1,1, ylim= c(0,2.3), type = "n", axes = FALSE, xlab = "", ylab = "")
axis(2, las=2)
par(mar = c(3,0,1,0))
FunPlot(physics, c("darkolivegreen3", "darkolivegreen4"), norm = FALSE)   
FunPlot(chemistry, c("plum2", "plum4"), norm = FALSE)    
FunPlot(medicine, c("skyblue", "skyblue3"), norm = FALSE)   
FunPlot(economics, c("orange1", "orange3"), norm = FALSE)  
FunPlot(literature, c("gray70", "gray40"), norm = FALSE)  
FunPlot(peace, c("mediumpurple1", "mediumpurple4"), norm = FALSE)  


