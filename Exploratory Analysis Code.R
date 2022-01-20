terrorismdata <- read.csv("C:/Users/Harun/Documents/Data Sets/globalterrorism.database.csv", na.strings = c("", "NA"))

library(dplyr) 
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(ggmap)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(OpenStreetMap)
library(sf)
library(plotly)
require(wordcloud)
require(tidytext)
require(syuzhet)
require(stringr)
require(kableExtra)
library(tm)

## clean the terrorism data

## Select specific columns of interest and rename them
terrorism.data <- terrorismdata %>%
  select(eventid, iyear,imonth, iday,country_txt, city, region_txt,longitude,latitude, motive, success, suicide, attacktype1_txt, weaptype1_txt, weapsubtype1_txt, targtype1_txt, targsubtype1_txt, nkill) %>%
  filter(nkill > 0) %>%
  rename(
    year = iyear,
    month = imonth,
    day = iday,
    country = country_txt,
    region = region_txt,
    killed = nkill
  )

## Remove all N/A's and their columns from remaining data

clean.terrorism.data <- na.omit(terrorism.data, cols='motive')

## introduction of data 

clean.terrorism.data %>% introduce()

clean.terrorism.data %>% plot_intro()

str(clean.terrorism.data)

## plot world map by terror attacks 

world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x =  long, y = lat, group = group)) +
  geom_point(data = clean.terrorism.data, aes(x = longitude, y = latitude, colour = "lightblue"), size = 0.7, show.legend = FALSE) +
  labs(x = 'Longitude',
       y= 'Latitude',
       title = 'Terrorist Attacks by country 1970-2019', face = "italic") +
  theme_bw()

## plot kills by region line graph 

region.subset <- clean.terrorism.data %>% 
  group_by(year,region) %>% 
  summarise(killed = sum(killed)) %>% 
  ungroup()

colnames(region.subset)<-c("Year","Region","Killed")

ggplot(data = region.subset, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

## type of attack line 

attack.subset <- clean.terrorism.data %>% 
  group_by(year,attacktype1_txt) %>%
  summarise(n = length(year)) %>%
  ungroup() 

colnames(attack.subset)<-c("Year","Type of attack","Number of events")

ggplot(data = attack.subset, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  geom_line() + geom_point() + theme_bw()

## Attacks by attack type

ggplot(clean.terrorism.data, aes(x = year)) + labs(title = "Attacks by Attack Type 1970-2019", x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "black", fill = "salmon") + facet_wrap(~attacktype1_txt, ncol = 4) + theme(axis.text.x = element_text(hjust = 1, size = 6)) +
  theme(strip.text = element_text(size = 8, face = "bold"))

# correlations between values

library(corrplot)

terrorCor <- clean.terrorism.data[,c("year","month","day", "killed", "success","suicide",
                   "killed")]
terrorCor <- na.omit(terrorCor)
correlations <- cor(terrorCor)
p <- corrplot(correlations, method="circle")


philippines <- clean.terrorism.data %>%
  select(country, motive, year) %>%
  filter(country == "Philippines", year == "2015") %>%
  view()

# success rate of terrorist attacks

success.line <- group_by(terrorism.data, year) %>% summarise(successrate = sum(success)/n(), failurerate = 1 - (sum(success)/n()))

ggplot(success.line ) +  geom_line(aes(year, successrate, color = successrate)) + geom_line(aes(year, failurerate, color = failurerate)) + 
  xlab("year") + ylab("Rate") +
  ggtitle("Success/Failure Rate of Terrorist Attacks")

                                    ### text analysis on motive world cloud ###

#get rid of the ID variable and keep just text
motive.words <- clean.terrorism.data %>%    
  select(motive)

#check the stop words
stop_words                     

#anti-join to get rid of the stop words in the text, now we have the useful words left

refined.motive.words <- motive.words %>%
  unnest_tokens(output = word, input = motive) %>%
  anti_join(stop_words,  by = "word") %>%
  count(word, sort = TRUE)     

#use wordcloud to visualise most frequent words

 col <- brewer.pal(8, "Accent") 
refined.motive.words %>%          
  anti_join(stop_words) %>%
  count(word, n) %>%
  with(wordcloud(word, n, max.words = 100, colors = col))

