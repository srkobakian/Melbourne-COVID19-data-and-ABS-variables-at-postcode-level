library(tidyverse)
library(sf)
library(absmapsdata)
library(plotly)
library(crosstalk)
library(readxl)
library(googlesheets4)



#STEP  Read 6 August COVID data from Age article by Butt and Stehle
#https://www.theage.com.au/national/victoria/victoria-coronavirus-data-find-the-number-of-active-covid-19-cases-in-your-postcode-20200731-p55hg2.html 

butt_url <-"https://docs.google.com/spreadsheets/d/1oxJt0BBPzk-w2Gn1ImO4zASBCdqeeLJRwHEA4DASBFQ/edit#gid=0"

#get sheet information
ssid <- as_sheets_id(butt_url)

#read 6 August case data and limit to Melbourne postcodes
cases <- read_sheet(butt_url, sheet = "Data (August 6)")

#STEP 02 get list of Melb postcodes and suburb names
#Copypasta from the Butt and Stehle article

melb_names <- read.csv("melbourne.postcode.list.csv", stringsAsFactors = FALSE)
library(stringi)
#Make suburb names more readable
melb_names <- melb_names %>% 
  mutate(Postcode = as.character(Postcode)) %>%
  rename(Suburb = City..Town) %>%
 mutate(Suburb = iconv(Suburb, 'utf-8', 'ascii', sub=' ')) %>%
  filter(!Postcode %in% c("Unknown","Others")) %>%
  mutate(Suburb = str_replace_all(Suburb, "melbourne","Melbourne")) %>%
  select(-State, -District)

#STEP 03 extract shapes from absmaps
#absmaps data - look at postcode level - melb postcodes only
#add melbourne features

mapdata <- postcode2016 %>%
  filter(postcode_2016 %in% melb_names$Postcode) %>%
  rename(Postcode = postcode_2016)


melb_names$Postcode <- as.character(melb_names$Postcode)

melb <- left_join(mapdata, melb_names, by = "Postcode")

#Step 04 add in case data to melbourne shape data

cases$Postcode<-as.character(cases$Postcode)

dat <- left_join(melb, cases, by = "Postcode")

dat$`Confirmed cases (ever)`[is.na(dat$`Confirmed cases (ever)`)]<-0

#Step 05
#determine layout and plot
#visualise to see that it makes sense

g <- list(showlegend = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
)

dat %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", Suburb, "<br>","Cases:", `Confirmed cases (ever)`)) %>%
  add_sf(color = ~`Confirmed cases (ever)`,
         hoveron = "points+fills") %>%
  layout(geo = g)

saveRDS(dat, "Melbourne_case_data.RDS")

