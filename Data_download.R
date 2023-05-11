###################################################
#
# Title: Data_download.R
# Purpose: download daily counts of species occurrence records (only those that are georeferenced and tagged as human observations) from the 
#          Global Biodiversity Information Facility (GBIF), using the GBIF API.
#
# Author: Stephanie Roilo, Technische Universität Dresden
# Date: started on July 1st 2022, last edited on May 11th 2023
#
###################################################
# set the language to EN
library(rgbif)  
library(dplyr)
library(ISOcodes)    # to access the ISO 3166 list of country codes
library(data.table)  # for fast reading of large datasets
library(gbifapi)

# download Covid OWID (Our World In Data) data and the Google Covid-19 community mobility report data
covid = fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
mob = fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

# set start and end dates of the time period of interest
start_date = "2019-01-01"  # to be entered in the format yyyy-mm-dd
end_date = "2022-10-15"

# prepare the list of all countries for which to download data
allc = ISO_3166_1$Alpha_2
# make a list of already processed countries (done) and those which still need to be run (undone)
done = list.files("C:/Users/sroilo/Desktop/GBIF/country_data_2019_2022/", pattern=".csv")
done = strsplit(done, "_2019-01-01_2022-10-15.csv")
undone = allc[-which(allc %in% done)]
print(undone)

# loop through all countries in the "undone" list
for ( cocode in undone) {
  print(paste0("doing country ", cocode))
  country_iso2 = cocode
  country_iso3 = ISO_3166_1$Alpha_3[ISO_3166_1$Alpha_2==cocode]
  
  # create a dataframe in which each row corresponds to a day
  dates = data.frame(Date = seq(as.Date(start_date), as.Date(end_date), by="days"))
  for (i in c(1:nrow(dates))) {
    date = dates$Date[i]
    # download the GBIF data entries for the day
    daydat = gbifapi(paste0("http://api.gbif.org/v1/occurrence/search?isGeoreferenced=true&basisOfRecord=HUMAN_OBSERVATION&country=", country_iso2,
                            "&eventDate=", date))
    dates$n_HumObs[i] = daydat$count
    # extract also the count of records only from eBird (InstitutionCode = CLO (Cornell Lab for Ornithology))
    daydat = gbifapi(paste0("http://api.gbif.org/v1/occurrence/search?isGeoreferenced=true&basisOfRecord=HUMAN_OBSERVATION&institutionCode=CLO&country=", country_iso2,
                            "&eventDate=", date))
    dates$n_CLO[i] = daydat$count
  }
  
  # filter the OWID-covid data by country, extract the add stringency index and bind it to the dataframe
  covid = covid[covid$iso_code==country_iso3,]
  covid$date = as.Date(covid$date)
  dates$stringency = covid$stringency_index[match(dates$Date, covid$date)]
  
  # filter the google mobility reports by country and bind them to the dataframe
  mob = mob[which(mob$country_region_code==country_iso2 & mob$sub_region_1==""),]
  mob$date = as.Date(mob$date)
  # add the percent change in visitors to parks
  dates$cParkVisit = mob$parks_percent_change_from_baseline[match(dates$Date, mob$date)]
  # add the percent change in time spent in residential areas
  dates$cHome = mob$residential_percent_change_from_baseline[match(dates$Date, mob$date)]
  
  # add info on day of week, so that we can identify the weekends
  dates$weekday = strftime(dates$Date, "%u") # get weekday in number format (1 is Monday)
  dates$weeknr = strftime(dates$Date, format = "%V") # get number of the week
  dates$year = strftime(dates$Date, format = "%Y") # get year
  #divide nr. of observations by 100, to ease plotting
  dates$HumObs_100 = dates$n_HumObs/100
  dates$CLO_100 = dates$n_CLO/100
  dates$iNaturalist_100 = dates$n_iNaturalist/100
  
  # save to file 
  write.table(dates, paste0("C:/Users/sroilo/Desktop/GBIF/country_data_2019_2022/", country_iso2, "_", start_date, "_", end_date, ".csv"), sep=",", dec=".", row.names=F)
  
}

# bind together datasets from all countries
files = list.files("C:/Users/sroilo/Desktop/GBIF/country_data_2019_2022/", pattern=".csv")
# extract country codes (2-lettered)
cnnames = vapply(strsplit(files,"_"), `[`, 1, FUN.VALUE=character(1))
allcn = data.frame()
for (i in c(1:length(files))) {
  dat1 = read.table(paste0("C:/Users/sroilo/Desktop/GBIF/country_data_2019_2022/", files[i]), sep=",", dec=".", header=T)
  dat1$country_iso2 = cnnames[i]
  dat1$Country = ISO_3166_1$Name[ISO_3166_1$Alpha_2==cnnames[i]]
  allcn = rbind(allcn, dat1)
}
# save to file the full dataset including 249 countries and dependent territories
write.table(allcn, "C:/Users/sroilo/Desktop/GBIF/Data_249_countries_20230321.csv", sep=",", dec=".", row.names=F) # download took place between March 19th and Marh 21st 2023.

rm(list=ls())
setwd("C:/Users/sroilo/Documents")
