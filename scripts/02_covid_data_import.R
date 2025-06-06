# COVID stringency index and Google Mobility data

library(dplyr)   
library(lubridate)
library(data.table)  # for fast reading of large datasets
library(ISOcodes)  # to access the ISO 3166 list of country codes

# load daily obs. data
load("data/data_n_obs.RData")


# download Covid OWID (Our World In Data) data and the Google Covid-19 community mobility report data
covid = data.table::fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
mob = data.table::fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

start_date = "2019-01-01"  
end_date = "2022-10-15"

# filter the data to the time period of interest
data_all_covid = data_obs %>% 
  filter(eventDate >= start_date, 
         eventDate <= end_date)

# add the 3-lettered isocode and the country name to the occurrence record dataset
data_all_covid$iso_code = ISO_3166_1$Alpha_3[match(data_all_covid$countrycode, ISO_3166_1$Alpha_2)]
data_all_covid$Country = ISO_3166_1$Name[match(data_all_covid$countrycode, ISO_3166_1$Alpha_2)]
# check which countries are missing in the ISO 3166-1 list
unique(data_all_covid$countrycode[is.na(data_all_covid$iso_code)])   # XK = Kosovo, and ZZ = "High Seas" -> observations in international waters
# fix this
data_all_covid[data_all_covid$countrycode=="XK", "iso_code"] = "XKX"
data_all_covid[data_all_covid$countrycode=="XK", "Country"] = "Kosovo"
# remove the "High Seas" observations
data_all_covid = data_all_covid[-which(data_all_covid$countrycode=="ZZ"),]

# filter the covid data to be merged with the occurrence record dataset
covid = covid %>% 
  select(iso_code, location, date, stringency_index, population) %>%
  filter(date >= start_date, 
         date <= end_date)
# fix isocodes for Kosovo, and remove the multi-country entries 
# (e.g. "World", continents, etc. which are preceded by OWID_)
covid$iso_code[covid$location=="Kosovo"] = "XKX"
covid = covid[!grepl("OWID", covid$iso_code),]

# merge the covid data with the occurrence record dataset
data_all_covid = merge(data_all_covid, covid, 
              by.x = c("iso_code", "eventDate"), by.y = c("iso_code", "date"), 
              all.x = TRUE, all.y = FALSE)

# merge the google mobility reports by country and bind them to the dataframe
# filter out all rows which have an iso_3166_2_code, as these mark subregions within a country
mob = mob %>% 
  filter(iso_3166_2_code == "",
         sub_region_1 == "",
         sub_region_2 == "",
         metro_area == "")
data_all_covid = merge(data_all_covid, mob[,c("country_region_code", "date",
                            "parks_percent_change_from_baseline",
                            "residential_percent_change_from_baseline")], 
              by.x = c("countrycode", "eventDate"), by.y = c("country_region_code", "date"),  
              all.x = TRUE, all.y =FALSE)

# add info on day of week, so that we can identify the weekends
data_all_covid$weekday = weekdays(data_all_covid$eventDate)
data_all_covid$weeknr = strftime(data_all_covid$eventDate, format = "%V") # get number of the week
# remove the column "location", containing country name from the OWID covid dataset
data_all_covid = data_all_covid %>% dplyr::select(-location)

# rename columns
names(data_all_covid)[10:13] <- c("Stringency_index","Population", "Change_park_visitors", "Change_time_at_home")
data_all_covid$eventDate = as.Date(data_all_covid$eventDate)


# save
save(data_all_covid, file = "data/data_all_covid.RData")

rm(covid, mob)
