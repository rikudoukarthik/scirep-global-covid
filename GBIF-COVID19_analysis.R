###################################################
#
# Title: GBIF-COVID19_analysis.R
# Purpose: calculate the change in the amount of records uploaded to the Global Biodiversity Information Facility (GBIF) during the COVID19 pandemic 
#          relative to pre-pandemic levels. Model how the change in records is affected by the lockdown restrictions in place in the different countries 
#          and their impacts on people's movements, as well as country-specific confounding factors (population size, economic class).
#          
# Author: Stephanie Roilo, Technische Universitaet Dresden
# Date: last edited on May 11th 2023
#
###################################################

Sys.setenv(LANGUAGE="en")
library(dplyr)
library(ggplot2)
library(viridis) 
library(ggExtra)
library(ISOcodes)
library(data.table)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)  # to avoid overlapping text labels in ggplots
library(visreg)   # to plot conditional plot of the linear model
library(DHARMa)   # to produce diagnostic plots of the model residuals
library(tmap)     # for nice maps

# read data from all countries, with downloaded GBIF observations, stringency index and movement data (see script "Data_download.R" for details on its preparation)
allcn = fread("C:/Users/sroilo/Desktop/GBIF/Anthropause_app/Data_249_countries_20230321.csv") 
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO","Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
# stringency index data start on January 22nd 2020, Google mobility data on February 15th 2020
# choose start and end date of the first global lockdown period
start_date = "2020-03-15"
end_date = "2020-05-01"
# compute means for all variables, across the selected period, for all countries 
cntall = allcn %>% filter(Date>=start_date & Date<=end_date) %>%
  group_by(Country) %>% summarise_at(vars(Nr_records, Stringency_index, Change_park_visits, Change_time_at_home), mean, na.rm=T)
# omit countries for which one (or more) of the variables is NA
cntall = na.omit(cntall) 
# compute the mean daily records in the same period of the previous year, to be used as baseline
cntall_PY = allcn %>% filter(Date >= (as.Date(start_date) - years(1)) & Date <= (as.Date(end_date) - years(1))) %>%
  group_by(Country) %>% summarise_at(vars(Nr_records), mean, na.rm=T)
# bind to the other dataframe
cntall$Records_prev_year = cntall_PY$Nr_records[match(cntall$Country, cntall_PY$Country)]
# compute the percent change in records collected between the two time periods (lockdown vs. pre-pandemic)
cntall$Change_records = ifelse(cntall$Records_prev_year>0, (cntall$Nr_records - cntall$Records_prev_year)/cntall$Records_prev_year*100, NA)
# add data on geographic region, continent, economic class, population size, from the NaturalEarth dataset
# add iso3 and iso2 codes so that the dataframe can be matched to the World map
cntall["adm0_a3"] <- ISO_3166_1$Alpha_3[match(cntall$Country, ISO_3166_1$Name)]
cntall["Country_code"] <- ISO_3166_1$Alpha_2[match(cntall$Country, ISO_3166_1$Name)]
# load shapefile of the countries of the world
wmap_all = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
  select(c("name_long", "adm0_a3","pop_est", "economy", "income_grp"))
# merge dataframe and shapefile, retain only countries with information
wmap = merge(wmap_all, cntall, by = "adm0_a3", all.y=T)
# check distribution of change_records
hist(wmap$Change_records, breaks=100)   # some outliers with extremely high positive increases in records collected!
# remove outliers, i.e. data points with absolute z-scores greater than 3
wmap = wmap[-which( abs(scale(wmap$Stringency_index))>3 | 
#                    abs(scale(wmap$Change_time_at_home))>3|   # no outlier here, also, variable not included in the model
                    abs(scale(wmap$Change_park_visits))>3 | 
                    abs(scale(wmap$Change_records))>3 |
                    abs(scale(log10(wmap$pop_est)))>3 ),]

###' REGRESSION MODEL ----------------------------------
# prepare data for modelling
dat = st_drop_geometry(wmap) 
# calculate the log10-transformation of the estimated population size, to correct for outliers (e.g. USA)
dat$log10_pop_est = log10(dat$pop_est)
# Economy groups are many and some have only few countries; merge some economy categories together into larger groups
dat$economy_coarse = "4. Least developed region"
dat$economy_coarse[which(dat$economy %in% c("1. Developed region: G7", "2. Developed region: nonG7"))] <- "1. Developed region"
dat$economy_coarse[which(dat$economy %in% c("3. Emerging region: BRIC","4. Emerging region: MIKT", "5. Emerging region: G20"))] <- "2. Emerging region"
dat$economy_coarse[which(dat$economy %in% c("6. Developing region"))] <- "3. Developing region"

# convert economy group to numeric to check for correlation among variables
dat$economy_nr = substr(dat$economy, 1, 1) %>% as.numeric()
dat$economyc_nr = substr(dat$economy_coarse, 1, 1) %>% as.numeric()
# we use Spearman's r to check for collinearity among variables
cor(dat[,c("Stringency_index", "Change_park_visits", "Change_time_at_home", "log10_pop_est","economyc_nr")], 
    method="spearman") # Change_park_visits and Change_time_at_home are strongly correlated! 
# keep change in park visitors
fit1 = glm(Change_records ~ Stringency_index + Change_park_visits + log10_pop_est + 
             economy_coarse, data=dat, family="gaussian")
summary(fit1)
sred = simulateResiduals(fit1, plot=T)  # outlier test significant! Check the dataset again
testOutliers(sred) 
# check data again to detect potential outliers
pairs(dat[,c("Stringency_index", "Change_park_visits", "Change_time_at_home", "Change_records")])
# still two outliers in the response variable, remove them
dat = dat[-which(abs(scale(dat$Change_records))>3),]
# refit model and check for patterns in the residuals
fit1 = glm(Change_records ~ Stringency_index + Change_park_visits + log10_pop_est + 
             economy_coarse, data=dat, family="gaussian")
summary(fit1)
sred = simulateResiduals(fit1, plot=T)  
testOutliers(sred)  # all fine now!
# plot simulated residuals against each covariate
DHARMa::plotResiduals(sred, form = dat$Stringency_index)
DHARMa::plotResiduals(sred, form = dat$Change_park_visits)
DHARMa::plotResiduals(sred, form = dat$log10_pop_est)
DHARMa::plotResiduals(sred, form = dat$economy_coarse)
# remove outliers also in the wmap shapefile
wmap = wmap[-which(abs(scale(wmap$Change_records))> 3),]
# write dataset to file
write.table(dat, "C:/Users/sroilo/Desktop/GBIF/LinRegr_data_20230509.csv", sep=";", dec=",", row.names=F)

### BUBBLE PLOT -------------------------------------
# plot a bubble plot to compare countries
g_change = ggplot() +
  geom_point(wmap, mapping = aes(x=Change_records, y=Stringency_index, size=Nr_records, colour=Change_park_visits, label=Country_code), alpha=0.8) +  
  scale_size(range=c(2,20)) + 
  scale_color_viridis(option="D", direction=1) +
  theme_minimal() + 
  theme(legend.text=element_text(size=12), legend.title=element_text(size=12), axis.title=element_text(size=12), legend.position = "bottom") +   
  labs(x="Change_records (%)", y="Stringency index", colour="Change in park visitors (%)", size="Nr. of records per day")
# add label names of countries
g_change = g_change + geom_text_repel(wmap, mapping = aes(x=Change_records, y=Stringency_index, label=Country_code))

# make a world map with Change_records as cloropleth
sf::sf_use_s2(FALSE)
wmap_all = wmap_all[-which(wmap_all$name_long=="Antarctica"),] # remove Antarctica to gain more space
changemap = tm_shape(wmap_all) + tm_borders(col="darkgrey") +
  tm_shape(wmap) + tm_polygons("Change_records", palette="RdYlBu", 
                                breaks = c( -100, -50, -25, -10, 10, 25, 50, 100, 200)) + 
  tm_layout(legend.text.size = 0.8, legend.outside=T) + tmap_options(check.and.fix = TRUE)
tmap_save(changemap, width= 6, height=3, filename="C:/Users/sroilo/Desktop/GBIF/images/Worldmap_change_records_20230509_2.jpeg")

#### COUNTRY LIST by eBird proportions ---------------------
# read data from all countries
allcn = fread("C:/Users/sroilo/Desktop/GBIF/Anthropause_app/Data_249_countries_20230321.csv") 
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO","Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
# compute total numbers of GBIF records and of eBird (CLO) records in 2019 (entire year) for all countries 
cntall = allcn %>% filter(Date>="2019-01-01" & Date<="2019-12-31") %>%
  group_by(Country) %>% summarise_at(vars(Nr_records, n_CLO), sum, na.rm=T)
# calculate ratio of eBird/GBIF data
cntall$eBird_prop = round(cntall$n_CLO/cntall$Nr_records*100, digits=0)
# add 2-lettered country code
cntall$Country_code = dat$Country_code[match(cntall$Country, dat$Country)]
# addr economy group information
cntall$economy_coarse = dat$economy_coarse[match(cntall$Country, dat$Country)]
# add also stringency and mobility information
cntall$Stringency_index = dat$Stringency_index[match(cntall$Country, dat$Country)]
cntall$Change_park_visits = dat$Change_park_visits[match(cntall$Country, dat$Country)]
cntall$Change_records = dat$Change_records[match(cntall$Country, dat$Country)]
# remove NAs (countries with no economy group due to e.g. too few records, or missing stringency or mobility data)
cntall = na.omit(cntall)
cntall = cntall[order(cntall$eBird_prop, decreasing=T),]
write.table(cntall, "C:/Users/sroilo/Desktop/GBIF/HomeRange/Countries_by_eBird_proportions_20230509.csv", sep=";", dec=",", row.names=F)

## repeat but selecting only the 2019 baseline period (2019-03-15 to 2019-05-01)
allcn = fread("C:/Users/sroilo/Desktop/GBIF/Anthropause_app/Data_249_countries_20230321.csv") 
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO","Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
cntall = allcn %>% filter(Date>="2019-03-15" & Date<="2019-05-01") %>%
  group_by(Country) %>% summarise_at(vars(Nr_records, n_CLO), sum, na.rm=T)
# calculate ratio of eBird/GBIF data
cntall$eBird_prop = round(cntall$n_CLO/cntall$Nr_records*100, digits=0)
# merge with the other economy group information
cntall$economy_coarse = dat$economy_coarse[match(cntall$Country, dat$Country)]
# remove NAs (countries with no economy group due to e.g. too few records, or missing stringency or mobility data)
cntall = na.omit(cntall)
cntall = cntall[order(cntall$n_CLO, decreasing=T),]
write.table(cntall, "C:/Users/sroilo/Desktop/GBIF/HomeRange/Countries_by_eBird_proportions_MarMay20230509.csv", sep=";", dec=",", row.names=F)



