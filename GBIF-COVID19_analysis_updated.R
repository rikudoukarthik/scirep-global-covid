###################################################
#
# Title: GBIF-COVID19_analysis_updated.R
# Purpose: calculate the change in the amount of records uploaded to the Global Biodiversity Information Facility (GBIF) during the COVID19 pandemic 
#          relative to pre-pandemic levels. Model how the change in records is affected by the lockdown restrictions in place in the different countries 
#          and their impacts on people's movements, as well as country-specific confounding factors (population size, economic class).
#          
# Author: Stephanie Roilo, TUD Dresden University of Technology, Germany & Bonn University, Germany
# Date: last edited on September 19th 2024
# R version: 4.4.1
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
library(DHARMa)   # to produce diagnostic plots of the model residuals
library(tmap)     # for nice maps
library(gridExtra)
library(nlme)     # for GLS
library(effects)  # to plot predictors' effects of the GLS model

# read data from all countries, with downloaded GBIF observations, stringency index and movement data (see script "Data_download.R" for details on its preparation)
allcn = fread("C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/Anthropause_app/Data_249_countries_20230321.csv", encoding = 'Latin-1') 
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
cntall[cntall$Country == "Turkey", "Country"] <- "Türkiye"  # correct the name of Turkey
cntall["adm0_a3"] <- ISO_3166_1$Alpha_3[match(cntall$Country, ISO_3166_1$Name)]
cntall["Country_code"] <- ISO_3166_1$Alpha_2[match(cntall$Country, ISO_3166_1$Name)]
# load shapefile of the countries of the world (rnaturalearth package version 1.0.1, and rnaturalearthdata version 1.0.0)
wmap_all = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
  select(c("name_long", "adm0_a3","pop_est", "economy", "income_grp"))

# merge dataframe and shapefile, retain only countries with information
wmap = merge(wmap_all, cntall, by = "adm0_a3", all.y=T)


###' REGRESSION MODEL ----------------------------------
# prepare data for modelling
dat = st_drop_geometry(wmap) 
# Economy groups are many and some have only few countries; merge some economy categories together into larger groups
dat$economy_coarse = "4. Least developed region"
dat$economy_coarse[which(dat$economy %in% c("1. Developed region: G7", "2. Developed region: nonG7"))] <- "1. Developed region"
dat$economy_coarse[which(dat$economy %in% c("3. Emerging region: BRIC","4. Emerging region: MIKT", "5. Emerging region: G20"))] <- "2. Emerging region"
dat$economy_coarse[which(dat$economy %in% c("6. Developing region"))] <- "3. Developing region"

# check for potential outliers
pairs(dat[,c("Stringency_index", "Change_park_visits","Change_time_at_home", "pop_est", "Change_records")])
# pop_est and Change_records have some outliers; let's apply a log10 transformation
dat$log10_pop_est = log10(dat$pop_est)
# Change_Records varies between -100 and +Inf, 
# check that no negative values equal to -100 are present in Change_records before applying the log10 transformation
min(dat$Change_records)  # -99.2528
# add an offset of 100 to Change_records as the values need to be positive to log them
dat$log10_Change_records = log10(dat$Change_records + 100) 
# convert economy group to numeric to check for outliers again
dat$economyc_nr = substr(dat$economy_coarse, 1, 1) %>% as.numeric()
pairs(dat[,c("Stringency_index", "Change_park_visits","Change_time_at_home", "log10_pop_est", "economyc_nr", "log10_Change_records")])
# things look better now!
# save dataset to file
write.table(dat, "C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/LinRegr_fullData_20240820.csv", sep=";", dec=",", row.names=F)

# check for correlation among explanatory variables
cor(dat[,c("Stringency_index", "Change_park_visits", "Change_time_at_home", "log10_pop_est","economyc_nr")], 
    method="spearman") # Change_park_visits and Change_time_at_home are strongly correlated!
# keep change in park visitors, we will not use change in time at home in the model

# fit a linear model to the data
fit1 = lm(log10_Change_records ~ Stringency_index + Change_park_visits + log10_pop_est + 
             economy_coarse, data=dat)
summary(fit1)#; plot(fit1)
res1 = simulateResiduals(fit1, plot=T)  # outlier test significant! 
# identify and remove outliers
outs_nr = outliers(res1, lowerQuantile = 0, upperQuantile = 1,
         return = c("index"))
# outlier countries are: Bahrain, Niger (very low Change_records), Kyrgyzstan and Togo (very high Change_records)
dat = dat[-c(outs_nr),]
# refit the model and check the residuals
# plot simulated residuals against each covariate
res1 = simulateResiduals(fit1, plot=T)  # no significant deviations
DHARMa::plotResiduals(res1, form = dat$Stringency_index)
DHARMa::plotResiduals(res1, form = dat$Change_park_visits)
DHARMa::plotResiduals(res1, form = dat$log10_pop_est) # combined adjusted quantile test significant!
DHARMa::plotResiduals(res1, form = dat$economy_coarse) # significant heteroskedasticity!
testCategorical(res1, catPred = dat$economy_coarse)
# Levene test for homogeneity of variance is significant for economy_coarse
E = rstandard(fit1)  
coplot(E ~ fitted(fit1) | economy_coarse, data = dat)

# expand the GLM with a GLS with different variance structure for each economy class
mgls <- nlme::gls(log10_Change_records ~ Stringency_index + Change_park_visits + log10_pop_est + 
                  economy_coarse, data=dat, weights = varIdent(form= ~ 1 | economy_coarse))
summary(mgls) #; plot(mgls)
plot(resid(mgls, type="normalized") ~ fitted(mgls))
plot(resid(mgls, type="normalized") ~ dat$Stringency_index)
plot(resid(mgls, type="normalized") ~ dat$log10_pop_est)
boxplot(resid(mgls, type="normalized") ~ dat$economy_coarse)
E = resid(mgls, type="normalized")  
coplot(E ~ fitted(mgls) | economy_coarse, data = dat) # variance is now homogeneous across economic classes

# plot conditional plots for the model's variables, using the effects package
plot(allEffects(mgls))

# plot the conditional effects for each predictor
p1 = ggplot(data.frame(effect("Stringency_index", mgls, xlevels=100)),
  aes(x=Stringency_index, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Stringency index") + ylim(1.6, 2.4)

p2 = ggplot(data.frame(effect("Change_park_visits", mgls, xlevels=100)),
            aes(x=Change_park_visits, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Change in park visitors (%)") + ylim(1.6, 2.4)

p3 = ggplot(data.frame(effect("log10_pop_est", mgls, xlevels=100)),
            aes(x=log10_pop_est, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("log10(Population size)") + ylim(1.6, 2.4)

p4 = ggplot(data.frame(effect("economy_coarse", mgls))) +
  geom_errorbar( aes(x=economy_coarse, ymin=lower, ymax=upper), width=0.4, colour="lightblue", alpha=0.9, size=1) +
  geom_point(aes(economy_coarse, fit), size=2) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Economic class")+ 
  ylim(1.6, 2.4)+theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=0.8))
grid.arrange(p1, p2, p3, p4, ncol=2) # saved as .pdf, 6x6in

# remove outliers also in the wmap shapefile
wmap = wmap[which(wmap$Country %in% dat$Country),]

### WORLD MAP -------------------------------------
# read data from all countries, with downloaded GBIF observations, stringency index and movement data (see script "Data_download.R" for details on its preparation)
allcn = fread("C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/Anthropause_app/Data_249_countries_20230321.csv", encoding = 'Latin-1') 
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO","Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
start_date = "2020-03-15"
end_date = "2020-05-01"
# compute mean number of records per day, across the selected period, for all countries 
cntall = allcn %>% filter(Date>=start_date & Date<=end_date) %>%
  group_by(Country) %>% summarise_at(vars(Nr_records), mean, na.rm=T)
# compute the mean daily records in the same period of the previous year, to be used as baseline
cntall_PY = allcn %>% filter(Date >= (as.Date(start_date) - years(1)) & Date <= (as.Date(end_date) - years(1))) %>%
  group_by(Country) %>% summarise_at(vars(Nr_records), mean, na.rm=T)
# bind to the other dataframe
cntall$Records_prev_year = cntall_PY$Nr_records[match(cntall$Country, cntall_PY$Country)]
# compute the percent change in records collected between the two time periods (lockdown vs. pre-pandemic)
cntall$Change_records = ifelse(cntall$Records_prev_year>0, (cntall$Nr_records - cntall$Records_prev_year)/cntall$Records_prev_year*100, NA)
# add iso3 and iso2 codes so that the dataframe can be matched to the World map
cntall[cntall$Country == "Turkey", "Country"] <- "Türkiye"  # correct the name of Turkey
cntall["adm0_a3"] <- ISO_3166_1$Alpha_3[match(cntall$Country, ISO_3166_1$Name)]
cntall["Country_code"] <- ISO_3166_1$Alpha_2[match(cntall$Country, ISO_3166_1$Name)]
# load shapefile of the countries of the world (rnaturalearth package version 1.0.1, and rnaturalearthdata version 1.0.0)
wmap_all = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
  select(c("name_long", "adm0_a3","pop_est", "economy", "income_grp"))
# correct one country code 
wmap_all$adm0_a3[which(wmap_all$name_long=="South Sudan")] <- "SSD"
# merge dataframe and shapefile, retain only countries with information
wmap = merge(wmap_all, cntall, by = "adm0_a3", all.y=T)
# remove empty geometries
wmap = wmap %>% filter(!st_is_empty(.))
# make a world map with Change_records as cloropleth
sf::sf_use_s2(FALSE)
wmap_all = wmap_all[-which(wmap_all$name_long=="Antarctica"),] # remove Antarctica to gain more space
changemap = tm_shape(wmap_all) + tm_borders(col="darkgrey") +
  tm_shape(wmap) + tm_polygons("Change_records", palette="RdYlBu", 
                                breaks = c( -100, -50, -25, -10, 0, 10, 25, 50, 100, 200)) + 
  tm_layout(legend.text.size = 0.8, legend.outside=T) + tmap_options(check.and.fix = TRUE)
tmap_save(changemap, width= 6, height=3, filename="C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/images/Worldmap_change_records_20240912.png")

#### COUNTRY LIST by eBird proportions ---------------------
# read data from all countries
allcn = fread("C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/Anthropause_app/Data_249_countries_20230321.csv") 
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
write.table(cntall, "C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/Countries_by_eBird_proportions_20230509.csv", sep=";", dec=",", row.names=F)

## repeat but selecting only the 2019 baseline period (2019-03-15 to 2019-05-01)
dat = read.table("C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/LinRegr_fullData_20240820.csv", sep=";", dec=",", header=T)
allcn = fread("C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/Anthropause_app/Data_249_countries_20230321.csv", encoding = 'Latin-1')
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO","Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
cntall = allcn %>% filter(Date>="2019-03-15" & Date<="2019-05-01") %>%
  group_by(Country) %>% summarise_at(vars(Nr_records, n_CLO), sum, na.rm=T)
# calculate ratio of eBird/GBIF data
cntall$eBird_prop = round(cntall$n_CLO/cntall$Nr_records*100, digits=0)
# merge with the other economy group information
cntall[cntall$Country == "Turkey", "Country"] <- "Türkiye"  # correct the name of Turkey
cntall$economy_coarse = dat$economy_coarse[match(cntall$Country, dat$Country)]
# remove NAs (countries with no economy group due to e.g. too few records, or missing stringency or mobility data)
cntall = na.omit(cntall)
cntall = cntall[order(cntall$n_CLO, decreasing=T),]
write.table(cntall, "C:/Users/steph/Documents/BESTMAP documents/Papers/GBIF_COVID19/GBIF/Table_S3_20240919.csv", sep=";", dec=",", row.names=F)

