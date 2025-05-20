# custom GBIF download
# https://www.gbif.org/occurrence/download?basis_of_record=HUMAN_OBSERVATION&month=3&month=4&month=5&year=2019,2022&occurrence_status=present

# Title: GBIF-COVID-9 analysis_20250130.R
# Purpose: Extract daily counts of species occurrence records (only those that are georeferenced and tagged as human observations) from the monthly snapshot of the
#          Global Biodiversity Information Facility (GBIF), dated January 1st, 2025. Merge them with information on human mobility and stringency index of lockdown 
#          regimes in different countries.
#
# Author: Stephanie Roilo, TUD Dresden University of Technology, Germany & University of Bonn, Germany
# Date: last edited on January 30th, 2025
#

library(arrow)
# library(rgbif)  
library(dplyr)   
library(lubridate)
library(data.table)  # for fast reading of large datasets
library(ISOcodes)  # to access the ISO 3166 list of country codes
library(DHARMa)
library(nlme)     # for GLS
library(ggplot2)
library(rnaturalearth)
library(sf)
# library(tmap)
# library(gridExtra)
# library(effects)  # for plotting conditional effects of the models
# library(npreg)  # for non-parametric smoothing splines

# load daily obs. data
load("data/data_n_obs.RData")


### Stringency index and mobility reports -------------------

# download Covid OWID (Our World In Data) data and the Google Covid-19 community mobility report data
covid = fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
mob = fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

# set start and end dates of the time period of interest
start_date = "2019-01-01"  # to be entered in the format yyyy-mm-dd
end_date = "2022-10-15"
# filter the data to the time period of interest
allcn = data_obs %>% filter(
  eventDate >= start_date, 
  eventDate <= end_date)
# add the 3-lettered isocode and the country name to the occurrence record dataset
allcn$iso_code = ISO_3166_1$Alpha_3[match(allcn$countrycode, ISO_3166_1$Alpha_2)]
allcn$Country = ISO_3166_1$Name[match(allcn$countrycode, ISO_3166_1$Alpha_2)]
# check which countries are missing in the ISO 3166-1 list
unique(allcn$countrycode[is.na(allcn$iso_code)])   # XK = Kosovo, and ZZ = "High Seas" -> observations in international waters
# fix this
allcn[allcn$countrycode=="XK", "iso_code"] = "XKX"
allcn[allcn$countrycode=="XK", "Country"] = "Kosovo"
# remove the "High Seas" observations
allcn = allcn[-which(allcn$countrycode=="ZZ"),]

# filter the covid data to be merged with the occurrence record dataset
covid = covid %>% 
  select(iso_code, location, date, stringency_index, population) %>%
  filter(
    date >= start_date, 
    date <= end_date)
# fix isocodes for Kosovo, and remove the multi-country entries (e.g. "World", continents, etc. which are preceded by OWID_)
covid$iso_code[covid$location=="Kosovo"] = "XKX"
covid = covid[!grepl("OWID", covid$iso_code),]
# merge the covid data with the occurrence record dataset
allcn = merge(allcn, covid, by.x = c("iso_code", "eventDate"), by.y = c("iso_code", "date"), all.x = TRUE, all.y =FALSE)

# merge the google mobility reports by country and bind them to the dataframe
# filter out all rows which have an iso_3166_2_code, as these mark subregions within a country
mob = mob %>% filter(
  iso_3166_2_code == "",
  sub_region_1 == "",
  sub_region_2 == "",
  metro_area == "")
allcn = merge(allcn, mob[,c("country_region_code", "date", "parks_percent_change_from_baseline", "residential_percent_change_from_baseline")], by.x = c("countrycode", "eventDate"), by.y = c("country_region_code", "date"),  all.x = TRUE, all.y =FALSE)

# add info on day of week, so that we can identify the weekends
allcn$weekday = weekdays(allcn$eventDate)
#allcn$weekday = strftime(allcn$eventDate, "%u") # get weekday in number format (1 is Monday)
allcn$weeknr = strftime(allcn$eventDate, format = "%V") # get number of the week
# remove the column "location", containing country name from the OWID covid dataset
allcn = allcn[,c(1:9, 11:16)]

# save to file
write.table(allcn, file = "Data_250_countries_Jan2025snapshot_20250109.csv", sep = ",", dec = ".", row.names = FALSE)

rm(covid, mob)


### Model effect of lockdown on changes in data collection ------------------------
allcn = fread("Data_250_countries_Jan2025snapshot_20250109.csv")

# rename columns
names(allcn)[10:13] <- c("Stringency_index","Population", "Change_park_visitors", "Change_time_at_home")
allcn$eventDate = as.Date(allcn$eventDate)

# calculate the number of records per country for the time period of the lockdown
nrec = allcn %>% filter(eventDate>=as.Date("2020-03-15") & eventDate<=as.Date("2020-05-01")) %>%
  group_by(Country) %>% 
  summarise_at(vars(n, n_eBird), sum)
# calculate the mean of the stringency index, the change in park visitors, and the change in time spent at home per country
vars = allcn %>% filter(eventDate>=as.Date("2020-03-15") & eventDate<=as.Date("2020-05-01")) %>%
  group_by(Country) %>% 
  summarise_at(vars(Stringency_index, Change_park_visitors, Change_time_at_home, Population), mean, na.rm=T) 
# merge the two dataframes
cntall = merge(nrec, vars, by = "Country", all = TRUE)
# add country codes and names
cntall = merge(cntall, allcn[!duplicated(allcn$Country),c("Country", "countrycode", "iso_code")], by = "Country", all.x = TRUE, all.y = FALSE)
# omit countries for which one (or more) of the variables is NA
cntall = na.omit(cntall)  # only 129 countries left
# calculate the number of records per country for the same time period in 2019
nrecPY = allcn %>% filter(eventDate>=as.Date("2019-03-15") & eventDate<=as.Date("2019-05-01")) %>%
  group_by(Country) %>% 
  summarise_at(vars(n, n_eBird), sum)
# bind to the other dataframe
cntall$n_prev_year = nrecPY$n[match(cntall$Country, nrecPY$Country)]
cntall$n_eBird_prev_year = nrecPY$n_eBird[match(cntall$Country, nrecPY$Country)]
# compute the percent change in records collected between the two time periods (lockdown vs. pre-pandemic)
cntall$Change_records = ifelse(cntall$n_prev_year>0, (cntall$n - cntall$n_prev_year)/cntall$n_prev_year*100, NA)
# compute the percent change in eBird records collected between the two time periods (lockdown vs. pre-pandemic)
cntall$Change_eBird_records = ifelse(cntall$n_eBird_prev_year>0, (cntall$n_eBird - cntall$n_eBird_prev_year)/cntall$n_eBird_prev_year*100, NA)

# add data on economic class and income group from the NaturalEarth dataset
# load shapefile of the countries of the world (rnaturalearth package version 1.0.1, and rnaturalearthdata version 1.0.0)
wmap_all = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
  select(c("name_long", "adm0_a3", "economy", "income_grp"))

# merge dataframe and shapefile, retain only countries with information
wmap = merge(wmap_all, cntall, by.x = "adm0_a3", by.y ="iso_code", all.x=F, all.y=T)

###' REGRESSION MODEL ----------------------------------
# prepare data for modelling
dat = st_drop_geometry(wmap) 
# omit countries for which one (or more) of the variables is NA
dat = dat %>% filter(!is.na(n),
                     !is.na(Stringency_index), 
                     !is.na(Population), 
                     !is.na(Change_park_visitors), 
                     !is.na(Change_time_at_home))  # 129 countries left

# Economy groups are many and some have only few countries; merge some economy categories together into larger groups
dat$economy_coarse = "4. Least developed region"
dat$economy_coarse[which(dat$economy %in% c("1. Developed region: G7", "2. Developed region: nonG7"))] <- "1. Developed region"
dat$economy_coarse[which(dat$economy %in% c("3. Emerging region: BRIC","4. Emerging region: MIKT", "5. Emerging region: G20"))] <- "2. Emerging region"
dat$economy_coarse[which(dat$economy %in% c("6. Developing region"))] <- "3. Developing region"

# check for potential outliers
pairs(dat[,c("Stringency_index", "Change_park_visitors","Change_time_at_home", "Population", "Change_records")])
# Population and Change_records have some outliers; let's apply a log10 transformation
dat$log10_Population = log10(dat$Population)
# Change_Records can vary between -100 and +Inf, 
# check that there are no values equal to -100 in Change_records before applying the log10 transformation
min(dat$Change_records)  # -99.13151
# add an offset of 100 to Change_records as the values need to be positive to log them
dat$log10_Change_records = log10(dat$Change_records + 100) 
# convert economy group to numeric to check for outliers again
dat$economyc_nr = substr(dat$economy_coarse, 1, 1) %>% as.numeric()
pairs(dat[,c("Stringency_index", "Change_park_visitors","Change_time_at_home", "log10_Population", "economyc_nr", "log10_Change_records")])
# things look better now!
# save dataset to file
write.table(dat, "LinRegr_fullData_Jan2025snapshot_20250109.csv", sep=";", dec=",", row.names=F)

# check for correlation among explanatory variables
cor(dat[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home", "log10_Population","economyc_nr")], 
    method="spearman") # Change_park_visitors and Change_time_at_home are strongly correlated!
# keep change in park visitors, we will not use change in time at home in the model

# fit a linear model to the data
fit1 = lm(log10_Change_records ~ Stringency_index + Change_park_visitors + log10_Population + 
            economy_coarse, data=dat)
summary(fit1)
res1 = simulateResiduals(fit1, plot=T)  # outlier test significant, and other significant deviations in the residuals!
# inspect residuals against single predictors
DHARMa::plotResiduals(res1, form = dat$Stringency_index)
DHARMa::plotResiduals(res1, form = dat$Change_park_visits)
DHARMa::plotResiduals(res1, form = dat$log10_Population) 
DHARMa::plotResiduals(res1, form = dat$economy_coarse) # significant heteroscedasticity!
testCategorical(res1, catPred = dat$economy_coarse)
# Levene test for homogeneity of variance is significant for economy_coarse
E = rstandard(fit1)  
coplot(E ~ fitted(fit1) | economy_coarse, data = dat)
# identify and remove outliers
#outs_nr = outliers(res1, lowerQuantile = 0, upperQuantile = 1, return = c("index"))
# outlier countries are: Bahrain, Gabon, Niger and Yemen, all with very low Change_records
#dat1 = dat[-c(outs_nr),]
# expand the GLM with a GLS with different variance structure for each economy class
mgls <- nlme::gls(log10_Change_records ~ Stringency_index + Change_park_visitors + log10_Population + 
                    economy_coarse, data=dat, weights = varIdent(form= ~ 1 | economy_coarse))
summary(mgls) #; plot(mgls)
anova(mgls)
plot(resid(mgls, type="normalized") ~ fitted(mgls))
plot(resid(mgls, type="normalized") ~ dat$Stringency_index)
plot(resid(mgls, type="normalized") ~ dat$log10_Population)
boxplot(resid(mgls, type="normalized") ~ dat$economy_coarse)
E = resid(mgls, type="normalized")  
coplot(E ~ fitted(mgls) | economy_coarse, data = dat) # variance is now homogeneous across economic classes

# plot conditional plots for the model's variables, using the effects package
plot(allEffects(mgls))

# plot the conditional effects for each predictor
p1 = ggplot(data.frame(effect("Stringency_index", mgls, xlevels=100)),
            aes(x=Stringency_index, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Stringency index") + ylim(1.35, 2.4)

p2 = ggplot(data.frame(effect("Change_park_visitors", mgls, xlevels=100)),
            aes(x=Change_park_visitors, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Change in park visitors (%)") + ylim(1.35, 2.4)

p3 = ggplot(data.frame(effect("log10_Population", mgls, xlevels=100)),
            aes(x=log10_Population, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("log10(Population size)") + ylim(1.35, 2.4)

p4 = ggplot(data.frame(effect("economy_coarse", mgls))) +
  geom_errorbar( aes(x=economy_coarse, ymin=lower, ymax=upper), width=0.4, colour="lightblue", alpha=0.9, linewidth=1) +
  geom_point(aes(economy_coarse, fit), size=2) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Economic class")+ 
  ylim(1.35, 2.4)+theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=0.8))
gp = grid.arrange(p1, p2, p3, p4, ncol=2) # saved as .pdf, 6x6in
# save plot to file
ggsave(plot=gp, "Conditional_effects_20250122.png", width= 6, height=6)  # also saved as pdf 6x6 inches

### Potential interactions between stringency and economic class ---------------
mglsi <- nlme::gls(log10_Change_records ~ Stringency_index + Change_park_visitors + log10_Population + economy_coarse + Stringency_index:economy_coarse, data=dat, weights = varIdent(form= ~ 1 | economy_coarse))
summary(mglsi)
# check significance of predictors
anova(mglsi) # interaction term non-significant
# for model selection, see Zuur et al (2009), pages 121-122: "To compare models with nested fixed effects (but with the same random structure), ML estimation must be used and not REML."
# also read page 223 "Two Ways of Using the Anova Command" in the same book
# compare the two models
anova(update(mgls, . ~ ., method = "ML"), update(mglsi, . ~ ., method = "ML"))
# model diagnostics
plot(resid(mglsi, type="normalized") ~ fitted(mglsi))
plot(resid(mglsi, type="normalized") ~ dat$Stringency_index)
plot(resid(mglsi, type="normalized") ~ dat$log10_Population)
boxplot(resid(mglsi, type="normalized") ~ dat$economy_coarse)
E = resid(mglsi, type="normalized")  
coplot(E ~ fitted(mglsi) | economy_coarse, data = dat) 
# plot conditional plots for the model's variables
plot(allEffects(mglsi))
# produce prettier plots per covariate
p5 = ggplot(data.frame(effect("Stringency_index", mglsi, xlevels=100)),
            aes(x=Stringency_index, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Stringency index") + ylim(1.35, 2.6)

p6 = ggplot(data.frame(effect("Change_park_visitors", mglsi, xlevels=100)),
            aes(x=Change_park_visitors, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Change in park visitors (%)") + ylim(1.35, 2.6)

p7 = ggplot(data.frame(effect("log10_Population", mglsi, xlevels=100)),
            aes(x=log10_Population, y=fit)) +
  geom_smooth(aes(ymin=lower, ymax=upper), stat="identity", color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("log10(Population size)") + ylim(1.35, 2.6)

p8 = ggplot(data.frame(effect("economy_coarse", mglsi))) +
  geom_errorbar( aes(x=economy_coarse, ymin=lower, ymax=upper), width=0.4, colour="lightblue", alpha=0.9, linewidth=1) +
  geom_point(aes(economy_coarse, fit), size=2) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Economic class")+ 
  ylim(1.35, 2.6)+theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=0.8))

efdf = data.frame(effect("Stringency_index:economy_coarse", mglsi))
p9 = ggplot(efdf) +
  # divide facets by economic class, in two rows of two facets
    facet_wrap(vars(economy_coarse), ncol=2) +
  # plot change_records against stringency index
  geom_line(aes(x=Stringency_index, y=fit), color="black") +
  geom_ribbon(aes(x=Stringency_index, ymin=lower, ymax=upper), fill="lightblue", alpha=0.5) +
  theme_bw() + ylab("log10(Change_records +100)") + xlab("Stringency index") + ylim(0.15, 3)
gp1 = grid.arrange(p5, p6, p7, p8, p9, ncol=2) # saved as .pdf, 6x6in

### WORLD MAP -----------------------------
# load shapefile of the countries of the world (rnaturalearth package version 1.0.1, and rnaturalearthdata version 1.0.0)
wmap_all = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
  select(c("name_long", "iso_a2_eh"))

# make a map of the new Change_records
# remove empty geometries
wmap = wmap %>% filter(!st_is_empty(.))
# make a world map with Change_records 
sf::sf_use_s2(FALSE)
wmap_all = wmap_all[-which(wmap_all$name_long=="Antarctica"),] # remove Antarctica to gain more space
changemap = tm_shape(wmap_all) + tm_borders(col="darkgrey") +
  tm_shape(wmap) + tm_polygons("Change_records", 
                               breaks = c(-100, -50, -25, -10, 0, 10, 25, 50, 100, 1500),
                               palette=c("#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"), 
                               labels = c("-100 to -50", "-50 to -25", "-25 to -10", "-10 to 0", "0 to 10", "10 to 25", "25 to 50", "50 to 100", "100 and above")) + 
  tm_layout(legend.text.size = 0.8, legend.outside=T) 
tmap_save(changemap, width= 6, height=3, filename="Worldmap_change_records_20250123.png")
# save to file cntall including Change_recrods also for countries not included in the linear model
write.table(cntall, "Change_records_AllCountries_20250109.csv", sep = ";", dec = ",", row.names = FALSE)

# make a map of the eBird change_records
changemap_ebird = tm_shape(wmap_all) + tm_borders(col="darkgrey") +
  tm_shape(wmap) + tm_polygons("Change_eBird_records", 
                               breaks = c(-100, -50, -25, -10, 0, 10, 25, 50, 100, 3814),
                               palette=c("#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"), 
                               labels = c("-100 to -50", "-50 to -25", "-25 to -10", "-10 to 0", "0 to 10", "10 to 25", "25 to 50", "50 to 100", "100 and above")) + 
 tm_layout(legend.text.size = 0.8, legend.outside=T) 
tmap_save(changemap_ebird, width= 6, height=3, filename="Worldmap_change_eBird_records_20250123.png")


### Model trends in data across time ----------------
# smooth the trend in number of records collected between March 15th and May 1st every year
# load aggregated counts of records for the time window March 15th to May 1st
data_sum_marmay = read.table("Records_per_country_MarMay_Jan2025snapshot_20250109.csv", sep = ";", dec = ",", header=T)
# remove 2024 as data for that year still needs to be uploaded to the platform
data_sum_marmay = data_sum_marmay[which(data_sum_marmay$year!=2024),]
# filter out countries not included in the linear model
dat = read.table("LinRegr_fullData_Jan2025snapshot_20250109.csv", sep=";", dec=",", header=T)
# make sure that the country code for Namibia is not read as an NA
dat$countrycode[dat$Country == "Namibia"] = as.character("NA")
data_sum_marmay$countrycode[is.na(data_sum_marmay$countrycode)] = as.character("NA")

# loop through countries
for (cnt in dat$countrycode){
  cntdat = data_sum_marmay[which(data_sum_marmay$countrycode==cnt),]
  # with smoothing parameter = 0.5
  smooth05 = npreg::ss(cntdat$year, cntdat$count, spar=0.5)
  png(file=paste0("~/images/smooths_spar05/Smooth05_MarMay_", cnt, ".png"), width= 800, height=500)
  plot(smooth05, main = paste0("Trend in GBIF records in ", dat$Country[dat$countrycode==cnt][1], " between March 15 and May 1; smoothing parameter = 0.5"),
       xlab="Year", ylab="Number of records")
  points(cntdat$count ~ cntdat$year, col="black", pch=19)
  dev.off()
  # with smoothing parameter = 0.4
  smooth04 = npreg::ss(cntdat$year, cntdat$count, spar=0.4)
  png(file=paste0("~/images/smooths_spar04/Smooth04_MarMay_", cnt, ".png"), width= 800, height=500)
  plot(smooth04, main = paste0("Trend in GBIF records in ", dat$Country[dat$countrycode==cnt][1], " between March 15 and May 1; smoothing parameter = 0.4"),
       xlab="Year", ylab="Number of records")
  points(cntdat$count ~ cntdat$year, col="black", pch=19)
  dev.off()
  # with smoothing parameter = 0.6
  smooth06 = npreg::ss(cntdat$year, cntdat$count, spar=0.6)
  png(file=paste0("~/images/smooths_spar06/Smooth06_MarMay_", cnt, ".png"), width= 800, height=500)
  plot(smooth06, main = paste0("Trend in GBIF records in ", dat$Country[dat$countrycode==cnt][1], " between March 15 and May 1; smoothing parameter = 0.6"),
       xlab="Year", ylab="Number of records")
  points(cntdat$count ~ cntdat$year, col="black", pch=19)
  dev.off()
  # extract predicted values and standard errors at the year 2020
  dat$smooth05_y[dat$countrycode == cnt] = smooth05$y[11]
  dat$smooth05_se[dat$countrycode == cnt] = predict(smooth05, cntdat$year, se.fit=TRUE)$se[11]
  dat$smooth04_y[dat$countrycode == cnt] = smooth04$y[11]
  dat$smooth04_se[dat$countrycode == cnt] = predict(smooth04, cntdat$year, se.fit=TRUE)$se[11]
  dat$smooth06_y[dat$countrycode == cnt] = smooth06$y[11]
  dat$smooth06_se[dat$countrycode == cnt] = predict(smooth06, cntdat$year, se.fit=TRUE)$se[11]
}
# save dataset to file
dat = dat[,c("countrycode", "Country", "economy_coarse", "n", "smooth05_y", "smooth05_se", "smooth04_y", "smooth04_se", "smooth06_y", "smooth06_se")]
# round to zero digits the predicted and se values
dat[,5:10] = round(dat[,5:10], digits=0)
# calculate the upper and lower confidence interval limits for each smooth
dat$smooth05_lwr = dat$smooth05_y - 2*dat$smooth05_se
dat$smooth05_upr = dat$smooth05_y + 2*dat$smooth05_se
dat$smooth04_lwr = dat$smooth04_y - 2*dat$smooth04_se
dat$smooth04_upr = dat$smooth04_y + 2*dat$smooth04_se
dat$smooth06_lwr = dat$smooth06_y - 2*dat$smooth06_se
dat$smooth06_upr = dat$smooth06_y + 2*dat$smooth06_se
# check whether the actual number of records in 2020 is within the confidence interval for each smooth
dat$inCI05 = ifelse(dat$n < dat$smooth05_lwr, "below", "within")
dat$inCI05 = ifelse(dat$n > dat$smooth05_upr, "above", dat$inCI05)
dat$inCI04 = ifelse(dat$n < dat$smooth04_lwr, "below", "within")
dat$inCI04 = ifelse(dat$n > dat$smooth04_upr, "above", dat$inCI04)
dat$inCI06 = ifelse(dat$n < dat$smooth06_lwr, "below", "within")
dat$inCI06 = ifelse(dat$n > dat$smooth06_upr, "above", dat$inCI06)

# save to file
write.table(dat, "Smoothed_trends_MarMay_20250119.csv", sep=";", dec=",", row.names=F)
