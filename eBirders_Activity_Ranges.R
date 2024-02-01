###################################################
#
# Title: eBirders_Activity_Ranges.R
# Purpose: compute the number of active eBirders per country, identify the most active eBirders per country
#          and calculate the area of their activity ranges.
#          
# Author: Stephanie Roilo, Technische Universität Dresden 
# Date: started on December 20th 2022, last edited on February 1st 2024
#
###################################################
# set the language to EN
Sys.setenv(LANGUAGE="en")
library(rgbif)
library(ggplot2)
library(dplyr)
library(viridis) 
library(ggExtra)
library(sf)
library(data.table)
library(units)
library(rnaturalearth)
library(rnaturalearthdata)
library(ISOcodes)
library(hrbrthemes)
library(ggrepel)  # to avoid overlapping text labels in ggplots
library(tidyr)  # to convert from wide to long format of dataframes

### download only the data from eBird, and extract the metadata on individual users -------------
### NOTE: this part was run in the HPC
country_iso2 = "IN"
# create a dataframe in which each row corresponds to a day
# we only select the dates between 15th of March and 1st of May in 2019 and 2020
dates = data.frame(Date = c(seq(as.Date("2019-03-15"), as.Date("2019-05-01"), by="days"),
                            seq(as.Date("2020-03-15"), as.Date("2020-05-01"), by="days")) )
# loop through each day and extract the number of eBird records, the number of eBirders active on that day, and their unique IDs
for (i in c(1:nrow(dates))) {
  date = as.character(dates$Date[i])
  daydat = occ_data(country=country_iso2, basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, 
                    eventDate = date, institutionCode = "CLO", limit=100000)
  # data download has a hard limit of 100000, if records are more than that, set the result to NA
  dates$n_CLO[i] = daydat$meta$count
  dates$n_obsr[i] = ifelse(daydat$meta$count > 100000, NA, length(unique(daydat$data$recordedBy)) )
  dates$ID_obsr[i] = ifelse(daydat$meta$count > 100000, NA, paste(sort(unique(daydat$data$recordedBy)), collapse=" | ") )
  write.table(dates, paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/CLO_", country_iso2, "_March15_May1_2019_2020.csv"), sep=";", dec=".", row.names = F)
}  

## for the USA, compile the data at the state level, to avoid too large datasets (>100000 records)
usa_cnts = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
             "District of Columbia", "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
             "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",  
             "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
             "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
             "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
             "West Virginia","Wisconsin","Wyoming")   # 51 states, including District of Columbia

for (uscnt in usa_cnts) {
  dates = data.frame(Date = c(seq(as.Date("2019-03-15"), as.Date("2019-05-01"), by="days"),
                              seq(as.Date("2020-03-15"), as.Date("2020-05-01"), by="days")) )
  # loop through each day and extract the number of eBird records, the number of eBirders active on that day, and their unique IDs
  for (i in c(1:nrow(dates))) {
    date = as.character(dates$Date[i])
    daydat = occ_data(country="US", stateProvince = uscnt, basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, 
                      eventDate = date, institutionCode = "CLO", limit=100000)
    # data download has a hard limit of 100000, if records are more than that, set the result to NA
    dates$n_CLO[i] = daydat$meta$count
    dates$n_obsr[i] = ifelse(daydat$meta$count > 100000, NA, length(unique(daydat$data$recordedBy)) )
    dates$ID_obsr[i] = ifelse(daydat$meta$count > 100000, NA, paste(sort(unique(daydat$data$recordedBy)), collapse=" | ") )
    write.table(dates, paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/US/CLO_US_", uscnt, "_March15_May1_2019_2020.csv"), sep=";", dec=".", row.names = F)
  }  
}
# now, calculate the total of eBird records and of unique observers throughout the USA
dates = data.frame(Date = c(seq(as.Date("2019-03-15"), as.Date("2019-05-01"), by="days"),
                            seq(as.Date("2020-03-15"), as.Date("2020-05-01"), by="days")) )
for (i in c(1:nrow(dates))) {
  date = as.character(dates$Date[i])
  daydat = occ_data(country="US", basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, 
                    eventDate = date, institutionCode = "CLO", limit=1)
  # read through all the single countries' data and put everything together
  all_obsr = read.table(paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/US/CLO_US_Alabama_March15_May1_2019_2020.csv"), sep=";", dec=".", header=T)[i, "ID_obsr"]
  for (uscnt in usa_cnts[2:51]) {
    cntdat = read.table(paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/US/CLO_US_", uscnt, "_March15_May1_2019_2020.csv"), sep=";", dec=".", header=T)
    all_obsr = paste0(all_obsr, " | ", cntdat[i,"ID_obsr"]) 
  }
  all_obsr_list = all_obsr %>% strsplit(split=" | ", fixed=T) 
  dates$n_CLO[i] = daydat$meta$count
  dates$n_obsr[i] = length(unique(unlist(all_obsr_list)))
  dates$ID_obsr[i] = paste(unique(unlist(all_obsr_list)), collapse= " | ")
  write.table(dates, paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/CLO_US_March15_May1_2019_2020.csv"), sep=";", dec=".", row.names = F)
} 
rm(dates, cntdat, all_obsr, all_obsr_list, uscnt, usa_cnts, daydat)

### CHANGE IN NUMBER of eBird records and of eBirders ------------------------------------
# list of countries selected for the analysis (10 per economic class) :
cnt_list = c("US", "CA", "ES", "AU", "GB", "PT", "IL", "DE", "FR", "SE",  # Developed region
             "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA",  # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  # least developed

# load the full country dataset and count total eBirders active during the two periods
tot_ebird = data.frame(Country = cnt_list)
for (i in c(1:length(cnt_list))) {
  cntr = tot_ebird$Country[i]
  dat = read.table(paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/CLO_", cntr, "_March15_May1_2019_2020.csv"), sep=";", dec=",", header=T)
  tot_ebird$eBird_records_2019[i] = sum(dat$n_CLO[dat$Date<= "2019-05-01" & dat$Date>= "2019-03-15"])
  tot_ebird$eBird_records_2020[i] = sum(dat$n_CLO[dat$Date<= "2020-05-01" & dat$Date>= "2020-03-15"])
  allobsr_2019 = paste(dat$ID_obsr[which(dat$Date<= "2019-05-01" & dat$Date>= "2019-03-15" & dat$ID_obsr!="")], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  tot_ebird$eBirders_2019[i] = length(unique(unlist(allobsr_2019)))
  allobsr_2020 = paste(dat$ID_obsr[which(dat$Date<= "2020-05-01" & dat$Date>= "2020-03-15" & dat$ID_obsr!="")], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  tot_ebird$eBirders_2020[i] = length(unique(unlist(allobsr_2020)))
}
# add info on stringency index and mobility data to the dataframe
cntall = read.table("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/Countries_by_eBird_proportions_20230509.csv", sep=";", dec=",", header=T)
names(cntall)[1] <- "Country_name"
cntall$Country = cntall$Country_code
tot_ebird = merge(tot_ebird, cntall[,c("Country_name", "economy_coarse", "Stringency_index", "Change_park_visits","Country")], by="Country")

# make cleveland dotplots to show differences in eBird records and in eBirders before (2019) and during (2020) lockdown
# order countries by economic class and number of eBird records in 2019
tot_ebird2 <- tot_ebird %>%
  mutate( Country =factor(Country,levels=c("US", "CA", "ES", "AU", "GB", "PT", "IL", "DE", "FR", "SE",     # Developed region
                                           "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA",     # emerging region
                                           "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",     # developing region
                                           "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")))   # least developed region

# plot number of eBird records (log10-transformed), ordered by economic class
ggplot(tot_ebird2) +
  geom_segment( aes(x=Country, xend=Country, y=log10(eBird_records_2019), yend=log10(eBird_records_2020)), color="darkgrey", linewidth=1) +
  geom_point( aes(x=Country, y=log10(eBird_records_2019)), color=rgb(0,0,1,0.5), size=3 ) +
  geom_point( aes(x=Country, y=log10(eBird_records_2020)), color=rgb(1,0,0,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("log10(Number of eBird records)")

# plot number of eBirders, ordered by economic class
ggplot(tot_ebird2) +
  geom_segment( aes(x=Country, xend=Country, y=log10(eBirders_2019), yend=log10(eBirders_2020)), color="darkgrey", size=1) +
  geom_point( aes(x=Country, y=log10(eBirders_2019)), color=rgb(0,0,1,0.5), size=3 ) +
  geom_point( aes(x=Country, y=log10(eBirders_2020)), color=rgb(1,0,0,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("log10(Number of eBirders)")

# save tot_ebird dataframe to file
write.table(tot_ebird, "C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/Tot_eBird_20240130.csv", sep=";", dec=",", row.names=F)

### ACTIVITY RANGES of individual eBirders ------------------------
## subset the country selection and run the activity range analysis
cnt_list = c("US", "CA", "ES", "AU", "GB", "PT", "IL", "DE", "FR", "SE", # Developed region
             "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK", # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ") # least developed

# load the shapefile of the world's borders to retrieve information on which countries eBirders visited
# N.B. downloading the shapefile at large scale will require the installation of the rnaturalearthhires package
wmap = ne_countries(scale = "large", type = "countries", returnclass = c("sf")) %>% 
  select(c("name_long","pop_est", "economy", "income_grp", "iso_a2", "iso_a3", "region_un", "subregion"))
# some isocodes are wrong in the shapefile; correct them
wmap[wmap$name_long=="France", "iso_a2"] <- "FR"
wmap[wmap$name_long=="Norway", "iso_a2"] <- "NO"
wmap[wmap$name_long=="Taiwan", "iso_a2"] <- "TW"

lockdown_start = "2019-03-15"  
lockdown_end = "2019-05-01"
# Switch off spherical geometry (s2) to avoid problems in the area calculation for larger polygons;
# read more about it here: https://github.com/r-spatial/sf/issues/1902 
sf::sf_use_s2(FALSE)

for (country_iso2 in cnt_list) {
  # filter out the chosen country to intersect records of eBirders
  cntr_shp = wmap %>% filter(iso_a2 == country_iso2)
  # create a folder to store the results
  cntr.dir <- paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/spatial/", country_iso2)
  dir.create(cntr.dir, showWarnings=FALSE, recursive=TRUE)
  # load the full country dataset and filter eBirders that have been most active in spring 2019 (March 15th to May 1st)
  dat = read.table(paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/CLO_", country_iso2, "_March15_May1_2019_2020.csv"), sep=";", dec=",", header=T)
  # collect all the eBirder ID which were active in spring 2019 (time interval must correspond to lockdown period in 2020)
  allobsr = paste(dat$ID_obsr[which(dat$Date>=as.Date(lockdown_start) & dat$Date<=as.Date(lockdown_end))], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  # order by most frequent observer (by nr. of recording days)
  freq = data.frame(table(allobsr)); freq$allobsr = as.character(freq$allobsr)
  freq = freq[order(freq$Freq, decreasing=T),]
  # remove zeros (no observers) from the list
  if ("" %in% freq$allobsr) {freq = freq[-which(freq$allobsr==""),]}
  # prepare columns to save additional information for each eBirder
  freq$Nr_obs_2019 = NA; freq$Nr_obs_2020 = NA
  freq$HR_area_km2_2019 = NA; freq$HR_area_km2_2020 = NA
  freq$HR_area_km2_2019 = NA; freq$HR_area_km2_2020 = NA
  freq$Countries_2019 = NA; freq$Countries_2020 = NA; freq$Countries_code_2019 = NA; freq$Countries_code_2020 = NA
  # loop through the first 30 most active observers in 2019 and calculate home ranges in 2019 and 2020, lockdown period
  for ( i in c(1:min(30, nrow(freq)))) {
    obsrID = freq$allobsr[i]
    # download occurrences for the given time interval for 2019 and 2020 separately
    dat19 = occ_data(basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, eventDate=paste0(lockdown_start, ",", lockdown_end),  
                     institutionCode = "CLO", recordedBy=obsrID, limit=100000)
    dat20 = occ_data(basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, eventDate=paste0(gsub(x=lockdown_start, pattern="2019", replacement="2020"), ",", gsub(x=lockdown_end, pattern="2019", replacement="2020")),
                     institutionCode = "CLO", recordedBy=obsrID, limit=100000)
    # data is divided by year, put everything together
    df = bind_rows(dat19$data, dat20$data)
    # get rid of some columns and of multiple observations at the same location on the same day
    df = df[which(!duplicated(df[,c("decimalLatitude","decimalLongitude","eventDate","recordedBy")])),
            c("decimalLatitude","decimalLongitude","basisOfRecord","species", 
              "year", "month","day","eventDate","recordedBy","institutionCode", "locality" )]
    # save the number of distinct (geographically and/or temporally) observations for each year
    freq$Nr_obs_2019[i] = nrow(df[df$year==2019,])
    freq$Nr_obs_2020[i] = nrow(df[df$year==2020,])
    # convert data to spatial object 
    dfs = st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs="epsg:4326")
    # create minimum convex polygon around the points, divided by year
    mcp19 = st_convex_hull(st_union(dfs[dfs$year=="2019",]))
    if (nrow(dfs[dfs$year=="2020",]) >0) { mcp20 = st_convex_hull(st_union(dfs[dfs$year=="2020",])) } else {mcp20 = NA}
    # save the area of the observer's home range for each year, in square km 
    freq$HR_area_km2_2019[i] = round(set_units(st_area(mcp19), km^2), digits=2)
    freq$HR_area_km2_2020[i] = ifelse(is.na(mcp20), NA, round(set_units(st_area(mcp20), km^2), digits=2))
    # join with world's shapefile
    jdfs = st_join(dfs, wmap)
    # extract information on countries in which the eBirder recorded species in 2019 and 2020 during the lockdown period
    freq$Countries_2019[i] = paste(unique(sort(na.omit(jdfs$name_long[jdfs$year=="2019"]))), collapse =", ")
    freq$Countries_2020[i] = paste(unique(sort(na.omit(jdfs$name_long[jdfs$year=="2020"]))), collapse =", ")
    freq$Countries_code_2019[i] = paste(unique(sort(na.omit(jdfs$iso_a2[jdfs$year=="2019"]))), collapse =", ")
    freq$Countries_code_2020[i] = paste(unique(sort(na.omit(jdfs$iso_a2[jdfs$year=="2020"]))), collapse =", ")
    st_write(jdfs, paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/spatial/", country_iso2, "/", obsrID, ".gpkg"))
  }
  # save the dataframe to file
  write.table(freq, paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/spatial/", country_iso2, "/eBirders.csv"), sep=";", dec=",", row.names=F)
}
rm(freq, df, dfs, jdfs, dat19, dat20, allobsr, dat)


# put together the information on all activity ranges for each country to produce boxplots
cnt_list = c("US", "CA", "ES", "AU", "GB", "PT", "IL", "DE", "FR", "SE", # Developed region
             "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK", # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  # least developed region

allranges = data.frame()
for (i in c(1:length(cnt_list))) {
    country_iso2 = cnt_list[i]
    freq = read.table(paste0("C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/spatial/", country_iso2, "/eBirders.csv"), sep=";", dec=",", header=T)
    freq = freq[1:min(30, nrow(freq)),]
    freq$Country = country_iso2
    allranges = rbind(allranges, freq)
}

# check some summary statistics
stat19 = aggregate(HR_area_km2_2019 ~ Country, data = allranges, FUN=mean)
stat20 = aggregate(HR_area_km2_2020 ~ Country, data = allranges, FUN=mean)
# compute the nr. of non-NA activity ranges calculated
count = function(x) {length(na.omit(x))}
count19 = aggregate(HR_area_km2_2019 ~ Country, data = allranges, FUN=count)
count19$Year = 2019; names(count19)[2] = "Nr_activity_ranges"
count20 = aggregate(HR_area_km2_2020 ~ Country, data = allranges, FUN=count)
count20$Year = 2020; names(count20)[2] = "Nr_activity_ranges"
counts = cbind(count19, count20)

# mutate from wide to long format and plot as boxplots, separately by year and country
# add nr. of home ranges for each boxplot
arlong <- gather(allranges[, c("HR_area_km2_2019","HR_area_km2_2020", "Country")], Year, Activity_range_km2, HR_area_km2_2019:HR_area_km2_2020, factor_key=F)
arlong$Year = gsub(pattern= "HR_area_km2_", replacement="", arlong$Year)
# number of eBird records, ordered by economic class
arlong2 <- arlong %>%
  mutate( Country =factor(Country,levels=c("US", "CA", "ES", "AU", "GB", "PT", "IL", "DE", "FR", "SE",   # Developed region
                                           "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA",   # emerging region
                                           "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",   # developing region
                                           "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ"))) # least developed region
# make boxplots
ggplot(arlong2, aes(x=Country, y=log10(Activity_range_km2 + 1), fill=Year)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c(rgb(0,0,1,0.7), rgb(1,0,0,0.7))) +
  theme_minimal() + 
  ylab("log10(Area of eBirder's activity range + 1)")
# with lighter shading to later merge to indicate significance
ggplot(arlong2, aes(x=Country, y=log10(Activity_range_km2 + 1), fill=Year)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3))) +
  theme_minimal() + 
  ylab("log10(Area of eBirder's activity range + 1)")

# Kolmogorov-Smirnov Tests to test whether the difference in area of home ranges in 2019 and 2020 is significant
kstest = data.frame(Country = cnt_list)
for ( i in seq_along(cnt_list)) {
  subset = allranges[allranges$Country == cnt_list[i],]
  KST = ks.test(x = subset$HR_area_km2_2019, y = subset$HR_area_km2_2020) 
  kstest$D_stat[i] = round(KST$statistic, digits=3)
  kstest$p_value[i] = round(KST$p.value, digits=3)
}
write.table(kstest, "C:/Users/sroilo/Desktop/BESTMAP documents/Papers/GBIF_COVID19/GBIF/HomeRange/KS_test_20240131.csv", sep=";", dec=",", row.names=F)

rm(list=ls())
setwd("C:/Users/sroilo/Documents")
