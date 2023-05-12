###################################################
#
# Title: eBirders_Activity_Ranges.R
# Purpose: compute the number of active eBirders per country, identify the most active eBirders per country
#          and calculate the area of their activity ranges.
#          
# Author: Stephanie Roilo, Technische Universität Dresden
# Date: started on December20th 2022, last edited on May 12th 2023
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
library(networkD3)  # to make Sankey plots

### download only the data from eBird, and extract the metadata on individual users -------------
### NOTE: this part was run in the HPC
country_name = "Australia"
country_iso2 = "AU"
# create a dataframe in which each row corresponds to a day
# we only select the dates between 15th of March and 1st of May in 2019 and 2020
dates = data.frame(Date = c(seq(as.Date("2019-03-15"), as.Date("2019-05-01"), by="days"),
                            seq(as.Date("2020-03-15"), as.Date("2020-05-01"), by="days")) )
# loop through each day and extract the number of eBird records, the number of eBirders active on that day, and their unique IDs
for (i in c(1:nrow(dates))) {
  date = dates$Date[i]
  daydat = occ_data(country=country_iso2, basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, 
                    eventDate = date, institutionCode = "CLO", limit=100000)
  # data download has a hard limit of 100000, if records are more than that, set the result to NA
  dates$n_CLO[i] = ifelse(daydat$meta$count <= 100000, daydat$meta$count, NA)
  dates$n_obsr[i] = ifelse(daydat$meta$count == 0, 0, length(unique(daydat$data$recordedBy)))
  dates$ID_obsr[i] = ifelse(daydat$meta$count == 0, 0, paste(sort(unique(daydat$data$recordedBy)), collapse=" | "))
  write.table(dates, paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/CLO_", country_iso2, "_March15_May1_2019_2020.csv"), sep=";", dec=".", row.names = F)
}  

### CHANGE IN NUMBER of eBird records and of eBirders ------------------------------------
# list of countries selected for the analysis (10 per economic class) :
cnt_list = c("AU", "GB", "PT", "IL", "DE", "FR", "SE", "NZ", "CZ", "NL", #Developed region
             "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", "VE",  # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  # least developed

# load the full country dataset and count total eBirders active during the two periods
tot_ebird = data.frame(Country = cnt_list)
for (i in c(1:length(cnt_list))) {
  cntr = tot_ebird$Country[i]
  dat = read.table(paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/CLO_", cntr, "_March15_May1_2019_2020.csv"), sep=";", dec=",", header=T)
  tot_ebird$eBird_records_2019[i] = sum(dat$n_CLO[dat$Date<= "2019-05-01" & dat$Date>= "2019-03-15"])
  tot_ebird$eBird_records_2020[i] = sum(dat$n_CLO[dat$Date<= "2020-05-01" & dat$Date>= "2020-03-15"])
  allobsr_2019 = paste(dat$ID_obsr[which(dat$Date<= "2019-05-01" & dat$Date>= "2019-03-15")], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  tot_ebird$eBirders_2019[i] = length(unique(na.omit(unlist(allobsr_2019))))
  allobsr_2020 = paste(dat$ID_obsr[which(dat$Date<= "2020-05-01" & dat$Date>= "2020-03-15")], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  tot_ebird$eBirders_2020[i] = length(unique(na.omit(unlist(allobsr_2020))))
}
# add info on stringency index and mobility data to the dataframe
cntall = read.table("C:/Users/sroilo/Desktop/GBIF/HomeRange/Countries_by_eBird_proportions_20230509.csv", sep=";", dec=",", header=T)
names(cntall)[1] <- "Country_name"
cntall$Country = ISO_3166_1$Alpha_2[match(cntall$Country_name, ISO_3166_1$Name)]
tot_ebird = merge(tot_ebird, cntall[,c("Country_name", "economy_coarse", "Stringency_index", "Change_park_visits","Country")], by="Country")

# make cleveland dotplots to show differences in eBird records and in eBirders before (2019) and during (2020) lockdown
# order countries by economic class and number of eBird records in 2019
tot_ebird2 <- tot_ebird %>%
  mutate( Country =factor(Country,levels=c("AU", "GB", "PT", "IL", "DE", "FR", "SE", "NZ", "CZ", "NL", #Developed region
                                           "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", "VE",  # emerging region
                                           "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
                                           "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")) )

# plot number of eBird records, ordered by economic class
ggplot(tot_ebird2) +
  geom_segment( aes(x=Country, xend=Country, y=eBird_records_2019, yend=eBird_records_2020), color="darkgrey", size=1) +
  geom_point( aes(x=Country, y=eBird_records_2019), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Country, y=eBird_records_2020), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("Number of eBird records")
# plot number of eBirders, ordered by economic class
ggplot(tot_ebird2) +
  geom_segment( aes(x=Country, xend=Country, y=eBirders_2019, yend=eBirders_2020), color="darkgrey", size=1) +
  geom_point( aes(x=Country, y=eBirders_2019), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Country, y=eBirders_2020), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("Number of eBirders")

# save tot_ebird dataframe to file
write.table(tot_ebird, "C:/Users/sroilo/Desktop/GBIF/HomeRange/Tot_eBird_20230509.csv", sep=";", dec=",", row.names=F)

### ACTIVITY RANGES of individual eBirders ------------------------
## subset the country selection and run the activity range analysis
cnt_list = c("AU", "GB", "PT", "IL", "DE", "FR", "SE", "NZ", "CZ", "NL", #Developed region
             "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", "VE",  # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  
# load the shapefile of the world's borders to retrieve information on which countries eBirders visited
wmap = ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>% 
  select(c("name_long","pop_est", "economy", "income_grp", "iso_a2", "iso_a3", "region_un", "subregion"))
lockdown_start = "2019-03-15"  
lockdown_end = "2019-05-01"

for (country_iso2 in cnt_list) {
  # filter out the chosen country to intersect records of eBirders
  cntr_shp = wmap %>% filter(iso_a2 == country_iso2)
  # create a folder to store the results
  cntr.dir <- paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/spatial/", country_iso2)
  dir.create(cntr.dir, showWarnings=FALSE, recursive=TRUE)
  # load the full country dataset and filter eBirders that have been most active in spring 2019 (March 15th to May 1st)
  dat = read.table(paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/CLO_", country_iso2, "_March15_May1_2019_2020.csv"), sep=";", dec=",", header=T)
  # collect all the eBirder ID which were active in spring 2019 (time interval must correspond to lockdown period in 2020)
  allobsr = paste(dat$ID_obsr[which(dat$Date>=as.Date(lockdown_start) & dat$Date<=as.Date(lockdown_end))], collapse=" | ") %>% strsplit(split=" | ", fixed=T)
  # order by most frequent observer (by nr. of recording days)
  freq = data.frame(table(allobsr)); freq$allobsr = as.character(freq$allobsr)
  freq = freq[order(freq$Freq, decreasing=T),]
  # remove zeros (no observers) from the list
  if ("0" %in% freq$allobsr) {freq = freq[-which(freq$allobsr=="0"),]}
  # prepare columns to save additional information for each eBirder
  freq$Nr_obs_2019 = NA; freq$Nr_obs_2020 = NA
  freq$HR_area_km2_2019 = NA; freq$HR_area_km2_2020 = NA
  freq$HR_area_km2_2019 = NA; freq$HR_area_km2_2020 = NA
  freq$Countries_2019 = NA; freq$Countries_2020 = NA; freq$Countries_code_2019 = NA; freq$Countries_code_2020 = NA
  freq$HR_in_country_2019 = NA; freq$HR_in_country_2020 = NA
  # loop through the first 30 most active observers in 2019 and calculate home ranges in 2019 and 2020, lockdown period
  # Switch off spherical geometry (s2) to avoid problems in the area calculation for larger polygons;
  # read more about it here: https://github.com/r-spatial/sf/issues/1902 
  sf::sf_use_s2(FALSE)
  # loop through the first 30 most active eBirders
  for ( i in c(1:30)) {
    obsrID = freq$allobsr[i]
    # download occurrences for the given time interval for 2019 and 2020 separately
    dat19 = occ_data(basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, eventDate=paste0(lockdown_start, ",", lockdown_end),  
                     institutionCode = "CLO", recordedBy=obsrID, limit=100000)
    dat20 = occ_data(basisOfRecord = "HUMAN_OBSERVATION", hasCoordinate=TRUE, eventDate=paste0(gsub(x=lockdown_start, pattern="2019", replacement="2020"), ",", gsub(x=lockdown_end, pattern="2019", replacement="2020")),
                     institutionCode = "CLO", recordedBy=obsrID, limit=100000)
    # data is divided by year, put everything together
    df = bind_rows(dat19$data, dat20$data)
    # get rid of some columns and of multiple observations at the same location on the same day
    df = df[-which(duplicated(df[,c("decimalLatitude","decimalLongitude","eventDate","recordedBy")])),
            c("decimalLatitude","decimalLongitude","basisOfRecord","species", 
              "year", "month","day","eventDate","recordedBy","institutionCode", "locality" )]
    # save the number of observations for each year
    freq$Nr_obs_2019[i] = nrow(df[df$year==2019,])
    freq$Nr_obs_2020[i] = nrow(df[df$year==2020,])
    # convert data to spatial object 
    dfs = st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs="epsg:4326")
    # create minimum convex polygon around the points, divided by year
    mcp19 = st_convex_hull(st_union(dfs[dfs$year=="2019",]))
    mcp20 = st_convex_hull(st_union(dfs[dfs$year=="2020",]))
    # plot with mapview to check results
    #  mapview(dfs, zcol="year") + mapview(mcp19, color="purple") + mapview(mcp20, color="yellow")
    # save the area of the observer's home range for each year, in square km 
    freq$HR_area_km2_2019[i] = round(set_units(st_area(mcp19), km^2), digits=2)
    freq$HR_area_km2_2020[i] = ifelse(length(mcp20)> 0, round(set_units(st_area(mcp20), km^2), digits=2), NA)
    # join with world's shapefile
    jdfs = st_join(dfs, wmap)
    # extract information on countries in which the eBirder recorded species in 2019 and 2020 during the lockdown period
    freq$Countries_2019[i] = paste(unique(sort(na.omit(jdfs$name_long[jdfs$year=="2019"]))), collapse =", ")
    freq$Countries_2020[i] = paste(unique(sort(na.omit(jdfs$name_long[jdfs$year=="2020"]))), collapse =", ")
    freq$Countries_code_2019[i] = paste(unique(sort(na.omit(jdfs$iso_a2[jdfs$year=="2019"]))), collapse =", ")
    freq$Countries_code_2020[i] = paste(unique(sort(na.omit(jdfs$iso_a2[jdfs$year=="2020"]))), collapse =", ")
    # check also if the home range overlaps is within the country or somewhere else
    freq$HR_in_country_2019[i] = st_intersects(cntr_shp, mcp19, sparse=F)
    freq$HR_in_country_2020[i] = ifelse(length(mcp20)> 0, st_intersects(cntr_shp, mcp20, sparse=F), NA)
    st_write(jdfs, paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/spatial/", country_iso2, "/", obsrID, ".gpkg"))
  }
  # save the dataframe to file
  write.table(freq, paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/spatial/", country_iso2, "/eBirders.csv"), sep=";", dec=",", row.names=F)
}
rm(freq, df, dfs, jdfs, dat19, dat20, allobsr, dat)


# put together the information on all activity ranges for each country to produce boxplots
cnt_list = c("AU", "GB", "PT", "IL", "DE", "FR", "SE", "NZ", "CZ", "NL", #Developed region
             "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", "VE",  # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  

allranges = data.frame()
for (i in c(1:length(cnt_list))) {
    country_iso2 = cnt_list[i]
    freq = read.table(paste0("C:/Users/sroilo/Desktop/GBIF/HomeRange/spatial/", country_iso2, "/eBirders.csv"), sep=";", dec=",", header=T)
    if (sum(is.na(freq$HR_in_country_2019)) != 0 ) { freq = freq[-which(is.na(freq$HR_in_country_2019)),] }
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
  mutate( Country =factor(Country,levels=c("AU", "GB", "PT", "IL", "DE", "FR", "SE", "NZ", "CZ", "NL",  #Developed region
                                           "CR", "MX", "BR", "AR", "PE", "CL", "TH", "TR", "ZA", "VE",  # emerging region
                                           "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
                                           "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")) )
# make boxplots
ggplot(arlong2, aes(x=Country, y=log10(Activity_range_km2 + 1), fill=Year)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c(rgb(0.2,0.7,0.1,0.5), rgb(0.7,0.2,0.1,0.5))) +
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
write.table(kstest, "C:/Users/sroilo/Desktop/GBIF/HomeRange/KS_test_20230509.csv", sep=";", dec=",", row.names=F)

#### Sankey diagram of eBirders' movements ------------------------------------------------------------------
# see tutorial here https://www.data-to-viz.com/graph/sankey.html 
# use Country as source and countries_code_2020 as target
sankey = data.frame()
for (cnt in unique(allranges$Country)) {
  subset = allranges[which(allranges$Country== cnt),]
  table = as.data.frame(t(table(subset$Countries_code_2020)))
  names(table) = c("Source_2019", "Target_2020", "Freq")
  table$Source_2019 = rep(cnt, times=nrow(table))
  sankey =  rbind(sankey, table)
}
# plot a subset of countries, to avoid overly complicated graphs
sankey = sankey[which(sankey$Source_2019 %in% c("NZ", "PE", "PA", "KH")),]  
#  create a node data frame
nodes = data.frame(name=c(as.character(sankey$Source_2019), as.character(sankey$Target_2020)) %>% unique())
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankey$IDsource=match(sankey$Source_2019, nodes$name)-1 
sankey$IDtarget=match(sankey$Target_2020, nodes$name)-1
# prepare colour scale - viridis-style
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#6DCD59FF","#1F9E89FF","#31688EFF","#482878FF"])'

# plot the Network
sankeyNetwork(Links = sankey, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget", LinkGroup = "Source_2019",
              Value = "Freq", NodeID = "name", colourScale=ColourScal,
              fontSize=20, fontFamily="Arial" , nodeWidth=40, nodePadding=20,
              margin= c(top=0, right=0, bottom=0, left=0), sinksRight=FALSE)

rm(list=ls())
setwd("C:/Users/sroilo/Documents")
