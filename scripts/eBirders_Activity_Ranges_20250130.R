###################################################
#
# Title: eBirders_Activity_Ranges_20250130.R
# Purpose: compute the number of active eBirders per country, identify the most active eBirders per country
#          and calculate the area of their activity ranges.
#          
# Author: Stephanie Roilo, TUD Dresden University of Technology, Germany & University of Bonn, Germany
# Date: last edited on January 30th, 2025
#
###################################################

library(arrow)
library(lubridate)
library(dplyr)
library(sf)
library(data.table)
library(units)
library(ISOcodes)
library(tidyr)  # to convert from wide to long format of dataframes
library(sf)
#library(mapview)  # to visually check home ranges

# for plotting
library(ggplot2)
#library(hrbrthemes)
#library(viridis) 
library(ggExtra)



# plot number of eBird records (log10-transformed), ordered by economic class
tot_ebird = data_sum_marmay_1920 %>% mutate(countrycode = factor(
  countrycode,levels=c("US", "CA", "ES", "AU", "GB", "TW", "PT", "IL", "DE", "FR",  # Developed region
                       "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "ZA", "TR",  # emerging region
                       "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
                       "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")))  # least developed region

# plot number of eBird records (log10-transformed), ordered by economic class                             
ggplot(tot_ebird) +
  geom_segment( aes(x=countrycode, xend=countrycode, y=log10(n_eBird_2019), yend=log10(n_eBird_2020)), color="darkgrey", linewidth=1) +
  geom_point( aes(x=countrycode, y=log10(n_eBird_2019)), color=rgb(0,0,1,0.5), size=3 ) +
  geom_point( aes(x=countrycode, y=log10(n_eBird_2020)), color=rgb(1,0,0,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("log10(Number of eBird records)")  # saved as pdf 3.5 x 8.5 inches

# plot number of eBirders, ordered by economic class
ggplot(tot_ebird) +
  geom_segment( aes(x=countrycode, xend=countrycode, y=log10(n_obsr_2019), yend=log10(n_obsr_2020)), color="darkgrey", size=1) +
  geom_point( aes(x=countrycode, y=log10(n_obsr_2019)), color=rgb(0,0,1,0.5), size=3 ) +
  geom_point( aes(x=countrycode, y=log10(n_obsr_2020)), color=rgb(1,0,0,0.5), size=3 ) +
  theme_minimal() +
  xlab("") +
  ylab("log10(Number of eBirders)")  # saved as pdf 3.5 x 8.5 inches


### ACTIVITY RANGES of individual eBirders ------------------------
# first, gather the observer ID of the 30 most active eBirders for the 40 analyzed countries
df_obsr = data.frame()
for (cnt in cnt_list) {
  ebirders19 = read.table(paste0("~/eBirders_2019/eBirders_", cnt, "_2019.csv"), sep=";", dec=",", header=T)
  # remove duplicates of the same observer and day
  ebirders19 = ebirders19 %>% 
    distinct(recordedby, year, month, day, .keep_all = TRUE) %>% 
    group_by(recordedby) %>% summarise(n_active_days = n()) %>% arrange(desc(n_active_days))
  ebirders19$countrycode = cnt
  df_obsr = rbind(df_obsr, ebirders19[1:min(30, nrow(ebirders19)),])
}
# save to file
write.table(df_obsr, "~/HomeRange/Top30_eBirders_xCountry_2019.csv", sep=";", dec=",", row.names=F)

# extract the occurrences of the top 30 most active observers per country in 2019
ebirders = local_df %>% 
  filter(
    is.na(decimallatitude) == FALSE,
    is.na(decimallongitude) == FALSE,
    institutioncode == "CLO",
    basisofrecord == "HUMAN_OBSERVATION",
    eventdate >= "2019-03-15", 
    eventdate <= "2019-05-01" 
  ) %>%
  select(countrycode, year, month, day, recordedby, decimallatitude, decimallongitude) %>% 
  collect() 
# unlist observers' IDs, which is currently in a list/array format, and filter out records from the same observer, location and day
selobs = ebirders %>% mutate(recordedby = unlist(recordedby)) %>%
  filter(recordedby %in% df_obsr$recordedby, .keep_all = TRUE) %>%
  arrange(year, month, day, recordedby) %>% 
  # remove multiple observations by the same eBirder on the same day at the same spot
  distinct(recordedby, year, month, day, decimallatitude, decimallongitude, .keep_all = TRUE) 
# save to file
write.table(selobs, "~/HomeRange/Top30_eBirders_observations_2019.csv", sep=";", dec=",", row.names=F)

# extract the occurrences of the top 30 most active observers per country in 2020
ebirders20 = local_df %>% 
  filter(
    is.na(decimallatitude) == FALSE,
    is.na(decimallongitude) == FALSE,
    institutioncode == "CLO",
    basisofrecord == "HUMAN_OBSERVATION",
    eventdate >= "2020-03-15", 
    eventdate <= "2020-05-01" 
  ) %>%
  select(countrycode, year, month, day, recordedby, decimallatitude, decimallongitude) %>% 
  collect() 
# unlist observers' IDs, which is currently in a list/array format, and filter out records from the same observer, location and day
selobs20 = ebirders20 %>% mutate(recordedby = unlist(recordedby)) %>%
  filter(recordedby %in% df_obsr$recordedby, .keep_all = TRUE) %>%
  arrange(year, month, day, recordedby) %>% 
  # remove multiple observations by the same eBirder on the same day at the same spot
  distinct(recordedby, year, month, day, decimallatitude, decimallongitude, .keep_all = TRUE) 
# save to file
write.table(selobs20, "~/HomeRange/Top30_eBirders_observations_2020.csv", sep=";", dec=",", row.names=F)

### HOME RANGE MAPPING --------------------
# convert the two dataframes containing the coordinates of the observations to spatial object
spatobs = st_as_sf(selobs, coords = c("decimallongitude", "decimallatitude"), crs="epsg:4326")
spatobs20 = st_as_sf(selobs20, coords = c("decimallongitude", "decimallatitude"), crs="epsg:4326")
# check that the points are correctly mapped out in space
mapview(spatobs[1:30,]); mapview(spatobs20[1:30,])

# loop through the shortlisted eBirders and calculate the minimum convex polygon area in 2019
for (i in seq_along(df_obsr$recordedby)) {
  one_obs = df_obsr$recordedby[i]
  df19 = spatobs[spatobs$recordedby==one_obs,]
  mcp19 = st_convex_hull(st_union(df19))
  df20 = spatobs20[spatobs20$recordedby==one_obs,]
  if (nrow(df20) > 0) { mcp20 = st_convex_hull(st_union(df20)) } else {mcp20 = NA}
  # save the area of the observer's home range for each year, in square km 
  df_obsr$HR_area_km2_2019[i] = round(set_units(st_area(mcp19), km^2), digits=2)
  df_obsr$HR_area_km2_2020[i] = ifelse(nrow(df20) == 0, NA, round(set_units(st_area(mcp20), km^2), digits=2))
  # add the country codes of all the countries where the observer recorded species
  df_obsr$Countries_code_2019[i] = paste(unique(df19$countrycode), collapse=", ")
  df_obsr$Countries_code_2020[i] = ifelse(nrow(df20) >0, paste(unique(df20$countrycode), collapse=", "), "")
}
# save to file
write.table(df_obsr, "~/HomeRange/eBirders_HomeRanges_20250120.csv", sep=";", dec=",", row.names=F)

allranges = df_obsr
# mutate from wide to long format and plot as boxplots, separately by year and country
# add nr. of home ranges for each boxplot
arlong <- gather(allranges[, c("HR_area_km2_2019","HR_area_km2_2020", "countrycode")], Year, Activity_range_km2, HR_area_km2_2019:HR_area_km2_2020, factor_key=F)
arlong$Year = gsub(pattern= "HR_area_km2_", replacement="", arlong$Year)
# number of eBird records, ordered by economic class
arlong2 <- arlong %>%
  mutate( countrycode =factor(countrycode,levels=c("US", "CA", "ES", "AU", "GB", "TW", "PT", "IL", "DE", "FR",   # Developed region
                                                   "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "ZA", "TR",   # emerging region
                                                   "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",   # developing region
                                                   "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ"))) # least developed region
# make boxplots
ggplot(arlong2, aes(x=countrycode, y=log10(Activity_range_km2 + 1), fill=Year)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c(rgb(0,0,1,0.7), rgb(1,0,0,0.7))) +
  theme_minimal() + 
  xlab("Country") + 
  ylab("log10(Area of eBirder's activity range + 1)") + scale_y_discrete(limits=c(0, 2, 4, 6, 8))

# saved ad pdf with 9.39 x 3.5 inches
# with lighter shading to later merge to indicate significance
ggplot(arlong2, aes(x=countrycode, y=log10(Activity_range_km2 + 1), fill=Year)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3))) +
  theme_minimal() + 
  xlab("Country") + 
  ylab("log10(Area of eBirder's activity range + 1)") + scale_y_discrete(limits=c(0, 2, 4, 6, 8))
# saved ad pdf with 9.39 x 3.5 inches


# Kolmogorov-Smirnov Tests to test whether the difference in area of home ranges in 2019 and 2020 is significant
kstest = data.frame(countrycode = cnt_list)
for ( i in seq_along(cnt_list)) {
  subset = allranges[allranges$countrycode == cnt_list[i],]
  KST = ks.test(x = subset$HR_area_km2_2019, y = subset$HR_area_km2_2020) 
  kstest$D_stat[i] = round(KST$statistic, digits=3)
  kstest$p_value[i] = round(KST$p.value, digits=3)
}

# add the number of home ranges calculated for each year
ebstats = allranges %>% group_by(countrycode) %>% 
  summarise(Nr_HR_2019 = sum(!is.na(HR_area_km2_2019)), 
            Nr_HR_2020 = sum(!is.na(HR_area_km2_2020)))
# merge the two dataframes
kstest = merge(kstest, ebstats, by="countrycode", all.x = T, all.y = F)
#save to file
write.table(kstest, "~/HomeRange/KS_test_20250120.csv", sep=";", dec=",", row.names=F)

