# exporting subsets of occurrence data required downstream

library(arrow)
library(dplyr)   
library(tidyr)
library(lubridate)
library(purrr)
library(glue)

# see instructions on how to work with Parquet format data: 
# https://data-blog.gbif.org/post/apache-arrow-and-parquet/ 

# set the path to where the GBIF snapshot was downloaded: ### PATH NOT RELATIVE ###
# parquet download on external hard disk
local_df <- open_dataset("../../../../media/kartrick/Vinay_s SSD/GBIF/occurrence.parquet")

# need latlong, country code, human observation, and >= 2010
local_df = local_df %>% 
  filter(is.na(decimallatitude) == FALSE,
         is.na(decimallongitude) == FALSE,
         is.na(countrycode) == FALSE,
         basisofrecord == "HUMAN_OBSERVATION",
         year >= 2010)


# no. obs. data --------------------------------------------

# gbif all human observations

# count number of observations per day per country
data_obs <- local_df %>%
  group_by(year, month, day, countrycode) %>%
  count() %>% 
  collect() %>% # collect arrow into tibble
  arrange(countrycode, year, month, day) %>% 
  # remove NAs (e.g. rows with missing days and/or months) 
  # Note that countrycode = "NA" (Namibia) is different than NA (not available)
  na.omit() %>% 
  mutate(eventDate = as.Date(paste(year, month, day, sep = "-")))

# add rows for missing days on which no record was collected
data_obs = data_obs %>% 
  merge(expand.grid(countrycode = unique(data_obs$countrycode), 
                    eventDate = unique(data_obs$eventDate)), 
        all = TRUE) %>% 
  # update missing rows with appropriate values
  mutate(n = replace_na(n, replace = 0),
         year = year(eventDate),
         month = month(eventDate),
         day = day(eventDate))


# only eBird
data_ebird_obs <- local_df %>% 
  filter(institutioncode == "CLO") %>% # Cornell Lab Ornith.
  group_by(year, month, day, countrycode) %>%
  count() %>% 
  collect() %>% 
  arrange(countrycode, year, month, day) %>% 
  # remove NAs (e.g. rows with missing days and/or months) 
  # Note that countrycode = "NA" (Namibia) is different than NA (not available)
  na.omit() %>% 
  mutate(eventDate = as.Date(paste(year, month, day, sep = "-"))) %>% 
  rename(n_eBird = n)

# merge to the daily counts of human observations
data_obs <- data_obs %>% 
  merge(data_ebird_obs[,c("countrycode", "eventDate", "n_eBird")], 
        by = c("countrycode", "eventDate"), 
        all = TRUE) %>% 
  mutate(n_eBird = replace_na(n_eBird, replace = 0))


# total obs/records between March 15 and May 1 per year per country
data_sum_marmay <- data_obs %>% 
  filter(eventDate >= as.Date(paste0(year(eventDate), "-03-15")), 
         eventDate <= as.Date(paste0(year(eventDate), "-05-01"))) %>%
  group_by(countrycode, year) %>%
  reframe(count = sum(n),
          count_eBird = sum(n_eBird))


# save to file
save(data_obs, data_sum_marmay, file = "data/data_n_obs.RData")


# activity range ----------------------------------------------------------

# ebird data and observer metadata

# define the list of countries, based on their eBird data volumes in 2019 (March 15 to May 1)
cnt_list = c("US", "CA", "ES", "AU", "GB", "TW", "PT", "IL", "DE", "FR",  # Developed region
             "IN", "CR", "MX", "BR", "AR", "PE", "CL", "TH", "ZA", "TR",  # emerging region
             "CO", "BZ", "PA", "GT", "EC", "HN", "MY", "MA", "AE", "HK",  # developing region
             "NP", "RW", "TZ", "BD", "KH", "MM", "HT", "SN", "UG", "MZ")  # least developed region

# x <- local_df %>% 
#   filter(countrycode == "RW",
#          year == 2019,
#          institutioncode == "CLO",
#          eventdate >= "2019-03-15", 
#          eventdate <= "2019-05-01") %>% 
#   select(countrycode, year, month, day, recordedby, decimallatitude, decimallongitude) %>% 
#   collect() %>% 
#   arrange(year, month, day, recordedby) %>% 
#   mutate(recordedby = unlist(recordedby))


map_over <- expand.grid(countries = cnt_list, years = 2019:2020)

start <- Sys.time()
data_ebirders <- map2(map_over$countries, map_over$years, ~ {

local_df %>% 
  filter(countrycode == .x,
         year == .y,
         institutioncode == "CLO",
         eventdate >= paste0(.y, "-03-15"), 
         eventdate <= paste0(.y, "-05-01")) %>%
  # filter((month == 3 & day >= 15) |
  #          (month == 5 & day == 1) |
  #          (month == 4)) %>% 
  select(countrycode, year, month, day, recordedby, decimallatitude, decimallongitude) %>% 
  collect() %>% 
  arrange(year, month, day, recordedby) %>% 
  mutate(recordedby = unlist(recordedby)) %>% 
  distinct()
  
}) %>% 
  list_rbind()
end <- Sys.time()
end-start


# # year 2019
# for (cnt in cnt_list){
#   ebirders$eventDate = as.Date(paste(ebirders$year, ebirders$month, ebirders$day, sep="-"))
#   write.table(ebirders, paste0("~/eBirders_2019/eBirders_", cnt, "_2019.csv"), sep=";", dec=",", row.names=F)
# } 
# 
# #year 2020
# for (cnt in cnt_list){
#   ebirders = local_df %>% 
#     filter(
#       countrycode == cnt,
#       is.na(decimallatitude) == FALSE,
#       is.na(decimallongitude) == FALSE,
#       institutioncode == "CLO",
#       basisofrecord == "HUMAN_OBSERVATION",
#       eventdate >= "2020-03-15", 
#       eventdate <= "2020-05-01" 
#     ) %>%
#     select(countrycode, year, month, day, recordedby, decimallatitude, decimallongitude) %>% 
#     collect() %>% arrange(year, month, day, recordedby)
#   # unlist observers' IDs, which is currently in a list/array format
#   ebirders$recordedby = unlist(ebirders$recordedby)
#   # filter out records from the same observer, location and day (same observer, same date, same location)
#   ebirders = ebirders %>% distinct(recordedby, year, month, day, decimallatitude, decimallongitude, .keep_all = TRUE)
#   ebirders$eventDate = as.Date(paste(ebirders$year, ebirders$month, ebirders$day, sep="-"))
#   write.table(ebirders, paste0("~/eBirders_2020/eBirders_", cnt, "_2020.csv"), sep=";", dec=",", row.names=F)
# } 



# separate df for only eBird obs in only 2019 and 2020
data_marmay_1920 <- data_sum_marmay %>% 
  filter(year %in% 2019:2020) %>% 
  select(-count) %>% 
  pivot_wider(names_from = "year", values_from = "count_eBird", names_glue = "n_eBird_{.name}")

data_marmay_1920 <- x %>% 
  group_by(countrycode, year) %>% 
  reframe(n_obsr = n_distinct(recordedby), 
          ID_obsr = paste(sort(unique(recordedby)), collapse = " | ")) %>% 
  pivot_wider(names_from = "year", values_from = c("n_obsr", "ID_obsr"), names_glue = "{.name}") %>% 
  right_join(data_marmay_1920, by = "countrycode") %>% 
  na.omit()



save(data_marmay_1920, file = "data/data_act_range.RData")


