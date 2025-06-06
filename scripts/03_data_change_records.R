# calculations for change in records during lockdown period

library(dplyr)   
library(tidyr)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
library(data.table)  # for fast reading of large datasets
library(ISOcodes)  # to access the ISO 3166 list of country codes
library(rnaturalearth)
library(sf)

# load daily obs. data
load("data/data_n_obs.RData")
# load stringency index & mobility data
load("data/data_all_covid.RData")



# calculate the number of records per country for the lockdown period in 3 years

nrec_three <- map_dfr(2020:2022, function(yr) {
  
  start_date <- as.Date(glue("{yr}-03-15"))
  end_date <- as.Date(glue("{yr}-05-01"))
  
  data_all_covid %>%
    filter(eventDate >= start_date & eventDate <= end_date) %>%
    group_by(Country) %>%
    summarise(across(c(n, n_eBird), sum), .groups = "drop") %>%
    mutate(year = yr)
  
})

nrec_2019 <- data_all_covid %>%
  filter(eventDate >= "2019-03-15" & eventDate <= "2019-05-01") %>%
  group_by(Country) %>%
  summarise(across(c(n, n_eBird), sum), .groups = "drop") %>% 
  rename(n_2019 = n, n_eBird_2019 = n_eBird)

nrec_three <- nrec_three %>% 
  left_join(nrec_2019, by = "Country") %>% 
  mutate(Change_records = if_else(n_2019 > 0, 100*(n - n_2019)/n_2019, NA),
         Change_eBird_records = if_else(n_eBird_2019 > 0, 100*(n_eBird - n_eBird_2019)/n_eBird_2019, NA))

# also for all 12 months of 2020
nrec_full2020 <- data_all_covid %>%
  filter(year %in% 2019:2020) %>%
  group_by(year, Country) %>%
  summarise(across(c(n, n_eBird), sum), .groups = "drop") %>%
  pivot_wider(names_from = "year", values_from = c(n, n_eBird), names_glue = "{.value}_{year}") %>% 
  mutate(Change_records = if_else(n_2019 > 0, 100*(n_2020 - n_2019)/n_2019, NA),
         Change_eBird_records = if_else(n_eBird_2019 > 0, 100*(n_eBird_2020 - n_eBird_2019)/n_eBird_2019, NA))



# calculate the mean of the stringency index, the change in park visitors, and 
# the change in time spent at home per country
vars_mean_three <- map_dfr(2020:2022, function(yr) {
  
  start_date <- as.Date(glue("{yr}-03-15"))
  end_date <- as.Date(glue("{yr}-05-01"))
  
  data_all_covid %>%
    filter(eventDate >= start_date & eventDate <= end_date) %>%
    group_by(Country) %>%
    summarise(across(c(Stringency_index, Change_park_visitors, Change_time_at_home, Population), 
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(year = yr,
           measure = "mean")
  
})
# media
vars_median_three <- map_dfr(2020:2022, function(yr) {
  
  start_date <- as.Date(glue("{yr}-03-15"))
  end_date <- as.Date(glue("{yr}-05-01"))
  
  data_all_covid %>%
    filter(eventDate >= start_date & eventDate <= end_date) %>%
    group_by(Country) %>%
    summarise(across(c(Stringency_index, Change_park_visitors, Change_time_at_home, Population), 
                     ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(year = yr,
           measure = "median")
  
})
# sd
vars_sd_three <- map_dfr(2020:2022, function(yr) {
  
  start_date <- as.Date(glue("{yr}-03-15"))
  end_date <- as.Date(glue("{yr}-05-01"))
  
  data_all_covid %>%
    filter(eventDate >= start_date & eventDate <= end_date) %>%
    group_by(Country) %>%
    summarise(across(c(Stringency_index, Change_park_visitors, Change_time_at_home, Population), 
                     ~ sd(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(year = yr,
           measure = "sd")
  
})
# combine all three
vars_three <- vars_mean_three %>% bind_rows(vars_median_three) %>% bind_rows(vars_sd_three)


# merge the two dataframes
data_lockdown_change <- vars_three %>% 
  left_join(nrec_three, by = c("Country", "year")) %>% 
  relocate(Country, n, n_eBird) %>% 
  # column saying whether or not has NA, i.e., whether or not removed from socioeconomic analysis
  mutate(analysed = !if_any(c(everything(), 
                              -c(Change_records, Change_eBird_records)), 
                            is.na)) %>% 
  # add country codes and names
  left_join(data_all_covid %>%
              distinct(Country, .keep_all = TRUE) %>%
              select(Country, countrycode, iso_code),
            by = "Country") %>% 
  # add data on economic class and income group from the NaturalEarth dataset
  # load shapefile of the countries of the world (rnaturalearth package 
  # version 1.0.1, and rnaturalearthdata version 1.0.0)
  left_join(ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
              select(c("name_long", "adm0_a3", "economy", "income_grp")),
            by = c("iso_code" = "adm0_a3")) %>% 
  # merge some economy categories together into larger groups
  mutate(economy_coarse = case_when(
    str_detect(economy, "Developed") ~ "1. Developed region",
    str_detect(economy, "Emerging")  ~ "2. Emerging region",
    str_detect(economy, "Developing") ~ "3. Developing region",
    TRUE ~ "4. Least developed region"
  )) 

nrec_full2020 <- nrec_full2020 %>% 
  # add country codes and names
  left_join(data_all_covid %>%
              distinct(Country, .keep_all = TRUE) %>%
              select(Country, countrycode, iso_code),
            by = "Country") %>% 
  left_join(ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
              select(c("name_long", "adm0_a3", "economy", "income_grp")),
            by = c("iso_code" = "adm0_a3")) %>% 
  # merge some economy categories together into larger groups
  mutate(economy_coarse = case_when(
    str_detect(economy, "Developed") ~ "1. Developed region",
    str_detect(economy, "Emerging")  ~ "2. Emerging region",
    str_detect(economy, "Developing") ~ "3. Developing region",
    TRUE ~ "4. Least developed region"
  )) 

# before removing NA rows, save country list showing analysed or not
data_socioecon <- data_lockdown_change %>% 
  distinct(Country, countrycode, iso_code, analysed, economy, economy_coarse, income_grp,
           Change_records, Change_eBird_records)


save(nrec_full2020, data_socioecon, data_lockdown_change,
     file = "data/data_change_records.RData")
