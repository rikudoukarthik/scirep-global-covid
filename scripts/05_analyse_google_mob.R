# explore what Google Mobility data looks like in detail

library(dplyr)   
library(stringr)
library(ggplot2)
library(patchwork)
library(sf)
library(Hmisc) # for CI
library(rnaturalearth)


# load stringency index & mobility data
load("data/data_all_covid.RData")


data_all_covid <- data_all_covid %>% 
  # add economic
  left_join(ne_countries(scale = "medium", type = "countries", returnclass = c("sf")) %>%
              select(c("adm0_a3", "economy")),
            by = c("iso_code" = "adm0_a3")) %>% 
  # merge some economy categories together into larger groups
  mutate(economy_coarse = case_when(
    str_detect(economy, "Developed") ~ "1. Developed region",
    str_detect(economy, "Emerging")  ~ "2. Emerging region",
    str_detect(economy, "Developing") ~ "3. Developing region",
    TRUE ~ "4. Least developed region"
  )) 

# combine multiple to facet
data_facet <- data_all_covid %>% 
  mutate(panel = "All regions") %>% 
  bind_rows(data_all_covid %>% 
              mutate(panel = economy_coarse)) %>% 
  bind_rows(data_all_covid %>% 
              filter(Country == "India") %>% 
              mutate(panel = "India")) %>% 
  mutate(panel = factor(panel, levels = c(
    "All regions", "1. Developed region", "2. Emerging region", "3. Developing region",
    "4. Least developed region", "India"
    )))


create_mob_plot <- function(data, metric) {
  
  if (metric == "park") {
    plot_limits <- c(-100, 600)
    plot_breaks <- c(-100, 0, 200, 400, 600)
    plot_axis_lab <- "Change in park visits (%)"
  } else if (metric == "home") {
    plot_limits <- c(-80, 80)
    plot_breaks <- seq(-80, 80, by = 20)
    plot_axis_lab <- "Change in time at home (%)"
  }

  data %>% 
    na.omit() %>% 
    mutate(year = factor(year),
           month = factor(month, levels = as.factor(1:12))) %>% 
    ggplot(aes(x = month, 
               y = if (metric == "park") Change_park_visitors else Change_time_at_home, 
               col = year, group = year, fill = year)) +
    geom_point(alpha = 0.1, position = position_dodge(width = 0.75)) +
    stat_summary(fun = mean, geom = "point", 
                 col = "black", shape = 21, size = 2.5, stroke = 2,
                 position = position_dodge(width = 0.75)) +
    # stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
    #              col = "black", width = 0.2, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0) +
    scale_fill_brewer(palette = "Dark2", name = "Year") +
    scale_colour_brewer(palette = "Dark2", name = "Year") +
    scale_y_continuous(limits = plot_limits, breaks = plot_breaks, name = plot_axis_lab) +
    theme_classic() +
    theme(legend.position = "bottom") +
    facet_wrap(~ panel, ncol = 3) +
    labs(x = "Month of year")
  
}


# plot_mob <- (create_mob_plot(data_facet, metric = "park") /
#   create_mob_plot(data_facet, metric = "home")) +
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = "A") &
#   theme(legend.position = "top",
#         plot.tag.location = "plot")


ggsave(create_mob_plot(data_facet, metric = "park"), file = "outputs/mobility_parks.png",
       width = 14, height = 7, units = "in", dpi = 300)
ggsave(create_mob_plot(data_facet, metric = "home"), file = "outputs/mobility_homes.png",
       width = 14, height = 7, units = "in", dpi = 300)
