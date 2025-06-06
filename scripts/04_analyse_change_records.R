# run change_records analyses and save outputs

library(dplyr)   
library(glue)
library(lubridate)
library(DHARMa)
library(nlme)     # for GLS
library(ggplot2)
library(sf)
library(patchwork)
library(effects)  # for plotting conditional effects of the models

# load change data
load("data/data_change_records.RData")



# helper function for continuous predictors
plot_effect_cont <- function(var, xlab_text, data) {
  
  df <- data.frame(effect(quo_name(enquo(var)), data, xlevels = 100))
  ggplot(df, aes(x = !!enquo(var), y = fit)) +
    geom_smooth(aes(ymin = lower, ymax = upper),
                stat = "identity", color = "black", fill = "lightblue", alpha = 0.5) +
    geom_hline(yintercept = 2) + # no change
    theme_bw() +
    ylab("log10(Change_records +100)") +
    xlab(xlab_text) +
    ylim(1.35, 2.4)
  
}

# plot conditional plots for the model's variables, using the effects package
create_cond_eff_plot <- function(data, full = TRUE) {
  
  p1 <- plot_effect_cont(Stringency_index, "Stringency index", data)
  p2 <- plot_effect_cont(Change_park_visitors, "Change in park visitors (%)", data)
  p3 <- plot_effect_cont(log10_Population, "log10(Population size)", data)
  
  p4 = data.frame(effect("economy_coarse", data)) %>% 
    ggplot() +
    geom_errorbar(aes(x=economy_coarse, ymin=lower, ymax=upper),
                  width=0.4, colour="lightblue", alpha=0.9, linewidth=1) +
    geom_point(aes(economy_coarse, fit), size=2) +
    geom_hline(yintercept = 2) + # no change
    theme_bw() + 
    ylab("log10(Change_records + 100)") + xlab("Economic class") + 
    ylim(1.35, 2.4)+
    theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=0.8))
  
  p_full <- if (full == TRUE) {
    (p1 | p2) / (p3 | p4)
  } else {
    p2 / p4
  }
  return(p_full)
  
}

create_map <- function(data) {
  
  data %>% 
    mutate(Change_records_cat = cut(Change_records,
                                    breaks = c(-100, -50, -25, -10, 0, 10, 25, 50, 100, 82000),
                                    labels = c("-100 to -50", "-50 to -25", "-25 to -10", "-10 to 0",
                                               "0 to 10", "10 to 25", "25 to 50", "50 to 100", "100 and above"),
                                    include.lowest = TRUE,
                                    right = FALSE)) %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = Change_records_cat)) +
    # scale_fill_brewer(palette = "RdBu", na.value = "grey50")
    # since +ve values have more bins than -ve, using manual fill scale
    scale_fill_manual(values = c("#f46d43", "#fdae61", "#fee090", "#ffffbf", 
                                 "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"), 
                      na.value = "grey50",
                      name = "Change in records (%)")  
  
}


# REGRESSION MODEL ----------------------------------

data_reg <- data_lockdown_change %>% 
  # (remove countries with NA for Google Mobility metrics, only 129 countries left)
  filter(if_all(-c(Change_records, Change_eBird_records), ~ !is.na(.))) %>% 
  st_drop_geometry() %>% 
  # doing transformations because apparently there are outliers
  # and records needs 100 offset to make all +ve before log
  mutate(log10_Population = log10(Population),
         log10_Change_records = log10(Change_records + 100))

data_reg_orig <- data_reg %>% filter(measure == "mean", year == 2020)

# GLS with different variance structure for each economy class
gls_orig <- nlme::gls(log10_Change_records ~ Stringency_index + Change_park_visitors + log10_Population + 
                        economy_coarse,
                      data = data_reg_orig, weights = varIdent(form = ~ 1 | economy_coarse))


data_reg_mean_2021 <- data_reg %>% filter(measure == "mean", year == 2021)

gls_mean_2021 <- nlme::gls(log10_Change_records ~ Stringency_index + Change_park_visitors + log10_Population + 
                             economy_coarse,
                           data = data_reg_mean_2021, weights = varIdent(form = ~ 1 | economy_coarse))


# summary(gls_mean_2021) #; plot(gls_mean_2021)
# anova(gls_mean_2021)
# plot(resid(gls_mean_2021, type="normalized") ~ fitted(gls_mean_2021))
# plot(resid(gls_mean_2021, type="normalized") ~ data_reg_mean_2021$Stringency_index)
# plot(resid(gls_mean_2021, type="normalized") ~ data_reg_mean_2021$log10_Population)
# boxplot(resid(gls_mean_2021, type="normalized") ~ data_reg_mean_2021$economy_coarse)
# E = resid(gls_mean_2021, type="normalized")  
# coplot(E ~ fitted(gls_mean_2021) | economy_coarse, data = data_reg_mean_2021) # variance is now homogeneous across economic classes


# save plot to file
ggsave(plot = create_cond_eff_plot(gls_mean_2021, full = TRUE), "outputs/cond_eff_mean_2021.png", 
       width = 7, height = 7, units = "in", dpi = 300)  # also saved as pdf 6x6 inches


# WORLD MAP -----------------------------

# make a map of the new Change_records

sf::sf_use_s2(FALSE)

# data_lockdown_change %>% 
#   filter(measure == "mean", year == 2020, 
#          # remove Antarctica to gain more space
#          Country != "Antarctica") %>% 
#   create_map()

map2021 <- data_lockdown_change %>% 
  filter(measure == "mean", year == 2021, 
         # remove Antarctica to gain more space
         Country != "Antarctica") %>% 
  create_map() +
  labs(title = "2021 lockdown") +
  theme_void() +
  theme(panel.border = element_rect(fill = NA))

map_full2020 <- nrec_full2020 %>% 
  filter(Country != "Antarctica") %>% 
  create_map() +
  labs(title = "2020 year") +
  theme_void() +
  theme(panel.border = element_rect(fill = NA))

full_plot <- (map2021 / map_full2020) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag.location = "plot",
        plot.tag.position = c(-0.05, 0.95),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10, "pt"))

ggsave(full_plot, file = "outputs/map_full.png", 
       width = 12, height = 8, units = "in", 
       dpi = 300)

