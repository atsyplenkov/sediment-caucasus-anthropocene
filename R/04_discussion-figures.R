library(tidyverse)
library(corrr)
library(atslib)
library(see)

load("data/tidy/boot_results.Rdata")

theme_set(
  theme_lucid(legend.position = "bottom") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_line(color = "grey50"))
)

t1 <- ter_ols_1987 %>% 
  filter(var == "ssl87") %>% 
  select(label,
         ssl = dec_change)

t2 <- crop_ols %>% 
  filter(var == "crop") %>% 
  select(label,
         crop = dec_change)


t3 <- forest_ols %>% 
  filter(var == "forest") %>% 
  select(label,
         forest = dec_change)

t4 <- gl_ols %>% 
  filter(var == "glacier") %>% 
  select(label,
         glacier = dec_change)

t1 %>% 
  left_join(t2) %>% 
  left_join(t3) %>% 
  left_join(t4) %>% 
  drop_na(crop) %>% 
  filter(forest > 0) %>%
  filter(crop < 0) %>% 
  select(-label) %>% 
  corrr::correlate(method = "spearman")

t1 %>% 
  left_join(t2) %>% 
  left_join(t3) %>% 
  left_join(t4) %>% 
  gather(var, val, -label, -ssl) %>% 
  filter(between(ssl, -10, 5)) %>% 
  ggplot(aes(y = ssl, x = val)) +
  geom_point(alpha = .4) +
  # atslib::Add_R2() +
  facet_wrap(~var,
             scales = "free")
 

# 1) Trend VS Area --------------------------------------------------------
(trend_area <- ter_boot_ci %>% 
   left_join(ter_gvk,
             by = "label") %>% 
   left_join(ter_alt,
             by = "label") %>% 
   mutate(alt_group = as_factor(alt_group)) %>% 
   mutate(alt_group = fct_relevel(alt_group,
                                  "< 500", "500-1000")) %>% 
   filter(between(mean, -10, 5)) %>%
   drop_na(area) %>% 
   ggplot(aes(y = mean,
              x = area,
              color = alt_group)) +
   geom_errorbar(aes(ymin = `2.5%`,
                     ymax = `97.5%`),
                 color = "grey70") +
   geom_point(aes(fill = alt_group),
              size = 2,
              color = "grey10",
              shape = 21) +
   # ggrepel::geom_text_repel(data = . %>% filter(alt_group == "> 1000"),
   #                          aes(label = label)) +
   geom_smooth(method = "lm",
               show.legend = F,
               linetype = "dashed",
               se = F) +
   Add_R2(add_line = F,
          show.legend = F) +
   scale_color_metro() +
   scale_fill_metro() +
   scale_x_log10() +
   annotation_logticks(sides = "b",
                       color = "grey50") +
   labs(subtitle = "(a)",
        x = expression("Catchment area, km"^2),
        y = "SSL change, %",
        fill = "Altitude group: ")) +
  theme(legend.position = "")

# 2) Trend VS Glacier -----------------------------------------------------
(trend_glacier <- ter_boot_ci %>% 
   left_join(gl_unc %>% 
               filter(year == 1986) %>% 
               rename(lower = `2.5%`,
                      upper = `97.5%`),
             by = "label") %>% 
   left_join(ter_alt,
             by = "label") %>% 
   mutate(alt_group = as_factor(alt_group)) %>% 
   mutate(alt_group = fct_relevel(alt_group,
                                  "< 500", "500-1000")) %>% 
   filter(between(mean, -10, 5)) %>%
   drop_na(glacier) %>% 
   filter(mean < 0) %>%
   filter(glacier > 1) %>%
   ggplot(aes(y = mean,
              x = glacier)) +
   geom_errorbar(aes(ymin = `2.5%`,
                     ymax = `97.5%`),
                 color = "grey70") +
   geom_errorbar(aes(xmin = lower,
                     xmax = upper),
                 color = "grey70") +
   geom_point(aes(fill = alt_group),
              size = 2,
              color = "grey10",
              shape = 21) +
   geom_smooth(method = "lm",
               se = F,
               linetype = "dashed",
               color = "black") +
   Add_R2(add_line = F) +
   scale_fill_metro() +
   scale_x_log10() +
   annotation_logticks(sides = "b",
                       color = "grey50") +
   labs(subtitle = "(b)",
        x = "Glacier area in 1986, %",
        y = "SSL change, %",
        color = "Altitude group: "))

# 3) Trend VS Cropland -----------------------------------------------------
(trend_crop <- ter_boot_ci %>% 
  left_join(crop_unc %>% 
              filter(year == 1987) %>% 
              rename(lower = `2.5%`,
                     upper = `97.5%`),
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  filter(between(mean, -10, 5)) %>%
  drop_na(crop) %>% 
  filter(mean < 0) %>%
  ggplot(aes(y = mean,
             x = crop)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_errorbar(aes(xmin = lower,
                    xmax = upper),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 2,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              color = "black",
              linetype = "dashed") +
  Add_R2(add_line = F) +
  scale_fill_metro() +
  scale_x_log10() +
  annotation_logticks(sides = "b",
                      color = "grey50") +
  labs(subtitle = "(c)",
       x = "Cropland area in 1987, %",
       y = "SSL change, %",
       color = "Altitude group: "))

(trend_forest <- ter_boot_ci %>% 
    left_join(forest_unc %>% 
                filter(year == 1987) %>% 
                rename(lower = `2.5%`,
                       upper = `97.5%`),
              by = "label") %>% 
    left_join(ter_alt,
              by = "label") %>% 
    mutate(alt_group = as_factor(alt_group)) %>% 
    mutate(alt_group = fct_relevel(alt_group,
                                   "< 500", "500-1000")) %>% 
    filter(between(mean, -10, 5)) %>%
    drop_na(forest) %>% 
    filter(mean < 0) %>%
    ggplot(aes(y = mean,
               x = forest)) +
    geom_errorbar(aes(ymin = `2.5%`,
                      ymax = `97.5%`),
                  color = "grey70") +
    geom_errorbar(aes(xmin = lower,
                      xmax = upper),
                  color = "grey70") +
    geom_point(aes(fill = alt_group),
               size = 2,
               color = "grey10",
               shape = 21) +
    geom_smooth(method = "lm",
                se = F,
                show.legend = F,
                color = "black",
                linetype = "dashed") +
    Add_R2(add_line = F) +
    scale_fill_metro() +
    # scale_color_metro() +
    scale_x_log10() +
    annotation_logticks(sides = "b",
                        color = "grey50") +
    labs(subtitle = "(d)",
         x = "Forest area in 1987, %",
         y = "SSL change, %",
         color = "Altitude group: "))

# 13) Monte Carlo correlation ---------------------------------------------
boot_change <- ter_boot_1987 %>% 
  transmute(label,
            id,
            ssl_change = dec_change) %>% 
  left_join(gl_boot %>% 
              transmute(
                label,
                id,
                gl_change = dec_change
              ),
            by = c("label", "id")) %>% 
  left_join(crop_boot %>% 
              transmute(
                label,
                id,
                crop_change = dec_change
              ),
            by = c("label", "id")) %>% 
  left_join(forest_boot %>% 
              transmute(
                label,
                id,
                forest_change = dec_change
              ),
            by = c("label", "id")) 

boot_cor <- boot_change %>% 
  select(-label) %>% 
  group_by(id) %>% 
  nest() %>%
  mutate(cor = map(data,
                   ~corrr::correlate(.x,
                                     method = "spearman",
                                     use = "pairwise.complete.obs",
                                     quiet = T) %>% 
                     corrr::focus(ssl_change))) %>% 
  unnest(cols = c(cor))

(trend_cor <- boot_cor %>% 
    mutate(term = case_when(
      str_detect(term, "gl") ~ "Glacier",
      str_detect(term, "crop") ~ "Cropland",
      str_detect(term, "forest") ~ "Forest"
    )) %>% 
    ggplot(aes(y = ssl_change,
               x = term)) +
    geom_hline(yintercept = 0) + 
    geom_boxplot() +
    scale_y_continuous(breaks = seq(-1, 1, .2),
                       minor_breaks = NULL) +
    labs(subtitle = "(e)",
         x = NULL,
         y = "Spearman r") +
    theme(panel.grid.major.y = element_line(color = "grey80")))

# Combine -----------------------------------------------------------------
ggpubr::ggarrange(trend_area, trend_glacier,
                  trend_crop, trend_forest,
                  nrow = 2,
                  ncol = 2,
                  common.legend = T,
                  legend = "bottom")

# library(patchwork)
# 
# (trend_area + trend_glacier)/
#   (trend_crop + trend_forest)/
#   trend_cor +
#   plot_layout(guides = "collect")
