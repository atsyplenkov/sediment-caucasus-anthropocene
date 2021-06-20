library(tidyverse)
library(corrr)
library(atslib)
library(see)
library(extrafont)
library(readxl)
library(stringi)

load("data/tidy/boot_results.Rdata")

theme_set(
  theme_lucid(legend.position = "bottom",
              base_family = "Noto Sans") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_line(color = "grey50"))
)

ter_gvk <- read_xlsx("data/raw/terek_SY.xlsx",
                     sheet = 1,
                     range = "A1:C39") %>% 
  rename(fullname = 1,
         area = 3) %>% 
  mutate(fullname = stri_trans_general(fullname,
                                       "russian-latin/bgn")) 

ter_alt <- read_xlsx("analysis/table1.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::select(label, alt_group, altitude_m)

# Trend distribution ------------------------------------------------------
trend_dens <- ter_boot %>% 
  dplyr::select(label,
                id,
                ssl = dec_change) %>% 
  left_join(ter_gvk,
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  drop_na(alt_group) %>% 
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  ggplot(aes(x = ssl,
             y = ..scaled..,
             color = alt_group,
             fill = alt_group)) +
  geom_vline(aes(xintercept = 0),
             size = .5) +
  geom_density(alpha = .5) +
  geom_segment(data = . %>%
                 group_by(alt_group) %>%
                 summarise(med = density(ssl)$x[which.max(density(ssl)$y)]),
               aes(x = med,
                   xend = med,
                   y = 0,
                   yend = 1,
                   color = alt_group),
               linetype = "dashed",
               show.legend = F) +
  geom_text(data = . %>%
              group_by(alt_group) %>%
              summarise(med = density(ssl)$x[which.max(density(ssl)$y)]) %>% 
              mutate(med = smart_round(med)),
            aes(x = med,
                y = 1,
                label = med,
                hjust = 0.5,
                vjust = -0.5),
            size = 3,
            family = "Noto Sans",
            show.legend = F) +
  coord_cartesian(xlim = c(-3.1, 1.5), ylim = c(-0.005, 1.05),
                  expand = F) +
  scale_color_metro() +
  scale_fill_metro() +
  labs(color = "Altitude group",
       fill = "Altitude group",
       x = expression("SSL change, %"%.%"yr"^-1),
       y = "Density") +
  theme(plot.margin = unit(c(0.1,
                             0.5, # right
                             -0.1, # bottom
                             0.1), "cm"))

# 0) Trend vs altitude ----------------------------------------------------
ter_boot_ci %>% 
  left_join(ter_gvk,
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  # dplyr::filter(alt_group == "500-1000") %>% 
  filter(between(mean, -10, 5)) %>%
  dplyr::select(mean, altitude_m) %>%
  drop_na() %>% 
  cor.test(~altitude_m+mean,
           data = .,
           method = "spearman")

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
        y = expression("SSL change, %"%.%"yr"^-1),
        fill = "Altitude group: ")) +
  theme(legend.position = "")

ter_boot_ci %>% 
  left_join(ter_gvk,
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  dplyr::filter(alt_group == "500-1000") %>% 
  filter(between(mean, -10, 5)) %>%
  dplyr::select(mean, area) %>%
  drop_na() %>% 
  cor.test(~area+mean,
           data = .,
           method = "spearman")

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
        y = expression("SSL change, %"%.%"yr"^-1),
        color = "Altitude group: "))

ter_boot_ci %>% 
  left_join(gl_unc %>% 
              filter(year == 1986) %>% 
              rename(lower = `2.5%`,
                     upper = `97.5%`),
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  filter(between(mean, -10, 5)) %>%
  drop_na(glacier) %>% 
  filter(mean < 0) %>%
  filter(glacier > 1) %>% 
  cor.test(~glacier+mean,
           data = .,
           method = "spearman")

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
       y = expression("SSL change, %"%.%"yr"^-1),
       color = "Altitude group: "))

ter_boot_ci %>% 
  left_join(crop_unc %>% 
              filter(year == 1987) %>% 
              rename(lower = `2.5%`,
                     upper = `97.5%`),
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  filter(between(mean, -10, 5)) %>%
  drop_na(crop) %>% 
  filter(mean < 0) %>%
  cor.test(~crop+mean,
           data = .,
           method = "spearman")

# 4) Trend vs forest -----------------------------------------------------
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
         y = expression("SSL change, %"%.%"yr"^-1),
         color = "Altitude group: "))

ter_boot_ci %>% 
  left_join(forest_unc %>% 
              filter(year == 1987) %>% 
              rename(lower = `2.5%`,
                     upper = `97.5%`),
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  filter(between(mean, -10, 5)) %>%
  drop_na(forest) %>% 
  filter(mean < 0) %>%
  cor.test(~forest+mean,
           data = .,
           method = "spearman")

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

# 6) Bootstrap with area, altitude ----------------------------------------
boot_area <- ter_boot %>% 
  dplyr::select(label, id, ssl = dec_change) %>% 
  left_join(ter_gvk,
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  left_join(
    forest_unc %>% 
      filter(year %in% c(1987)) %>% 
      dplyr::select(label, year, forest) %>% 
      pivot_wider(names_from = year,
                  values_from = forest) %>% 
      rename_at(vars(-1),
                ~paste0("forest_", .)),
    by = "label"
  ) %>% 
  left_join(
    crop_unc %>% 
      filter(year %in% c(1987)) %>% 
      dplyr::select(label, year, crop) %>% 
      pivot_wider(names_from = year,
                  values_from = crop) %>% 
      rename_at(vars(-1),
                ~paste0("crop_", .)),
    by = "label"
  ) %>% 
  left_join(
    gl_unc %>% 
      filter(year %in% c(1986)) %>%
      dplyr::select(label, year, glacier) %>% 
      pivot_wider(names_from = year,
                  values_from = glacier) %>% 
      rename_at(vars(-1),
                ~paste0("glacier_", .)),
    by = "label"
  ) %>% 
  dplyr::select(-fullname) %>% 
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  mutate(alt_group = fct_recode(alt_group,
                                "(a) < 500" = "< 500",
                                "(b) 500-1000" = "500-1000",
                                "(c) > 1000" = "> 1000"))

boot_cor_area <-
  boot_area %>% 
  dplyr::select(-label) %>% 
  group_by(alt_group, id) %>% 
  nest() %>%
  mutate(cor = map(data,
                   ~corrr::correlate(.x,
                                     method = "spearman", 
                                     use = "pairwise.complete.obs",
                                     quiet = T) %>% 
                     corrr::focus(ssl))) %>% 
  unnest(cols = c(cor))

# boot_cor_area %>%
#   ggplot(aes(y = ssl,
#              x = term)) +
#   geom_hline(yintercept = 0) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(-1, 1, .2),
#                      minor_breaks = NULL) +
#   labs(subtitle = "(e)",
#        x = NULL,
#        y = "Spearman r") +
#   theme(panel.grid.major.y = element_line(color = "grey80"))

boot_cor_area_plot <- boot_cor_area %>%
  drop_na(alt_group) %>% 
  group_by(alt_group, term) %>% 
  mutate(outlier.high = ssl > quantile(ssl, .75) + 1.50*IQR(ssl),
         outlier.low = ssl < quantile(ssl, .25) - 1.50*IQR(ssl),
         outlier.color = case_when(outlier.high ~ "red",
                                   outlier.low ~ "red",
                                   outlier.low == F | outlier.high == F ~ "black")) %>% 
  mutate(term = case_when(
    str_detect(term, "alt") ~ "Altitude",
    str_detect(term, "area") ~ "Area",
    str_detect(term, "gl") ~ "Glacier",
    str_detect(term, "crop") ~ "Cropland",
    str_detect(term, "forest") ~ "Forest"
  )) %>%
  ggplot(aes(y = ssl,
             x = term)) +
  geom_jitter(
    aes(color = outlier.color),
              width = .3,
              alpha = .05,
              show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  # geom_boxplot(outlier.alpha = 0.5, outlier.size = .7) +
  scale_y_continuous(breaks = seq(-1, 1, .2),
                     minor_breaks = NULL) +
  labs(x = NULL,
       y = "Spearman r") +
  ggsci::scale_color_lancet() +
  facet_wrap(~alt_group,
             nrow = 2, ncol = 2) +
  theme(panel.grid.major.y = element_line(color = "grey80"),
        strip.background = element_blank())

# Combine -----------------------------------------------------------------
trend_unc_plots <- ggpubr::ggarrange(trend_area, trend_glacier,
                  trend_crop, trend_forest,
                  nrow = 2,
                  ncol = 2,
                  common.legend = T,
                  legend = "bottom")

ggsave("figures/fig6_trend_density.png",
       trend_dens,
       dpi = 500,
       w = 7, h = 4)

ggsave("figures/fig7_trend_unc_plots.png",
       trend_unc_plots,
       dpi = 500,
       w = 8, h = 8)

ggsave("figures/fig8_boot_cor_plot.png",
       boot_cor_area_plot,
       dpi = 500,
       w = 7, h = 6)
