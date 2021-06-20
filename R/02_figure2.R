library(tidyverse)
library(see)
library(lubridate)
library(readxl)
library(atslib)
library(extrafont)
library(sf)
library(glue)

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

# 1) SY record ------------------------------------------------------------
terek <- read_xlsx("data/raw/terek_SY.xlsx",
                   sheet = "month_kgs") %>% 
  gather(label, sr, -year, -month)

ter_alt <- st_read("data/spatial/ter_gages.shp") %>% 
  st_drop_geometry() %>% 
  dplyr::select(label, H=H_mean) %>% 
  mutate(alt_group = case_when(
    H <= 500 ~ "< 500",
    dplyr::between(H, 500, 1000) ~ "500-1000",
    H >= 1000 ~ "> 1000"
  )) %>% 
  mutate(alt_group = as_factor(alt_group),
         alt_group = fct_relevel(alt_group,
                                 c("< 500",
                                   "500-1000",
                                   "> 1000")))

# 2) Data availability ----------------------------------------------------
terek %>% 
  group_by(label, year) %>% 
  summarise(mean = mean(sr, na.rm = T),
            cv = sd(sr, na.rm = T)/mean) %>% 
  drop_na() %>% 
  ggplot(aes(x=year, y = label)) + 
  geom_errorbarh(aes(xmax = year, xmin = year), size = 2) +
  scale_x_continuous(name = "",
                     limits = c(1925, 2020),
                     breaks = seq(1925, 2020, by = 10))

monthly_data_avail <- terek %>% 
  group_by(label, year) %>% 
  summarise(mean = mean(sr, na.rm = T),
            .groups = "drop") %>%
  group_by(label) %>% 
  summarise(nn = sum(!is.na(mean)), .groups = "drop")

# 3) Hydrograph ------------------------------------------------------------
major_rivers <- c(
  "Ard-Ta",
  "Bel-Ko",
  "Che-Ba",
  "Cheg-Nc",
  "Mal-Ka",
  "Sun-Br",
  "Uru-Kh",
  "Ter-Kaz",
  "Ard-Nz",
  "Arg-Du"
)

(ter_hydrographs <- 
  terek %>% 
  filter(label %in% major_rivers) %>% 
  left_join(monthly_data_avail,
            by = "label") %>%  
  left_join(ter_alt,
            by = "label") %>%
  mutate(label = glue("{label} ({nn} yr)")) %>% 
  mutate(month = as.integer(month)) %>% 
  group_by(label, month) %>% 
  summarise(ssl_month = mean(sr, na.rm = T),
            ssl_se = sd(sr, na.rm = T)/sqrt(sum(!is.na(sr))),
            alt_group = unique(alt_group),
            .groups = "drop") %>%
  ggplot(aes(x = month,
             y = ssl_month)) +
  geom_ribbon(aes(ymin = ssl_month - ssl_se,
                  ymax = ssl_month + ssl_se,
                  fill = label),
              alpha = .4) +
  geom_line(aes(color = label)) +
  geom_point(aes(color = label),
             shape = 21,
             size = 1,
             show.legend = F) +
  see::scale_color_metro() +
  see::scale_fill_metro() +
  scale_x_continuous(breaks = c(1:12),
                     minor_breaks = NULL,
                     labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = 0.001)) +
  labs(x = "",
       y = expression(italic("SSD")*", kg"%.%"s"^-1),
       color = "",
       fill = "") +
  facet_wrap(~alt_group, scales = "free_y")
  )

# SAVE --------------------------------------------------------------------
ggsave("figures/fig2_hydrographs.png",
       ter_hydrographs,
       dpi = 500,
       w = 12.5,
       h = 5)

