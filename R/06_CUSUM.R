library(tidyverse)
library(readxl)
library(visdat)
library(naniar)
library(imputeTS)
library(see)
library(extrafont)
library(naniar)
library(atslib)

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

theme_set(theme_hp())

load("data/tidy/terraclim.Rdata")
load("data/tidy/models.Rdata")

# 1) Gap filling ----------------------------------------------------------
terek <- read_xlsx("data/raw/terek_SY.xlsx",
                   sheet = "data_kgs",
                   # skip = 1,
                   range = "A2:AV96") %>% 
  rename(year = 1) %>% 
  gather(label, ssl, -year)

major_add <- terek %>% 
  filter(label %in% c(
    "Ard-Ta",
    "Bel-Ko",
    "Fia-Ta",
    "Cheg-Nc",
    "Mal-Ka",
    "Sun-Br"
  )) %>% 
  filter(year < 1958) %>% 
  group_by(label) %>% 
  mutate(ssl2 = imputeTS::na_interpolation(ssl)) %>% 
  ungroup() %>% 
  filter(year > 1945)

ssl_db <- major_df %>% 
  left_join(terraclim,
            by = c("label", "year")) %>% 
  mutate(ssl2 = ifelse(label == "Uru-Kh" & year < 1966,
                       NA_real_,
                       ssl2)) %>% 
  drop_na(ssl2) %>% 
  ungroup() %>% 
  bind_rows(major_add %>% 
              mutate(flag = "Observed")) %>% 
  group_by(label) %>% 
  arrange(year,
          .by_group = T)

# 2) Imputation period ----------------------------------------------------------------
major_rivers <- c(
  "Ard-Ta",
  "Bel-Ko",
  "Che-Ba",
  "Cheg-Nc",
  "Fia-Ta",
  "Kam-Ol",
  "Mal-Ka",
  "Sun-Br",
  "Uru-Kh"
)

ssl_db %>% 
  filter(year >= 1945) %>%
  filter(label %in% major_rivers) %>%
  dplyr::select(label, year, ssl2, flag) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = label)) + 
  geom_errorbarh(aes(xmax = year,
                     xmin = year,
                     color = flag),
                 size = 2,
                 alpha = .6) +
  labs(x = "",
       y = "",
       color = "Mean Annual SSD") +
  scale_x_continuous(breaks = seq(1958, 2020, by = 10)) +
  scale_color_metro(reverse = T)

# 3) CUSUM ----------------------------------------------------------------
ssl_cu <- ssl_db %>% 
  group_by(label) %>% 
  drop_na(ssl2) %>% 
  mutate(ssl2 = 31.5 * ssl2) %>% # t*10^3
  mutate(sy_mean = mean(ssl2),
         sy_cv = sd(ssl2)/sy_mean) %>% 
  mutate(K = ssl2/sy_mean - 1) %>%
  mutate(CDC = cumsum(K)/sy_cv) %>% 
  ungroup()

# 4) Pettitt test ------------------------------------------------------------
library(trend)
pett <- ssl_db %>% 
  group_by(label) %>% 
  nest() %>% 
  mutate(pt = map_int(data,
                      ~trend::pettitt.test(.x$ssl2)$estimate)) %>% 
  mutate(p = map_dbl(data,
                     ~trend::pettitt.test(.x$ssl2)$p.value)) %>% 
  mutate(start = map_dbl(data, ~first(.x$year)),
         end = map_dbl(data, ~last(.x$year))) %>% 
  select(-data) %>% 
  ungroup() %>% 
  mutate(break_year = c(start:end)[pt]) %>% 
  mutate(p_factor = ifelse(p < 0.05, "sign", "non"))

# Taylor's method ---------------------------------------------------------
library(ChangePointTaylor)
library(ggpubr)
library(ggrepel)

set.seed(2021)
ssl_taylor <- ssl_db %>% 
  group_by(gage = label) %>% 
  nest() %>% 
  mutate(taylor = map(data,
                      ~change_point_analyzer(
                        x = .x$ssl2,
                        label = .x$year,
                        min_tbl_conf = .6, 
                        n_bootstraps = 10000,
                        method = "CUSUM"
                      ))) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = c(taylor)) %>% 
  filter(change_conf >= .8)  

# CUSUM plots -------------------------------------------------------------
cusum_plots <- major_rivers %>% 
  map(function(NO)
    ssl_cu %>% 
      filter(label == NO) %>% 
      ggplot(aes(x = year,
                 y = CDC)) +
      geom_line() +
      geom_point(aes(fill = flag),
                 shape = 21) +
      geom_vline(data = ssl_taylor %>%
                   filter(gage == NO),
                 aes(xintercept = label,
                     linetype = "(Taylor, 2000)",
                     color = "(Taylor, 2000)")) +        
      geom_vline(data = pett %>%
                   filter(label == NO),
                 aes(xintercept = break_year,
                     linetype = "(Pettitt, 1979)",
                     color = "(Pettitt, 1979)")) +
      geom_text(data = pett %>%
                  filter(label == NO),
                aes(x = break_year,
                    y = Inf,
                    label = break_year,
                    hjust = 0.5,
                    vjust = -1.2),
                color = "#E51400",
                size = 3,
                family = "Roboto Condensed") +    
      geom_text(data = ssl_taylor %>%
                  filter(gage == NO),
                aes(x = label,
                    y = Inf,
                    label = label,
                    hjust = 0.5,
                    vjust = -0.2),
                size = 3,
                color = "#647687",
                family = "Roboto Condensed") +
      coord_cartesian(clip = "off") +
      labs(title = NO,
           fill = "Mean Annual SSD",
           color = "Changepoint test",
           linetype = "Changepoint test",
           x = "",
           y = "") +
      scale_linetype_manual(values = c("dashed",
                                       "solid")) +
      scale_x_continuous(breaks = seq(1925, 2020, by = 10)) +
      see::scale_color_metro(reverse = T) +
      see::scale_fill_flat(reverse = T) +
      theme(plot.margin = unit(c(0.1,
                                 0.5, 
                                 -0.1, 
                                 0.1), "cm"))) %>% 
  ggpubr::ggarrange(plotlist = .,
                    common.legend = T, 
                    legend = "bottom") %>% 
  ggpubr::annotate_figure(left = "CUSUM SSD")

cusum_plots

# SAVE --------------------------------------------------------------------
ggsave("figures/fig4_cusum-plots.png",
       cusum_plots,
       dpi = 600,
       w = 10, h = 6)

ssl_taylor %>% 
  ungroup() %>% 
  transmute(Label = gage,
            Year = label,
            `Confidence interval` = qdap::bracketXtract(`CI (95%)`),
            `Confidence level` = scales::percent(change_conf,
                                                 accuracy = 0.1),
            From = smart_round(From),
            To = smart_round(To)
  ) %>% 
  unnest(cols = c(`Confidence interval`)) %>% 
  left_join(pett %>% 
              transmute(Label = label,
                        `Year (Pettitt, 1979)` = break_year,
                        `p-value` = smart_round(p)),
            by = "Label") %>% 
  writexl::write_xlsx("analysis/table4_cusum.xlsx")
