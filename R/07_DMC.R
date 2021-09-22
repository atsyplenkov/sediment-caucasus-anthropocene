library(tidyverse)
library(readxl)
library(visdat)
library(naniar)
library(imputeTS)
library(see)
library(extrafont)
library(naniar)

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

load("data/tidy/terraclim.Rdata")
load("data/tidy/models.Rdata")

# 1) Data reading ----------------------------------------------------------
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


day_spl <- tibble(
  label = c(
    "Ard-Ta", "Bel-Ko", "Che-Ba",
    "Cheg-Nc", "Fia-Ta", "Kam-Ol",
    "Mal-Ka", "Sun-Br", "Uru-Kh"
  ), 
  break_year = c(
    1992, 2009, 1989,
    1987, 1984, 1993,
    1989, 1993, 2005
  )
)

ssl_dmc <- ssl_db %>%
  filter(year > 1957) %>% 
  left_join(day_spl,
            by = "label") %>% 
  mutate(spl = ifelse(year <= break_year,
                      "before",
                      "after")) %>%   
  mutate(spl = ifelse(label == "Uru-Kh" & year < 1982,
                      NA,
                      spl)) %>% 
  mutate(spl = ifelse(label == "Kam-Ol" & year < 1981,
                      NA,
                      spl)) %>% 
  filter(!is.na(spl)) %>% 
  mutate(spl = as_factor(spl)) %>%
  group_by(label) %>% 
  mutate(ssl_c = cumsum(ssl2),
         p_c = cumsum(psum))


# 2) Double-Mass Curves ---------------------------------------------------
models_dmc <- ssl_dmc %>% 
  group_by(label, spl) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(ssl_c ~ p_c,
                       data = .x))) %>% 
  # mutate(mod = map(data,
  #                  ~nls(ssl_c ~ a + b*p_c,
  #                       data = .x,
  #                       algorithm = "port",
  #                       start=c(a=1, b=1),
  #                       upper=c(a=Inf, b=Inf),
  #                       lower = c(a = 1, b = -Inf)))) %>%  
  mutate(intercept = map_dbl(mod,
                             ~coef(.x)[1]),
         coef = map_dbl(mod,
                        ~coef(.x)[2])) %>% 
  filter(spl == "before") %>% 
  ungroup() %>% 
  select(label, intercept, coef) 

ssl_dmc %>% 
  group_by(label, spl) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(ssl_c ~ p_c,
                       data = .x))) %>% 
  mutate(intercept = map_dbl(mod,
                             ~coef(.x)[1]),
         coef = map_dbl(mod,
                        ~coef(.x)[2])) %>% 
  dplyr::select(label, spl, coef) %>% 
  ungroup() %>% 
  spread(spl, coef) %>% 
  mutate(dec = before/after) %>% 
  summary()


dmc_graph <- ssl_dmc %>% 
  left_join(models_dmc,
            by = "label") %>% 
  mutate(calc = intercept + coef * p_c) %>% 
  ggplot(aes(x = p_c,
             y = ssl_c,
             color = spl,
             shape = spl)) +
  geom_point() +
  geom_line(aes(x = p_c,
                y = calc),
            size = .6,
            linetype = "dashed") +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black",
              alpha = .5,
              size = .6,
              se = F,
              show.legend = F) +
  ggpmisc::stat_poly_eq(aes(label = paste(stat(eq.label))),
                        formula = y ~ x, 
                        eq.with.lhs = "italic(sum(SSD))~`=`~",
                        eq.x.rhs = "italic(sum(P))",
                        parse = TRUE,
                        label.y = c(0.95, 0.80),
                        size = 2.5) +
  see::scale_color_metro() +
  labs(x = "Cumulative summer precipitation, mm",
       y = expression("Cumulative suspended sediment discharge, "*"kg"^-1%.%"s"),
       color = "Transition year:",
       shape = "Transition year:") +
  facet_wrap(~label,
             scales = "free")

table4 <- ssl_dmc %>% 
  left_join(models_dmc,
            by = "label") %>% 
  mutate(calc = intercept + coef * p_c) %>% 
  group_by(label) %>%
  summarise(Sc = max(calc),
            So = max(ssl_c),
            dif = Sc-So,
            prc = scales::percent(dif/Sc)) 

table5 <- ssl_dmc %>% 
  left_join(models_dmc,
            by = "label") %>% 
  group_by(label) %>% 
  mutate(calc = intercept + coef * p_c) %>%  
  mutate(calc_lag = lag(calc, default = 0),
         pred = calc - calc_lag) %>% 
  group_by(label, spl) %>% 
  summarise(Sao = mean(ssl2),
            Sco = mean(pred)) %>% 
  mutate(reduct = first(Sao) - Sao,
         reduct_p = scales::percent(reduct/first(Sao)),
         prcp = first(Sao) - Sco,
         prcp_p = scales::percent(prcp/reduct),
         human = Sco - Sao,
         human_p = scales::percent(human/reduct)) %>% 
  ungroup() %>% 
  mutate_at(vars(reduct, prcp, human),
            ~case_when(spl == "before" ~ NA_real_,
                       TRUE ~ .)) %>% 
  mutate_at(vars(reduct_p, prcp_p, human_p),
            ~case_when(spl == "before" ~ NA_character_,
                       TRUE ~ .)) %>% 
  left_join(day_spl,
            by = "label") %>% 
  mutate(spl = paste0(spl, " ", break_year)) %>% 
  dplyr::select(-break_year)

table4 %>% 
  mutate_at(vars(prc),
            ~parse_number(.)) %>% 
  summary()

table5 %>% 
  mutate_at(vars(contains("_p")),
            ~parse_number(.)) %>% 
  summary()

# SAVE --------------------------------------------------------------------
ggsave("figures/fig5_dmc-plots.png",
       dmc_graph,
       dpi = 600,
       w = 10, h = 8)

table4 %>% 
  left_join(day_spl,
            by = "label") %>% 
  dplyr::select(label, break_year, Sc:prc) %>% 
  mutate_at(vars(Sc, So, dif),
            ~atslib::smart_round(.)) %>% 
  rename(Label = label,
         `Transition year` = break_year,
         SSDc = Sc,
         SSDo = So,
         `SSDc-SSDo` = dif,
         `100·(SSDc-SSDo)/SSDc` = prc) %>% 
  writexl::write_xlsx("analysis/table4.xlsx")

table5 %>% 
  mutate_if(is.numeric,
            ~atslib::smart_round(.)) %>% 
  rename(
    Label = label,
    Period = spl,
    SSDo = Sao,
    SSDc = Sco,
    dSSD = reduct,
    dSSD_p = reduct_p
  ) %>% 
  writexl::write_xlsx("analysis/table5.xlsx")
