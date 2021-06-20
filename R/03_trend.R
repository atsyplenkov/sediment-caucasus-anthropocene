library(tidyverse)
library(lubridate)
library(readxl)
library(atslib)
library(extrafont)
library(see)
library(rkt)
library(zyp)
library(stringi)
library(truncnorm)
library(sf)
library(skimr)
library(tgme)

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

major_rivers <- c("Ard-Ta",
                  "Bel-Ko",
                  "Che-Ba",
                  "Cheg-Nc",
                  "Kam-Ol",
                  "Mal-Ka",
                  "Fia-Ta",
                  "Sun-Br",
                  "Uru-Kh")

# 1) SY record ------------------------------------------------------------
terek <- read_xlsx("data/raw/terek_SY.xlsx",
                   sheet = "data_kgs",
                   # skip = 1,
                   range = "A2:AQ96") %>% 
  rename(year = 1) %>% 
  gather(label, ssd, -year) %>% 
  mutate(ssl = ssd * 31.5)

ter_gvk <- read_xlsx("data/raw/terek_SY.xlsx",
                     sheet = 1,
                     range = "A1:C39") %>% 
  rename(fullname = 1,
         area = 3) %>% 
  mutate(fullname = stri_trans_general(fullname,
                                       "russian-latin/bgn"))


# 2) Particle size distribution -------------------------------------------
rosgen <- read_xlsx("data/raw/from_Williams_Rosgen_1989.xlsx") %>% 
  filter(!is.na(river))

plain_rivers <- c(
  "Clearwater - Spalding",
  "Iowa - Wapello",
  "Snake - Anatone",
  "Wapsipinico - DeWitt"
)

ups_df <- rosgen %>% 
  filter(!river %in% plain_rivers) %>% 
  rename(below4 = 4,
         below8 = 5) %>% 
  select(4,5) %>% 
  gather(size, val) %>% 
  drop_na(val) %>% 
  group_by(size) %>% 
  summarise(
    n = n(),
    mean = mean(val),
    sd = sd(val),
    se = sd/sqrt(n),
    min = min(val),
    max = max(val),
    q0025 = quantile(val,
                     probs = c(0.025)),
    q0975 = quantile(val,
                     probs = c(0.975))
  ) %>% 
  filter(size == "below4")

# 2) Uncertainty ----------------------------------------------------------
unc_final <- function(x){
  
  ume <- rnorm(1000,
               mean = 1,
               sd = .2)
  
  ups <- truncnorm::rtruncnorm(1000,
                               a = ups_df$q0025/100,
                               b = ups_df$q0975/100,
                               mean = ups_df$mean/100, 
                               sd = ups_df$sd/100)
  
  sim <- x * ume + x * ups
  
  quantile(sim, probs = c(0.025, 0.975)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(ssl_sim = list(data.frame(sim,
                                    id = 1:1000)))
}

set.seed(2021)
ter_unc <- terek %>%
  drop_na(ssl) %>% 
  group_by(year, label) %>% 
  nest() %>% 
  mutate(ssl_sim = map(data, ~unc_final(.x$ssl))) %>%
  unnest(cols = c(data, ssl_sim)) %>% 
  ungroup()

ter_boot <- ter_unc  %>% 
  group_by(label) %>% 
  mutate(average = mean(ssl, na.rm = T)) %>% 
  dplyr::select(-ssd) %>% 
  unnest(cols = c(ssl_sim)) %>% 
  group_by(label, average, id) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(sim ~ year,
                       data = .x))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup() 

ter_boot_ci <- ter_boot %>% 
  group_by(label) %>% 
  summarise(mean = mean(dec_change),
            `2.5%` = quantile(dec_change,
                              probs = 0.025),
            `97.5%` = quantile(dec_change,
                               probs = 0.975))

tgme("ter_boot")
# 3) Trend slopes ---------------------------------------------------------
ter_ols <-
  ter_unc %>% 
  dplyr::select(-ssd, -ssl_sim) %>% 
  group_by(label) %>% 
  mutate(average = mean(ssl, na.rm = T)) %>% 
  gather(var, val, -year, -label, -average) %>% 
  group_by(label, var, average) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(val ~ year,
                       data = .x,
                       na.action = "na.exclude"))) %>% 
  # mutate(average = map_dbl(data, ~(mean(.x$val, na.rm = T)))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(p_value = map_dbl(mod,
                           ~(summary(.x)$coefficients[,4][2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  mutate(n = map_dbl(data, ~sum(!is.na(.x$val)))) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup()
# 
# slope_envelope <- function(x_low, x_up, yr){
#   
#   low <- (first(x_low)-last(x_up))/(last(yr)-first(yr))
#   up <- (first(x_up)-last(x_low))/(last(yr)-first(yr))
#   
#   
#   data.frame(slope_l = low,
#              slope_u = up)
# }
# 
# ter_unc %>% 
#   dplyr::select(-ssd) %>% 
#   group_by(label) %>% 
#   nest() %>% 
#   mutate(mod = map(data,
#                    ~lm(ssl ~ year,
#                        data = .x,
#                        na.action = "na.exclude"))) %>% 
#   mutate(average = map_dbl(data, ~(mean(.x$ssl, na.rm = T)))) %>%
#   mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
#   mutate(p_value = map_dbl(mod,
#                            ~(summary(.x)$coefficients[,4][2]))) %>% 
#   mutate(ci_slope = map(data,
#                            ~slope_envelope(.x$`2.5%`,
#                                            .x$`97.5%`,
#                                            .x$year))) %>%
#   unnest(ci_slope)
#   mutate(dec_change = slope / average * 100) %>% 
#   mutate(n = map_dbl(data, ~sum(!is.na(.x$ssl))))

slope_ci <- function(df){
  
  what <- unique(df$var)[str_detect(unique(df$var),
                                    "2.5|9",
                                    negate = T)]
  
  low_up <- full_join(
    df %>% 
      dplyr::filter(var == "2.5%") %>% 
      dplyr::select(label,
                    low = dec_change),
    df %>% 
      filter(var == "97.5%") %>% 
      dplyr::select(label,
                    up = dec_change),
    by = "label"
  ) %>% 
    mutate_at(vars(low, up),
              ~round(., 2)) %>% 
    mutate(ci = paste0("[",
                       low, "; ",
                       up,
                       "]"))
  
  df %>% 
    filter(!var %in% c("97.5%", "2.5%")) %>% 
    left_join(low_up,
              by = "label") %>% 
    transmute(label,
              change_ci = paste0(round(dec_change, 2),
                              " ",
                              ci),
              p_value) %>% 
    magrittr::set_colnames(c("label",
                             what,
                             paste0(what, "_p")))
  
}

# 4) Plots ----------------------------------------------------------------
(ter_trends <- ter_unc %>% 
   dplyr::select(-ssd) %>% 
   filter(label %in% major_rivers |
            label == "Ter-Ko" | 
            label == "Ter-Vl" |
            label == "Mal-Pr") %>% 
   ggplot(aes(x = year,
              y = ssl)) +
   geom_ribbon(aes(ymin = `2.5%`,
                   ymax = `97.5%`),
               alpha = .4) +
   geom_line() +
   geom_smooth(aes(color = "All measuring period"),
               method = "lm",
               se = F) +
   geom_smooth(data = . %>%
                 filter(year >=1987) %>% 
                 filter(label != "Sun-Br"),
               aes(color = "After 1987"),
               method = "lm",
               se = F) + 
   scale_x_continuous(breaks = seq(1925,
                                   2020,
                                   by = 15)) +
   scale_y_log10(minor_breaks = NULL) +
   scale_color_manual(values = c("#ff3030", "#0099cc")) +
   annotation_logticks(sides = "l",
                       color = "grey60") +
   facet_wrap(~label,
              scales = "free_y") +
   labs(x = NULL,
        y = expression("Suspended sediment load, 10"^3%.%"t"%.%"yr"^-1),
        color = NULL))

# 5) Glacier changes ------------------------------------------------------
glaciers <- read_xlsx("data/raw/terek_SY.xlsx",
                      sheet = "summary",
                      range = "B1:J39") %>% 
  dplyr::select(label, glacier1960:glacier2014)

ws <- st_read("data/spatial/ter_basins.shp") %>% 
  mutate(area = st_area(.),
         area = as.numeric(area)/10^6)

gldf <- ws %>% 
  st_drop_geometry() %>% 
  dplyr::select(label, area) %>% 
  right_join(glaciers,
             by = "label") %>% 
  mutate_at(vars(glacier1960:glacier2014), ~(100*./area)) %>% 
  gather(year, glacier, -label, -area) %>% 
  mutate(year = parse_number(year))

ugl <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .08)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(gl_sim = list(data.frame(sim,
                         id = 1:1000)))
  
}

set.seed(2021)
gl_unc <- gldf %>% 
  mutate(glacier = ifelse(glacier == 0,
                          NA_real_,
                          glacier)) %>% 
  group_by(label, year) %>% 
  nest() %>% 
  mutate(gl_sim = map(data, ~ugl(.x$glacier))) %>%
  unnest(cols = c(data, gl_sim)) %>% 
  ungroup()

gl_boot <- gl_unc  %>% 
  group_by(label) %>% 
  mutate(average = mean(glacier, na.rm = T)) %>% 
  filter(!label %in% c("Bel-Ko",
                       "Nal-Be",
                       "Kam-Ol")) %>% 
  dplyr::select(-area) %>% 
  unnest(cols = c(gl_sim)) %>% 
  group_by(label, average, id) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(sim ~ year,
                       data = .x))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup() 

gl_boot_ci <- gl_boot %>% 
  group_by(label) %>% 
  summarise(mean = mean(dec_change),
            `2.5%` = quantile(dec_change,
                       probs = 0.025),
            `97.5%` = quantile(dec_change,
                              probs = 0.975))

gl_ols <-
  gl_unc %>% 
  filter(!label %in% c("Bel-Ko",
                       "Nal-Be",
                       "Kam-Ol")) %>% 
  dplyr::select(-area, -gl_sim) %>% 
  group_by(label) %>% 
  mutate(average = mean(glacier, na.rm = T)) %>% 
  gather(var, val, -year, -label, -average) %>% 
  group_by(label, var, average) %>% 
  nest() %>%
  mutate(mod = map(data,
                   ~lm(val ~ year,
                       data = .x,
                       na.action = "na.exclude"))) %>% 
  # mutate(average = map_dbl(data, ~(mean(.x$val, na.rm = T)))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(p_value = map_dbl(mod,
                           ~(summary(.x)$coefficients[,4][2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup()

tgme("gl")

# 6) Cropland changes -----------------------------------------------------
load("data/tidy/ter_cropland.Rdata")

crop_df <- ter_c_change %>% 
  mutate_at(vars(c1987:c2015), ~(100*./area)) %>% 
  gather(year, crop, -label, -area) %>% 
  mutate(year = parse_number(year))

ucr <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .24)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(cr_sim = list(data.frame(sim,
                                    id = 1:1000)))
  
}

set.seed(2021)
crop_unc <- crop_df %>% 
  group_by(label, year) %>% 
  nest() %>% 
  mutate(crop_sim = map(data, ~ucr(.x$crop))) %>%
  unnest(cols = c(data, crop_sim)) %>% 
  ungroup()

crop_boot <- crop_unc  %>% 
  group_by(label) %>% 
  mutate(average = mean(crop, na.rm = T)) %>% 
  dplyr::select(-area) %>% 
  unnest(cols = c(cr_sim)) %>% 
  group_by(label, average, id) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(sim ~ year,
                       data = .x))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup() 

crop_boot_ci <- crop_boot %>% 
  group_by(label) %>% 
  summarise(mean = mean(dec_change),
            `2.5%` = quantile(dec_change,
                              probs = 0.025),
            `97.5%` = quantile(dec_change,
                               probs = 0.975))

crop_ols <-
  crop_unc %>% 
  dplyr::select(-area, -cr_sim) %>% 
  group_by(label) %>% 
  mutate(average = mean(crop, na.rm = T)) %>% 
  gather(var, val, -year, -label, -average) %>% 
  group_by(label, var, average) %>% 
  nest() %>%
  mutate(mod = map(data,
                   ~lm(val ~ year,
                       data = .x,
                       na.action = "na.exclude"))) %>% 
  # mutate(average = map_dbl(data, ~(mean(.x$val, na.rm = T)))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(p_value = map_dbl(mod,
                           ~(summary(.x)$coefficients[,4][2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup()

tgme("crop")
# 7) Forest change --------------------------------------------------------
load("data/tidy/ter_forest.Rdata")

for_df <- ter_f_change %>% 
  mutate_at(vars(f1987:f2015), ~(100*./area)) %>% 
  gather(year, forest, -label, -area) %>% 
  mutate(year = parse_number(year))

ufor <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .1)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(forest_sim = list(data.frame(sim,
                                    id = 1:1000)))
  
}

set.seed(2021)
forest_unc <- for_df %>% 
  group_by(label, year) %>% 
  nest() %>% 
  mutate(for_sim = map(data, ~ufor(.x$forest))) %>%
  unnest(cols = c(data, for_sim)) %>% 
  ungroup()

forest_boot <- forest_unc  %>% 
  group_by(label) %>% 
  mutate(average = mean(forest, na.rm = T)) %>% 
  dplyr::select(-area) %>% 
  unnest(cols = c(forest_sim)) %>% 
  group_by(label, average, id) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(sim ~ year,
                       data = .x))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup() 

forest_boot_ci <- forest_boot %>% 
  group_by(label) %>% 
  summarise(mean = mean(dec_change),
            `2.5%` = quantile(dec_change,
                              probs = 0.025),
            `97.5%` = quantile(dec_change,
                               probs = 0.975))

tgme("forest_boot")

forest_ols <-
  forest_unc %>% 
  dplyr::select(-area, -forest_sim) %>% 
  group_by(label) %>% 
  mutate(average = mean(forest, na.rm = T)) %>% 
  gather(var, val, -year, -label, -average) %>% 
  group_by(label, var, average) %>% 
  nest() %>%
  mutate(mod = map(data,
                   ~lm(val ~ year,
                       data = .x,
                       na.action = "na.exclude"))) %>% 
  # mutate(average = map_dbl(data, ~(mean(.x$val, na.rm = T)))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(p_value = map_dbl(mod,
                           ~(summary(.x)$coefficients[,4][2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup()

# 8) After 1984 -----------------------------------------------------------
ter_ols_1987 <-
  ter_unc %>% 
  dplyr::select(-ssd, -ssl_sim) %>% 
  rename(ssl87 = ssl) %>% 
  # filter(year >= 1987) %>%
  filter(between(year, 1987, 2015))  %>% 
  group_by(label) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  dplyr::select(-n) %>% 
  mutate(average = mean(ssl87, na.rm = T)) %>% 
  gather(var, val, -year, -label, -average) %>% 
  group_by(label, var, average) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(val ~ year,
                       data = .x,
                       na.action = "na.exclude"))) %>% 
  # mutate(average = map_dbl(data, ~(mean(.x$val, na.rm = T)))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(p_value = map_dbl(mod,
                           ~(summary(.x)$coefficients[,4][2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup()

ter_boot_1987 <-
  ter_unc  %>% 
  filter(between(year, 1987, 2015))  %>% 
  group_by(label) %>% 
  mutate(average = mean(ssl, na.rm = T)) %>% 
  dplyr::select(-ssd) %>% 
  unnest(cols = c(ssl_sim)) %>% 
  group_by(label, average, id) %>% 
  nest() %>% 
  mutate(mod = map(data,
                   ~lm(sim ~ year,
                       data = .x))) %>% 
  mutate(slope = map_dbl(mod, ~(.x$coefficients[2]))) %>% 
  mutate(dec_change = slope / average * 100) %>% 
  dplyr::select(-data, -mod) %>% 
  ungroup() 

ter_boot_ci_1987 <- ter_boot_1987 %>% 
  group_by(label) %>% 
  summarise(mean = mean(dec_change, na.rm = T),
            `2.5%` = quantile(dec_change,
                              probs = 0.025,
                              na.rm = T),
            `97.5%` = quantile(dec_change,
                               probs = 0.975,
                               na.rm = T))

tgme("1987")

# 9) Collective table -----------------------------------------------------
# all_ols <- ter_ols %>% 
#   slope_ci() %>% 
#   left_join(ter_ols_1987 %>% 
#               slope_ci(),
#             by = "label") %>% 
#   left_join(crop_ols %>% 
#               slope_ci(),
#             by = "label") %>% 
#   left_join(forest_ols %>% 
#               slope_ci(),
#             by = "label") %>% 
#   left_join(gl_ols %>% 
#               slope_ci(),
#             by = "label")

boot_ci <- function(obj){
  
  what <- deparse(substitute(obj)) %>% 
    str_remove(., "_boot_ci")
  
  obj %>% 
    mutate_if(is.numeric,
              ~smart_round(.)) %>% 
    transmute(label,
              tt = glue("{mean} [{`2.5%`}; {`97.5%`}]")) %>% 
    magrittr::set_colnames(c("label", what))
  
}

all_boot <- boot_ci(ter_boot_ci) %>% 
  left_join(boot_ci(ter_boot_ci_1987),
            by = "label") %>% 
  left_join(boot_ci(crop_boot_ci),
            by = "label") %>% 
  left_join(boot_ci(forest_boot_ci),
            by = "label") %>% 
  left_join(boot_ci(gl_boot_ci) ,
            by = "label")
  
ter_alt <- read_xlsx("analysis/table1.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::select(label, alt_group, altitude_m)

ter_alt %>%
  left_join(all_boot,
            by = "label") %>% 
  dplyr::select(dplyr::everything(), -altitude_m) %>% 
  writexl::write_xlsx("analysis/table2.xlsx")

# 10) Explore -------------------------------------------------------------
ter_boot_ci %>% 
  left_join(ter_alt,
            by = "label") %>%
  drop_na(alt_group) %>% 
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  # group_by(alt_group) %>% 
  filter(mean > 0) %>%
  skim()


ter_boot_ci_1987 %>% 
  filter(label %in% c("Mal-Pr", "Sun-Br", "Sun-Gr", "Ter-Ch", "Ter-El", "Ter-Ka", "Ter-Ko", "Ter-Mo", "Ter-St", "Ard-Ta", "Bak-Za", "Bel-Ko", "Che-Ba", "Che-So", "Cheg-Nc", "Fia-Gu", "Fia-Ta", "Giz-Gi", "Kam-Ol", "Mal-Ka", "Mal-Kh", "Ter-Vl", "Uru-Kh", "Ard-Nz", "Cheg-Vc", "Fia-Vf")) %>% 
  mutate(tt = mean > 0) %>% 
  group_by(tt) %>%
  skim()

crop_boot_ci %>% 
  filter(mean < 0) %>%
  summary()

gl_boot_ci %>% 
  # filter(mean < 0) %>% 
  summary()


# 11) Discuss -------------------------------------------------------------
# ols_to_gr <- function(df){
#   
#   what <- unique(df$var)[str_detect(unique(df$var),
#                                     "2.5|9",
#                                     negate = T)]
#   df %>% 
#     filter(var == what) %>%
#     left_join(ter_alt,
#               by = "label") %>%
#     drop_na(alt_group) %>% 
#     mutate(alt_group = as_factor(alt_group)) %>% 
#     mutate(alt_group = fct_relevel(alt_group,
#                                    "< 500", "500-1000")) %>% 
#     group_by(alt_group) %>% 
#     summarise(mean = mean(dec_change, na.rm = T),
#               sd = sd(dec_change, na.rm = T),
#               se = sd/sqrt(n()),
#               q25 = quantile(dec_change,
#                              na.rm = T,
#                              probs = .25),
#               median = median(dec_change, na.rm = T),
#               q75 = quantile(dec_change,
#                              na.rm = T,
#                              probs = .75),
#               .groups = "drop") %>% 
#     mutate(type = what)
#   
# }
# 
# table6 <- list(ter_ols,
#      ter_ols_1987,
#      crop_ols,
#      forest_ols,
#      gl_ols) %>% 
#   map_dfr(ols_to_gr) %>% 
#   gather(var, val, -type, -alt_group) %>% 
#   spread(type, val) %>% 
#   dplyr::select(alt_group, var,
#                 ssl, ssl87,
#                 everything()) %>% 
#   mutate_if(is.numeric,
#             ~atslib::smart_round(.))

rets_trend <- read_xlsx("data/raw/terek_SY.xlsx",
                        sheet = 1,
                        range = "B1:P39") %>% 
  dplyr::select(label,
                contains("rets"),
                contains("gusarov")) %>% 
  filter(!is.na(Qmax_rets)|!is.na(SSL_gusarov))

table7 <- ter_ols %>% 
  filter(var == "ssl") %>% 
  dplyr::select(label, ssl = dec_change) %>% 
  left_join(ter_ols_1987 %>% 
              filter(var == "ssl87") %>% 
              dplyr::select(label,
                     ssl87 = dec_change),
            by = "label") %>% 
  right_join(rets_trend,
            by = "label") %>% 
  mutate_at(vars(contains("rets")), ~./10)
  
library(corrr)

table7 %>% 
  # filter(!str_detect(label, "Ter|Ty")) %>%
  select(-label) %>%
  correlate(method = "spearman") %>%
  shave() 

noter <- table7 %>% 
  mutate(Qjuly_rets = ifelse(label == "Ter-Ko",
                             -0.71, Qjuly_rets)) %>% 
  mutate(Qaug_rets = ifelse(label == "Bak-Za",
                             -0.61, Qaug_rets)) %>% 
  filter(!str_detect(label, "Ter"))

cor.test(noter$ssl,
         noter$Qjune_rets,
         method = "spearman")

noter %>% 
  drop_na(SSL_gusarov) %>% 
  ggplot(aes(ssl, SSL_gusarov)) +
  geom_point() +
  atslib::Add_R2()

# SAVE --------------------------------------------------------------------
ls()[str_detect(ls(), "boot")] %>%
  save(list = .,
       file = "data/tidy/boot_results.Rdata")

ggsave("figures/fig3_trend-plots.png",
       plot = ter_trends,
       dpi = 600,
       w = 12, h = 6)

writexl::write_xlsx(table6,
                    "analysis/table6_alt-trends.xlsx")

table7 %>% 
  mutate_if(is.numeric,
            ~atslib::smart_round(.)) %>% 
writexl::write_xlsx("analysis/table7_comparison.xlsx")
