library(tidyverse)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(parameters)
library(effectsize)
library(performance)
library(insight)
library(simputation)
library(glue)
library(forecast)
library(report)
library(modelbased)
library(extrafont)
library(see)

theme_set(
  theme_lucid(base_family = "Roboto Condensed") +
    theme(legend.position = "bottom",
          strip.background = element_blank()
    )
)

# 1) Read NOAA data -------------------------------------------------------
noaa <- read_csv("data/meteo/noaa_all.csv") %>% 
  janitor::clean_names() %>% 
  filter(name != "BOTLIH, RS")

# load TerraClim info for imputation
load("data/tidy/noaa_terraclim.Rdata")

# 2) NOAA station locations
# noaa %>% 
#   group_by(name) %>% 
#   summarise(lat = first(latitude),
#             lon = first(longitude)) %>% 
#   st_as_sf(coords = c("lon", "lat"),
#            crs = 4326) %>%
#   st_write("data/meteo/noaa_sf.shp")

# 3) Compute statistics
noaa_tidy <- noaa %>% 
  transmute(name,
            year = year(date),
            date,
            prcp,
            tavg,
            tmax,
            tmin) %>%
  filter(year %in% c(1958:2018)) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(data_rain = map(data, ~filter(.x, tavg > 2)))

sdii_func <- function(df) {
  df %>% 
    drop_na(prcp) %>% 
    filter(prcp > 1) %>% 
    group_by(year) %>% 
    summarise(n_rain = n(),
              P = sum(prcp, na.rm = T),
              SDII = P/n_rain) %>% 
    mutate_all(~as.numeric(.))
}

temp_func <- function(df){
  df %>% 
    drop_na(tavg) %>% 
    group_by(year) %>% 
    summarise(#n_temp = n(),
              Tav = mean(tavg, na.rm = T),
              Tsum = sum(tavg > 5)) %>% 
    mutate_all(~as.numeric(.))
}

noaa_temp <- noaa_tidy %>% 
  mutate(temp = map(data,
                    ~temp_func(.x))) %>% 
  unnest(cols = c(temp)) %>% 
  complete(year = seq(1958, 2018, by = 1)) %>%
  select(-data:-data_rain) %>% 
  ungroup()

noaa_rain <- noaa_tidy %>% 
  mutate(rain = map(data,
                    ~sdii_func(.x))) %>% 
  unnest(cols = c(rain)) %>% 
  complete(year = seq(1958, 2018, by = 1)) %>%
  select(-data:-data_rain) %>% 
  ungroup() %>% 
  mutate(P = ifelse(n_rain < 10,
                    NA_real_,
                    P)) %>% 
  mutate(SDII = ifelse(n_rain < 10,
                       NA_real_,
                       SDII)) %>% 
  mutate(n_rain = ifelse(n_rain < 10,
                         NA_real_,
                         n_rain))

noaa_cau <- noaa_temp %>% 
  full_join(noaa_rain,
            by = c("name", "year")) %>% 
  group_by(name) %>% 
  arrange(year, .by_group = T) %>% 
  ungroup()

# Meteo imputation ---------------------------------------------------------
noaa_imp <- noaa_cau %>% 
  left_join(noaa_terraclim,
            by = c("name", "year")) %>% 
  group_by(name) %>% 
  impute_rf(Tav + Tsum + n_rain + P + SDII ~ psum + srad + aet) %>% 
  ungroup() %>% 
  dplyr::select(-psum:-aet)

noaa_tidy <- noaa_imp %>% 
  gather(var, value, -name, -year) %>% 
  mutate(name = str_remove(name, ", GG|, RS"),
         name = str_to_sentence(name)) %>% 
  transmute(var = glue("{var} ({name})"),
            year,
            value) %>% 
  pivot_wider(names_from = var,
              id_cols = year)

# Model ssl ---------------------------------------------------------------
major_rivers <- c(
  "Ard-Ta",
  "Bel-Ko",
  "Che-Ba",
  "Cheg-Nc",
  "Kam-Ol",
  "Mal-Ka",
  "Sun-Br",
  "Uru-Kh",
  "Fia-Ta"
)

ssl_box <- readxl::read_xlsx("data/raw/terek_SY.xlsx",
                             sheet = "data_kgs",
                             # skip = 1,
                             range = "A2:AV96") %>% 
  rename(year = 1) %>% 
  gather(label, ssl, -year) %>% 
  filter(label %in% major_rivers) %>% 
  filter(year >=1958) %>% 
  group_by(label) %>% 
  nest() %>%
  mutate(lamda = map_dbl(data, ~forecast::BoxCox.lambda(.x$ssl,
                                                        method = "loglik",
                                                        lower = -3,
                                                        upper = 3))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(ssl2 = ssl^lamda) #%>% 
  # mutate(ssl2 = ifelse(label == "Fia-Ta",
                       # log10(ssl),
                       # ssl2))

ssl_mod <- ssl_box %>% 
  left_join(noaa_tidy,
            by = c("year")) %>% 
  group_by(label) %>% 
  nest() %>% 
  mutate(df = map(data,
                  ~select(.x, year, ssl2:`SDII (Vladikavkaz)`)))
  

# 1) Ard-Ta ---------------------------------------------------------------
ardta_df <- ssl_mod %>% 
  filter(label == "Ard-Ta") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                contains("Kazbek")
  ) %>% 
  dplyr::select(-contains("SDII"),
                -`Tav (Vladikavkaz)`)

ardta_lm <- lm(ssl2 ~ .,
               data = ardta_df) %>% 
  select_parameters()

r2(ardta_lm)  
check_collinearity(ardta_lm)
check_heteroscedasticity(ardta_lm)
check_autocorrelation(ardta_lm)

predicted <- modelbased::estimate_response(ardta_lm)

ardta_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 2) Bel-Ko ---------------------------------------------------------------
belko_df <- ssl_mod %>% 
  filter(label == "Bel-Ko") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                contains("Kazbek"),
                # contains("Grozyj"),
                # contains("Gudermes"),
                contains("Shadzhatmaz"),
                contains("pereval")
  ) %>% 
  select(
    -contains("SDII (Vladikavkaz)"),
  )

belko_lm <- lm(ssl2 ~ .,
               data = belko_df) %>% 
  select_parameters()

r2(belko_lm)  
check_collinearity(belko_lm)
check_heteroscedasticity(belko_lm)

parameters::p_value(belko_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(belko_lm)

belko_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 3) Che-Ba ---------------------------------------------------------------
cheba_df <- ssl_mod %>% 
  filter(label == "Che-Ba") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                # contains("Kazbek"),
                # contains("Grozyj"),
                contains("Gudermes"),
                contains("Shadzhatmaz"),
                # contains("pereval")
  ) %>% 
  select(-contains("SDII (Gudermes)"))

cheba_lm <- lm(ssl2 ~ .,
               data = cheba_df) %>% 
  select_parameters()

r2(cheba_lm)  
check_collinearity(cheba_lm)
check_heteroscedasticity(cheba_lm)

parameters::p_value(cheba_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(cheba_lm)

cheba_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 4) Cheg-Nc ---------------------------------------------------------------
chegnc_df <- ssl_mod %>% 
  filter(label == "Cheg-Nc") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                # contains("Nalcik"),
                contains("Kazbek"),
                # contains("Grozyj"),
                contains("Gudermes"),
                contains("Shadzhatmaz"),
                contains("pereval")
  ) %>%
  select(
    # -contains("Tav (Vladikavkaz)"),
    -contains("P (Shadzhatmaz)"),
    # -contains("Tsum (Kluhorskij pereval)"),
    )

chegnc_lm <- lm(ssl2 ~ .,
               data = chegnc_df) %>% 
  select_parameters()

r2(chegnc_lm)  
check_collinearity(chegnc_lm)
check_heteroscedasticity(chegnc_lm)

parameters::p_value(chegnc_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(chegnc_lm)

chegnc_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 5) Kam-Ol ---------------------------------------------------------------
kamol_df <- ssl_mod %>% 
  filter(label == "Kam-Ol") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                # contains("Nalcik"),
                contains("Kazbek"),
                # contains("Grozyj"),
                contains("Gudermes"),
                contains("Shadzhatmaz"),
                # contains("pereval")
  ) %>% 
  select(
    -contains("n_rain (Kluhorskij pereval)"),
    -contains("SDII (Kluhorskij pereval)"),
    -contains("Tav (Kluhorskij pereval)"),
    
  )

kamol_lm <- lm(ssl2 ~ .,
               data = kamol_df) %>% 
  select_parameters()

r2(kamol_lm)  
check_collinearity(kamol_lm)
check_heteroscedasticity(kamol_lm)

parameters::p_value(kamol_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(kamol_lm)

kamol_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 6) Mal-Ka ---------------------------------------------------------------
malka_df <- ssl_mod %>% 
  filter(label == "Mal-Ka") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                contains("Kazbek"),
                # contains("Grozyj"),
                contains("Gudermes"),
                contains("Shadzhatmaz"),
                contains("pereval")
  ) %>% 
  select(
    -contains("Tsum (Shadzhatmaz)"),
    -contains("SDII (Kluhorskij pereval)"),
    -contains("P (Gudermes)")
    # -contains("n_rain (Nalcik)")
  )

malka_lm <- lm(ssl2 ~ .,
               data = malka_df) %>% 
  select_parameters()

r2(malka_lm)  
check_collinearity(malka_lm)
check_heteroscedasticity(malka_lm)

parameters::p_value(malka_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(malka_lm)

malka_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 7) Sun-Br ---------------------------------------------------------------
sunbr_df <- ssl_mod %>% 
  filter(label == "Sun-Br") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                # contains("Nalcik"),
                contains("Kazbek"),
                contains("Grozyj"),
                contains("Gudermes"),
                # contains("Shadzhatmaz"),
                # contains("pereval")
  ) %>% 
  select(
    -contains("n_rain (Gudermes)")
  )

sunbr_lm <- lm(ssl2 ~ .,
               data = sunbr_df) %>% 
  select_parameters()

r2(sunbr_lm)  
check_collinearity(sunbr_lm)
check_autocorrelation(sunbr_lm)
check_heteroscedasticity(sunbr_lm)

parameters::p_value(sunbr_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(sunbr_lm)

sunbr_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 8) Uru-Kh ---------------------------------------------------------------
urukh_df <- ssl_mod %>% 
  filter(label == "Uru-Kh") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                contains("Kazbek"),
                contains("Oni"),
                # contains("Ambrolauri"),
                contains("Shadzhatmaz"),
                # contains("pereval")
  ) %>% 
  select(
    -contains("n_rain (Vladikavkaz)"),
    -contains("n_rain (Oni)"),
    -contains("Tav (Ambrolauri)"),
  )

urukh_lm <- lm(ssl2 ~ .,
               data = urukh_df) %>% 
  select_parameters()

r2(urukh_lm)  
check_collinearity(urukh_lm)
check_autocorrelation(urukh_lm)
check_heteroscedasticity(urukh_lm)

parameters::p_value(urukh_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(urukh_lm)

urukh_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# 9) Fia-Ta ---------------------------------------------------------------
fiata_df <- ssl_mod %>% 
  filter(label == "Fia-Ta") %>% 
  unnest(cols = c(df)) %>% 
  ungroup() %>%
  dplyr::select(year, ssl2,
                contains("Vladikavkaz"),
                contains("Nalcik"),
                contains("Kazbek"),
                contains("Oni"),
                # contains("Ambrolauri"),
                contains("Shadzhatmaz"),
                # contains("pereval")
  ) %>% 
  select(
    -contains("n_rain (Vladikavkaz)"),
    -contains("SDII (Nalcik)"),
    -contains("SDII (Shadzhatmaz)"),
    -contains("n_rain (Oni)"),
    -contains("Tav (Ambrolauri)"),
  )

fiata_lm <- lm(ssl2 ~ .,
               data = fiata_df) %>% 
  select_parameters()

r2(fiata_lm)  
check_collinearity(fiata_lm)
check_heteroscedasticity(fiata_lm)

parameters::p_value(fiata_lm) %>% arrange(p)

predicted <- modelbased::estimate_response(fiata_lm)

fiata_df %>% 
  drop_na(ssl2) %>% 
  mutate(Predicted = predicted$Predicted) %>% 
  ggplot(aes(x = ssl2, y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# Impute! ----------------------------------------------------------------
library(imputeTS)

major_df <- ssl_mod %>% 
  mutate(df_new = map2(df, label,
                       ~case_when(
                         .y == "Ard-Ta" ~ predict(ardta_lm, .x),
                         .y == "Fia-Ta" ~ predict(fiata_lm, .x),
                         .y == "Bel-Ko" ~ predict(belko_lm, .x),
                         .y == "Che-Ba" ~ predict(cheba_lm, .x),
                         .y == "Cheg-Nc" ~ predict(chegnc_lm, .x),
                         .y == "Kam-Ol" ~ predict(kamol_lm, .x),
                         .y == "Mal-Ka" ~ predict(malka_lm, .x),
                         .y == "Sun-Br" ~ predict(sunbr_lm, .x),
                         .y == "Uru-Kh" ~ predict(urukh_lm, .x)
                       ))) %>% 
  unnest(cols = c(df_new, data)) %>% 
  mutate(df_new = ifelse(df_new < 0,
                         NA_real_,
                         df_new)) %>%
  mutate(.pred = df_new^(1/lamda)) %>% 
  # Fia-Ta
  # mutate(.pred = ifelse(label == "Fia-Ta",
  #                       10^df_new,
  #                       .pred)) %>% 
  mutate(.pred = case_when(
    (label == "Kam-Ol" & .pred > 100) ~ NA_real_,
    (label == "Uru-Kh" & .pred > 100) ~ NA_real_,
    TRUE ~ .pred
  )) %>% 
  mutate(.pred = na_interpolation(.pred)) %>%
  transmute(label, year, ssl,
            # .pred = df_new^(1/lamda),
            ssl2 = ifelse(is.na(ssl),
                          .pred,
                          ssl),
            flag = ifelse(is.na(ssl),
                          "Imputed",
                          "Observed"))

major_df %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = ssl2)) +
  geom_point(aes(y = ssl2,
                 fill = flag),
             shape = 21) +
  facet_wrap(~label, 
             scales = "free_y")

# SAVE models -------------------------------------------------------------
save("ardta_lm", "cheba_lm", "kamol_lm", "malka_lm",
     "urukh_lm", "sunbr_lm", "belko_lm", "chegnc_lm",
     "fiata_lm",
     "major_df",
     file = "data/tidy/models.Rdata")

