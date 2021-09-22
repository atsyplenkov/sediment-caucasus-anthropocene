library(tidyverse)
library(sf)
library(readxl)
library(data.table)
library(glue)
library(stringi)

all_rivers <- c("Bak-Pr", "Mal-Pa", "Mal-Pr", "Sun-Br", 
                "Sun-Gr", "Ter-Ch", "Ter-El", "Ter-Ka",
                "Ter-Ko", "Ter-Mo", "Ter-St", "Ard-Ta",
                "Bak-Za", "Bel-Ko", "Che-Ba", "Che-So",
                "Cheg-Nc", "Fia-Gu", "Fia-Ta", "Giz-Gi",
                "Kam-Ol", "Mal-Ka", "Mal-Kh", "Ter-Vl",
                "Uru-Kh", "Ard-Nz", "Cheg-Vc", "Fia-Vf",
                "Gen-Tm", "Giz-Da", "Giz-Vk", "Ter-Kaz", "Ter-Nl")


# 1) Load data ---------------------------------------------------
dams <- st_read("data/spatial/terek_dam.shp")

ter_bas <- st_read("data/spatial/ter_basins-stats.shp") %>% 
  st_drop_geometry() %>% 
  dplyr::select(-CROP_count)

ter_gages <- st_read("data/spatial/ter_gages.shp") %>% 
  st_drop_geometry() %>% 
  dplyr::select(label, H=H_mean)

ter_gvk <- read_xlsx("data/raw/terek_SY.xlsx",
                     sheet = 1,
                     range = "A1:C39") %>% 
  rename(fullname = 1,
         area = 3) %>% 
  mutate(fullname = stri_trans_general(fullname,
                                   "russian-latin/bgn"))

terek <- read_xlsx("data/raw/terek_SY.xlsx",
          sheet = "data_kgs",
          # skip = 1,
          range = "A2:AV96") %>% 
  rename(year = 1) %>% 
  gather(label, ssl, -year)

terek_max <- read_xlsx("data/raw/terek_SY.xlsx",
                       sheet = "max_daily") %>% 
  gather(label, max_ssl, -year)

# 2) Handle HPP impact ----------------------------------------------------
dams_imp <- dams %>% 
  st_drop_geometry() %>% 
  filter(impact == "yes") %>%
  arrange(year) %>%
  select(name_en, year, impact_on) %>% 
  mutate(impact_on = str_split(impact_on, ", ")) %>% 
  as_tibble() %>% 
  unnest(cols = c(impact_on)) %>% 
  group_by(label = impact_on) %>% 
  summarise(first_year = min(year),
            hpp = name_en[year == first_year])

# 3) Years of measurements ------------------------------------------------
years_range <- function(df){
  df %>% 
    mutate(temp = year) %>% 
    complete(temp = seq(min(year), max(year), by = 1)) %>% 
    mutate(gr = rleid(is.na(year))) %>% 
    filter(!is.na(year)) %>% 
    group_by(gr) %>% 
    summarise(n = n(),
              range = ifelse(n == 1,
                             glue::glue("{year}"),
                             glue::glue("{first(year)}-{last(year)}")),
              .groups = "drop") %>% 
    pull(range) %>% 
    paste(collapse = ", ")
}

mean_years <- terek %>% 
  drop_na(ssl) %>% 
  group_by(label) %>% 
  nest() %>% 
  mutate(mean_available = map_chr(data,
                              ~years_range(.x))) %>% 
  dplyr::select(-data) %>% 
  ungroup()

max_years <- terek_max %>% 
  drop_na(max_ssl) %>% 
  group_by(label) %>% 
  nest() %>% 
  mutate(max_available = map_chr(data,
                                  ~years_range(.x))) %>% 
  dplyr::select(-data) %>% 
  ungroup()

mean_years_range <- terek %>% 
  drop_na(ssl) %>% 
  group_by(label) %>% 
  summarise(mean_n = sum(!is.na(ssl)),
            mean_range = glue("{first({year})}-{last({year})}"))

max_years_range <- terek_max %>% 
  drop_na(max_ssl) %>% 
  group_by(label) %>% 
  summarise(max_n = sum(!is.na(max_ssl)),
            max_range = glue("{first({year})}-{last({year})}"))

terek %>% 
  group_by(label) %>% 
  summarise(mean = mean(ssl, na.rm = T),
            sd = sd(ssl, na.rm = T))

ter_sy <- terek %>% 
  drop_na() %>% 
  group_by(label) %>% 
  summarise(mean = mean(ssl, na.rm = T)) %>% 
  ungroup()

# 4) Combine table 1 --------------------------------------------------------
table1 <- ter_gvk %>% 
  left_join(ter_bas %>% 
              transmute(label,
                     H_min = round(HHmin),
                     H_max = round(HHmax)),
            by = "label") %>% 
  left_join(ter_gages,
            by = "label") %>% 
  left_join(ter_sy,
            by = "label") %>% 
  left_join(mean_years_range,
            by = "label") %>% 
  left_join(max_years_range,
            by = "label") %>% 
  left_join(
    dams_imp %>% 
      transmute(label,
                dam_effect = glue("reg. by {hpp} HPP since {first_year}")),
    by = "label"
  ) %>% 
  # Altitude group
  mutate(alt_group = case_when(
    H <= 500 ~ "< 500",
    dplyr::between(H, 500, 1000) ~ "500-1000",
    H >= 1000 ~ "> 1000"
  )) %>% 
  mutate(alt_group = as_factor(alt_group),
         alt_group = fct_relevel(alt_group,
                                 c("< 500",
                                   "500-1000",
                                   "> 1000"))) %>% 
  group_by(alt_group) %>% 
  arrange(label, .by_group = T) %>% 
  ungroup() %>% 
  filter(label %in% all_rivers)

# 5) Create Supplementary Table 1 -------------------------------------------------------
sup_table1 <- dams %>% 
  st_drop_geometry() %>% 
  transmute(name_en,
            river,
            year,
            power,
            lat = st_coordinates(dams)[,2],
            lon = st_coordinates(dams)[,1]) %>% 
  arrange(river)

# SAVE --------------------------------------------------------------------
library(writexl)

table1 %>% 
  mutate_at(vars(area, H),
            ~round(.)) %>% 
  transmute(`Gauging station` = fullname,
            Label = label,
            `Area (A), km2` = area,
            `Altitude, m` = H,
            `Alt. range` = paste0(H_min, "-", H_max),
            `Mean annual suspended sediment discharge, kg/s` = atslib::smart_round(mean),
            `Number of mean annual SSD values, years` = mean_n,
            `Years range` = mean_range,
            `Comments` = dam_effect,
            alt_group) %>% 
  writexl::write_xlsx("analysis/table1.xlsx")

sup_table1 %>% 
  rename(`HPP name` = name_en,
         `River` = river,
         `Opening date` = year,
         `Installed capacity, MW` = power,
         `Latitude, °` = lat,
         `Longitude, °` = lon) %>% 
  writexl::write_xlsx("analysis/Supplementary Table 1.xlsx")

save("dams_imp", file = "data/tidy/ter_dams.Rdata")
