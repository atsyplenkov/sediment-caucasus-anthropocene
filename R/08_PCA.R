library(tidyverse)
library(extrafont)
library(factoextra)
library(atslib)

theme_set(theme_hp())

load("data/tidy/terraclim.Rdata")
load("data/tidy/boot_results_for_pca.Rdata")

ter_boot_pca <- ter_boot_ci %>% 
  select(label,
         ssl = mean) %>% 
  left_join(forest_unc %>% 
              filter(year == 1987) %>% 
              select(label,
                     forest),
            by = "label") %>% 
  left_join(crop_unc %>% 
              filter(year == 1987) %>% 
              select(label,
                     crop),
            by = "label") %>% 
  left_join(gl_unc %>% 
              filter(year == 1986) %>% 
              select(label,
                     glacier,
                     area),
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>%
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  mutate(glacier = ifelse(is.na(glacier), 0, glacier)) %>% 
  drop_na() %>% 
  left_join(terraclim %>% 
              group_by(label) %>% 
              summarise_all(~mean(.)) %>% 
              select(-year,
                     -qmean)) %>% 
  rename(Cropland = crop,
         Forest = forest,
         Glacier = glacier,
         SSL = ssl,
         Area = area,
         Altitude = altitude_m,
         Temperature = aet,
         Precipitation = psum,
         `S. Radiation` = srad)


res.pca <- ter_boot_pca %>% 
  select(-alt_group,
         -label) %>% 
  prcomp(scale = TRUE)

(pca_plot <-
    fviz_pca_biplot(res.pca,
                    addEllipses = T,
                    col.ind = ter_boot_pca$alt_group,
                    col.var = "gray20",
                    repel = T,
                    palette = see::palette_metro()(3),
                    label = "var",
                    alpha = .3) +
    labs(title = "",
         # subtitle = "b",
         color = "Altitude group",
         shape = "Altitude group",
         fill = "Altitude group") +
    theme_hp() +
    theme(plot.margin = margin(rep(1, 4))))


ter_boot_pca %>% 
  filter(between(ssl, -10, 10)) %>% 
  ggplot(aes(x = psum,
             y = ssl)) +
  geom_point() +
  Add_R2(formula = "y ~ x")


ggsave("figures/fig9_pca_plot.png",
       pca_plot,
       dpi = 500,
       w = 8, h = 6.5)
