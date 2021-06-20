ter_boot_1987 %>% 
  dplyr::select(label,
                id,
                ssl = dec_change) %>% 
  left_join(ter_gvk,
            by = "label") %>% 
  left_join(ter_alt,
            by = "label") %>% 
  drop_na(alt_group) %>% 
  drop_na(ssl) %>% 
  mutate(alt_group = as_factor(alt_group)) %>% 
  mutate(alt_group = fct_relevel(alt_group,
                                 "< 500", "500-1000")) %>% 
  ggplot(aes(x = ssl,
             y = ..scaled..,
             color = alt_group,
             fill = alt_group)) +
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
            family = "Roboto Condensed") +
  coord_cartesian(xlim = c(-50, 60),
                  ylim = c(0, 1.05),
                  expand = F) +
  scale_color_metro() +
  scale_fill_metro()

ter_boot %>% 
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
            family = "Roboto Condensed") +
  coord_cartesian(xlim = c(-3.1, 1.5), ylim = c(0, 1.05),
                  expand = F) +
  scale_color_metro() +
  scale_fill_metro() +
  theme(plot.margin = unit(c(0.1,
                             0.5, # right
                             -0.1, # bottom
                             0.1), "cm"))


