# params


## ---- libs-funs ----

# Libraries
library(tidyverse)
library(knitr)
# library(DT)
library(leaflet)
library(HHSKwkl)
library(readxl)
library(lubridate)
library(glue)
# library(crosstalk)
library(sf)
# library(patchwork)
library(ggbeeswarm)

# Other options

options(OutDec = ",")

# ggplot2

theme_set(hhskthema())

# DT

dt_labels_nederlands()

my_datatable <- function(df, ...) {
  DT::datatable(data = df, extensions = 'Buttons',
            options = list(dom = 'lfirtpB', buttons = c('csv', 'excel', 'pdf'), pageLength = 10), ...)
}

## ---- load-data ----

ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992, quiet = TRUE)

kreeften_per_soort <-
  read_excel("data/kreeften_per_soort.xlsx",
             col_types = c("text", "numeric", "numeric", "date", "text", "text", "numeric")) %>%
  mutate(deelgebied = case_when(str_detect(mp, "S_") ~ "Schieland", str_detect(mp, "K_") ~"Krimpenerwaard", str_detect(mp, "VKRH") ~"Krimpenerwaard"))

alles_per_soort<- read_excel("data/kreeften_per_soort_alles.xlsx",
                             col_types = c("text", "numeric", "numeric", "date", "text", "text", "numeric", "text")) %>%
  mutate(deelgebied = case_when(str_detect(mp, "S_") ~ "Schieland", str_detect(mp, "K_") ~"Krimpenerwaard", str_detect(mp, "VKRH") ~"Krimpenerwaard"))

alles_per_soort_2<- read_excel("data/kreeften_per_soort_alles_sam.xlsx",
                               col_types = c("text", "numeric", "numeric", "date", "text", "text", "numeric", "numeric")) %>%
  mutate(deelgebied = case_when(str_detect(mp, "S_") ~ "Schieland", str_detect(mp, "K_") ~"Krimpenerwaard", str_detect(mp, "VKRH") ~"Krimpenerwaard"))


toestemming <- readxl::read_excel("data/kreeften_toestemming.xlsx") %>% rename(mp = `meetpuntcode`)

## ---- opzet ----

toestemming_kaart <-
  toestemming %>%
  mutate(toestemming = fct_recode(toestemming, "Geen reactie" = "nee", Ja = "ja", "Te laat" = "te laat"),
         toestemming = fct_relevel(toestemming, c("Ja", "Te laat", "Geen reactie"))
         ) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoomin = 1) +
  geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
  geom_sf(aes(fill = toestemming), shape = 21, size = 3) +
  scale_fill_manual(values = c(hhskblauw, "white", "grey60")) +
  guides(fill = guide_legend("Toestemming")) +
  hhskthema_kaart() +
  theme(axis.line = element_blank()) +
  labs(title = "Toestemming per locatie in 2021",
       caption = "Er is in 2021 voor 125 locaties toestemming verkregen.\nDe overige locaties zijn niet bemonsterd.")

## ---- resultaten ----

totaal_vangst_kaart <-
  kreeften_per_soort %>%
  group_by(mp, x, y) %>%
  summarise(waarde = sum(waarde)) %>%
  ungroup() %>%
  st_as_sf(coords = c("x", "y"), crs = 28992) %>%

  ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoomin = 1) +
  geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
  geom_sf(aes(size = waarde), shape = 21, fill = "darkred", colour = "grey20", data = . %>% filter(waarde > 0)) +
  geom_sf(shape = 21, size = 2, data = . %>% filter(waarde == 0)) +
  # scale_colour_viridis_c(direction = -1) +
  scale_size(range = c(2,6)) +
  guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
  hhskthema_kaart() +
  theme(axis.line = element_blank()) +
  labs(title = "Aantal gevangen kreeften in 2021",
       caption = "Er zijn vooral Rode Amerikaanse rivierkreeften gevangen.\nAndere soorten komen slechts incidenteel voor.")



soort_kaart <-
  kreeften_per_soort %>%
  filter(waarde > 0) %>%
  mutate(naam = fct_reorder(naam, waarde, .desc = TRUE)) %>%
  st_as_sf(coords = c("x", "y"), crs = 28992) %>%

  ggplot() +
  # ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
  geom_sf(size = 2, colour = "darkred", data = . %>% filter(waarde > 0)) +
  guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
  hhskthema_kaart() +
  facet_wrap(~naam) +
  theme(axis.line = element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(hjust = 0)) +
  labs(title = "Verspreiding per soort in 2021")



aantal_aangetroffen_soort <-
  kreeften_per_soort %>%
  group_by(deelgebied) %>%
  mutate(aantal_locs = n_distinct(mp)) %>%
  group_by(deelgebied, naam) %>%
  summarise(aantal = n(),
            aantal_locs = first(aantal_locs),
            frac = aantal / aantal_locs)


aantal_aangetroffen <-
  kreeften_per_soort %>%
  group_by(deelgebied) %>%
  mutate(aantal_locs = n_distinct(mp)) %>%
  filter(!is.na(naam)) %>%
  summarise(aantal = n_distinct(mp),
            aantal_locs = first(aantal_locs),
            frac = aantal / aantal_locs) %>%
  mutate(naam = "Alle soorten rivierkreeften")

plot_aantallen <-
  bind_rows(aantal_aangetroffen_soort, aantal_aangetroffen) %>%
  filter(!is.na(naam)) %>%
  mutate(tekst = glue("{aantal} van de {aantal_locs}")) %>%
  mutate(naam = fct_reorder(naam, frac)) %>%
  ggplot(aes(frac, naam)) +
  geom_col(fill = hhskblauw) +
  geom_text(aes(label = tekst), x = 1.25, hjust = 1, size = 3, colour = "grey50") +
  facet_wrap(~deelgebied, ncol = 1, scales = "free_x", strip.position = "left") +
  labs(y = "",
       x = "",
       title = "Aantal locaties met Amerikaanse rivierkreeften") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent,
                     limits = c(0,1.25), expand = c(0,0), position = "top") +
  hhskthema() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey50"),
        strip.text = element_text(colour = "white", face = "plain"),
        strip.placement = "outside",
        plot.title.position = "plot")

aantal_per_soort <-
  kreeften_per_soort %>%
  filter(!is.na(naam)) %>%
  group_by(naam) %>%
  mutate(naam2 = glue("{naam}\n n = {n()}")) %>%
  ggplot(aes(fct_reorder(naam2, waarde, .desc = TRUE), waarde)) +
  # geom_beeswarm(width = 0.20, size = 2, colour = hhskblauw) +
  geom_quasirandom(width = 0.20, size = 2, colour = hhskblauw) +
  # geom_jitter(width = 0.20, size = 2, colour = hhskblauw) +
  scale_y_continuous(limits = c(0, 90), expand = c(0,0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Aantal gevangen kreeften per locatie",
       y = "Aantal",
       x = "",
       caption = "Locaties zonder kreeften zijn niet meegenomen")



alles_vangst_kaart <-
  alles_per_soort_2 %>%
  #group_by(mp, x, y) %>%
  #summarise(waarde = sum(waarde)) %>%
  #ungroup() %>%
  st_as_sf(coords = c("x", "y"), crs = 28992) %>%

  ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoomin = 1) +
  geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
  geom_sf(aes(size = waarde), shape = 21, colour = "grey20", fill= "darkred", data = . %>% filter(waarde > 0)) +
  #geom_sf(aes(size = waarde, colour = factor(jaar)),  data = . %>% filter(waarde > 0)) +
  geom_sf(shape = 21, colour = "grey20", size = 2, data = . %>% filter(waarde == 0)) +
  #scale_colour_manual(values = c("grey40", "darkred")) +
  scale_size(range = c(2,6)) +
  facet_wrap(~fct_rev(as.character(jaar)), ncol = 1)+
  guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal"), shape = guide_legend("Jaar")) +
  hhskthema_kaart() +
  theme(axis.line = element_blank()) +
  labs(title = "Aantal gevangen kreeften",
       caption = "Er zijn vooral Rode Amerikaanse rivierkreeften gevangen.\nAndere soorten komen slechts incidenteel voor.")



soort_kaart_per_jaar <-
  alles_per_soort %>%
  filter(waarde > 0) %>%
  mutate(naam = fct_reorder(naam, waarde, .desc = TRUE)) %>%
  st_as_sf(coords = c("x", "y"), crs = 28992) %>%

  ggplot() +
  # ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
  geom_sf(size = 2, colour = "darkred", data = . %>% filter(waarde > 0)) +
  guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
  hhskthema_kaart() +
  facet_grid(rows=vars(fct_rev(jaar)), cols=vars(naam), labeller = label_wrap_gen(), switch = "y") +
    theme(axis.line = element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(hjust = 0.5)) +
  labs(title = "Verspreiding per soort per jaar")

## ---- discussie ----



## ---- bijlage ----

pal <-  colorNumeric(c(hhskblauw, "darkred"), c(0, 1))

kaart_bijlage <-
  kreeften_per_soort %>%
  arrange(desc(waarde)) %>%
  mutate(tekst = ifelse(is.na(naam), "Geen kreeften", glue("{naam}: <b>{waarde}x</b>"))) %>%
  group_by(mp, x, y) %>%
  summarise(tekst = glue_collapse(tekst, sep = "</br>"), waarde = sum(waarde) > 0) %>%
  ungroup() %>%
  mutate(tekst = glue("{mp}<hr>{tekst}")) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  sf::st_transform(crs = 4326) %>%
  leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Kaart") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Luchtfoto") %>%
  leaflet::addLayersControl(baseGroups = c("Kaart", "Luchtfoto"),
                            options = leaflet::layersControlOptions(collapsed = FALSE), position = "topleft") %>%
  addPolylines(data = sf::st_transform(ws_grens, crs = 4326),
               opacity = 1, color = "grey", weight = 3, label = "waterschapsgrens") %>%
  addCircleMarkers(label = ~mp, popup = ~tekst,
                   # color = "grey",
                   # opacity = 1,
                   fillColor = ~pal(waarde),
                   fillOpacity = 1,
                   stroke = FALSE, radius = 8) %>%
  addLegend(colors = c(hhskblauw, "darkred"), labels = c("Afwezig", "Aanwezig"), title = "Kreeften")


tabel_bijlage <-
  kreeften_per_soort %>%
  select(-datum, -taxonnaam) %>%
  pivot_wider(names_from = naam, values_from = waarde, values_fill = 0) %>%
  #select(-`NA`) %>%
  select(-x, -y) %>%  # anders past het niet
  relocate(deelgebied) %>%
  # relocate(`Rode Amerikaanse Rivierkreeft` .after = y)
  rename(Meetpunt = mp, Deelgebied = deelgebied) %>% # , X = x, Y = y
  my_datatable(rownames = FALSE)

tabel_bijlage2 <-
  kreeften_per_soort %>%
  select(deelgebied, mp, x, y) %>%
  distinct() %>%
  rename(Meetpunt = mp, Deelgebied = deelgebied, X = x, Y = y) %>%
  DT::datatable(extensions = 'Buttons',
                options = list(dom = 'lfirtpB',
                               buttons = c('csv', 'excel', 'pdf'),
                               pageLength = 5))

## ---- 2020 vergelijking ----
# kreeften_per_soort_2020 <-
#   read_excel("data/kreeften_per_soort_2020.xlsx",
#              col_types = c("text", "numeric", "numeric", "date", "text", "text", "numeric")) %>%
#   mutate(deelgebied = case_when(str_detect(mp, "S_") ~ "Schieland", str_detect(mp, "K_") ~"Krimpenerwaard", str_detect(mp, "VKRH") ~"Krimpenerwaard"))

# totaal_vangst_kaart_2020 <-
#   kreeften_per_soort_2020 %>%
#   group_by(mp, x, y) %>%
#   summarise(waarde = sum(waarde)) %>%
#   ungroup() %>%
#   st_as_sf(coords = c("x", "y"), crs = 28992) %>%
#
#   ggplot() +
#   ggspatial::annotation_map_tile(type = "cartolight", zoomin = 1) +
#   geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
#   geom_sf(aes(size = waarde), colour = "darkred", data = . %>% filter(waarde > 0)) +
#   geom_sf(shape = 21, size = 2, data = . %>% filter(waarde == 0)) +
#   # scale_colour_viridis_c(direction = -1) +
#   # scale_size(range = c(3,7)) +
#   guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
#   hhskthema_kaart() +
#   theme(axis.line = element_blank()) +
#   labs(title = "Aantal gevangen kreeften 2020",
#        caption = "Er zijn vooral Rode Amerikaanse rivierkreeften gevangen.\nAndere soorten komen slechts incidenteel voor.")
#
# soort_kaart_2020 <-
#   kreeften_per_soort_2020 %>%
#   filter(waarde > 0) %>%
#   mutate(naam = fct_reorder(naam, waarde, .desc = TRUE)) %>%
#   st_as_sf(coords = c("x", "y"), crs = 28992) %>%
#
#   ggplot() +
#   # ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
#   geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
#   geom_sf(size = 2, colour = "darkred", data = . %>% filter(waarde > 0)) +
#   guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
#   hhskthema_kaart() +
#   facet_wrap(~naam) +
#   theme(axis.line = element_blank(),
#         strip.background = element_rect(fill = NA, colour = NA),
#         strip.text = element_text(hjust = 0)) +
#   labs(title = "Verspreiding per soort 2020")



# soort_kaart_alles <-
#   alles_per_soort %>%
#   filter(waarde > 0) %>%
#   mutate(naam = fct_reorder(naam, waarde, .desc = TRUE)) %>%
#   st_as_sf(coords = c("x", "y"), crs = 28992) %>%
#
#   ggplot() +
#   # ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
#   geom_sf(data = ws_grens, fill = NA, colour = "grey50") +
#   geom_sf(size = 2, colour = "darkred", data = . %>% filter(waarde > 0)) +
#   guides(colour = guide_legend("Aantal"), size = guide_legend("Aantal")) +
#   hhskthema_kaart() +
#   facet_wrap(~naam) +
#   theme(axis.line = element_blank(),
#         strip.background = element_rect(fill = NA, colour = NA),
#         strip.text = element_text(hjust = 0)) +
#   labs(title = "Verspreiding per soort 2020 en 2021")


