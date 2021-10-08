library(tidyverse)
library(lubridate)
library(HHSKwkl)

theme_set(hhskthema())

namen <- tibble::tribble(
                      ~taxonnaam,                                  ~naam,
           "Procambarus clarkii",        "Rode Amerikaanse Rivierkreeft",
            "Orconectes virilis", "Geknobbelde Amerikaanse Rivierkreeft",
            "Procambarus acutus",  "Gestreepte Amerikaanse Rivierkreeft",
            "Orconectes limosus",    "Gevlekte Amerikaanse Rivierkreeft"
           )

locs <- readxl::read_excel("data/20200522_kreeftenmonitoring HHSK 2020_DAWACO_v20200630.xlsx",
                           sheet = "DAWACO TRAJECTEN") %>% select(mp = `Code traject`, x = XBEGIN, y = YBEGIN)

kreeften <- readxl::read_excel("data/20200522_kreeftenmonitoring HHSK 2020_DAWACO_v20200630.xlsx") %>%
  rename(mp = `Code traject`,
         taxonnaam = Taxonnaam,
         lengte = `Lengte in cm`,
         waarde = Waarde,
         type = `Kreeft / bijvangst`,
         datum = `Datum Bemonstering`) %>%
  left_join(namen, by = "taxonnaam") %>%
  filter(!is.na(naam)) %>%
  right_join(locs, by = "mp") %>%
  select(mp, x, y, datum, taxonnaam, naam, lengte, waarde)

kreeften %>% openxlsx::write.xlsx("data/kreeften_per_soort_en_lengte.xlsx")

kreeften %>%
  group_by(mp, x, y, datum, taxonnaam, naam) %>%
  summarise(waarde = sum(waarde, na.rm = TRUE)) %>%
  openxlsx::write.xlsx("data/kreeften_per_soort.xlsx")


