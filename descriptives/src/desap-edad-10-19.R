#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, svglite, jsonlite, mxmaps,
               ggrepel, varhandle, data.table)

# Files
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/src/tema.R"))

source(paths$tema)

# READ ====
drive_auth("sierra.wells@datacivica.org")
motha_drive <- drive_ls(as_id("1F7nl00R48u_iXEDVVvqtcbfF9t8UfJbU"))

data(mxhexbin.map)

# EDAD 10-19

edad_10_19_id <- motha_drive %>% 
  filter(name %like% "10_19") %>% 
  pull(id)

drive_download(as_id(edad_10_19_id),
               path = paste0(tempdir(), "/ents_by_year_status.csv"),
               overwrite = TRUE)

edad_10_19 <- read_csv(paste0(tempdir(), "/ents_by_year_status.csv"))

file.remove(paste0(tempdir(), "/ents_by_year_status.csv"))

# BY ENTIDAD (CUALQUIER EDAD)

ent_gen_id <- motha_drive %>% 
  filter(name == "ents_by_year_status.csv") %>% 
  pull(id)

drive_download(as_id(ent_gen_id),
               path = paste0(tempdir(), "/ents_by_year_status.csv"),
               overwrite = TRUE)

ent_gen <- read_csv(paste0(tempdir(), "/ents_by_year_status.csv"))

ent_gen_total_by_year <- ent_gen %>% 
  group_by(ent_code, year) %>% 
  summarize(n_total = sum(n))

file.remove(paste0(tempdir(), "/ents_by_year_status.csv"))

# POBLACIÓN POR ENTIDAD

pob_id <- drive_ls(as_id("1ZCf0i2KI-_8bEngd-c4HrBAPsTm-weIO")) %>% 
  filter(name %like% "pob") %>% 
  pull(id)

stopifnot(length(pob_id) == 1)

drive_download(as_id(pob_id),
               path = paste0(tempdir(), "/pob.csv"),
               overwrite = TRUE)

pob_ent <- read_csv(paste0(tempdir(), "/pob.csv")) %>% 
  filter(nivel == "entidad") %>% 
  select(entidad, nom_ent, pobtot)

file.remove(paste0(tempdir(), "/pob.csv"))

# GRAPH PREP ====
fecha <- "22-7-2024"

# What percent of the year are the days between the first of 2024 and fecha?
percent_of_year <- as.numeric(as.Date(fecha, format = "%d-%m-%Y") - as.Date("01-01-2024", format = "%d-%m-%Y")) / 365

caption <- paste0("Fuente: Elaboración por Data Cívica a partir del dashboard de la Versión de Consulta Pública del RNPDNO\n(consulta realizada ",
                  fecha, ")")

# Make centroids for maps
map_centroids <- mxhexbin.map %>%
  group_by(id) %>%
  summarize(max_long = max(long),
            max_lat = max(lat),
            min_long = min(long),
            min_lat = min(lat),
            centroid_long = (max_long + min_long) / 2,
            centroid_lat = (max_lat + min_lat) / 2)

# MAPA DE FILL COLORS: ¿CÓMO HA CAMBIADO EL % DE DESAPARICIONES QUE SON DE MORRAS DE 10 A 19 AÑOS? ====

cambio_pp_desap_morras <- edad_10_19 %>% 
  filter(sexo == "Mujer" & !is.na(year)) %>% 
  group_by(entidad, ent_code, year) %>% 
  summarize(n = sum(n)) %>%
  right_join(ent_gen_total_by_year,
             by = c("year", "ent_code")) %>% 
  # Fill in all years 2006-2024 with 0 if not present
  complete(year = 2006:2024,
           fill = list(n = 0,
                       n_total = 0)) %>%
  mutate(year_group = case_when(
           year %in% 2006:2010 ~ "year_2006_2010",
           year %in% 2019:2023 ~ "year_2019_2023")) %>% 
  filter(!is.na(year_group)) %>% 
  group_by(ent_code, year_group) %>%
  summarize(perc_morritas = sum(n) / sum(n_total)) %>% 
  pivot_wider(names_from = year_group,
              values_from = perc_morritas) %>%
  mutate(cambio_pp = year_2019_2023 - year_2006_2010,
         ent_code = str_pad(ent_code, 2, pad = "0")) %>% 
  left_join(mxhexbin.map, by = c("ent_code" = "id")) %>% 
  left_join(map_centroids, by = c("ent_code" = "id")) %>% 
  mutate(
    state_abbr = unfactor(state_abbr),
    state_abbr = ifelse(state_abbr == "DF", "CDMX", state_abbr))

cambio_pp_morras_mapa <- ggplot(cambio_pp_desap_morras, aes(x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = cambio_pp), color = gray_dark) +
  geom_text(aes(x = centroid_long, y = centroid_lat,
                label = paste0(state_abbr, "\n",
                               ifelse(cambio_pp > 0, "+", ""),
                               round(cambio_pp * 100, 1), " pp")),
            color = gray_light, family = font_serif_bold, size = 3) +
  labs(title = "A partir de 2006, ¿cómo cambió el porcentaje de personas desaparecidas registradas que eran\nmujeres de 10 a 19 años?",
       subtitle = "Cambio en puntos porcentuales (p.p.) de este porcentaje para el periodo 2019-2023\ncon respecto al periodo 2006-2010",
       fill = "En comparación con otras desapariciones, las desapariciones de mujeres entre 10 y 19 años...",
       caption = caption) +
  scale_fill_gradient2(low = pal[1], mid = "white", high = pal[3], midpoint = 0,
                       breaks = c(-0.3, 0, 0.3),
                       limits = c(-0.3, 0.3),
                       labels = c("disminuyeron", "permanecieron igual", "aumentaron")) +
  tema +
  guides(fill = guide_colorbar(barwidth = 14, ticks.colour = NA,
                               title.position = "top")) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.position = "top")

for (device in devices) {
  ggsave(filename = paste0(paths$output, "cambio_morras_mapa", device),
         plot = cambio_pp_morras_mapa, width = 10, height = 8)
}

# DESAPARICIONES EN SLP + BCS: MORRAS VS. GENERAL ====
# slp_bcs_morras_vs_total <- edad_10_19 %>% 
#   filter(sexo == "Mujer") %>% 
#   group_by(entidad, ent_code, year) %>%
#   summarize(n = sum(n)) %>%
#   right_join(ent_gen_total_by_year,
#              by = c("year", "ent_code")) %>%
#   complete(year = 2006:2024,
#            fill = list(n = 0,
#                        n_total = 0)) %>% 
#   mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
#   left_join(pob_ent, by = c("ent_code" = "entidad")) %>% 
#   pivot_longer(cols = c(n, n_total),
#                names_to = "tipo",
#                values_to = "n") %>% 
#   mutate(
#     n = ifelse(year == 2024, n / percent_of_year, n),
#     tasa = (n * 100000) / pobtot) %>% 
#   filter(entidad %in% c("SAN LUIS POTOSI", "BAJA CALIFORNIA SUR"),
#          year >= 2006)
# 
# slp_bcs_morras_fiebres <- ggplot(slp_bcs_morras_vs_total, aes(x = year, y = tasa,
#                                                               color = tipo)) +
#   geom_line(data = slp_bcs_morras_vs_total %>% filter(year != 2024),
#             linewidth = 1) +
#   geom_line(data = slp_bcs_morras_vs_total %>% filter(year %in% 2023:2024),
#             linewidth = 1, linetype = "dashed") +
#   geom_point(size = 3) +
#   facet_wrap(~entidad, scales = "free_y")
#   geom_text(data = slp_bcs_morras_vs_total %>% filter(year == 2024) %>% distinct(),
#             aes(label = str_wrap(entidad, 15), x = 2025.1, y = tasa),
#             family = font_serif_bold) +
#   labs(title = "Tasa de desapariciones por cada 100,000 habitantes",
#        subtitle = "Mujeres de 10 a 19 años vs. desapariciones totales",
#        x = "", y = "", shape = "Tipo de desaparición",
#        caption = caption) +
#   scale_shape_manual(values = c("n" = 24, "n_total" = 25)) +
#   scale_linetype_manual(values = c("n" = "solid", "n_total" = "dashed")) +
#   scale_color_manual(values = c(pal[1], pal[2])) +
#   tema +
#   guides(
#     shape = guide_legend(title.position = "top", title.hjust = 0.5),
#     linetype = guide_legend(title.position = "top", title.hjust = 0.5),
#     color = "none"
#   ) +
#   theme(legend.position = "top")

# Didn't think it was that informative. Not saving for now.
  
# done.
