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

# ESTATUS VÍCTIMAS BY ENTIDAD

estatus_victimas_id <- motha_drive %>% 
  filter(name == "ents_by_year_status.csv") %>% 
  pull(id)

drive_download(as_id(estatus_victimas_id),
               path = paste0(tempdir(), "/ents_by_year_status.csv"),
               overwrite = TRUE)

estatus_victimas <- read_csv(paste0(tempdir(), "/ents_by_year_status.csv"))

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
  select(entidad, nom_ent, pobtot, pobfem, pobmas)

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

# TASA ACUMULADA (CUALQUIER ESTATUS) POR ENTIDAD + SEXO ====
tasa_acum_for_lollis_sexo <- estatus_victimas %>% 
  filter(sexo != "sexo_na" & year >= 2006) %>% 
  group_by(entidad, ent_code, sexo) %>%
  summarize(n = sum(n)) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
  ungroup() %>%
  left_join(pob_ent %>% 
              select(entidad, nom_ent, pobmas, pobfem) %>% 
              pivot_longer(cols = pobmas:pobfem, names_to = "sexo", values_to = "pob") %>% 
              mutate(sexo = ifelse(sexo == "pobmas", "Hombre", "Mujer")),
            by = c("ent_code" = "entidad", "sexo")) %>%
  mutate(
    pob = as.numeric(pob),
    tasa = n / pob * 100000,
    sexo = case_when(
      sexo == "Hombre" ~ "Hombres",
      sexo == "Mujer" ~ "Mujeres"),
    nom_ent = case_when(
      nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
      nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
      T ~ nom_ent)) %>% 
  select(nom_ent, tasa, sexo) %>% 
  group_by(nom_ent) %>% 
  mutate(min_or_max = ifelse(tasa == max(tasa), "max", "min"),
         # Difference between the max and min values
         diff = max(tasa) - min(tasa),
         # Pull value of sexo for the max value of tasa
         max_sexo = sexo[which.max(tasa)]) %>%
  ungroup()

tasa_acum_lollis_sexo <- ggplot(tasa_acum_for_lollis_sexo,
                           aes(x = tasa, y = reorder(nom_ent, tasa), color = sexo)) +
  geom_line(aes(group = nom_ent), color = gray_dark) +
  geom_point(size = 8) +
  geom_text(aes(label = round(tasa, 0)),
            family = font_serif_bold,
            nudge_x = ifelse(tasa_acum_for_lollis_sexo$min_or_max == "max", 40, -40)) +
  labs(title = "Por cada 100,000 habitantes, ¿cuántas personas desaparecidas\nse registraron entre 2006 y 2024?",
       subtitle = "Se incluyen tanto personas localizadas como personas sin localizar",
       x = "Personas desaparecidas por cada 100,000 habitantes", y = "", color = "",
       caption = caption) +
  scale_color_manual(values = pal) +
  tema +
  theme(legend.position = "top") +
  guides(color = guide_legend(reverse = TRUE))

for (device in devices) {
  ggsave(filename = paste0(paths$output, "tasa_acum_ent_lollis_sexo", device),
         plot = tasa_acum_lollis_sexo, width = 10, height = 10)
}

# TASA ACUMULADA (CUALQUIER ESTATUS) POR ENTIDAD ====
tasa_acum_for_lollis <- estatus_victimas %>% 
  filter(year >= 2006) %>% 
  group_by(entidad, ent_code) %>%
  summarize(n = sum(n)) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
  ungroup() %>%
  left_join(pob_ent %>% 
              select(entidad, nom_ent, pobtot),
            by = c("ent_code" = "entidad")) %>%
  mutate(
    tasa = n / pobtot * 100000,
    nom_ent = case_when(
      nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
      nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
      T ~ nom_ent)) %>% 
  select(nom_ent, tasa)

tasa_acum_lollis <- ggplot(tasa_acum_for_lollis,
                           aes(x = tasa, y = reorder(nom_ent, tasa))) +
  geom_segment(aes(xend = 0, yend = reorder(nom_ent, tasa)),
               color = gray_dark) +
  geom_point(size = 10, color = pal[1]) +
  geom_text(aes(label = round(tasa, 0)),
            family = font_serif_bold, color = "white") +
  labs(title = "Por cada 100,000 habitantes, ¿cuántas personas desaparecidas\nse registraron entre 2006 y 2024?",
       x = "Personas desaparecidas por cada 100,000 habitantes", y = "",
       caption = caption) +
  tema
  
for (device in devices) {
  ggsave(filename = paste0(paths$output, "tasa_acum_ent_lollis", device),
         plot = tasa_acum_lollis, width = 10, height = 10)
}

# ESTATUS DE PERSONAS DESAPARECIDAS POR ENTIDAD ====
estatus_para_barras <- estatus_victimas %>% 
  filter(year >= 2006) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
  left_join(pob_ent %>% select(entidad, nom_ent),
            by = c("ent_code" = "entidad")) %>%
  group_by(nom_ent, ent_code, status) %>%
  summarize(n = sum(n)) %>% 
  group_by(nom_ent) %>%
  mutate(total = sum(n), 
         perc = n / total,
         status = case_when(
           status == "no_local" ~ "Sin localizar",
           status == "local_sv" ~ "Localizadas sin vida",
           status == "local_cv" ~ "Localizadas con vida"),
         nom_ent = case_when(
           nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
           nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
           nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
           T ~ nom_ent))

order_ent_estatus <- estatus_para_barras %>% 
  filter(status == "Sin localizar") %>% 
  arrange(perc) %>% 
  pull(nom_ent)

estatus_barras <- ggplot(estatus_para_barras %>% 
                           mutate(nom_ent = factor(nom_ent, levels = order_ent_estatus)),
                        aes(y = nom_ent, x = perc, fill = status)) +
  geom_col() +
  geom_label(aes(label = scales::percent(perc, accuracy = 0.1)),
            family = font_serif_bold, color = "white", position = position_stack(vjust = 0.5),
            show.legend = F) +
  labs(title = "Estatus de personas desaparecidas registradas por entidad federativa",
       subtitle = "Se incluyen personas desaparecidas entre 2006 y 2024",
       x = "", y = "", fill = "",
       caption = caption) +
  scale_fill_manual(values = pal) +
  tema +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top") 

for (device in devices) {
  ggsave(filename = paste0(paths$output, "estatus_ent_barras", device),
         plot = estatus_barras, width = 14, height = 10)
}

# CAMBIO EN # DE DESAPARICIONES (MAP) ====
cambio_perc <- estatus_victimas %>%
  mutate(year_group = case_when(
    year %in% 2006:2010 ~ "years_2006_2010",
    year %in% 2019:2023 ~ "years_2019_2023")) %>% 
  filter(!is.na(year_group)) %>% 
  group_by(ent_code, entidad, year_group) %>% 
  summarize(n = sum(n)) %>% 
  pivot_wider(names_from = year_group, values_from = n) %>% 
  mutate(perc_cambio = ((years_2019_2023 - years_2006_2010) / years_2006_2010),
         ent_code = str_pad(ent_code, 2, pad = "0"),
         perc_cube_root = sign(perc_cambio) * abs(perc_cambio) ^ (1/3)) %>%
  left_join(mxhexbin.map, by = c("ent_code" = "id")) %>% 
  left_join(map_centroids, by = c("ent_code" = "id")) %>% 
  mutate(
    state_abbr = unfactor(state_abbr),
    state_abbr = ifelse(state_abbr == "DF", "CDMX", state_abbr))

# MAX: Tlax, BCS, Campeche
ents_mas_aumento <- cambio_perc %>% 
  select(entidad, ent_code, perc_cambio) %>%
  distinct() %>% 
  ungroup() %>% 
  slice_max(order_by = perc_cambio, n = 3) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>% 
  pull(ent_code)

# MIN: Guanajuato, Coahuila
ents_mas_disminucion <- cambio_perc %>% 
  select(entidad, ent_code, perc_cambio) %>%
  distinct() %>% 
  ungroup() %>% 
  slice_min(order_by = perc_cambio, n = 3) %>% 
  filter(perc_cambio < 0) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
  pull(ent_code)

cambio_perc_map <- ggplot(cambio_perc, aes(x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = perc_cube_root), color = gray_dark) +
  geom_text(aes(x = centroid_long, y = centroid_lat,
                label = paste0(state_abbr, "\n",
                               ifelse(perc_cambio > 0, "+", ""),
                               scales::comma(round(perc_cambio * 100, 1)), "%")),
            color = gray_light, family = font_serif_bold, size = 3) +
  labs(title = "¿Cómo cambió el número de personas desaparecidas registradas a partir de 2006?",
       subtitle = "Cambio porcentual en registros de personas desaparecidas durante el periodo 2019-2023\ncon respecto a 2006-2010",
       fill = "El número de registros de personas desaparecidas...",
       caption = caption) +
  scale_fill_gradient2(low = pal[1], mid = "white", high = pal[3], midpoint = 0,
                       breaks = c(-6.1, 0, 6.1),
                       limits = c(-6.1, 6.1),
                       labels = c("disminuyó", "permaneció igual", "aumentó")) +
  tema +
  guides(fill = guide_colorbar(barwidth = 12, ticks.colour = NA,
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
  ggsave(filename = paste0(paths$output, "cambio_ent_map", device),
         plot = cambio_perc_map, width = 10, height = 8)
}

# CAMBIO EN TASA DE DESAPARICIONES (FIEBRES) ====
cambio_tasa <- estatus_victimas %>% 
  filter(year >= 2006) %>%
  group_by(year, ent_code) %>%
  reframe(n = sum(n),
            n = ifelse(year == 2024, n / percent_of_year, n)) %>% 
  mutate(ent_code = str_pad(ent_code, 2, pad = "0")) %>%
  left_join(pob_ent %>% select(entidad, nom_ent, pobtot),
            by = c("ent_code" = "entidad")) %>%
  mutate(
    tasa = n / pobtot * 100000,
    nom_ent = case_when(
      nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
      nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
      T ~ nom_ent),
    ent_color = ifelse(ent_code %in% c(ents_mas_aumento, ents_mas_disminucion), ent_code, NA_character_))

cambio_tasa_fiebres <- ggplot(cambio_tasa, aes(x = year, y = tasa,
                                               group = nom_ent, color = nom_ent)) +
  geom_line(
    data = cambio_tasa %>% filter(is.na(ent_color)),
    linewidth = 0.5, color = "#d3d3d3") +
  geom_line(
    data = cambio_tasa %>% filter(!is.na(ent_color) & year != 2024),
    linewidth = 1.5) +
  geom_line(
    data = cambio_tasa %>% filter(!is.na(ent_color) & year %in% 2023:2024),
    linewidth = 1, linetype = "dashed") +
  geom_point(
    data = cambio_tasa %>% filter(!is.na(ent_color)),
    aes(shape = ifelse(ent_code %in% ents_mas_aumento, "aumento", "disminución"),
        fill = nom_ent),
    size = 3) +
  geom_text(
    data = cambio_tasa %>% filter(!is.na(ent_color) & year == 2024) %>% distinct(),
    aes(label = str_wrap(nom_ent, 15), x = 2025.1, y = case_when(nom_ent == "Guanajuato" ~ tasa - 4,
                                                                 nom_ent == "Tlaxcala" ~ tasa + 2,
                                                                 T ~ tasa)),
    family = font_serif_bold) +
  scale_shape_manual(values = c("aumento" = 24, "disminución" = 25)) +
  labs(title = "Tasa de personas desaparecidas registradas por cada 100,000 habitantes",
       subtitle = "Por entidad federativa",
       x = "", y = "", shape = "Entidades* con mayor...",
       caption = paste0(str_replace(caption, "\n", " "),
                        "\n*Entidades con mayor aumento/disminución en términos de cambio porcentual en el número de registros de personas desaparecidas 2019-2023 vs. 2006-2010")) +
  scale_color_manual(values = c(pal[1:6], "#d3d3d3")) +
  scale_fill_manual(values = c(pal[1:6])) +
  scale_x_continuous(breaks = seq(2006, 2024, 2)) +
  tema +
  guides(
    fill = "none", 
    color = "none",
    shape = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  theme(legend.position = "top")

for (device in devices) {
  ggsave(filename = paste0(paths$output, "cambio_tasa_ent_fiebres", device),
         plot = cambio_tasa_fiebres, width = 14, height = 8)
}

# done.