#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, svglite, jsonlite)

# Files
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/src/tema.R"))

source(paths$tema)

# READ ====
drive_auth("sierra.wells@datacivica.org")
motha_drive <- drive_ls(as_id("1F7nl00R48u_iXEDVVvqtcbfF9t8UfJbU"))

# GENERAL
general_id <- motha_drive %>% 
  filter(name == "general.json") %>% 
  pull(id)

drive_download(as_id(general_id),
               path = paste0(tempdir(), "/general.json"),
               overwrite = TRUE)

general <- fromJSON(paste0(tempdir(), "/general.json"))

file.remove(paste0(tempdir(), "/general.json"))

general_anual_hom <- general[["anual"]][["Hombre"]] %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>%
  mutate(sexo = "hombres", 
         year = as.numeric(year))

general_anual_muj <- general[["anual"]][["Mujer"]] %>%
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>%
  mutate(sexo = "mujeres", 
         year = as.numeric(year))

general_anual_sexo_na <- general[["anual"]][["Indeterminado"]] %>%
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>%
  mutate(sexo = "sexo_na", 
         year = as.numeric(year))

general_anual <- bind_rows(general_anual_hom, general_anual_muj, general_anual_sexo_na) %>% 
  filter(year >= 2006) %>% 
  group_by(year) %>% 
  mutate(total = sum(n)) %>%
  filter(sexo != "sexo_na")

rm(general_anual_hom, general_anual_muj, general_anual_sexo_na)

# ESTATUS VÍCTIMAS

estatus_victimas_id <- motha_drive %>% 
  filter(name == "status_by_year.csv") %>% 
  pull(id)

drive_download(as_id(estatus_victimas_id),
               path = paste0(tempdir(), "/status_by_year.csv"),
               overwrite = TRUE)

estatus_victimas <- read_csv(paste0(tempdir(), "/status_by_year.csv"))

file.remove(paste0(tempdir(), "/status_by_year.csv"))

# GRAPH PREP ====
fecha <- "22-7-2024"

caption <- paste0("Fuente: Elaboración por Data Cívica a partir del dashboard de la Versión de Consulta Pública del RNPDNO\n(consulta realizada ",
                  fecha, ")")

# NÚMERO TOTAL DE DESAPARICIONES POR AÑO Y SEXO ====

# What percent of the year are the days between the first of 2024 and fecha?
percent_of_year <- as.numeric(as.Date(fecha, format = "%d-%m-%Y") - as.Date("01-01-2024", format = "%d-%m-%Y")) / 365

total_anual_for_graf <- general_anual %>% 
  mutate(n = ifelse(year == 2024, n / percent_of_year, n),
         total = ifelse(year == 2024, total / percent_of_year, total))

total_anual_graf <- ggplot(total_anual_for_graf,
                           aes(x = year, y = n, color = sexo)) +
  geom_line(data = total_anual_for_graf %>% filter(year <= 2023),
            linewidth = 1.5) +
  geom_line(data = total_anual_for_graf %>% filter(year >= 2023),
             linetype = "dotted", linewidth = 1.5) +
  geom_point(size = 3) +
  geom_line(data = total_anual_for_graf %>% filter(year <= 2023),
            aes(y = total), color = ejes, linewidth = 1.5) +
  geom_line(data = total_anual_for_graf %>% filter(year >= 2023),
            aes(y = total), linetype = "dotted", color = ejes, linewidth = 1.5) +
  geom_point(aes(y = total), color = ejes, size = 3) +
  geom_label(data = total_anual_for_graf %>% filter(year == 2024),
             aes(label = str_to_sentence(sexo), y = n),
             nudge_y = 2000,  family = font_serif_bold, fill = fondo) +
  geom_label(data = total_anual_for_graf %>% filter(year == 2024),
             aes(label = "Total", y = total),
             nudge_y = 2000, color = ejes,  family = font_serif_bold, fill = fondo) +
  scale_x_continuous(breaks = seq(2006, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 30000, 5000), labels = scales::comma) +
  scale_color_manual(values = pal) +
  labs(title = "Número de registros de personas desaparecidas por año y sexo",
       subtitle = "2006-2024",
       x = "Año de desaparición",
       y = "",
       caption = paste0(caption,
                        "\nLa observación de 2024 se ajusta a la proporción de días transcurridos en el año")) +
  tema +
  theme(legend.position = "none")

for (device in devices){
  ggsave(filename = paste0(paths$output, "total_anual_fiebre", device),
         plot = total_anual_graf, width = 10, height = 5)
}

# BARRAS: LOCALIZADAS CON VIDA, LOCALIZADAS SIN VIDA, SIN LOCALIZAR (POR SEXO) ====
estatus_victimas_for_barras <- estatus_victimas %>% 
  filter(sexo != "sexo_na" & year >= 2006) %>%
  group_by(status, sexo) %>% 
  summarise(n = sum(n)) %>%
  group_by(sexo) %>% 
  mutate(total_sexo = sum(n),
         perc = n / total_sexo,
         status = case_when(
           status == "local_cv" ~ "Localizadas con vida",
           status == "local_sv" ~ "Localizadas sin vida",
           status == "no_local" ~ "Sin localizar"),
         status = factor(status, levels = c("Sin localizar", "Localizadas sin vida", "Localizadas con vida")))

estatus_victimas_sexo_barras <- ggplot(estatus_victimas_for_barras,
                                  aes(x = 0, y = perc, fill = status)) +
  geom_col(position = position_fill()) +
  geom_label(aes(label = paste0(status, ": ", scales::percent(perc, accuracy = 0.1)), y = perc, color = status),
            position = position_stack(vjust = 0.5), fill = fondo, family = font_serif_bold) +
  facet_wrap(~sexo,
             labeller = labeller(sexo = c("Hombre" = "Hombres", "Mujer" = "Mujeres"))) +
  labs(title = "Estatus de las personas desaparecidas registradas por sexo",
       subtitle = "Personas desaparecidas entre 2006 y 2024",
       x = "", y = "", fill = "",
       caption = caption) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  tema +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

for (device in devices){
  ggsave(filename = paste0(paths$output, "estatus_victimas_sexo_barras", device),
         plot = estatus_victimas_sexo_barras, width = 10, height = 5)
}

# CUMULATIVE COUNT OF PERSONAS DESAPARECIDAS POR ESTATUS ====
cumulative_labels <- estatus_victimas %>% 
  mutate(year = ifelse(is.na(year), 0, year)) %>% 
  group_by(status, year) %>% 
  summarise(n = sum(n)) %>%
  group_by(status) %>%
  mutate(cumul_label = cumsum(n)) %>% 
  filter(year == 2024) %>% 
  mutate(status = case_when(
    status == "local_cv" ~ "Localizadas con vida",
    status == "local_sv" ~ "Localizadas sin vida",
    status == "no_local" ~ "Sin localizar")) %>% 
  select(status, cumul_label)

estatus_victimas_cumulative <- estatus_victimas %>% 
  filter(!is.na(year)) %>% 
  group_by(status, year) %>% 
  summarise(n = sum(n)) %>%
  group_by(status) %>%
  mutate(cumulative = cumsum(n),
         status = case_when(
           status == "local_cv" ~ "Localizadas con vida",
           status == "local_sv" ~ "Localizadas sin vida",
           status == "no_local" ~ "Sin localizar")) %>% 
  filter(year >= 2006) %>% 
  left_join(cumulative_labels, by = "status")

# In total: 324,464 have ever disappeared (since 1972)
# of which 115,894 (35.7%) are still missing
# 192,451 (59.4%) have been found alive
# 16,119 (5%) have been found deceased

estatus_victimas_cumulative_graf <- ggplot(estatus_victimas_cumulative,
                                           aes(x = year, y = cumulative, fill = status)) +
  geom_area() +
  geom_label(data = estatus_victimas_cumulative %>% filter(year == 2024),
             aes(label = paste0(status, ":\n", scales::comma(cumul_label)), color = status, x = 2022.5),
             position = position_stack(vjust = 0.5),
            fill = fondo, family = font_serif_bold) +
  scale_x_continuous(breaks = seq(2006, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 300000, 50000), labels = scales::comma) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(title = "Número acumulado de registros de personas desaparecidas por estatus",
       subtitle = "2006-2024",
       x = "Año de desaparición",
       y = "",
       fill = "",
       caption = paste0(caption,
                        "\nSuma acumulada incluye registros desde 1952.",
                        "\nTotales incluyen registros sin año de desaparición especificado.")) +
  tema +
  theme(legend.position = "none")


for (device in devices){
  ggsave(filename = paste0(paths$output, "estatus_victimas_cumulative", device),
         plot = estatus_victimas_cumulative_graf, width = 10, height = 5)
}

# done.
