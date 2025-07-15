#
# Author: SW
# Maintainer(s): SW, AF, PB
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

# ESTATUS VÍCTIMAS BY EDAD

estatus_victimas_id <- motha_drive %>% 
  filter(name == "edad_by_year_status.csv") %>% 
  pull(id)

drive_download(as_id(estatus_victimas_id),
               path = paste0(tempdir(), "/edad_by_year_status.csv"),
               overwrite = TRUE)

estatus_victimas <- read_csv(paste0(tempdir(), "/edad_by_year_status.csv")) %>% 
  mutate(edad_categ = factor(edad_categ,
                             levels = c("Menores de 10 años",
                                        "De 10 a 19 años",
                                        "De 20 a 29 años",
                                        "De 30 a 44 años",
                                        "De 45 a 59 años",
                                        "60 años y más")))

file.remove(paste0(tempdir(), "/edad_by_year_status.csv"))

stopifnot(all(!is.na(estatus_victimas$edad_categ)),
          all(c("local_cv", "local_sv", "no_local") %in% estatus_victimas$status))

# GRAPH PREP ====
fecha <- "22-7-2024"

caption <- paste0("Fuente: Elaboración por Data Cívica a partir del dashboard de la Versión de Consulta Pública del RNPDNO\n(consulta realizada ",
                  fecha, ")")

# DISTRIBUCIÓN DE EDADES POR SEXO ====
dist_edad_for_barras <- estatus_victimas %>% 
  filter(sexo != "sexo_na" & year >= 2006) %>%
  group_by(edad_categ, sexo) %>% 
  summarise(n = sum(n)) %>% 
  group_by(sexo) %>% 
  mutate(perc = n / sum(n))

dist_edad_barras <- ggplot(dist_edad_for_barras,
                           aes(x = 0, y = perc, fill = edad_categ)) +
  geom_col(position = position_fill()) +
  geom_label(aes(label = paste0(edad_categ, ": ", scales::percent(perc, accuracy = 0.1)),
                 color = edad_categ),
            position = position_fill(vjust = 0.5), fill = fondo, family = font_serif_bold) +
  facet_wrap(~sexo,
             labeller = labeller(sexo = c("Hombre" = "Hombres", "Mujer" = "Mujeres"))) +
  labs(title = "Distribuctión de edad entre las personas desaparecidas registradas",
       subtitle = "Por sexo",
       caption = paste0(caption,
                        "\nSe incluyen personas desaparecidas entre 2006 y 2024."),
       x = "", y = "") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  tema +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

for (device in devices){
  ggsave(filename = paste0(paths$output, "dist_edad_barras", device),
         plot = dist_edad_barras, width = 10, height = 7)
}

# HEAT MAP: LIKELIHOOD OF EACH STATUS BY AGE + SEX ====
prob_status_edad_sexo <- estatus_victimas %>% 
  filter(sexo != "sexo_na" & year >= 2006) %>%
  group_by(edad_categ, sexo, status) %>% 
  summarise(n = sum(n)) %>% 
  group_by(edad_categ, sexo) %>% 
  mutate(perc = n / sum(n))

prob_status_edad_sexo_heat <- ggplot(prob_status_edad_sexo,
                                     aes(x = 0, y = fct_rev(edad_categ), fill = perc)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.1), color = perc < 0.2),
            family = font_serif_bold) +
  facet_grid(sexo ~ status,
             labeller = labeller(sexo = c("hombres" = "Hombres", "mujeres" = "Mujeres"),
                                 status = c("local_cv" = "Localizadas con vida?",
                                            "local_sv" = "Localizadas sin vida?",
                                            "no_local" = "Sin localizar?"))) +
  labs(
    title = "Por edad y sexo, ¿qué porcentaje de las personas desaparecidas registradas son...",
    caption = paste0(caption,
                     "\nSe incluyen personas desaparecidas entre 2006 y 2024."),
    x = "", y = "") +
  scale_fill_gradient(low = grad[2], high = grad[1]) +
  scale_color_manual(values = c("TRUE" = gray_light, "FALSE" = "white")) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

for (device in devices){
  ggsave(filename = paste0(paths$output, "prob_status_edad_sexo_heat", device),
         plot = prob_status_edad_sexo_heat, width = 10, height = 7)
}

# FIEBRE: NÚMERO DE DESAPARICIONES POR AÑO Y EDAD ====

# What percent of the year are the days between the first of 2024 and fecha?
percent_of_year <- as.numeric(as.Date(fecha, format = "%d-%m-%Y") - as.Date("01-01-2024", format = "%d-%m-%Y")) / 365

desaps_edad_sexo <- estatus_victimas %>% 
  filter(sexo != "sexo_na" & year >= 2006) %>%
  group_by(edad_categ, sexo, year) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(n = ifelse(year == 2024, n / percent_of_year, n))

desaps_edad_sexo_fiebre <- ggplot(desaps_edad_sexo,
                                 aes(x = year, y = n, color = edad_categ)) +
  geom_line(data = desaps_edad_sexo %>% filter(year <= 2023)) +
  geom_line(data = desaps_edad_sexo %>% filter(year >= 2023),
            linetype = "dotted") +
  geom_point() +
  facet_wrap(~sexo,
             labeller = labeller(sexo = c("Hombre" = "Hombres", "Mujer" = "Mujeres"))) +
  labs(title = "Número de registros de personas desaparecidas por año, sexo y edad",
       subtitle = "2006-2024",
       x = "Año de desaparición", y = "",
       color = "Edad",
       caption = paste0(caption,
                        "\nLa observación de 2024 se ajusta a la proporción de días transcurridos en el año")) +
  scale_x_continuous(breaks = seq(2006, 2024, 2)) +
  scale_y_continuous(breaks = seq(0, 6000, 1000), labels = scales::comma) +
  scale_color_manual(values = pal) +
  tema

for (device in devices){
  ggsave(filename = paste0(paths$output, "anual_edad_sexo_fiebre", device),
         plot = desaps_edad_sexo_fiebre, width = 10, height = 5)
}

# done.
