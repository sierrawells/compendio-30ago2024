# Author: Sierra Wells
# Maintainer(s):  AF, SW, PB, IS
# Copyright:   2024, Data Cívica, GPL v2 or later
# ====================================================

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(googledrive, here, tidyverse, ggrepel, data.table, jsonlite)

# Files
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/src/tema.R"))

source(paths$tema)

drive_auth("sierra.wells@datacivica.org")
drive_in <- drive_ls(as_id("1ZCf0i2KI-_8bEngd-c4HrBAPsTm-weIO"))

# Read data ====

# ENVIPE (empezó a medir desapariciones en 2013)
envipe_id <- drive_in %>% 
  filter(name %like% "envipe") %>% 
  pull(id)

drive_download(envipe_id,
               path = paste0(tempdir(), "/envipe.csv"),
               overwrite = TRUE)

envipe <- read_csv(paste0(tempdir(), "/envipe.csv"))

file.remove(paste0(tempdir(), "/envipe.csv"))

# RNPDNO
rnpdno_gen_id <- drive_in %>% 
  filter(name %like% "scrapeo") %>% 
  pull(id) %>% 
  drive_ls() %>%
  filter(name == "general.json") %>%
  pull(id)

drive_download(rnpdno_gen_id,
               path = paste0(tempdir(), "/rnpdno_gen.json"),
               overwrite = TRUE)

rnpdno_by_year_hom <- fromJSON(paste0(tempdir(), "/rnpdno_gen.json"))[["anual"]][["Hombre"]] %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "count") %>% 
  mutate(year = as.numeric(str_remove(year, "X")),
         sexo = "hombre")

rnpdno_by_year_muj <- fromJSON(paste0(tempdir(), "/rnpdno_gen.json"))[["anual"]][["Mujer"]] %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "count") %>% 
  mutate(year = as.numeric(str_remove(year, "X")),
         sexo = "mujer")

rnpdno_by_year_ind <- fromJSON(paste0(tempdir(), "/rnpdno_gen.json"))[["anual"]][["Indeterminado"]] %>%
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "year", values_to = "count") %>% 
  mutate(year = as.numeric(str_remove(year, "X")),
         sexo = NA_character_)

rnpdno_by_year <- bind_rows(rnpdno_by_year_hom, rnpdno_by_year_muj, rnpdno_by_year_ind)

file.remove(paste0(tempdir(), "/rnpdno_gen.json"))

# Graph prep ====
caption_ambos <- "Fuente: Elaboración por Data Cívica a partir de la ENVIPE 2013-2023 y del RNPDNO"

# ENVIPE VS. RNPNDO ====

# Totales según cada fuente
rndpno_for_graf <- rnpdno_by_year %>% 
  filter(year %in% envipe$year) %>% 
  group_by(year) %>%
  summarise(total_anual = sum(count))

compar_rnpdno_envipe <- ggplot()+
  geom_ribbon(data = envipe, aes(x = year, y = total_anual, group = 1, ymin = lw, ymax = up),
              alpha = 0.1, fill = pal[1]) +
  geom_point(data = envipe, aes(x = year, y = total_anual, group = 1, color = "ENVIPE"),
             size = 3) +
  geom_line(data = envipe, aes(x = year, y = total_anual, group = 1, color = "ENVIPE"),
            linewidth = 1.5) +
  geom_point(data = rndpno_for_graf, aes(x = year, y = total_anual, group = 1, color = "RNPDNO"),
             size = 3) +
  geom_line(data =rndpno_for_graf , aes(x = year, y = total_anual, group = 1, color = "RNPDNO"),
            linewidth = 1.5) +
  scale_x_continuous(limits = c(2012, 2022), breaks = seq(2012, 2022, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 90000),
                     breaks = seq(0, 80000, 20000)) +
  scale_color_manual(values = pal) +
  labs(title = "¿Cuántas personas desaparecidas hubo en México durante cada año 2012-2022?",
       subtitle = "Según la ENVIPE y el RNPDNO",
       color = "",
       x = "",
       y = "Número estimado de personas",
       caption = paste0("Fuente: Elaboración por Data Cívica a partir de la ENVIPE 2013-2023 y del RNPDNO",
                        "\nLa ENVIPE solo mide desapariciones a partir de 2012.")) +
  tema 

for (device in devices){
  ggsave(paste0(paths$output, "compar_rnpdno_envipe", device),
         plot = compar_rnpdno_envipe,
         width = 10, height = 8)
}

# ¿Cuántas veces más desapariciones estima la ENVIPE que el RNPDNO?
diff_veces_envipe_rnpdno <- envipe %>% 
  rename(total_envipe = total_anual) %>%
  left_join(rndpno_for_graf %>% 
              rename(total_rnpdno = total_anual), by = "year") %>% 
  mutate(veces = round(total_envipe / total_rnpdno, 1))

# 2022: 2.2 veces más
# 2012 (max): 6.4 veces más
# avg (by year): 2.75

# done.