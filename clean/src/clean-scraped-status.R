#
# Author: SW
# Maintainer(s): SW, AF, PB
# License:  Data Cívica 2024 ©
# ---------------------------------------------


# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, jsonlite)

# Paths
paths <- list(input = here("clean/input"))

# READ ====
drive_auth("sierra.wells@datacivica.org")
motha_drive_id <- as_id("1F7nl00R48u_iXEDVVvqtcbfF9t8UfJbU")
motha_drive <- drive_ls(motha_drive_id)

# DICCIONARIOS
dict_drive <- motha_drive %>% 
  filter(name == "diccionarios") %>% 
  pull(id) %>% 
  drive_ls()

# Estatus víctimas
estatus_id <- dict_drive %>% 
  filter(name == "estatus_victimas.json") %>% 
  pull(id)

drive_download(as_id(estatus_id),
               path = paste0(tempdir(), "/estatus_victimas.json"),
               overwrite = TRUE)

estatus_victimas_dict <- fromJSON(paste0(tempdir(), "/estatus_victimas.json")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "status", values_to = "file_number") %>% 
  mutate(file_number = as.numeric(file_number))

file.remove(paste0(tempdir(), "/estatus_victimas.json"))

# JSONS

input_file_names <- list.files(paths$input, recursive = TRUE, full.names = TRUE) %>% 
  keep(~!str_detect(., "edad") & str_detect(., "estatus_victimas") & !str_detect(., "ent"))

tempo_ls <- list()

for (i in 1:length(input_file_names)){
  file_name <- input_file_names[i]
  
  # Extract estatus (between "/estatus_victimas/" and ".json")
  estatus_code <- str_extract(file_name, "(?<=/estatus_victimas/)(.*)(?=.json)") %>% 
    as.numeric()
  
  tempo_hom <- fromJSON(file_name)[["anual"]][["Hombre"]] %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>% 
    mutate(estatus_code = estatus_code,
           sexo = "Hombre",
           year = as.numeric(year))
  
  tempo_muj <- fromJSON(file_name)[["anual"]][["Mujer"]] %>%
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>% 
    mutate(estatus_code = estatus_code,
           sexo = "Mujer",
           year = as.numeric(year))
  
  tempo_sexo_na <- fromJSON(file_name)[["anual"]][["Indeterminado"]] %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "year", values_to = "n") %>% 
    mutate(estatus_code = estatus_code,
           sexo = "sexo_na",
           year = as.numeric(year))
  
  tempo <- bind_rows(tempo_hom, tempo_muj, tempo_sexo_na)
  tempo_ls[[i]] <- tempo
}

stopifnot(length(tempo_ls) == length(input_file_names))

# CLEAN ====

status_by_year <- bind_rows(tempo_ls) %>% 
  left_join(estatus_victimas_dict, by = c("estatus_code" = "file_number")) %>% 
  select(-estatus_code) %>%
  filter(status %in% c("PERSONAS DESAPARECIDAS Y NO LOCALIZADAS",
                        "PERSONAS LOCALIZADAS SIN VIDA",
                        "PERSONAS LOCALIZADAS CON VIDA")) %>% 
  mutate(status = case_when(
    status == "PERSONAS DESAPARECIDAS Y NO LOCALIZADAS" ~ "no_local",
    status == "PERSONAS LOCALIZADAS SIN VIDA" ~ "local_sv",
    status == "PERSONAS LOCALIZADAS CON VIDA" ~ "local_cv"))

stopifnot(all(c("no_local", "local_sv", "local_cv") %in% status_by_year$status))

# EXPORT ====
write_csv(status_by_year, paste0(tempdir(), "/status_by_year.csv"))

drive_upload(paste0(tempdir(), "/status_by_year.csv"),
             motha_drive_id,
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/status_by_year.csv"))

# done.
