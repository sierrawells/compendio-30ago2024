#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, svglite, showtext)

# Paths
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/src/tema.R"))

# Tema
source(paths$tema)

# READ ====
drive_auth("sierra.wells@datacivica.org")
drive_in <- drive_ls(as_id("1u80_4_-ySSqtKmSAnblPh5HNHHvi1V6P")) %>% 
  filter(name %in% c("t_dgrales_pdes_loc.rds", "t_pdes_enfoque_diferenciado.rds",
                     "t_pdes_hechos.rds", "t_medios_contacto_prep.rds"))

for (file in drive_in$name) {
  file_path <- paste0(tempdir(), "/", file)
  
  drive_download(file, path = file_path, overwrite = T)
  
  tempo <- readRDS(file_path)
  
  assign(str_remove(file, ".rds"), tempo)
  
  file.remove(file_path)
}

rm(tempo)

# JOIN ====
join_vars <- c("id", "id_reg", "id_per", "autoridad")

rnpdno_joined <- t_dgrales_pdes_loc %>%
  left_join(t_pdes_enfoque_diferenciado, by = join_vars, relationship = "many-to-many") %>%
  left_join(t_pdes_hechos, by = join_vars, relationship = "many-to-many") %>%
  left_join(t_medios_contacto_prep, by = join_vars, relationship = "many-to-many") %>%
  select(-starts_with("info"))

# CLEAN ====
rnpdno_clean <- rnpdno_joined %>% 
  mutate(
    # Replace "SIN DATO" or "SE DESCONOCE" with NA
    across(everything(), ~ ifelse(.x == "SIN DATO" | .x == "SE DESCONOCE", NA_character_, .))) %>% 
  mutate(
    # Are there any enfoque diferenciado vars that are non-NA?
    any_enfoque_diferenciado_reported = ifelse(
      rowSums(!is.na(select(., starts_with("enfoque_diferenciado_")))) > 0, T, NA),
    # Registro = confidencializado si nombre es eliminado
    registro_confidencial = case_when(
      datos_generales_nombre == "ELIMINADO-9" ~ T,
      datos_generales_primer_apellido == "ELIMINADO-10" ~ T,
      T ~ F),
    year_hechos =
      ifelse(
        # If hechos_1_fecha_y_hora_de_hechos is not NA and does not contain "ELIMINADO", then extract year
        !is.na(hechos_1_fecha_y_hora_de_hechos) & !str_detect(hechos_1_fecha_y_hora_de_hechos, "ELIMINADO"),
        as.numeric(substr(hechos_1_fecha_y_hora_de_hechos, 7, 10)), NA_real_),
    autoridad_tipo = case_when(
      startsWith(autoridad, "COMISION") ~ "comision",
      str_detect(autoridad, "^FISCALIA|^PROCURADURIA") ~ "fiscalia",
      T ~ NA_character_),
    autoridad_ent = 
      case_when(
        autoridad == "FISCALIA GENERAL DE LA REPUBLICA" ~ "FGR (federal)",
        autoridad == "COMISION NACIONAL DE BUSQUEDA DE PERSONAS" ~ "CNB (nacional)",
        str_detect(autoridad, "AGUASCALIENTES") ~ "Aguascalientes",
        str_detect(autoridad, "BAJA CALIFORNIA SUR") ~ "Baja California Sur",
        str_detect(autoridad, "BAJA CALIFORNIA") ~ "Baja California",
        str_detect(autoridad, "CAMPECHE") ~ "Campeche",
        str_detect(autoridad, "CHIAPAS") ~ "Chiapas",
        str_detect(autoridad, "CHIHUAHUA") ~ "Chihuahua",
        str_detect(autoridad, "CIUDAD DE MEXICO") ~ "CDMX",
        str_detect(autoridad, "COAHUILA") ~ "Coahuila",
        str_detect(autoridad, "COLIMA") ~ "Colima",
        str_detect(autoridad, "DURANGO") ~ "Durango",
        str_detect(autoridad, "ESTADO DE MEXICO") ~ "Estado de México",
        str_detect(autoridad, "GUANAJUATO") ~ "Guanajuato",
        str_detect(autoridad, "GUERRERO") ~ "Guerrero",
        str_detect(autoridad, "HIDALGO") ~ "Hidalgo",
        str_detect(autoridad, "JALISCO") ~ "Jalisco",
        str_detect(autoridad, "MICHOACAN") ~ "Michoacán",
        str_detect(autoridad, "MORELOS") ~ "Morelos",
        str_detect(autoridad, "NAYARIT") ~ "Nayarit",
        str_detect(autoridad, "NUEVO LEON") ~ "Nuevo León",
        str_detect(autoridad, "OAXACA") ~ "Oaxaca",
        str_detect(autoridad, "PUEBLA") ~ "Puebla",
        str_detect(autoridad, "QUERETARO") ~ "Querétaro",
        str_detect(autoridad, "QUINTANA ROO") ~ "Quintana Roo",
        str_detect(autoridad, "SAN LUIS POTOSI") ~ "San Luis Potosí",
        str_detect(autoridad, "SINALOA") ~ "Sinaloa",
        str_detect(autoridad, "SONORA") ~ "Sonora",
        str_detect(autoridad, "TABASCO") ~ "Tabasco",
        str_detect(autoridad, "TAMAULIPAS") ~ "Tamaulipas",
        str_detect(autoridad, "TLAXCALA") ~ "Tlaxcala",
        str_detect(autoridad, "VERACRUZ") ~ "Veracruz",
        str_detect(autoridad, "YUCATAN") ~ "Yucatán",
        str_detect(autoridad, "ZACATECAS") ~ "Zacatecas",
        T ~ NA_character_)) %>% 
  filter(year_hechos >= 2012) # Solo incluir registros de 2012 en adelante

# Estandarizar nombres de variables
names(rnpdno_clean) <- names(rnpdno_clean) %>% 
  ifelse(str_detect(., "^hechos_|^enfoque_diferenciado_|^medios_de_contacto_"),
         str_replace(., "_1_", "_"), .)

# WHAT % OF VARS OF INTEREST ARE REPORTED? ====
vars_to_analyze <- c(
  "datos_generales_sexo" = "Sexo",
  "datos_generales_estado_civil" = "Estado civil",
  "datos_generales_nacionalidad" = "Nacionalidad",
  "datos_generales_edad_en_anos" = "Edad",
  "datos_generales_escolaridad" = "Escolaridad",
  "datos_generales_habla_espanol" = "¿Habla español?",
  "medios_de_contacto_telefono_o_correo_electronico" = "Medio de contacto",
  "enfoque_diferenciado_presenta_alguna_condicion_de_discapacidad" = "¿Tiene alguna discapacidad?",
  "enfoque_diferenciado_pueblo_o_comunidad_indigena" = "¿Pertenece a alguna comunidad indígena?",
  "enfoque_diferenciado_idioma_o_lengua_indigena" = "¿Habla alguna lengua indígena?",
  "enfoque_diferenciado_religion" = "Religión",
  "enfoque_diferenciado_es_persona_extranjera_en_mexico" = "¿Es extranjere?",
  "enfoque_diferenciado_es_periodista" = "¿Es periodista?",
  "enfoque_diferenciado_es_defensor_de_derechos_humanos" = "¿Es defensor de DD.HH.?",
  "enfoque_diferenciado_es_servidor_publico" = "¿Es servidor público?",
  "enfoque_diferenciado_pertenece_a_algun_sindicato" = "¿Pertenece a algún sindicato?",
  "enfoque_diferenciado_pertenece_a_la_comunidad_lgbttti_lesbico_gays_bisexuales_transgeneros_trasvestis_transexuales_e_intersexuales" = "¿Pertenece a la comunidad LGBT?",
  "enfoque_diferenciado_pertenece_a_alguna_ong_organizacion_no_gubernamental" = "¿Pertenece a alguna ONG?")

perc_reported_vars_df <- data.frame()

for (name in names(vars_to_analyze)) {
  
  tempo <- rnpdno_clean %>% 
    summarize(perc_reported = round(mean(!is.na(!!sym(name))), 3)) %>%
    mutate(var = name) %>% 
    relocate(var)
  
  perc_reported_vars_df <- bind_rows(perc_reported_vars_df, tempo)
}

perc_reported_vars_df <- perc_reported_vars_df %>% 
  mutate(var_desc = vars_to_analyze[var])

perc_reported_vars_lolli <- ggplot(perc_reported_vars_df,
                                   aes(x = perc_reported,
                                       y = reorder(str_wrap(var_desc, 25), perc_reported))) +
  geom_segment(aes(xend = 0, yend = reorder(str_wrap(var_desc, 25), perc_reported)), 
               color = "#272628") +
  geom_point(size = 10, color = pal[2]) +
  geom_text(aes(label = paste0(as.character(round(100 * perc_reported, 0)), "%")),
            size = 3, color = "white", family = font_serif_bold) +
  labs(title = "Porcentaje de registros de personas desaparecidas que cuentan con las siguientes variables relleanadas",
       subtitle = "Para registros de 2012 en adelante", y = "Variable", x = "",
       caption = paste0("Fuente: Elaboración por Data Cívica con la versión pública del RNPDNO en formato JSON, actualizado el 5 de agosto de 2023"), x = "", y = "") +
  tema +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

for (device in devices){
  ggsave(filename = paste0(paths$output, "rnpdno_vars_lolli", device),
         plot = perc_reported_vars_lolli, width = 12, height = 8, dpi = 300)
}

# ÍNDICE DE VARS REPORTED POR AUTORIDAD ====
vars_to_analyze_autoridad <- c(
  "any_enfoque_diferenciado_reported",
  "hechos_fecha_y_hora_de_hechos",
  "datos_generales_edad_en_anos",
  "medios_de_contacto_telefono_o_correo_electronico")

autoridades_w_indices <- rnpdno_clean %>% 
  filter(!is.na(autoridad_tipo) & !is.na(autoridad_ent)) %>% 
  group_by(autoridad_tipo, autoridad_ent) %>% 
  reframe(
    n_registros = length(unique(id)),
    across(.cols = vars_to_analyze_autoridad,
           .fns = ~ mean(!is.na(.x)))) %>% 
  ungroup() %>% 
  mutate(
    # Create índice which is mean of vars_to_analyze_autoridad
    indice = rowMeans(select(., all_of(vars_to_analyze_autoridad))))
    

# FISCALÍAS

indices_fisc <- autoridades_w_indices %>% 
  filter(autoridad_tipo == "fiscalia") %>% 
  mutate(
    autoridad_ent_label = ifelse(
      # If indice is max for that autoridad_tipo, then add [n registros] to label
      indice == max(indice),
      paste0(autoridad_ent, "\n[", n_registros, " registros]"),
      paste0(autoridad_ent, " [", n_registros, "]")),
    autoridad_ent_label = fct_reorder(autoridad_ent_label, indice))

lolli_fisc_ent_indices <- ggplot(indices_fisc,
                            aes(x = indice, y = autoridad_ent_label)) +
  geom_segment(aes(xend = 0, yend = autoridad_ent_label), color = "#272628") + 
  geom_point(size = 10, color = pal[2]) +
  geom_text(aes(label = round(indice, 2)),
            size = 3, color = "white", family = font_serif_bold) +
  labs(title = "¿Qué fiscalías tienen los registros de personas desaparecidas más completos?",
       subtitle = paste0("Índice de completitud de registro es el promedio del % de registros que reportan:",
                         "\nMedio de contacto, edad, fecha y hora de los hechos y cualquier variable de enfoque diferenciado*"),
       caption = paste0("Fuente: Elaboración por Data Cívica con la versión pública del RNPDNO en formato JSON, actualizado el 5 de agosto de 2023",
                        "\n*Las variables de 'enfoque diferenciado' documentan pertenencia a grupos vulnerables (periodistas, personas indígenas,\ndefensores de DD.HH., etc.)"),
       x = "Índice de completitud de registro", y = "(Menos completos --> más completos)") +
  expand_limits(y= c(0, length(unique(indices_fisc$autoridad_ent_label)) + 1)) +
  tema +
  theme(legend.position = "none")

for (device in devices){
  ggsave(filename = paste0(paths$output, "rnpdno_vars_fisc_lolli", device),
         plot = lolli_fisc_ent_indices, width = 12, height = 10)
}

# COMISIONES

indices_comision <- autoridades_w_indices %>% 
  filter(autoridad_tipo == "comision") %>% 
  mutate(
    autoridad_ent_label = ifelse(
      # If indice is max for that autoridad_tipo, then add [n registros] to label
      indice == max(indice),
      paste0(autoridad_ent, "\n[", n_registros, " registros]"),
      paste0(autoridad_ent, " [", n_registros, "]")),
    autoridad_ent_label = fct_reorder(autoridad_ent_label, indice))

lolli_comis_ent_indices <- ggplot(indices_comision,
                            aes(x = indice, y = autoridad_ent_label)) +
  geom_segment(aes(xend = 0, yend = autoridad_ent_label), color = "#272628") + 
  geom_point(size = 10, color = pal[2]) +
  geom_text(aes(label = round(indice, 2)),
            size = 3, color = "white", family = font_serif_bold) +
  labs(title = "¿Qué comisiones de búsqueda tienen los registros de personas desaparecidas más completos?",
       subtitle = paste0("Índice de completitud de registro es el promedio del % de registros que reportan:",
                         "\nMedio de contacto, edad, fecha y hora de los hechos y cualquier variable de enfoque diferenciado*"),
       caption = paste0("Fuente: Elaboración por Data Cívica con la versión pública del RNPDNO en formato JSON, actualizado el 5 de agosto de 2023",
                        "\n*Las variables de 'enfoque diferenciado' documentan pertenencia a grupos vulnerables (periodistas, personas indígenas,\ndefensores de DD.HH., etc.)"),
       x = "Índice de completitud de registro", y = "(Menos completos --> más completos)") +
  expand_limits(y= c(0, length(unique(indices_comision$autoridad_ent_label)) + 1)) +
  tema +
  theme(legend.position = "none")

for (device in devices){
  ggsave(filename = paste0(paths$output, "rnpdno_vars_comis_lolli", device),
         plot = lolli_comis_ent_indices, width = 12, height = 10)
}

# MEDIO DE CONTACTO REPORTED POR AUTORIDAD ====

# FISCALÍAS

medios_contacto_fisc <- autoridades_w_indices %>% 
  filter(autoridad_tipo == "fiscalia") %>% 
  mutate(
    autoridad_ent_label = ifelse(
      # If indice is max for that autoridad_tipo, then add [n registros] to label
      medios_de_contacto_telefono_o_correo_electronico == max(medios_de_contacto_telefono_o_correo_electronico),
      paste0(autoridad_ent, "\n[", n_registros, " registros]"),
      paste0(autoridad_ent, " [", n_registros, "]")),
    autoridad_ent_label = fct_reorder(autoridad_ent_label, medios_de_contacto_telefono_o_correo_electronico))

lolli_fisc_ent_medios_contacto <- ggplot(medios_contacto_fisc,
                                 aes(x = medios_de_contacto_telefono_o_correo_electronico,
                                     y = autoridad_ent_label)) +
  geom_segment(aes(xend = 0, yend = autoridad_ent_label), color = "#272628") + 
  geom_point(size = 10, color = pal[2]) +
  geom_text(aes(label = scales::percent(medios_de_contacto_telefono_o_correo_electronico, accuracy = 0.1)),
            size = 3, color = "white", family = font_serif_bold) +
  labs(title = "¿Qué porcentaje de registros documentan un medio de contacto para la persona que reporta?",
       subtitle = "Por fiscalía",
       caption = paste0("Fuente: Elaboración por Data Cívica con la versión pública del RNPDNO en formato JSON, actualizado el 5 de agosto de 2023"),
       x = "", y = "") +
  expand_limits(y= c(0, length(unique(medios_contacto_fisc$autoridad_ent_label)) + 1)) +
  tema +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

for (device in devices){
  ggsave(filename = paste0(paths$output, "lolli_fisc_ent_medios_contacto", device),
         plot = lolli_fisc_ent_medios_contacto, width = 12, height = 10)
}

# COMISIONES

medios_contacto_comis <- autoridades_w_indices %>% 
  filter(autoridad_tipo == "comision") %>% 
  mutate(
    autoridad_ent_label = ifelse(
      # If indice is max for that autoridad_tipo, then add [n registros] to label
      medios_de_contacto_telefono_o_correo_electronico == max(medios_de_contacto_telefono_o_correo_electronico),
      paste0(autoridad_ent, "\n[", n_registros, " registros]"),
      paste0(autoridad_ent, " [", n_registros, "]")),
    autoridad_ent_label = fct_reorder(autoridad_ent_label, medios_de_contacto_telefono_o_correo_electronico))

lolli_comis_ent_medios_contacto <- ggplot(medios_contacto_comis,
                                 aes(x = medios_de_contacto_telefono_o_correo_electronico,
                                     y = autoridad_ent_label)) +
  geom_segment(aes(xend = 0, yend = autoridad_ent_label), color = "#272628") +
  geom_point(size = 10, color = pal[2]) +
  geom_text(aes(label = scales::percent(medios_de_contacto_telefono_o_correo_electronico, accuracy = 0.1)),
            size = 3, color = "white", family = font_serif_bold
  ) +
  labs(title = "¿Qué porcentaje de registros documentan un medio de contacto para la persona que reporta?",
       subtitle = "Por comisión",
       caption = paste0("Fuente: Elaboración por Data Cívica con la versión pública del RNPDNO en formato JSON, actualizado el 5 de agosto de 2023"),
       x = "", y = "") +
  expand_limits(y= c(0, length(unique(medios_contacto_comis$autoridad_ent_label)) + 1)) +
  tema +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

for (device in devices){
  ggsave(filename = paste0(paths$output, "lolli_comis_ent_medios_contacto", device),
         plot = lolli_comis_ent_medios_contacto, width = 12, height = 10)
}

# done.
