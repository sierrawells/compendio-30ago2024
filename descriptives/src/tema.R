#
# Autora: SW
# Mantenedoras: SW, AF, PB, IS
# Licencia:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# PREP
if(!require(pacman)) install.packages("pacman")
pacman::p_load(svglite, showtext)

devices <- c(".png", ".svg")

showtext_auto(enable = TRUE, record = TRUE)
showtext_opts(dpi = 300)

font_dir <- file.path(Sys.getenv("HOME"), "/Downloads/")
font_add("Roboto Mono", 
         regular = paste0(font_dir, "Roboto_Mono/static/RobotoMono-Regular.ttf"))
font_add("Roboto Mono Bold", 
         regular = paste0(font_dir, "Roboto_Mono/static/RobotoMono-Bold.ttf"))
font_add("Roboto Mono Italic", 
         regular = paste0(font_dir, "Roboto_Mono/static/RobotoMono-Italic.ttf"))
font_add("Roboto Serif",
         regular = paste0(font_dir, "Roboto_Serif/static/RobotoSerif_28pt-Regular.ttf"))
font_add("Roboto Serif Bold",
         regular = paste0(font_dir, "Roboto_Serif/static/RobotoSerif_28pt-Bold.ttf"))

stopifnot(all(c("Roboto Mono", "Roboto Mono Bold", "Roboto Serif", "Roboto Serif Bold", "Roboto Mono Italic")
              %in% font_families()))

# PALETA DE COLORES
pal <- c("#9dbfd5", "#444843", "#f4b93f",	"#ed7174", "#d7addb",
         "#a5daa6",	"#989cb9",	"#aca698",	"#589383")

# DEGRADADOS
grad <- c("#ed7172", "#fef6f7")

# COLORES CONTSTANTES
ejes <- "#c1c1c1"

gray_dark <- "#212221"
gray_medium <- "#444843"
gray_light <- "#616363"

fondo <- "#fcfef3"

# FONTS 
font_serif <- "Roboto Serif"
font_mono <- "Roboto Mono"
font_mono_bold <- "Roboto Mono Bold"
font_serif_bold <- "Roboto Serif Bold"
font_mono_italic <- "Roboto Mono Italic"

# TEMA PARA GRÁFICAS ====
tema <- theme(
  plot.title = element_text(family = font_serif_bold,
                            color = gray_dark,
                            hjust = 0.5),
  plot.subtitle = element_text(family = font_mono,
                               color = gray_medium,
                               hjust = 0.5),
  axis.text.x = element_text(family = font_mono_bold,
                             color = gray_dark),
  axis.text.y = element_text(family = font_mono,
                             color = gray_dark),
  plot.caption = element_text(family = font_mono_italic,
                              color = gray_light,
                              hjust = 0),
  axis.title = element_text(family = font_mono_italic,
                            color = gray_dark),
  legend.title = element_text(family = font_mono_bold,
                              color = gray_dark),
  legend.text = element_text(family = font_mono,
                             color = gray_dark),
  legend.background = element_rect(fill = fondo),
  strip.text = element_text(
    family = font_mono_bold,
    color = gray_dark),
  axis.line = element_line(color = ejes),
  axis.ticks = element_line(color = ejes),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = fondo),
  plot.background = element_rect(fill = fondo))

# done.