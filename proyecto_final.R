install.packages(c(
  "WDI",          # Acceso directo a la API del World Bank
  "tidyverse",    # Manipulación y visualización de datos
  "ggplot2",      # Gráficos
  "countrycode",  # Estandarizar nombres/códigos de países
  "corrplot",     # Matrices de correlación
  "ggrepel",      # Etiquetas en gráficos sin solapamiento
  "patchwork",    # Combinar múltiples gráficos
  "scales",       # Formateo de ejes
  "gganimate",    # Animaciones temporales (evolución histórica)
  "wbstats"       # Alternativa/complemento a WDI, con búsqueda más cómoda
))

library(WDI)
library(tidyverse)
library(countrycode)

# ────────────────── 1. Definir indicadores ────────────────────────────────────

# Hacemos un vector con los códigos de cada variable a descargar

indicadores <- c(
  gini          = "SI.POV.GINI",
  mort_infantil = "SP.DYN.IMRT.IN",
  esp_vida      = "SP.DYN.LE00.IN",
  mort_enf_notrans  = "SH.DYN.NCOM.ZS",
  obesidad      = "SH.STA.OWAD.ZS",
  suicidio      = "SH.STA.SUIC.P5"
)

# Debido a un error que no sabemos solucionar, la variable del PIB la descargamos a parte

gdp <- WDI(
  indicator = c(pib_percap = "NY.GDP.PCAP.PP.KD"),
  start = 1980,
  end = 2023,
  extra = FALSE
)

# ──────────────── 2. Descargar datos (1980–2023) ──────────────────────────────

datos <- WDI(
  indicator = indicadores,
  start = 1980,
  end = 2023,
  extra = TRUE   # Esta opción añade la región, el nivel de renta y un par de cosas más
)

# ───────────────────────── 3. Limpiar ─────────────────────────────────────────
library(dplyr)

datos_limpios <- datos %>% 
  filter(region != "Aggregates")   # quitamos los agregados regionales

datos_limpios <- datos_limpios %>%
  left_join(gdp, by = c("country", "iso2c","iso3c", "year"))

datos_simple <- datos_limpios %>% 
  select(country, iso3c, year, gini, mort_infantil, esp_vida, mort_enf_notrans, 
         obesidad, suicidio, income, pib_percap) 

# Nos quedamos solo con las variables que nos interesan

# ───────── 4. Snapshot reciente para clasificar países ────────────────────────
# Vamos a hacer un filter() de los últimos 10 años para después sacar la
# media del GINI de cada país en estos últimos 10 años, y utilizar esa media
# como guía para hablar de niveles de desigualdad en cada país.

# Haremos lo mismo con el PIB, para poder comparar países con una riqueza similar.
gini_reciente <- datos_limpios %>%
  filter(year >= 2013, year <= 2023) %>% 
  group_by(iso3c, country) %>%
  summarise(
    gini_medio     = mean(gini, na.rm = TRUE),
    gdp_medio      = mean(pib_percap, na.rm = TRUE),
    income_level   = first(income),
    .groups = "drop"
  ) %>%
  filter(!is.na(gini_medio), !is.na(gdp_medio))

# Filtramos para retirar los valores perdidos de nuestros datos, y poder comparar.


#───────────────────5. Gráfico de dispersión GINI x PIB─────────────────────────

library(ggplot2)
library(ggrepel)
  
ggplot(gini_reciente, aes(x = gdp_medio, y = gini_medio, color = income_level)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35") +
  geom_text_repel(aes(label = country), size = 3) +
  labs(
    title = "Relación entre PIB per cápita y desigualdad",
    subtitle = "Media 2015–2022 · World Bank",
    x = "PIB per cápita (PPP, dólares constantes)",
    y = "Índice de Gini",
    color = "Nivel de renta"
  ) +
  theme_minimal()

# Este gráfico es nuestra justificación para no comparar a lo loco

# ──────────────── 6. Filtrar países con rentas similares ──────────────────────

# Para este trabajo, nos basamos en los estudios realizados por Wilkinson, que 
# dice que en países con el mismo nivel de riqueza, aquellos que tienen un 
# reparto más desigual de la riqueza presentan más problemas de salud.


paises_renta_alta <- gini_reciente %>%
  filter(income_level == "High income") %>%
  arrange(gini_medio)

paises_renta_baja <- gini_reciente %>%
  filter(income_level == "Low income") %>%
  arrange(gini_medio)

# ──────────────────── 7. Seleccionar grupos extremos ──────────────────────────

n <- 8

Hrent_equal    <- paises_renta_alta %>% slice_head(n = n) %>% mutate(grupo = "Países de renta alta con más igualdad")
Hrent_unequal <- paises_renta_alta %>% slice_tail(n = n) %>% mutate(grupo = "Países de renta alta con menos igualdad")

Lrent_equal    <- paises_renta_baja %>% slice_head(n = n) %>% mutate(grupo = "Países de renta baja con más igualdad")
Lrent_unequal <- paises_renta_baja %>% slice_tail(n = n) %>% mutate(grupo = "Países de renta baja con menos igualdad")

grupos_HI <- bind_rows(Hrent_equal , Hrent_unequal)
HI_select <- grupos_HI$iso3c

grupos_LI <- bind_rows(Lrent_equal , Lrent_unequal)
LI_select <- grupos_LI$iso3c

# ─────────── 8. Filtrar serie temporal solo para esos países ──────────────────

datos_grupos_HI <- datos_simple %>%
  filter(iso3c %in% HI_select) %>%
  left_join(grupos_HI %>% select(iso3c, grupo), by = "iso3c")

datos_grupos_LI <- datos_simple %>%
  filter(iso3c %in% LI_select) %>%
  left_join(grupos_LI %>% select(iso3c, grupo), by = "iso3c")

# ─────────── 9. Evolución temporal: esperanza de vida por grupo ───────────────

datos_grupos_HI %>%
  filter(!is.na(esp_vida)) %>%
  group_by(year, grupo) %>%
  summarise(esp_vida_media = mean(esp_vida, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = esp_vida_media, color = grupo)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Esperanza de vida: países más iguales vs. más desiguales",
    subtitle = "Países de renta alta · World Bank",
    x = NULL, y = "Años de esperanza de vida", color = NULL
  ) +
  theme_minimal()

datos_grupos_LI %>%
  filter(!is.na(esp_vida)) %>%
  group_by(year, grupo) %>%
  summarise(esp_vida_media = mean(esp_vida, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = esp_vida_media, color = grupo)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Esperanza de vida: países más iguales vs. más desiguales",
    subtitle = "Países de renta baja · World Bank",
    x = NULL, y = "Años de esperanza de vida", color = NULL
  ) +
  theme_minimal()


# Histograma del Gini por nivel de renta
ggplot(gini_reciente, aes(x = gini_medio, fill = income_level)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ income_level) +
  labs(title = "Distribución del Gini por nivel de renta") +
  theme_minimal()
