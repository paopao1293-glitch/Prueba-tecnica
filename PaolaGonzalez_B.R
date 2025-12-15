## Prueba técnica – Vacante: Analista de Datos Senior (Data Cívica)
#
# Elaborado por: Paola González
# Última modificación: 12/dic/2025
#
# Prueba - Parte B: Estadísticas de Defunciones Registradas (INEGI, 2024)
# Enfoque: presuntos homicidios (LISTA_MEX == 55)
#
# Objetivo:
# Realizar un análisis exploratorio de los presuntos homicidios registrados en 2024
# e identificar tres hallazgos descriptivos para divulgación (blog).
#
# Contenido:
# 1. Cargar librerías necesarias
# 2. Cargar microdatos (archivos .dbf descargados previamente)
# 3. Limpieza de datos: enfoque en presuntos homicidios
# 4. Análisis exploratorio y creación de variables indicadoras
# 5. Gráfica 1. Total de homicidios por grupo de edad y sexo
# 6. Gráfica 2. Composición indígena/afrodescendiente por sexo (urbano/rural)
# 7. Gráfica 3. Perfil sociodemográfico y de contexto de mujeres víctimas


# 1. Cargar librerías necesarias
library(foreign)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


# 2. Cargar microdatos de defunciones INEGI 2024 (EDR) desde archivos .dbf locales
ruta <- "C:/Users/Paola González/Documents/Aplicaciones/Data Civica/defunciones_base_datos_2024_dbf/DEFUN24.dbf"

# Lectura de verificación (opcional). La lectura definitiva se realiza abajo con file.path().
datos <- read.dbf(ruta, as.is = TRUE)
str(datos)

carpeta <- "C:/Users/Paola González/Documents/Aplicaciones/Data Civica/defunciones_base_datos_2024_dbf"
datos       <- read.dbf(file.path(carpeta, "DEFUN24.dbf"), as.is = TRUE)

# Nota: Los siguientes catálogos/tablas se cargan para posible enriquecimiento,
# pero no se usan en este análisis exploratorio (pueden omitirse si se busca simplificar).
cod_adicio  <- read.dbf(file.path(carpeta, "COD_ADICIO.dbf"), as.is = TRUE)
parentesco  <- read.dbf(file.path(carpeta, "PARENTESCO.dbf"), as.is = TRUE)
listamex    <- read.dbf(file.path(carpeta, "LISTAMEX.dbf"), as.is = TRUE)
ent         <- read.dbf(file.path(carpeta, "CATEMLDE24.dbf"), as.is = TRUE)


# 3. Limpieza de datos: enfoque en presuntos homicidios
# Filtrar presuntos homicidios (LISTA_MEX == 55).
datos <- subset(datos, LISTA_MEX == 55)

# Crear data frame con variables de interés para análisis regional, interseccional y de perfil.
names(datos)
datos <- as.data.frame(datos)[, c(
  "LISTA_MEX", "ENT_REGIS", "CONINDIG", "OCUPACION", "PAR_AGRE", "EMBARAZO",
  "LENGUA", "ESCOLARIDA", "VIO_FAMI", "REL_EMBA", "EDO_CIVIL",
  "MATERNAS", "SEXO", "NACIONALID", "TIPO_DEFUN", "EDAD_AGRU",
  "NACESP_CVE", "AFROMEX", "EDAD", "AREA_UR"
)]


# 4. Análisis exploratorio y creación de variables indicadoras
# Nota: Estas tablas son chequeos rápidos de distribución.
class(datos$SEXO)
class(datos$EDAD_AGRU)
table(datos$ENT_REGIS)
table(datos$PAR_AGRE)
table(datos$LISTA_MEX)
table(datos$NACIONALID)
table(datos$TIPO_DEFUN)

# Crear variables indicadoras para facilitar el análisis
# Notas de definición:
# - indigena: proxy por hablante de lengua indígena (LENGUA == 1).
# - esc_basica: definida por codificación del Descriptor de archivos.
# - en_pareja: definida por codificación del Descriptor de archivos.
# - grupo_edad: basado en EDAD_AGRU (no en EDAD en años); revisar codificación del Descriptor de archivos.
datos <- datos %>%
  mutate(
    EDAD_AGRU_num = suppressWarnings(as.integer(EDAD_AGRU)),
    
    indigena      = if_else(LENGUA == 1, 1L, 0L),
    embarazada    = if_else(EMBARAZO == 1, 1L, 0L),
    esc_basica    = if_else(between(ESCOLARIDA, 1, 8), 1L, 0L),
    violencia_fam = if_else(VIO_FAMI == 1, 1L, 0L),
    en_pareja     = if_else(EDO_CIVIL %in% c(4, 5), 1L, 0L),
    mujer         = if_else(SEXO == 2, 1L, 0L),
    extranje      = if_else(NACIONALID == 2, 1L, 0L),
    afro_des      = if_else(AFROMEX == 1, 1L, 0L),
    urbano        = if_else(AREA_UR == 1, 1L, 0L),
    
    grupo_edad = case_when(
      EDAD_AGRU_num %in% c(6L, 7L, 8L)              ~ "adolescentes_10_19",
      EDAD_AGRU_num <= 6L                           ~ "infancias_0-9",
      EDAD_AGRU_num %in% c(9L, 10L, 11L)            ~ "jovenes_20_34",
      EDAD_AGRU_num %in% c(12L, 13L, 14L, 15L, 16L) ~ "adultos_36_59",
      between(EDAD_AGRU_num, 17L, 29L)              ~ "adultos_mas_60",
      EDAD_AGRU_num == 30L                          ~ NA_character_,
      TRUE                                          ~ NA_character_
    )
  )

table(datos$grupo_edad)

# Creación de grupos excluyentes para identificación de etnicidad y afrodescendencia
datos <- datos %>%
  mutate(
    solo_indigena = if_else(indigena == 1L & afro_des == 0L, 1L, 0L),
    solo_afro     = if_else(indigena == 0L & afro_des == 1L, 1L, 0L),
    indigena_afro = if_else(indigena == 1L & afro_des == 1L, 1L, 0L)
  )

# Nota: Esta tabla incluye también "Ninguna" (población no indígena y no afrodescendiente).
# En la gráfica 2 se analiza únicamente población indígena y/o afrodescendiente (se excluye "Ninguna").
tabla_cruce <- datos %>%
  mutate(
    categoria = case_when(
      solo_indigena == 1L ~ "Solo indígena",
      solo_afro     == 1L ~ "Solo afrodescendiente",
      indigena_afro == 1L ~ "Indígena y afro",
      TRUE                ~ "Ninguna"
    )
  ) %>%
  count(categoria)

tabla_cruce


# 5. Gráfica 1. Total de homicidios por grupo de edad y sexo
# Propósito: comparar magnitudes absolutas para identificar grupos con mayor concentración de casos.
grafica_edad_sexo <- datos %>%
  mutate(
    sexo = if_else(mujer == 1L, "Mujeres", "Hombres"),
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "infancias_0-9",
        "adolescentes_10_19",
        "jovenes_20_34",
        "adultos_36_59",
        "adultos_mas_60"
      )
    )
  ) %>%
  filter(!is.na(grupo_edad)) %>%
  count(grupo_edad, sexo)

ggplot(grafica_edad_sexo, aes(x = grupo_edad, y = n, fill = sexo)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(
    values = c(
      "Mujeres" = "#984ea3",
      "Hombres" = "#4daf4a"
    )
  ) +
  labs(
    x = "Grupo de edad",
    y = "Total de homicidios",
    fill = "Sexo",
    title = "Totales de homicidios por grupo de edad y sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


# 6. Gráfica 2. Composición indígena y afrodescendiente por sexo (urbano/rural)
# Nota: Las proporciones se calculan dentro de la población indígena y/o afrodescendiente.
# No representa el porcentaje sobre el total de homicidios.
grafica_base <- datos %>%
  mutate(
    area = if_else(urbano == 1L, "Urbano", "Rural"),
    sexo = if_else(mujer == 1L, "Mujeres", "Hombres"),
    identidad = case_when(
      solo_indigena == 1L ~ "Solo indígena",
      solo_afro     == 1L ~ "Solo afrodescendiente",
      indigena_afro == 1L ~ "Indígena y afro",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(identidad)) %>%
  count(area, sexo, identidad) %>%
  group_by(area, sexo) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(grafica_base, aes(x = sexo, y = pct, fill = identidad)) +
  geom_col(width = 0.7, color = "white") +
  facet_wrap(~ area) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "Solo indígena" = "#1b9e77",
      "Solo afrodescendiente" = "#7570b3",
      "Indígena y afro" = "#d95f02"
    )
  ) +
  labs(
    x = "Sexo",
    y = "Porcentaje dentro de población indígena y/o afrodescendiente",
    fill = "Identidad",
    title = "Composición indígena y afrodescendiente por sexo",
    subtitle = "Barras apiladas 100% por condición urbana/rural"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )


# 7. Gráfica 3. Perfil sociodemográfico y de contexto de mujeres víctimas de homicidio
# Nota: Se muestran proporciones descriptivas.
grafica_perfil_mujeres <- datos %>%
  filter(mujer == 1L) %>%
  summarise(
    Embarazada            = mean(embarazada == 1L, na.rm = TRUE),
    `Escolaridad básica`  = mean(esc_basica == 1L, na.rm = TRUE),
    `Violencia familiar`  = mean(violencia_fam == 1L, na.rm = TRUE),
    `En pareja`           = mean(en_pareja == 1L, na.rm = TRUE),
    Extranjera            = mean(extranje == 1L, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "caracteristica",
    values_to = "proporcion"
  )

ggplot(grafica_perfil_mujeres,
       aes(x = proporcion, y = reorder(caracteristica, proporcion))) +
  geom_col(fill = "#984ea3", width = 0.7) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Porcentaje de mujeres víctimas",
    y = "",
    title = "Perfil sociodemográfico y de contexto de las mujeres víctimas de homicidio",
    subtitle = "Porcentaje de mujeres que presentan cada característica"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank()
  )
