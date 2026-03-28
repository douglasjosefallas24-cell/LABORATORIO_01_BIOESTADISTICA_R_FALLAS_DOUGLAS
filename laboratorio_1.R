#========================================================
# Laboratorio 01
# Douglas Fallas Mora
# version 1 3/12/2026
#========================================================

#Cargar paquetes ----

library(tidyverse) 
library(janitor)
library(lubridate)
library(readxl)

#importar archivos ----
#medición 1
datos_1_original <- read.delim("~/Bioesradistca en R/R_PATO/bosque_seco_med1.txt",
                               header = TRUE,
                               sep = "\t",
                               fileEncoding = "UTF-16")
glimpse(datos_1_original)
names(datos_1_original)

#medición 2
datos_2_original <- read_excel("~/Bioesradistca en R/R_PATO/bosque_seco_med2.xlsx")
glimpse(datos_1_original)
names(datos_1_original)

#Limpiar nombres de columnas ----

datos_1 <-  datos_1_original %>%
  clean_names()

names(datos_1)

datos_2 <-  datos_2_original %>%
  clean_names()

names(datos_2)

#Estandarizar nombres de columnas ----

datos_1 <-  datos_1 %>%
  rename(
    parcela = parcela,
    arbol = arbol,
    especie_1 = especie,
    diametro_cm_1 = diametro_cm,
    altura_m_1 = altura_m
  )

datos_2 <-  datos_2 %>%
  rename(
    parcela_2 = parcela,
    arbol_2 = arbol,
    especie_2 = species,
    diametro_cm_2 = dbh_cm,
    altura_m_2 = altura_m
  )


#Limpiar espacios en variables de texto ----

datos_1 <-  datos_1 %>%
  mutate(across(where(is.character), str_trim))

count(datos_1, parcela, sort = TRUE)
count(datos_1, arbol, sort = TRUE)
count(datos_1, especie_1, sort = TRUE)

datos_2 <-  datos_2 %>%
  mutate(across(where(is.character), str_trim))

count(datos_2, parcela_2, sort = TRUE)
count(datos_2, arbol_2, sort = TRUE)
count(datos_2, especie_2, sort = TRUE)
      
#Recodificar categorias inconsistentes ----

datos_1 <-  datos_1 %>%
  mutate(
      especie_1 = case_when(
      especie_1 %in% c("bursera simaruba") ~ "Bursera simaruba",
      especie_1 %in% c("guazuma ulmifolia") ~ "Guazuma ulmifolia",
      especie_1 %in% c("cordia alliodora" , "cordia.alliodora", "Cordia.alliodora")~ "Cordia alliodora",
      TRUE ~ especie_1
    )
)

datos_2 <-  datos_2 %>%
  mutate(
    especie_2 = case_when(
      especie_2 %in% c("bursera simaruba", "B. simaruba", "Bursera simaruba") ~ "Bursera simaruba",
      especie_2 %in% c("guazuma ulmifolia", "G. ulmifolia","Guazuma ulmifolia" ) ~ "Guazuma ulmifolia",
      especie_2 %in% c("Cordia alliodora" , "cordia alliodora" , "C. alliodora")~ "Cordia alliodora",
      TRUE ~ especie_2
    )
  )
datos_2 <-  datos_2 %>%
  mutate(
      parcela_2 = case_when(
      parcela_2 %in% c("P1", "parcela_1") ~ "P1",
      TRUE ~ parcela_2
    )
  )

#Crear columna id_arbol----

datos_1 <- datos_1 %>%
  mutate(arbol = str_pad(arbol, width = 2, pad = "0"))

datos_1 <- datos_1 %>%
  mutate(id_arbol = paste(parcela, arbol, sep = "-"))

datos_2 <- datos_2 %>%
  mutate(id_arbol = paste(arbol_2))

#Juntar ambas bases de datos en una sola----

datos_juntos <- merge(datos_1, datos_2, by = "id_arbol")

#Revición general de la base junta----

head(datos_juntos)
summary(datos_juntos)
dim(datos_juntos)


#Revisar que la base de datos unida esté correcta----

datos_juntos %>%
  filter(especie_1 != especie_2)

#Estandarizar las especies de la med 2 con la med 1----

datos_juntos <-  datos_juntos %>%
  mutate(especie_2 = especie_1
  )

#Borrar columnas repertidas y que no aportan----

datos_depurada <-  datos_juntos %>%
  select (-parcela_2,-arbol_2, -especie_2)

#Trabajar campos vacios ----

datos_depurada <- datos_depurada %>%
  group_by(parcela) %>%
  mutate(altura_m_1 = if_else(
    is.na(altura_m_1),
    round(mean(altura_m_1, na.rm = TRUE), 1),
    altura_m_1
  )) %>%
  ungroup()

datos_depurada <- datos_depurada %>%
  group_by(parcela) %>%
  mutate(altura_m_2 = if_else(
    is.na(altura_m_2),
    round(mean(altura_m_2, na.rm = TRUE), 1),
    altura_m_2
  )) %>%
  ungroup()

#Trabajar campos en 0 como mortalidad----

datos_depurada <-  datos_depurada %>%
  mutate(
    mortalidad = case_when(
      diametro_cm_2 <=0 ~ 0,
      diametro_cm_2 > 0 ~ 1,
      TRUE ~ NA_real_
  )
)

datos_depurada <-  datos_depurada %>%
  mutate(
    mortalidad= case_when(
    mortalidad %in% c("1") ~ "Vivo",
    mortalidad %in% c("0") ~ "Muerto"
    )
)

count(datos_depurada, mortalidad)
#Exportar base limpia ----
write_csv(datos_depurada, "~/Bioesradistca en R/R_PATO/Base_datos_lab_01.csv")
#Generación de resultados----

#cuadro resumen
cuadro_resumen <- datos_depurada %>%
  group_by(especie_1) %>%
  summarise(
    n = n(),
    
    diametro_prom_cm_med_1 = round(mean(diametro_cm_1, na.rm = TRUE), 1),
    diametro_se_med_1 = round(sd(diametro_cm_1, na.rm = TRUE) / sqrt(n), 2),
    
    diametro_prom_cm_med_2 = round(mean(diametro_cm_2, na.rm = TRUE), 1),
    diametro_se_med_2 = round(sd(diametro_cm_2, na.rm = TRUE) / sqrt(n), 2),
    
    altura_prom_cm_med_1 = round(mean(altura_m_1, na.rm = TRUE), 2),
    altura_se_med_1 = round(sd(altura_m_1, na.rm = TRUE) / sqrt(n), 2),
    
    altura_prom_cm_med_2 = round(mean(altura_m_2, na.rm = TRUE), 2),
    altura_se_med_2 = round(sd(altura_m_2, na.rm = TRUE) / sqrt(n), 2)
    
  ) %>%
  ungroup()

write_csv(cuadro_resumen, "~/Bioesradistca en R/R_PATO/Cuadro resumen.csv")

#Grafico de incremento promedio por año para el diametro

incremento <- datos_depurada %>%
  mutate(incremento = diametro_cm_2 - diametro_cm_1) %>%
  group_by(especie_1) %>%
  summarise(
    incremento_prom = mean(incremento, na.rm = TRUE),
    incremento_se = sd(incremento, na.rm = TRUE) / sqrt(sum(!is.na(incremento)))
  ) %>%
  ungroup()

ggplot(incremento, aes(x = especie_1, y = incremento_prom)) +
  geom_col(fill = "#20B2AA") +
  geom_errorbar(aes(ymin = incremento_prom - incremento_se,
                    ymax = incremento_prom + incremento_se),
                width = 0.2) +
  labs(
    x = "Especie",
    y = "Incremento promedio (cm)"
  ) +
  theme_minimal()

#Gráfico de cantidad de árboles vivos por ha, por especie

area_ha <-  0.05

arboles_vivos <- datos_depurada %>%
  group_by(especie_1) %>%
  summarise(
    vivos_ano_1 = n(),  # todos estaban vivos inicialmente
    vivos_ano_2 = sum(mortalidad == "Vivo", na.rm = TRUE)
  ) %>%
  mutate(
    vivos_ha_ano_1 = vivos_ano_1 / area_ha,
    vivos_ha_ano_2 = vivos_ano_2 / area_ha
  )

arboles_vivos <- arboles_vivos %>%
  pivot_longer(
    cols = c(vivos_ha_ano_1, vivos_ha_ano_2),
    names_to = "ano",
    values_to = "arboles_ha"
  ) %>%
  mutate(
    ano = recode(ano,
                  "vivos_ha_ano_1" = "Año 1",
                  "vivos_ha_ano_2" = "Año 2")
  )

ggplot(arboles_vivos, aes(x = especie_1, y = arboles_ha, fill = ano)) +
  geom_col(position = "dodge") +
  labs(
    x = "Especie",
    y = "Árboles vivos por hectárea",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Gráfico de área basal por ha

basal_area <- function(diametro_cm) {
  diametro_m <- diametro_cm / 100
  area_basal <- pi * (diametro_m^2) / 4
  return(area_basal)
}

area_ha <- 0.1

area_basal_especie <- datos_depurada %>%
  mutate(
    area_basal_1 = basal_area(diametro_cm_1),
    area_basal_2 = if_else(mortalidad == "Vivo" & diametro_cm_2 > 0,
                           basal_area(diametro_cm_2),
                           0)
  ) %>%
  group_by(especie_1) %>%
  summarise(
    area_basal_total_ano_1 = sum(area_basal_1, na.rm = TRUE),
    area_basal_total_ano_2 = sum(area_basal_2, na.rm = TRUE)
  ) %>%
  mutate(
    area_basal_ha_ano_1 = area_basal_total_ano_1 / area_ha,
    area_basal_ha_ano_2 = area_basal_total_ano_2 / area_ha
  ) %>%
  select(especie_1, area_basal_ha_ano_1, area_basal_ha_ano_2) %>%
  pivot_longer(
    cols = c(area_basal_ha_ano_1, area_basal_ha_ano_2),
    names_to = "ano",
    values_to = "area_basal_ha"
  ) %>%
  mutate(
    ano = recode(ano,
                 "area_basal_ha_ano_1" = "Año 1",
                 "area_basal_ha_ano_2" = "Año 2")
  )

ggplot(area_basal_especie, aes(x = especie_1, y = area_basal_ha, fill = ano)) +
  geom_col(position = "dodge") +
  labs(
    x = "Especie",
    y = expression("Área basal (" * m^2 * "/ha)"),
    fill = "Año"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#FIN