# =============================================================================
# TALLER: Manejo de Datos con dplyr
# CIENFI - Workshop
# Autor: Cienfi
# =============================================================================
# VERSIÓN ESTUDIAMTE
# =============================================================================

### setup
cat("\f")
rm(list = ls())

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------

# install.packages("tidyverse")  # correr solo una vez en la Consola
# install.packages("janitor")    # correr solo una vez en la Consola

library(tidyverse)
library(janitor)


# -----------------------------------------------------------------------------
# 1. CARGA DE DATOS
# -----------------------------------------------------------------------------

bd <- read_csv("input/bd.csv")

str(bd)   # estructura original: mayúsculas, tildes, espacios


# -----------------------------------------------------------------------------
# 2. LIMPIEZA INICIAL
# -----------------------------------------------------------------------------

# --- 2.1 Nombres de columnas: limpiar con janitor ---

# Primero quitar tildes y ñ de los nombres
names(bd) <- iconv(names(bd), from = "UTF-8", to = "ASCII//TRANSLIT")

# Limpieza tradicional con janitor
bd <- clean_names(bd)
names(bd)

# --- 2.2 Renombrar columnas que quedaron poco claras ---
bd <- rename(bd,
             anio             = ano,                          # AÑO        → ano → anio
             tipo_persona     = tipo_de_persona,              # TIPO DE PERSONA
             tamano_empresa   = tamano_de_empresa,            # TAMAÑO DE EMPRESA
             monto            = monto_otorgado,               # MONTO OTORGADO ($)
             genero           = genero_del_solicitante,       # GENERO DEL SOLICITANTE
             situacion        = situacion_del_solicitante,    # SITUACIÓN DEL SOLICITANTE
             descendencia     = descendencia_del_solicitante) # DESCENDENCIA DEL SOLICITANTE

names(bd)

# --- 2.3 Quitar tildes y convertir a minúsculas en los valores de texto ---
bd <- mutate(bd,
             across(where(is.character),
                    ~ tolower(iconv(.x, from = "UTF-8", to = "ASCII//TRANSLIT"))))

# --- 2.4 Crear monto en millones (dividir entre 1.000.000) ---
bd <- mutate(bd, monto_mill = monto / 1000000)

# Verificación final
head(bd)
str(bd)


# =============================================================================
# 3. TRANSFORMACIONES BÁSICAS — UN VERBO A LA VEZ
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1 select(): seleccionar columnas
# -----------------------------------------------------------------------------

# Nos quedamos con las columnas más relevantes para el análisis
bd_sel <- select(bd, anio, mes, sector_economico, municipio,
                 tamano_empresa, genero, monto, monto_mill, plazo)

head(bd_sel)

# También podemos excluir columnas con -
bd_sin_linea <- select(bd, -linea_fonder, -descendencia)
names(bd_sin_linea)


# -----------------------------------------------------------------------------
# 3.2 rename(): renombrar columnas
# -----------------------------------------------------------------------------

# Supongamos que queremos un nombre más corto para plazo
bd_ren <- rename(bd_sel, plazo_meses = plazo)
names(bd_ren)


# -----------------------------------------------------------------------------
# 3.3 filter(): filtrar filas
# -----------------------------------------------------------------------------

# Filtro simple: solo Cali
bd_cali <- filter(bd_sel, municipio == "cali")
nrow(bd_cali)

# Filtro simple: montos mayores a 3 millones
bd_grandes <- filter(bd_sel, monto_mill > 3)
nrow(bd_grandes)

# Dos condiciones (Y): Cali Y microempresas
bd_cali_micro <- filter(bd_sel, municipio == "cali" & tamano_empresa == "microempresa")
nrow(bd_cali_micro)

# Dos condiciones (O): Cali O Buenaventura
bd_dos_muni <- filter(bd_sel, municipio %in% c("cali", "buenaventura"))
nrow(bd_dos_muni)

# Mujeres con monto entre 1 y 3 millones
bd_mujeres_med <- filter(bd_sel, genero == "mujer" & monto_mill >= 1 & monto_mill <= 3)
nrow(bd_mujeres_med)


# -----------------------------------------------------------------------------
# 3.4 arrange(): ordenar filas
# -----------------------------------------------------------------------------

# De menor a mayor monto
bd_asc <- arrange(bd_sel, monto_mill)
head(bd_asc)

# De mayor a menor monto
bd_desc <- arrange(bd_sel, desc(monto_mill))
head(bd_desc)

# Por municipio (A→Z) y dentro de cada municipio por monto (mayor→menor)
bd_doble <- arrange(bd_sel, municipio, desc(monto_mill))
head(bd_doble, 10)


# -----------------------------------------------------------------------------
# 3.5 mutate(): crear nuevas variables
# -----------------------------------------------------------------------------

# Clasificar monto: "alto" si supera 3 millones, "bajo" en otro caso
bd_sel <- mutate(bd_sel, nivel_monto = ifelse(monto_mill > 3, "alto", "bajo"))

# Variable indicadora: 1 si es mujer, 0 si no
bd_sel <- mutate(bd_sel, es_mujer = ifelse(genero == "mujer", 1, 0))

head(select(bd_sel, genero, es_mujer, monto_mill, nivel_monto))


# -----------------------------------------------------------------------------
# 3.6 summarise(): resumen global
# -----------------------------------------------------------------------------

resumen_global <- summarise(bd_sel,
                            n_creditos     = n(),
                            monto_total    = sum(monto_mill),
                            monto_promedio = mean(monto_mill),
                            monto_max      = max(monto_mill),
                            monto_min      = min(monto_mill))
resumen_global


# =============================================================================
# 4. DESCRIPTIVAS POR GRUPO
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1 KPIs por sector económico
# -----------------------------------------------------------------------------

bd_agr <- group_by(bd_sel, sector_economico)
kpi_sector <- summarise(bd_agr,
                        n_creditos     = n(),
                        monto_total    = sum(monto_mill),
                        monto_promedio = mean(monto_mill),
                        .groups = "drop")
arrange(kpi_sector, desc(monto_total))


# -----------------------------------------------------------------------------
# 4.2 KPIs por municipio
# -----------------------------------------------------------------------------

bd_agr2 <- group_by(bd_sel, municipio)
kpi_muni <- summarise(bd_agr2,
                      n_creditos     = n(),
                      monto_total    = sum(monto_mill),
                      monto_promedio = mean(monto_mill),
                      .groups = "drop")
arrange(kpi_muni, desc(monto_total))



# =============================================================================
# 5. EJERCICIOS — PREGUNTAS DEL NEGOCIO
# =============================================================================
# Instrucciones:
#   - Lee bien la pregunta antes de escribir código
#   - Guarda cada resultado en un objeto con el nombre indicado
#   - Imprime el objeto al final de cada ejercicio para verificar tu respuesta
# =============================================================================


# -----------------------------------------------------------------------------
# EJERCICIO 1
# -----------------------------------------------------------------------------
# El equipo directivo quiere saber cuáles son los 5 municipios que recibieron
# el mayor monto total de créditos (en millones). Necesitan esta lista ordenada
# de mayor a menor para priorizar visitas de seguimiento.

## Pista: Cali es el No 1



# -----------------------------------------------------------------------------
# EJERCICIO 2
# -----------------------------------------------------------------------------
# El área de género quiere entender si existe diferencia en el monto promedio
# otorgado entre hombres y mujeres. Calcula el monto promedio y el número de
# créditos para cada categoría de género.

## Pista: En promedio a las mujeres se les desemobolsa un monto inferior 




# -----------------------------------------------------------------------------
# EJERCICIO 3
# -----------------------------------------------------------------------------
# Un analista necesita revisar manualmente los créditos de mayor riesgo:
# aquellos otorgados a microempresas, con monto mayor a 4 millones y
# plazo mayor a 24 meses.

## Pista: Cuando nos referimos a "manualmente", queremos obtener una nueva base (objeto) 
# con esta nueva información. 




# -----------------------------------------------------------------------------
# EJERCICIO 4
# -----------------------------------------------------------------------------
# El área comercial quiere un tablero con KPIs por sector económico Y
# tamaño de empresa.


## Pista: El sector economico de mayor plazo es el de la construcción, con tan 
# solo 1 crédito desembolsado a la pequeña empresa. 



