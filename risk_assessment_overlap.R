### RISK ASSESSMENT CALCULATION - SIF Project El Salvador ###


## Luis de la Rua - Dec 2025

# SETTINGS =====================================================================

# libraries loaded from the script below that also deals with conflicts

source("setup.R")

# Paths

dir <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/SLV/" # main folder
layers <- paste0(dir,"layers/") # GIS input
temp <- paste0(dir, "temp/") # temporary files and trash

# 1. IMPORT ALL THE ELEMENTS ==================================================
# Vulnerability by District with all the 3 dimensions and composed index IVMC
vul <- vect(paste0(layers,"indices de vulnerabilidad slv/indices VMC.shp"))

# Population input - use Worldpop 2025 for the moment
# https://hub.worldpop.org/geodata/summary?id=73247
pop <- rast(paste0(layers,"slv_pop_2025_CN_100m_R2025A_v1.tif"))

# Hazard Areas from Google embedding and AI to determine 
haz_flood <- vect(paste0(layers,"capas susceptibilidad/Susceptibilidad_Inundación_SV_dc05251724.shp"))

# check CRS is the same for the 3 layers
crs(vul) == crs(pop)
crs(haz_flood) == crs(pop)
crs(haz_flood) == crs(vul)

# reproject everything to the raster projection as it is easier to reproject vector layers
vul <- project(vul, crs(pop))
haz_flood <- project(haz_flood, crs(pop))

# Check again
crs(vul) == crs(pop)
crs(haz_flood) == crs(pop)
crs(haz_flood) == crs(vul)

# 2. PROCESS HAZARD INFORMATION ===============================================

## 2.1 Plot hazard data using categories ----

# Force the specific order of the categories
haz_flood$risk_class <- factor(haz_flood$risk_class, 
                               levels = c("Bajo", "Medio", "Alto", "Extremo"))

library(ggplot2)

ggplot(data = haz_flood) +
  # 1. Plot the geometry with no borders (color = NA)
  geom_sf(aes(fill = risk_class), color = NA) +
  scale_fill_manual(
      values = c(
        "Bajo"    = "#eff3ff",  
        "Medio"   = "#bdd7e7",  
        "Alto"    = "#6baed6",  
        "Extremo" = "#08519c"
    ), 
    name = "Nivel de Riesgo" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Riesgo de Inundación",
       subtitle = "Categorizado por Intensidad")

## 2.2 Intersect vul and hazard and pop ----
# Simplify vulnerability layer ----
vul <- vul %>% 
  select(c(NA3, NAM, D1, D2, D3, IVMC))

# Calculate population within each of the hazard tiles 
haz_flood_pop <- exactextractr::exact_extract(raster(pop),st_as_sf(haz_flood),
                                              fun ='sum')
# Merge the population counts with the tile layer
haz_flood_pop <- haz_flood_pop %>% 
  cbind(haz_flood,.) %>% 
  rename(wpop_2025 = y)

global(pop, fun = "sum", na.rm = TRUE)
sum(haz_flood_pop$wpop_2025)

# Intersect with Vulnerability 
# The result is the tile hazard layer including all the information from the district
# layer that will help us to aggregate exposure at district level.

## 2.3 Calculate population within exposure areas by district.

risk <- intersect(haz_flood_pop, vul)
