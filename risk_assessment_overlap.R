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
# Simplify vulnerability layer
vul <- vul %>% 
  select(c(NA3, NAM, D1, D2, D3, IVMC))

# Intersect with Vulnerability 
# The result is the tile hazard layer including all the information from the district
# layer that will help us to aggregate exposure at district level.
risk <- terra::intersect(haz_flood, vul)
names(risk)

# Calculate population within each of the tiles intersected with the district layer 
haz_flood_pop <- exactextractr::exact_extract(raster(pop),st_as_sf(risk),
                                              fun ='sum')

# Merge the population counts with the tile layer
haz_flood_pop <- haz_flood_pop %>% 
  cbind(risk,.) %>% 
  rename(wpop_2025 = y)

global(pop, fun = "sum", na.rm = TRUE)
sum(haz_flood_pop$wpop_2025)


# Summarise to obtain vulnerability and exposure results on a table at 
# District level
risk_dist <- haz_flood_pop %>% 
  as.data.frame() %>% 
  group_by(NA3, risk_class) %>% 
  summarise(wpop_risk = sum(wpop_2025, na.rm = TRUE))

# Merge with district layer
risk_dist <- merge(risk_dist, vul, by = 'NA3')
  
risk_dist_wide <- risk_dist %>% 
  pivot_wider(
    names_from = risk_class,
    values_from = wpop_risk,
    values_fill = 0) %>% 
  select(c(NA3, Extremo, Alto, Medio, Bajo)) %>% 
  mutate(wpop_risk = Extremo + Alto + Medio + Bajo, # to calculate total population per district
         per_flood_extremo = Extremo / wpop_risk,
         per_flood_alto = Alto / wpop_risk,
         per_flood_medio = Medio / wpop_risk,
         per_flood_bajo = Bajo / wpop_risk) %>% 
  rename(flood_extremo = Extremo,
         flood_alto = Alto,
         flood_medio = Medio,
         flood_bajo = Bajo)

nrow(risk_dist_wide)



## 2.3 Join table to spatial ----
# Layer at district level  
risk_layer_dist <- merge(vul,risk_dist_wide, by = 'NA3')



