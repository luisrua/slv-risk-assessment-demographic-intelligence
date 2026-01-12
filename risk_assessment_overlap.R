### RISK ASSESSMENT CALCULATION - SIF Project El Salvador ###


## Luis de la Rua - Dec 2025

# SETTINGS =====================================================================

# Clean workspace
rm(list = ls())
gc()

# libraries loaded from the script below that also deals with conflicts

source("setup.R")

# Additional libraries
library(tmap)
library(biscale)
library(cowplot)
library(stringi) # to clean latin encoding
library(ggplot2)

# Paths

dir <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/SLV/" # main folder
layers <- paste0(dir,"layers/") # GIS input
temp <- paste0(dir, "temp/") # temporary files and trash

# 1. IMPORT ALL THE ELEMENTS ==================================================
# Vulnerability by District with all the 3 dimensions and composed index IVMC
vul <- vect(paste0(layers,"indices VMC.shp"))
# import the vulnerability layer that contains de right district names. Old vulnerability layer
vul_names <- vect(paste0(layers,"indices de vulnerabilidad slv/indices VMC.shp")) %>% 
  select(c(NAM, NA3)) %>% 
  mutate(across(where(is.character), function(x) {
    # Try to repair common Spanish encoding issues (Latin1 -> UTF-8)
    fixed_text <- iconv(x, from = "latin1", to = "UTF-8")
    # If iconv fails (returns NA), keep the original text but strip bad bytes
    # ifelse(is.na(fixed_text), iconv(x, to = "UTF-8", sub = ""), fixed_text)
  }))


vul<- vul %>% 
mutate(across(where(is.character), function(x) {
  # Try to repair common Spanish encoding issues (Latin1 -> UTF-8)
  fixed_text <- iconv(x, from = "latin1", to = "UTF-8")
  # If iconv fails (returns NA), keep the original text but strip bad bytes
  # ifelse(is.na(fixed_text), iconv(x, to = "UTF-8", sub = ""), fixed_text)
}))

vul <- vul %>% 
  select(-NAM) %>% 
  merge(.,vul_names, by = "NA3")

# Population input - use Worldpop 2025 for the moment
# https://hub.worldpop.org/geodata/summary?id=73247
pop <- rast(paste0(layers,"slv_pop_2025_CN_100m_R2025A_v1.tif"))

# Hazard Areas from Google embedding and AI to determine 
haz <- vect(paste0(layers,"SusceptibilidadInundaciónDeslaveSequía_SV_N.shp"))
haz_eq <- vect(paste0(layers,"Susceptibilidad_DañosSismo_SV.gpkg"))

# Check CRS is the same for the 3 layers
crs(vul) == crs(pop)
crs(haz) == crs(pop)
crs(haz) == crs(vul)
crs(haz_eq) == crs(vul)
crs(haz_eq) == crs(pop)
# Reproject everything to the raster projection as it is easier to reproject vector layers
vul <- project(vul, crs(pop))
haz <- project(haz, crs(pop))
haz_eq <- project(haz_eq, crs(pop))

# Check again
crs(vul) == crs(pop)
crs(haz) == crs(pop)
crs(haz) == crs(vul)

# Strip down haz layer removing unnecessary fields
haz <- haz %>% 
  select(c(risk_flood ,risk_lands, risk_droug, RiskFloodN,
           RiskLandN, RiskDrouN))

haz_eq <- haz_eq %>% 
  select(c(risk_embed_seis, risk_phys_seis, risk_seismic ))


# Reclass Exposure classes based on thresholds
# For floods. Flood
  # Bajo	0 - 3.694764
  # MEdio	3.694765 - 4.294008
  # Alto	4.29409 - 5.866357
  # Extremo	5.866358 - 10.0000

  # LandSlides
  # Bajo 0 - 2.60673
  # Medio	2.60674 - 4.252207
  # Alto	4.252208 - 5.3763989
  # Extremo	5.3763990 - 10.00000
  # 
  # Droughts
  # Bajo	0 - 3.203229
  # MEdio	3.203230 - 4.962889
  # Alto	4.962890 - 6.397829
  # Extremo	6.397830 - 10.0000

haz <- haz %>% 
  mutate(
    # Flood Risk
    fl_risk_class = case_when(
      RiskFloodN >= 0 & RiskFloodN <= 3.694764 ~ "Bajo",
      RiskFloodN > 3.694764 & RiskFloodN <= 4.294008 ~ "Medio",
      RiskFloodN > 4.294008 & RiskFloodN <= 5.866357 ~ "Alto",
      RiskFloodN > 5.866357 & RiskFloodN <= 10.0000 ~ "Extremo",
      TRUE ~ NA_character_
    ),
    
    # Landslide Risk (Fixed typos and syntax)
    ls_risk_class = case_when(
      RiskLandN >= 0 & RiskLandN <= 2.60673 ~ "Bajo",
      RiskLandN > 2.60673 & RiskLandN <= 4.252207 ~ "Medio",
      RiskLandN > 4.252207 & RiskLandN <= 5.3763989 ~ "Alto",
      RiskLandN > 5.3763989 & RiskLandN <= 10.0000 ~ "Extremo",
      TRUE ~ NA_character_
    ),
    
    # Drought Risk
    dr_risk_class = case_when(
      RiskDrouN >= 0 & RiskDrouN <= 3.203229 ~ "Bajo",
      RiskDrouN > 3.203229 & RiskDrouN <= 4.962889 ~ "Medio",
      RiskDrouN > 4.962889 & RiskDrouN <= 6.397829 ~ "Alto",
      RiskDrouN > 6.397829 & RiskDrouN <= 10.0000 ~ "Extremo",
      TRUE ~ NA_character_
    )
  )

# Determine quantiles of the risk earthquake variable to create the classes
quantile(haz_eq$risk_seismic, na.rm=T)

haz_eq <- haz_eq %>% 
  mutate(eq_risk_class = case_when(
    risk_seismic >= 0 & risk_seismic <= 0.3318172  ~ "Bajo",
    risk_seismic > 0.3318172 & risk_seismic <= 0.4240609 ~ "Medio", 
    risk_seismic > 0.4240609 & risk_seismic <= 0.5043801  ~ "Alto",
    risk_seismic > 0.5043801 &  risk_seismic <= 1.000 ~ "Extremo",
    TRUE ~ NA_character_))

# 2. PROCESS HAZARD INFORMATION ===============================================

## 2.1 Plot hazard data using categories ----

# Force the specific order of the categories
haz$fl_risk_class <- factor(haz$fl_risk_class, 
                               levels = c("Bajo", "Medio", "Alto", "Extremo"))

haz$ls_risk_class <- factor(haz$ls_risk_class, 
       levels = c("Bajo", "Medio", "Alto", "Extremo"))

haz$dr_risk_class <- factor(haz$dr_risk_class, 
       levels = c("Bajo", "Medio", "Alto", "Extremo"))

haz_eq$eq_risk_class <- factor(haz_eq$eq_risk_class, 
                               levels = c("Bajo", "Medio", "Alto", "Extremo"))

flood_hazard <- ggplot(data = haz) +
  # 1. Plot the geometry with no borders (color = NA)
  geom_sf(aes(fill = fl_risk_class), color = NA) +
  scale_fill_manual(
      values = c(
        "Bajo"    = "#eff3ff",  
        "Medio"   = "#bdd7e7",  
        "Alto"    = "#6baed6",  
        "Extremo" = "#08519c"
    ), 
    name = "Nivel de Amenaza" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Amenaza de Inundación",
       subtitle = "Categorizado por Intensidad")

flood_hazard

landslide_hazard <- ggplot(data = haz) +
  # 1. Plot the geometry with no borders (color = NA)
  geom_sf(aes(fill = ls_risk_class), color = NA) +
  scale_fill_manual(
    values = c(
      "Bajo"    = "#edf8e9",  
      "Medio"   = "#bae4b3",  
      "Alto"    = "#74c476",  
      "Extremo" = "#238b45"
    ), 
    name = "Nivel de Amenaza" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Amenaza de Deslizamiento de Tierra",
       subtitle = "Categorizado por Intensidad")

landslide_hazard

drought_hazard <- ggplot(data = haz) +
  # 1. Plot the geometry with no borders (color = NA)
  geom_sf(aes(fill = dr_risk_class), color = NA) +
  scale_fill_manual(
    values = c(
      "Bajo"    = "#fff5eb",  
      "Medio"   = "#fdbe85",  
      "Alto"    = "#fd8d3c",  
      "Extremo" = "#d94701"
    ), 
    name = "Nivel de Amenaza" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Amenaza de Sequía",
       subtitle = "Categorizado por Intensidad")

drought_hazard

earthquake_hazard <- ggplot(data = haz_eq) +
  geom_sf(aes(fill = eq_risk_class), color = NA) +
  scale_fill_manual(
    values = c(
      "Bajo"    = "#edf8fb",  
      "Medio"   = "#a6bbd9",  
      "Alto"    = "#896bb1",  
      "Extremo" = "#810f7c"
    ), 
    name = "Nivel de Amenaza" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Amenaza de Sísmica",
       subtitle = "Categorizado por Intensidad")

earthquake_hazard

# Save the three maps
# ggsave(filename = paste0(dir,"maps/Flood_Hazard_Categories.png"), plot = flood_hazard, 
#        width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = paste0(dir,"maps/Landslide_Hazard_Categories.png"), plot = landslide_hazard, 
#        width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = paste0(dir,"maps/Drought_Hazard_Categories.png"), plot = drought_hazard, 
#        width = 8, height = 6, units = "in", dpi = 300)

## 2.2 Intersect vul and hazard and pop ----
# Simplify vulnerability layer
vul <- vul %>% 
  select(-c(UID, ASD, COD, NA2, PPL, ACC,
            CCN, SDV, SDP, SRT, TXT))

# Intersect with Vulnerability 
# The result is the tile hazard layer including all the information from the district
# layer that will help us to aggregate exposure at district level.
risk <- terra::intersect(haz, vul)
names(risk)

risk_eq <- terra::intersect(haz_eq, vul)

# Calculate population within each of the tiles intersected with the district layer 
risk_pop <- exactextractr::exact_extract(raster(pop),st_as_sf(risk),
                                              fun ='sum')

risk_eq_pop <- exactextractr::exact_extract(raster(pop),st_as_sf(risk_eq),
                                         fun ='sum')

# Merge the population counts with the tile layer
risk_pop <- risk_pop %>% 
  cbind(risk,.) %>% 
  rename(wpop_2025 = y)

global(pop, fun = "sum", na.rm = TRUE)
sum(risk_pop$wpop_2025, na.rm = T)

risk_eq_pop <- risk_eq_pop %>% 
  cbind(risk_eq,.) %>% 
  rename(wpop_2025 = y)

# Summarise to obtain vulnerability and exposure results on a table at District level
risk_dist_fl <- risk_pop %>% 
  as.data.frame() %>% 
  group_by(NA3, fl_risk_class) %>% 
  summarise(wpop_fl_risk = sum(wpop_2025, na.rm = TRUE)) 

risk_dist_dr <- risk_pop %>% 
  as.data.frame() %>%
  group_by(NA3, dr_risk_class) %>% 
  summarise(wpop_dr_risk = sum(wpop_2025, na.rm = TRUE)) 

risk_dist_ls <- risk_pop %>% 
  as.data.frame() %>%
  group_by(NA3, ls_risk_class) %>% 
  summarise(wpop_ls_risk = sum(wpop_2025, na.rm = TRUE)) 

risk_dist_eq <- risk_eq_pop %>% 
  as.data.frame() %>%
  group_by(NA3, eq_risk_class   ) %>% 
  summarise(wpop_eq_risk = sum(wpop_2025, na.rm = TRUE)) 

  
risk_dist_fl_wide <- risk_dist_fl %>% 
  pivot_wider(
    names_from = fl_risk_class,
    values_from = wpop_fl_risk,
    values_fill = 0) %>%
  select(c(NA3, Extremo, Alto, Medio, Bajo)) %>% 
  mutate(wpop = Extremo + Alto + Medio + Bajo, # to calculate total population per district
         per_flood_extremo = Extremo / wpop,
         per_flood_alto = Alto / wpop,
         per_flood_medio = Medio / wpop,
         per_flood_bajo = Bajo / wpop) %>% 
  rename(flood_extremo = Extremo,
         flood_alto = Alto,
         flood_medio = Medio,
         flood_bajo = Bajo)%>% 
  merge(vul,., by = 'NA3')


risk_dist_dr_wide <- risk_dist_dr %>% 
  pivot_wider(
    names_from = dr_risk_class,
    values_from = wpop_dr_risk,
    values_fill = 0) %>% 
  select(c(NA3, Extremo, Alto, Medio, Bajo)) %>% 
  mutate(wpop = Extremo + Alto + Medio + Bajo, # to calculate total population per district
         per_drought_extremo = Extremo / wpop,
         per_drought_alto = Alto / wpop,
         per_drought_medio = Medio / wpop,
         per_drought_bajo = Bajo / wpop) %>% 
  rename(drought_extremo = Extremo,
         drought_alto = Alto,
         drought_medio = Medio,
         drought_bajo = Bajo)%>% 
  merge(vul,., by = 'NA3')
  
risk_dist_ls_wide <- risk_dist_ls %>%
  pivot_wider(
    names_from = ls_risk_class,
    values_from = wpop_ls_risk,
    values_fill = 0) %>% 
  mutate(wpop = Extremo + Alto + Medio + Bajo, # to calculate total population per district
         per_landslide_extremo = Extremo / wpop,
         per_landslide_alto = Alto / wpop,
         per_landslide_medio = Medio / wpop,
         per_landslide_bajo = Bajo / wpop) %>% 
  rename(landslide_extremo = Extremo,
         landslide_alto = Alto,
         landslide_medio = Medio,
         landslide_bajo = Bajo) %>% 
  merge(vul,., by = 'NA3')

risk_dist_eq_wide <- risk_dist_eq %>% 
  pivot_wider(
    names_from = eq_risk_class,
    values_from = wpop_eq_risk,
    values_fill = 0) %>%
  mutate(wpop = Extremo + Alto + Medio + Bajo, # to calculate total population per district
         per_eq_extremo = Extremo / wpop,
         per_eq_alto = Alto / wpop,
         per_eq_medio = Medio / wpop,
         per_eq_bajo = Bajo / wpop) %>%
  rename(eq_extremo = Extremo,
         eq_alto = Alto,
         eq_medio = Medio,
         eq_bajo = Bajo) %>%
  merge(vul,., by = 'NA3')
  

## 2.3 Join table to spatial ----

# THESE ARE THE BASE FOR THE LAYERS AND THE EXCEL TABLES.
# They Contain exposure and vulnerability information at district level
risk_layer_fl_dist <- risk_dist_fl_wide
risk_layer_dr_dist <- risk_dist_dr_wide
risk_layer_ls_dist <- risk_dist_ls_wide
risk_layer_eq_dist <- risk_dist_eq_wide

# writeVector(risk_layer_fl_dist,
#             paste0(layers,"risk_assessment/slv_risk_assessment_districts.gpkg"),
#             layer ='floods',
#             overwrite=T)

# writeVector(risk_layer_dr_dist,
#             paste0(layers,"risk_assessment/slv_risk_assessment_districts.gpkg"),
#             layer ='droughts',
#             overwrite=T)

# writeVector(risk_layer_ls_dist,
#             paste0(layers,"risk_assessment/slv_risk_assessment_districts.gpkg"),
#             layer ='landslides',
#             overwrite=T)
# writeVector(risk_layer_eq_dist,
#             paste0(layers,"risk_assessment/slv_risk_assessment_districts.gpkg"),
#             layer ='earthquake',
#             overwrite=T)

# 3. MAPPING RESULTS ===========================================================

## 3.1 Flood Drought and Landslide Risk Bivariate Map ----
# --- Prepare Data & Palette ---

# Importing lac admin boundaries 
ab <- vect(paste0(dir,"layers/lac_ab_pol_4326.gpkg"), layer = "lac_ab_pol_4326" )
ab <-  st_make_valid(st_as_sf(ab))
# plot(ab)

# Testing custom palette
# This palette moves from Light Grey (Low-Low) to Blue (High Flood) and Orange (High Vuln),
# meeting at Red (High Risk) in the top-right corner (3-3).
custom_pal_red <- c(
  "1-1" = "#ffe0c4", # Low Exp, Low Vuln (Grey)
  "2-1" = "#fdae61",
  "3-1" = "#ed771d", # High Exp, Low Vuln (Blue)
  "1-2" = "#fecbae",
  "2-2" = "#f8926a",
  "3-2" = "#ef5d2e",
  "1-3" = "#f49a9a", # Low Exp, High Vuln (Orange/Pink)
  "2-3" = "#ee6867",
  "3-3" = "#eb090b"  # High Exp, High Vuln (RED - High Risk)
)

### --- 3.1.1 Prepare Data for Bivariate representation ----
# Floods
risk_layer_biv_fl <- risk_layer_fl_dist %>% 
  mutate(flood_exp_ext_alt = per_flood_extremo + per_flood_alto)


data_bivariate_fl <- bi_class(st_as_sf(risk_layer_biv_fl),
                           x = flood_exp_ext_alt,
                           y = VUL_P,
                           style = "fisher",
                           dim = 3) %>% 
  filter(NA3 != '0000')

# Droughts
risk_layer_biv_dr <- risk_layer_dr_dist %>% 
  mutate(drought_exp_ext_alt = per_drought_extremo + per_drought_alto)

data_bivariate_dr <- bi_class(st_as_sf(risk_layer_biv_dr),
                              x = drought_exp_ext_alt,
                              y = VUL_P,
                              style = "fisher",
                              dim = 3) %>% 
  filter(NA3 != '0000')

# Landslides
risk_layer_biv_ls <- risk_layer_ls_dist %>% 
  mutate(landslide_exp_ext_alt = per_landslide_extremo + per_landslide_alto)


data_bivariate_ls <- bi_class(st_as_sf(risk_layer_biv_ls),
                              x = landslide_exp_ext_alt,
                              y = VUL_P,
                              style = "fisher",
                              dim = 3) %>% 
  filter(NA3 != '0000')

# Earthquakes 
risk_layer_biv_eq <- risk_layer_eq_dist %>% 
  mutate(equake_exp_ext_alt = per_eq_extremo + per_eq_alto)

data_bivariate_eq <- bi_class(st_as_sf(risk_layer_biv_eq),
                              x = equake_exp_ext_alt,
                              y = VUL_P,
                              style = "fisher",
                              dim = 3) %>% 
  filter(NA3 != '0000')

### --- 3.1.2 Create High-Res Legends for the three hazard scenarios ----
legend_fl <- bi_legend(pal = custom_pal_red,
                    dim = 3,
                    xlab = "Mayor Amenaza Inundaciones",
                    ylab = "Mayor Vulnerabilidad",
                    size = 12) + # Base size
  theme(
    axis.title.x = element_text(size = 18), # Customize X label
    axis.title.y = element_text(size = 18, angle = 90)  # Customize Y label
  )

ggsave(filename = "flood_biv_legend.png", plot = legend_fl, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

legend_dr <- bi_legend(pal = custom_pal_red,
                       dim = 3,
                       xlab = "Mayor Amenaza Seguía",
                       ylab = "Mayor Vulnerabilidad",
                       size = 12) + # Base size
  theme(
    axis.title.x = element_text(size = 18), # Customize X label
    axis.title.y = element_text(size = 18, angle = 90)  # Customize Y label
  )

ggsave(filename = "dr_biv_legend.png", plot = legend_dr, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

legend_ls <- bi_legend(pal = custom_pal_red,
                       dim = 3,
                       xlab = "Mayor Amenaza Deslaves",
                       ylab = "Mayor Vulnerabilidad",
                       size = 12) + # Base size
  theme(
    axis.title.x = element_text(size = 18), # Customize X label
    axis.title.y = element_text(size = 18, angle = 90)  # Customize Y label
  )

ggsave(filename = "ls_biv_legend.png", plot = legend_ls, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

legend_eq <- bi_legend(pal = custom_pal_red,
                       dim = 3,
                       xlab = "Mayor Amenaza Sísmica",
                       ylab = "Mayor Vulnerabilidad",
                       size = 12) + # Base size
  theme(
    axis.title.x = element_text(size = 18), # Customize X label
    axis.title.y = element_text(size = 18, angle = 90)  # Customize Y label
  )

ggsave(filename = "eq_biv_legend.png", plot = legend_ls, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

### --- 3.1.3. Plot Maps  ----
tmap_mode("plot")

# Turning it off switches R to a "flat" mode (GEOS) which is much more forgiving of minor topology errors.
sf::sf_use_s2(FALSE) 

# Remove El Salvador from bkground layer
ab <- ab %>%
  filter(GID_0 != "SLV")

# Layers for high risk districts
high_risk_labels_fl <- data_bivariate_fl %>% 
  select(c(bi_class,NA3, NAM, )) %>% 
  filter(bi_class == "3-3")

# Restircting to 20 labels to make them fit in map the top 20 exp/vuln districts
high_risk_labels_dr <- data_bivariate_dr %>%
  select(c(bi_class, NA3, NAM, drought_exp_ext_alt, IVMC)) %>% # Make sure IVMC is selected
  filter(bi_class == "3-3") %>%
  arrange(desc(drought_exp_ext_alt), desc(IVMC)) %>%
  slice(1:20)
  
high_risk_labels_ls <- data_bivariate_ls %>% 
  select(c(bi_class,NA3, NAM, landslide_exp_ext_alt, IVMC)) %>% 
  filter(bi_class == "3-3") %>% 
  arrange(desc(landslide_exp_ext_alt), desc(IVMC)) %>%
  slice(1:20)

high_risk_labels_eq <- data_bivariate_eq %>% 
  select(c(bi_class,NA3, NAM, equake_exp_ext_alt, IVMC)) %>% 
  filter(bi_class == "3-3") %>% 
  arrange(desc(equake_exp_ext_alt), desc(IVMC)) %>%
  slice(1:20)

  

# Increase bbox to fit labels at the edges of the map# 1. Create a slightly larger bounding box (e.g., 5% bigger)
bbox_new <- st_bbox(data_bivariate_fl) # Get original box

# Manually expand the limits (xmin, ymin, xmax, ymax)
# This calculates the width/height and subtracts/adds 5% to the edges
x_range <- bbox_new["xmax"] - bbox_new["xmin"]
y_range <- bbox_new["ymax"] - bbox_new["ymin"]

bbox_new["xmin"] <- bbox_new["xmin"] - (0.05 * x_range)
bbox_new["xmax"] <- bbox_new["xmax"] + (0.05 * x_range)
bbox_new["ymin"] <- bbox_new["ymin"] - (0.05 * y_range)
bbox_new["ymax"] <- bbox_new["ymax"] + (0.05 * y_range)


# Set the maps

#### FLOOD RISK MAP ----------------------------------------
flood_bivar_map <-
  # Background map
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  # Main bivariate map
  tm_shape(data_bivariate_fl) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(
      values = custom_pal_red, 
      value.na = "grey80"  # <--- Explicitly define the color for NA values here
    ),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) + 
  # Labels for high risk districts
  tm_shape(high_risk_labels_fl) +
  tm_labels_highlighted(
    text = "NAM", 
    size = 0.6,
    col = "black",
      bgcol = "white",  
      bgcol_alpha = 0.5,
    options = opt_tm_labels(just = "center") 
    ) +
  
  tm_title("Análisis de Riesgo de Inundaciones - El Salvador",
           size = 1) + 
  tm_logo("flood_biv_legend.png", height = 5.5, position = c("left", "bottom")) +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

flood_bivar_map

tmap_save(
  tm = flood_bivar_map, 
  filename = paste0(dir,"maps/Bivariate_Flood_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)


# Display
print(flood_bivar_map)
print(legend)

#### DROUGHT RISK MAP --------------------------------------
drought_bivar_map <-
  # Background map
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  # Main bivariate map
  tm_shape(data_bivariate_dr) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(
      values = custom_pal_red, 
      value.na = "grey80"  # <--- Explicitly define the color for NA values here
    ),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) + 
  # Labels for high risk districts
  tm_shape(high_risk_labels_dr) +
  tm_labels_highlighted(
    text = "NAM",   
    size = 0.6,
    col = "black",
    bgcol = "white",  
    bgcol_alpha = 0.5,

    options = opt_tm_labels(just = "center")
  ) +
  
  tm_title("Análisis de Riesgo de Sequías - El Salvador",
           size = 1) + 
  tm_logo("dr_biv_legend.png", height = 5.5, position = c("left", "bottom")) +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

drought_bivar_map

tmap_save(
  tm = drought_bivar_map, 
  filename = paste0(dir,"maps/Bivariate_Drought_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)

#### LANDSLIDES RISK MAP --------------------------------------

landslide_bivar_map <-
  # Background map
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  # Main bivariate map
  tm_shape(data_bivariate_ls) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(
      values = custom_pal_red, 
      value.na = "grey80"  # <--- Explicitly define the color for NA values here
    ),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) + 
  # Labels for high risk districts
  tm_shape(high_risk_labels_ls) +
  tm_labels_highlighted(
    text = "NAM",   
    size = 0.6,
    col = "black",
    bgcol = "white",  
    bgcol_alpha = 0.5,
    options = opt_tm_labels(just = "center") 
  ) +
  
  tm_title("Análisis de Riesgo de Deslizamientos de Tierra - El Salvador",
           size = 1) + 
  tm_logo("ls_biv_legend.png", height = 5.5, position = c("left", "bottom")) +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

landslide_bivar_map

tmap_save(
  tm = landslide_bivar_map, 
  filename = paste0(dir,"maps/Bivariate_Landslide_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)

#### EARTHQUAKE RISK MAP ----------------------------------------
equake_bivar_map <-
  # Background map
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  # Main bivariate map
  tm_shape(data_bivariate_eq) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(
      values = custom_pal_red, 
      value.na = "grey80"  # <--- Explicitly define the color for NA values here
    ),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) + 
  # Labels for high risk districts
  tm_shape(high_risk_labels_eq) +
  tm_labels_highlighted(
    text = "NAM", 
    size = 0.6,
    col = "black",
    bgcol = "white",  
    bgcol_alpha = 0.5,
    options = opt_tm_labels(just = "center") 
  ) +
  
  tm_title("Análisis de Riesgo Sísmico - El Salvador",
           size = 1) + 
  tm_logo("eq_biv_legend.png", height = 5.5, position = c("left", "bottom")) +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )
equake_bivar_map

tmap_save(
  tm = equake_bivar_map, 
  filename = paste0(dir,"maps/Bivariate_Earthquke_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)

## 3.2 Map the Vulnerability Dimensions ----

# --- 1. SETUP LISTS ---
dims_to_map <- c("D1", "D2", "D3", "IVMC")

# Fixed: Removed nested c()
palettes <- c("brewer.purples", "brewer.greens", "brewer.blues", "brewer.oranges")

titles <- c(
  "Dimensión 1: Índice de Sensibilidad Ambiental y Climática del Hogar", 
  "Dimensión 2: Índice de Capacidad Adaptativa del Hogar",
  "Dimensión 3: Índice de Diferencial Demográfico del Hogar",
  "Índice agregado de Vulnerabilidad Medioambiental y Climática - IVMC"
)

# Fixed: Renamed second "D1" to "D2"
thresholds <- list(
  "D1"   = c(0.417, 0.483, 0.549, 0.615),
  "D2"   = c(0.598, 0.671, 0.744, 0.818), # <--- Fixed typo here
  "D3"   = c(0.394, 0.405, 0.416, 0.428),
  "IVMC" = c(0.449, 0.501, 0.554, 0.607)
)

# --- 2. LOOP ---
for (i in 1:length(dims_to_map)) {
  
  var_name  <- dims_to_map[i]
  pal_name  <- palettes[i]
  map_title <- titles[i]
  
  print(paste("Mapping:", var_name))
  
  # A. Get Raw Thresholds
  raw_breaks <- thresholds[[var_name]]
  
  if (is.null(raw_breaks)) stop(paste("Error: No thresholds found for", var_name))
  
  # B. Prepare Breaks & Labels
  # We extract the middle two numbers to define the "Medium" band
  # Example: c(min, cut1, cut2, max)
  cut1 <- raw_breaks[2]
  cut2 <- raw_breaks[3]
  
  # Round for clean labels (e.g., "0.48")
  c1_txt <- round(cut1, 3)
  c2_txt <- round(cut2, 3)
  
  # C. Create SAFE Breaks for Mapping
  # We use -Inf and Inf to ensure NO data is dropped (no grey polygons)
  # The map will treat everything below cut1 as "Low" and everything above cut2 as "High"
  safe_breaks <- c(-Inf, cut1, cut2, Inf)
  
  # D. Custom Labels
  custom_labels <- c(
    paste0("< ", c1_txt),           # Low
    paste0(c1_txt, " - ", c2_txt),  # Medium
    paste0("> ", c2_txt)            # High
  )
  
  # --- 3. BUILD MAP ---
  vulnerability_map <- 
    # Context
    tm_shape(ab, bbox = bbox_new) +
    tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
    
    # Main Data
    tm_shape(data_bivariate_ls) +
    tm_polygons(
      fill = var_name,
      
      fill.scale = tm_scale_intervals(
        style = "fixed", 
        breaks = safe_breaks,      # Use the safe -Inf/Inf breaks
        labels = custom_labels,    # Use the clean text labels
        values = pal_name,
        value.na = "darkgrey"
      ),
      
      col = "black", 
      lwd = 0.1, 
      col_alpha = 0.5,
      fill.legend = tm_legend(title = "") # No title needed since labels are self-explanatory
    ) +
    
    # Layout
    tm_title(map_title) +
    tm_layout(
      frame = FALSE, 
      bg.color = "#dbf1ff",
      legend.position = c("left", "bottom")
    )
  
  # --- 4. SAVE ---
  tmap_save(
    tm = vulnerability_map,
    filename = paste0(dir, "maps/Vulnerability_Map_", var_name, ".png"),
    dpi = 300,
    width = 10,
    height = 8
  )
}
vulnerability_map

## 3.3 Map Flood Hazard Zones ----

flood_colors <- c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c")

# 2. Create the map
flood_hazard_map <- 
  # Background
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
  
  # Map
  tm_shape(haz) +
  tm_polygons(
    fill = "fl_risk_class",
    col  = "fl_risk_class",   # <--- The Trick: Border matches Fill
    lwd  = 0.5,
    # Small width to bridge the gap
    
    # Define the same palette for BOTH fill and col
    fill.scale = tm_scale_categorical(values = flood_colors),
    col.scale  = tm_scale_categorical(values = flood_colors),
    
    # Hide the extra legend generated by 'col'
    col.legend  = tm_legend_hide(),
    fill.legend = tm_legend(title = "Nivel de Amenaza",
                            position = c("right", "top"))
  ) +
    # Text below title
  tm_credits(
    text = "Categorizado por Intensidad", 
    size = 0.9,                 # Slightly smaller than title (default is ~1.0)
    col = "grey30",             # Dark grey for visual hierarchy
    position = c("left", "top") # Same position as title
  ) +
  
  # Titles and Layout
  tm_title("Evaluación de Amenaza de Inundación") +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.05, 0.05, 0.05, 0.05),
      bg.color = "#dbf1ff"  # Light blue "Sea" color
    )

# View it
flood_hazard_map

# Save
tmap_save(
  tm = flood_hazard_map,
  filename = paste0(dir, "maps/Flood_Hazard_SLV.png"),
  dpi = 300,
  width = 10,
  height = 8
)

## 3.4 Map Drought Hazard Zones ----

colors <- c("#fff5eb", "#fdbe85", "#fd8d3c", "#d94701")


# 2. Create the map
# remove na tile somewhere in the map
haz_dr <- haz %>% 
  filter(!is.na(dr_risk_class))

drought_hazard_map <- 
  # Background
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
  
  # Map
  tm_shape(haz_dr) +
  tm_polygons(
    fill = "dr_risk_class",
    col  = "dr_risk_class",   # <--- The Trick: Border matches Fill
    lwd  = 0.5,
    # Small width to bridge the gap
    
    # Define the same palette for BOTH fill and col
    fill.scale = tm_scale_categorical(values = colors),
    col.scale  = tm_scale_categorical(values = colors),
    
    # Hide the extra legend generated by 'col'
    col.legend  = tm_legend_hide(),
    fill.legend = tm_legend(title = "Nivel de Amenaza",
                            position = c("right", "top"))
  ) +
  # Text below title
  tm_credits(
    text = "Categorizado por Intensidad", 
    size = 0.9,                 # Slightly smaller than title (default is ~1.0)
    col = "grey30",             # Dark grey for visual hierarchy
    position = c("left", "top") # Same position as title
  ) +
  
  # Titles and Layout
  tm_title("Evaluación de Amenaza de Sequías") +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.05, 0.05, 0.05, 0.05),
    bg.color = "#dbf1ff"  # Light blue "Sea" color
  )

# View it
drought_hazard_map

# Save
tmap_save(
  tm = flood_hazard_map,
  filename = paste0(dir, "maps/Drought_Hazard_SLV.png"),
  dpi = 300,
  width = 10,
  height = 8
)

## 3.5 Map Landslide Hazard Zones ----

ls_colors <- c("#edf8e9", "#bae4b3", "#74c476", "#238b45")

# 2. Create the map
landslide_hazard_map <- 
  # Background
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
  
  # Map
  tm_shape(haz) +
  tm_polygons(
    fill = "fl_risk_class",
    col  = "fl_risk_class",   # <--- The Trick: Border matches Fill
    lwd  = 0.5,
    # Small width to bridge the gap
    
    # Define the same palette for BOTH fill and col
    fill.scale = tm_scale_categorical(values = ls_colors),
    col.scale  = tm_scale_categorical(values = ls_colors),
    
    # Hide the extra legend generated by 'col'
    col.legend  = tm_legend_hide(),
    fill.legend = tm_legend(title = "Nivel de Amenaza",
                            position = c("right", "top"))
  ) +
  # Text below title
  tm_credits(
    text = "Categorizado por Intensidad", 
    size = 0.9,                 # Slightly smaller than title (default is ~1.0)
    col = "grey30",             # Dark grey for visual hierarchy
    position = c("left", "top") # Same position as title
  ) +
  
  # Titles and Layout
  tm_title("Evaluación de Amenaza de Deslizamientos de Tierra") +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.05, 0.05, 0.05, 0.05),
    bg.color = "#dbf1ff"  # Light blue "Sea" color
  )

# View it
landslide_hazard_map

# Save
tmap_save(
  tm = landslide_hazard_map,
  filename = paste0(dir, "maps/Landslide_Hazard_SLV.png"),
  dpi = 300,
  width = 10,
  height = 8
)

## 3.6 Map Earthquake Hazard Zones ----

eq_colors <-c("#edf8fb", "#a6bbd9", "#896bb1", "#810f7c")

# 2. Create the map
equake_hazard_map <- 
  # Background
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
  
  # Map
  tm_shape(haz_eq) +
  tm_polygons(
    fill = "eq_risk_class",
    col  = "eq_risk_class",   # <--- The Trick: Border matches Fill
    lwd  = 0.5,
    # Small width to bridge the gap
    
    # Define the same palette for BOTH fill and col
    fill.scale = tm_scale_categorical(values = eq_colors),
    col.scale  = tm_scale_categorical(values = eq_colors),
    
    # Hide the extra legend generated by 'col'
    col.legend  = tm_legend_hide(),
    fill.legend = tm_legend(title = "Nivel de Amenaza",
                            position = c("right", "top"))
  ) +
  # Text below title
  tm_credits(
    text = "Categorizado por Intensidad", 
    size = 0.9,                 # Slightly smaller than title (default is ~1.0)
    col = "grey30",             # Dark grey for visual hierarchy
    position = c("left", "top") # Same position as title
  ) +
  
  # Titles and Layout
  tm_title("Evaluación de Amenaza Sísmica") +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.05, 0.05, 0.05, 0.05),
    bg.color = "#dbf1ff"  # Light blue "Sea" color
  )

# View it
equake_hazard_map

# Save
tmap_save(
  tm = equake_hazard_map,
  filename = paste0(dir, "maps/Earthquake_Hazard_SLV.png"),
  dpi = 300,
  width = 10,
  height = 8
)

## 3.7 Side by Side Exp and Vulnerability map -----

final_comparison <- tmap_arrange(flood_hazard_map, vulnerability_map, ncol = 2)
print(final_comparison)

# 2. Save it using tmap_save
tmap_save(
  tm = final_comparison,
  filename = paste0(dir,"maps/fl_hazard_vs_IVMC.jpg"),
  width = 12,      # Width in inches
  height = 6,      # Height in inches
  units = "in",    # Unit for width/height
  dpi = 300        # Resolution (300 is print quality)
)

# 4. WORKING AROUND OUTPUTS ===================================================
## 4.1 Export layers to map risk in QGIS ----

writeVector(vect(data_bivariate_fl), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'data_bivariate_fl_dist',
            overwrite = T)

writeVector(vect(data_bivariate_dr), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'data_bivariate_dr_dist',
            insert = T)

writeVector(vect(data_bivariate_ls), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'data_bivariate_ls_dist',
            insert = T)
writeVector(vect(data_bivariate_eq), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'data_bivariate_eq_dist',
            insert = T)

## 4.2 Export risk assessment tables into excel but clean formats, names and fields ----
names(data_bivariate_fl)

flood_risk_dist_table <- data_bivariate_fl %>% 
  as_data_frame() %>% 
  st_drop_geometry() %>% 
  arrange(NA3) %>% 
  select(-c(FCODE, geometry)) %>% 
  relocate(NAM, .after = NA3) %>% 
  relocate(c(VUL_P,P_VUL_1,  P_VUL_2, P_VUL_3,P_DEMO_1,P_DEMO_2, P_DEMO_3, 
             P_SENS_1, P_SENS_2, P_SENS_3, P_ADAP_1, P_ADAP_2, P_ADAP_3), .after = bi_class) %>% 
  rename(
    `District Code` = NA3,
    `District Name` = NAM,
    `Población Wpop 2025` = wpop,
    `Vulnerabilidad Sensibilidad` = D1,
    `Vulnerabilidad Adaptabilidad` = D2,
    `Vulnerabilidad Diferencial Demografico` = D3,
    `Pobl. Amenaza Extrema Inundación` = flood_extremo,
    `Pobl. Amenaza Alta Inundación` = flood_alto,
    `Pobl. Amenaza Media Inundación` = flood_medio,
    `Pobl. Amenaza Baja Inundación` = flood_bajo,
    `% Pobl. Amenaza Extrema Inundación` = per_flood_extremo,
    `% Pobl. Amenaza Alta Inundación` = per_flood_alto,
    `% Pobl. Amenaza Media Inundación` = per_flood_medio,
    `% Pobl. Amenaza Baja Inundación` = per_flood_bajo,
    `% Pobl. Amenaza Extrema + Alta Inundación` = flood_exp_ext_alt,
    `Clase Riesgo Inundacion - IVMC` = bi_class,
    `Categoría dominante de vulnerabilidad global` = VUL_P ,
    `% Pobl. en baja vulnerabilidad global` = P_VUL_1 ,  
    `% Pobl. en vulnerabilidad global intermedia` = P_VUL_2 , 
    `% Pobl. en alta vulnerabilidad global` = P_VUL_3 ,
    `% Pobl. en baja presión/diferencial demográfico (D3)` = P_DEMO_1 ,
    `% Pobl. en presión/diferencial demográfico (D3)` = P_DEMO_2, 
    `% Pobl. en alta presión/diferencial demográfico (D3)` =  P_DEMO_3 , 
    `% Pobl. en baja sensibilidad (D1)` = P_SENS_1, 
    `% Pobl. en sensibilidad intermedia (D1)` = P_SENS_2, 
    `% Pobl. en alta sensibilidad (D1)` =  P_SENS_3 , 
    `% Pobl. en baja capacidad adaptativa (D2)` = P_ADAP_1 , 
    `% Pobl. en capacidad adaptativa intermedia (D2)` = P_ADAP_2, 
    `% Pobl. en alta capacidad adaptativa (D2)` = P_ADAP_3 )

names(data_bivariate_dr)

drought_risk_dist_table <- data_bivariate_dr %>% 
  as_data_frame() %>% 
  st_drop_geometry() %>% 
  arrange(NA3) %>% 
  select(-c(geometry, FCODE)) %>% 
  relocate(NAM, .after = NA3) %>%
  relocate(c(VUL_P,P_VUL_1,  P_VUL_2, P_VUL_3,P_DEMO_1,P_DEMO_2, P_DEMO_3, 
             P_SENS_1, P_SENS_2, P_SENS_3, P_ADAP_1, P_ADAP_2, P_ADAP_3), .after = bi_class) %>% 
  rename(    `District Code` = NA3,
             `District Name` = NAM,
             `Población Wpop 2025` = wpop,
             `Vulnerabilidad Sensibilidad` = D1,
             `Vulnerabilidad Adaptabilidad` = D2,
             `Vulnerabilidad Diferencial Demografico` = D3,
             `Pobl. Amenaza Extrema Sequía` = drought_extremo,
             `Pobl. Amenaza Alta Sequía` = drought_alto,
             `Pobl. Amenaza Media Sequía` = drought_medio,
             `Pobl. Amenaza Baja Sequía` = drought_bajo,
             `% Pobl. Amenaza Extrema Sequía` = per_drought_extremo,
             `% Pobl. Amenaza Alta Sequía` = per_drought_alto,
             `% Pobl. Amenaza Media Sequía` = per_drought_medio,
             `% Pobl. Amenaza Baja Sequía` = per_drought_bajo,
             `% Pobl. Amenaza Extrema + Alta Sequía` = drought_exp_ext_alt,
             `Clase Riesgo Sequía - IVMC` = bi_class,
             `Categoría dominante de vulnerabilidad global` = VUL_P ,
             `% Pobl. en baja vulnerabilidad global` = P_VUL_1 ,  
             `% Pobl. en vulnerabilidad global intermedia` = P_VUL_2 , 
             `% Pobl. en alta vulnerabilidad global` = P_VUL_3 ,
             `% Pobl. en baja presión/diferencial demográfico (D3)` = P_DEMO_1 ,
             `% Pobl. en presión/diferencial demográfico (D3)` = P_DEMO_2, 
             `% Pobl. en alta presión/diferencial demográfico (D3)` =  P_DEMO_3 , 
             `% Pobl. en baja sensibilidad (D1)` = P_SENS_1, 
             `% Pobl. en sensibilidad intermedia (D1)` = P_SENS_2, 
             `% Pobl. en alta sensibilidad (D1)` =  P_SENS_3 , 
             `% Pobl. en baja capacidad adaptativa (D2)` = P_ADAP_1 , 
             `% Pobl. en capacidad adaptativa intermedia (D2)` = P_ADAP_2, 
             `% Pobl. en alta capacidad adaptativa (D2)` = P_ADAP_3 )

names(data_bivariate_ls)

landslide_risk_dist_table <- data_bivariate_ls %>% 
  as_data_frame() %>% 
  st_drop_geometry() %>% 
  arrange(NA3) %>% 
  select(-c(geometry, FCODE, `NA`)) %>% 
  relocate(NAM, .after = NA3) %>% 
  relocate(c(VUL_P,P_VUL_1,  P_VUL_2, P_VUL_3,P_DEMO_1,P_DEMO_2, P_DEMO_3, 
             P_SENS_1, P_SENS_2, P_SENS_3, P_ADAP_1, P_ADAP_2, P_ADAP_3), .after = bi_class) %>% 
  rename(
    `District Code` = NA3,
    `District Name` = NAM,
    `Población Wpop 2025` = wpop,
    `Vulnerabilidad Sensibilidad` = D1,
    `Vulnerabilidad Adaptabilidad` = D2,
    `Vulnerabilidad Diferencial Demografico` = D3,
    `Pobl. Amenaza Extrema Deslave` = landslide_extremo,
    `Pobl. Amenaza Alta Deslave` = landslide_alto,
    `Pobl. Amenaza Media Deslave` = landslide_medio,
    `Pobl. Amenaza Baja Deslave` = landslide_bajo,
    `% Pobl. Amenaza Extrema Deslave` = per_landslide_extremo,
    `% Pobl. Amenaza Alta Deslave` = per_landslide_alto,
    `% Pobl. Amenaza Media Deslave` = per_landslide_medio,
    `% Pobl. Amenaza Baja Deslave` = per_landslide_bajo,
    `% Pobl. Amenaza Extrema + Alta Deslave` = landslide_exp_ext_alt,
    `Clase Riesgo Deslave - IVMC` = bi_class,
    `Categoría dominante de vulnerabilidad global` = VUL_P ,
    `% Pobl. en baja vulnerabilidad global` = P_VUL_1 ,  
    `% Pobl. en vulnerabilidad global intermedia` = P_VUL_2 , 
    `% Pobl. en alta vulnerabilidad global` = P_VUL_3 ,
    `% Pobl. en baja presión/diferencial demográfico (D3)` = P_DEMO_1 ,
    `% Pobl. en presión/diferencial demográfico (D3)` = P_DEMO_2, 
    `% Pobl. en alta presión/diferencial demográfico (D3)` =  P_DEMO_3 , 
    `% Pobl. en baja sensibilidad (D1)` = P_SENS_1, 
    `% Pobl. en sensibilidad intermedia (D1)` = P_SENS_2, 
    `% Pobl. en alta sensibilidad (D1)` =  P_SENS_3 , 
    `% Pobl. en baja capacidad adaptativa (D2)` = P_ADAP_1 , 
    `% Pobl. en capacidad adaptativa intermedia (D2)` = P_ADAP_2, 
    `% Pobl. en alta capacidad adaptativa (D2)` = P_ADAP_3 )

names(data_bivariate_eq)

equake_risk_dist_table <- data_bivariate_eq %>% 
  as_data_frame() %>% 
  st_drop_geometry() %>% 
  arrange(NA3) %>% 
  select(-c(FCODE, geometry)) %>% 
  relocate(NAM, .after = NA3) %>% 
  relocate(c(VUL_P,P_VUL_1,  P_VUL_2, P_VUL_3,P_DEMO_1,P_DEMO_2, P_DEMO_3, 
             P_SENS_1, P_SENS_2, P_SENS_3, P_ADAP_1, P_ADAP_2, P_ADAP_3), .after = bi_class) %>% 
  rename(
    `District Code` = NA3,
    `District Name` = NAM,
    `Población Wpop 2025` = wpop,
    `Vulnerabilidad Sensibilidad` = D1,
    `Vulnerabilidad Adaptabilidad` = D2,
    `Vulnerabilidad Diferencial Demografico` = D3,
    `Pobl. Amenaza Extrema Seísmo` = eq_extremo,
    `Pobl. Amenaza Alta Seísmo` = eq_alto,
    `Pobl. Amenaza Media Seísmo` = eq_medio,
    `Pobl. Amenaza Baja Seísmo` = eq_bajo,
    `% Pobl. Amenaza Extrema Seísmo` = per_eq_extremo,
    `% Pobl. Amenaza Alta Seísmo` = per_eq_alto,
    `% Pobl. Amenaza Media Seísmo` = per_eq_medio,
    `% Pobl. Amenaza Baja Seísmo` = per_eq_bajo,
    `% Pobl. Amenaza Extrema + Alta Inundación` = equake_exp_ext_alt,
    `Clase Riesgo Inundacion - IVMC` = bi_class,
    `Categoría dominante de vulnerabilidad global` = VUL_P ,
    `% Pobl. en baja vulnerabilidad global` = P_VUL_1 ,  
    `% Pobl. en vulnerabilidad global intermedia` = P_VUL_2 , 
    `% Pobl. en alta vulnerabilidad global` = P_VUL_3 ,
    `% Pobl. en baja presión/diferencial demográfico (D3)` = P_DEMO_1 ,
    `% Pobl. en presión/diferencial demográfico (D3)` = P_DEMO_2, 
    `% Pobl. en alta presión/diferencial demográfico (D3)` =  P_DEMO_3 , 
    `% Pobl. en baja sensibilidad (D1)` = P_SENS_1, 
    `% Pobl. en sensibilidad intermedia (D1)` = P_SENS_2, 
    `% Pobl. en alta sensibilidad (D1)` =  P_SENS_3 , 
    `% Pobl. en baja capacidad adaptativa (D2)` = P_ADAP_1 , 
    `% Pobl. en capacidad adaptativa intermedia (D2)` = P_ADAP_2, 
    `% Pobl. en alta capacidad adaptativa (D2)` = P_ADAP_3 )



wb <- createWorkbook()

# Style 1: Header (Blue background, White Bold Text)
header_style <- createStyle(
  fontSize = 11, 
  fontColour = "white", 
  fgFill = "#08519c", 
  halign = "center", 
  valign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

# Style 2: Percentage Columns (e.g. 0.12 -> 12.0%)
pct_style <- createStyle(numFmt = "0.0%") 
pct_style2 <- createStyle(numFmt = "0.0\"%\"")

# Style 3: Number Columns (Comma separator)
num_style <- createStyle(numFmt = "#,##0")

# Style 4: Ratio/Index Columns (3 decimal places)
rat_style <- createStyle(numFmt = "0.000")


data_list <- list(
  flood_risk_dist_table, 
  drought_risk_dist_table,     # Replace with your actual variable name for drought
  landslide_risk_dist_table,
  equake_risk_dist_table
  # Replace with your actual variable name for landslide
)

sheet_names <- c(
  "Riesgo_Inundaciones_SLV", 
  "Riesgo_Sequia_SLV", 
  "Riesgo_Deslizamientos_SLV",
  "Riesgo_Seísmo_SLV"
)


for (i in seq_along(data_list)) {
  
  # A. Extract current data and name for this iteration
  current_df    <- data_list[[i]]
  current_sheet <- sheet_names[i]
  
  print(paste("Processing sheet:", current_sheet)) # Optional progress tracker
  
  # B. Add Sheet & Write Data
  addWorksheet(wb, current_sheet)
  writeData(wb, current_sheet, current_df, startRow = 1, startCol = 1)
  
  # Header Style
  addStyle(wb, current_sheet, header_style, 
           rows = 1, cols = 1:ncol(current_df), gridExpand = TRUE)
  
  # Percent Style (Cols 12-16)
  addStyle(wb, current_sheet, pct_style, 
           rows = 2:(nrow(current_df)+1), cols = 12:16, gridExpand = TRUE)
  # Percent Style (Cols 19:31)
  addStyle(wb, current_sheet, pct_style2, 
           rows = 2:(nrow(current_df)+1), cols = 19:31, gridExpand = TRUE)
  
  # Number Style (Cols 7-11)
  addStyle(wb, current_sheet, num_style, 
           rows = 2:(nrow(current_df)+1), cols = 7:11, gridExpand = TRUE)
  
  # Ratio Style (Cols 3-6)
  addStyle(wb, current_sheet, rat_style, 
           rows = 2:(nrow(current_df)+1), cols = 3:6, gridExpand = TRUE)
  
  # D. Formatting Layout
  freezePane(wb, current_sheet, firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, current_sheet, cols = 1:ncol(current_df), widths = "auto")
}


saveWorkbook(wb, paste0(dir, "tables/SLV_Risk_Assessment.xlsx"), overwrite = TRUE)



## 4.3 Graphs to include in report ----

### 4.3.1 Top 10 Flood Risk % population ----
# Filter for the top 10 districts by percentage risk
top_risk_fl <- data_bivariate_fl %>% 
  filter(bi_class =="3-3") %>% 
  mutate(flood_exp_ext_alt = flood_exp_ext_alt*100) %>% 
  arrange(desc(flood_exp_ext_alt)) %>% 
  slice(1:10)
  
top_10_risk_chart <- ggplot(top_risk_fl, 
       # Reorder the District Name (Y-axis) based on the risk percentage (X-axis) 
       # so the highest risk is at the top.
       aes(x = flood_exp_ext_alt , 
           y = fct_reorder(NAM, flood_exp_ext_alt))
       )+
  
  # Create the bar geometry
  geom_col(fill = "#0072B2", width = 0.6) + 
  
  # Add risk percentage labels to the end of the bars
  geom_text(aes(label = paste0(round(flood_exp_ext_alt), "%")), 
            hjust = -0.1, 
            size = 3,
            col = "#333333") +
  
  # Set chart title and axis labels
  labs(
    title = "Riesgo a Inundaciones: 10 Distritos con Mayor\nAfectación Poblacional en Porcentaje",
    x = "% de población en zonas de amenaza extrema y alta a inundación  (%)",
    y = "Distrito"
  ) +
  
  # Customize the theme (optional, for aesthetics)
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  
  # Ensure the X-axis starts at 0 and doesn't exceed 100
  scale_x_continuous(limits = c(0, max(top_risk_fl$flood_exp_ext_alt) * 1.1),
                     labels = function(x) paste0(x, "%")
                     )

top_10_risk_chart

ggsave(
  filename = paste0(dir, "maps/Top10_Risk_Chart_per_Flood.jpg"), # Saving to your existing maps folder
  plot = top_10_risk_chart,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
)

### 4.3.2 Top 10 flood risk districts Population counts ---- 
# Prepare data for top 10 ranking
top_risk_pop <- data_bivariate_fl %>% 
  mutate(pop_risk_ext_alt = flood_extremo   + flood_alto ) %>% 
  arrange(desc(pop_risk_ext_alt)) %>% 
  slice(1:10)

top_10_risk_pop_chart <- ggplot(top_risk_pop, 
                            # Reorder the District Name (Y-axis) based on the risk percentage (X-axis) 
                            # so the highest risk is at the top.
                            aes(x = pop_risk_ext_alt , 
                                y = fct_reorder(NAM, pop_risk_ext_alt))
)+
  
  # Create the bar geometry
  geom_col(fill = "#0072B2", width = 0.6) + 
  
  # Add risk percentage labels to the end of the bars
  geom_label(aes(label = scales::comma(round(pop_risk_ext_alt,0), accuracy = 1)), 
            hjust = -0.1, 
            size = 3,
            col = "#333333",
            fill = 'white',
            label.size = 0
            ) +     # Removes the black border
  
  # Set chart title and axis labels
  labs(
    title = "Riesgo a Inundaciones: 10 Distritos con Mayor\nAfectación Poblacional Total",
    x = "Población en zonas de amenaza extrema y alta a inundación (hab.)",
    y = "Distrito"
  ) +
  
  # Customize the theme (optional, for aesthetics)
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  # Add separator to X-axis and expand limits slightly for labels
  scale_x_continuous(
    labels = scales::comma, 
    limits = c(0, max(top_risk_pop$pop_risk_ext_alt) * 1.15) # Expand 15% to fit text
  )

top_10_risk_pop_chart

# Save
ggsave(
  filename = paste0(dir, "maps/Top10_Risk_Chart_pop_Flood.jpg"), # Saving to your existing maps folder
  plot = top_10_risk_pop_chart,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
) 

### 4.3.3 Top Flood Risk Districts Vulnerability factors display. ----

# Filter for the top 10 districts by percentage risk
top_risk_fl_vulnprof <- data_bivariate_fl %>% 
  filter(bi_class =="3-3") %>% 
  mutate(flood_exp_ext_alt = flood_exp_ext_alt*100) %>% 
  arrange(desc(flood_exp_ext_alt)) %>% 
  slice(1:10)


top_risk_fl_vulnprof <- top_risk_fl_vulnprof %>% 
  pivot_longer(
    cols = c(P_SENS_3,P_ADAP_1, P_DEMO_3),
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  mutate(Category = factor(Category, 
                           levels = c("P_SENS_3", "P_ADAP_1", "P_DEMO_3"), # Bottom to Top
                           labels = c("% pobl. en alta sensibilidad (D1)", "% pobl. en baja capacidad adaptativa (D2)", 
                                      "% pobl. en alta presión/diferencial demográfico (D3)")))



plot_top10_Risk_fl_vulnprof <- ggplot(top_risk_fl_vulnprof, 
       aes(x = Value,  # <--- CHANGED: Removed '* 100' (Assumes data is already 0-100)
           y = fct_reorder(NAM, flood_exp_ext_alt), 
           fill = Category)) +
  
  geom_col(position = position_dodge( width = 0.8), width = 0.7) +
  
  # 2. Labels
  geom_text(aes(label = ifelse(Value > 1, paste0(round(Value, 0), "%"), "")), 
            position = position_dodge(width = 0.7),
            hjust = -0.2, 
            size = 2, 
            color = "black") +
  
  # 3. Colors
  scale_fill_manual(
    values = c(
      "% pobl. en alta sensibilidad (D1)"                   = "#d95f02",
      "% pobl. en baja capacidad adaptativa (D2)"           = "#7570b3",
      "% pobl. en alta presión/diferencial demográfico (D3)" = "#1b9e77"
    )
  ) +
  
  # 4. Layout
  labs(
    title = "Perfil de Vulnerabilidad en Distritos de Mayor Riesgo a Inundaciones",
    subtitle = "Composición de factores sociales, demográficos y de capacidad",
    x = "Porcentaje de Población (%)",
    y = "Distrito",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  
  # 5. X Axis
  # Since 'x' is now just 'Value' (e.g., 85), the labels will naturally be "85%"
  # --- THIS IS THE FIX ---
  # Forces the legend to have 1 column (vertical stack)
  guides(fill = guide_legend(ncol = 1)) +
  
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))
                     )

plot_top10_Risk_fl_vulnprof

ggsave(
  filename = paste0(dir, "maps/Top10_Risk_Vulnprofile_Flood.jpg"), # Saving to your existing maps folder
  plot = plot_top10_Risk_fl_vulnprof,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
) 


----------------------

### 4.3.4 Population by Risk to Flooding Class ----
# 1. FORCE-DEFINE the palette with explicit names right here
# This ensures R knows exactly which color belongs to "1-1"
custom_pal_red <- c(
  "1-1" = "#ffe0c4", "2-1" = "#fdae61", "3-1" = "#ed771d",
  "1-2" = "#fecbae", "2-2" = "#f8926a", "3-2" = "#ef5d2e",
  "1-3" = "#f49a9a", "2-3" = "#ee6867", "3-3" = "#eb090b"
)

# 2. PREPARE DATA (With an extra safety cleaning step)
risk_class_summary <- data_bivariate_fl %>%
  st_drop_geometry() %>%
  # Safety Step: Remove ANY spaces from the column before grouping
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  # Set factor levels to match the palette names exactly
  mutate(bi_class = factor(bi_class, levels = names(custom_pal_red)))

# 3. PLOT
pop_by_risk_class_fl <- ggplot(risk_class_summary, aes(x = bi_class, y = Total_Population, fill = bi_class)) +
  
  # Bars
  geom_col(width = 0.7) +
  
  # Labels
  geom_text(aes(label = scales::comma(Total_Population, accuracy = 1)), 
            vjust = -0.5, size = 3) +
  
  # Colors: We use the palette we defined in Step 1
  scale_fill_manual(values = custom_pal_red, guide = "none") +
  
  # X-Axis Labels: Map the codes to the text here
  scale_x_discrete(labels = c(
    "1-1" = "Vul Baja\nExp Baja",   "2-1" = "Vul Baja\nExp Media",   "3-1" = "Vul Baja\nExp Alta",
    "1-2" = "Vul Media\nExp Baja",  "2-2" = "Vul Media\nExp Media",  "3-2" = "Vul Media\nExp Alta",
    "1-3" = "Vul Alta\nExp Baja",   "2-3" = "Vul Alta\nExp Media",   "3-3" = "Vul Alta\nExp Alta"
  )) +
  
  # Titles & Theme
  labs(
    title = "Población Total por Clase de Riesgo a Inundaciones",
    subtitle = "Exposición a inundaciones (Exp.) vs Vulnerabilidad (Vul.)",
    x = "Clase de Riesgo (Exposición - Vulnerabilidad)",
    y = "Población Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))

# 4. PRINT
print(pop_by_risk_class_fl)
 
ggsave(
  filename = paste0(dir, "maps/Pop_by_risk_class_Flood.jpg"), # Saving to your existing maps folder
  plot = pop_by_risk_class_fl,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
) 




### 4.3.5 Top 10 Drought Risk % population ----
top_risk_dr <- data_bivariate_dr %>% 
  filter(bi_class == "3-3") %>% 
  mutate(drought_exp_ext_alt = drought_exp_ext_alt * 100) %>% 
  arrange(desc(drought_exp_ext_alt)) %>% 
  slice(1:10)

top_10_risk_chart_dr <- ggplot(top_risk_dr, 
                               aes(x = drought_exp_ext_alt, 
                                   y = fct_reorder(NAM, drought_exp_ext_alt))) +
  
  # Bar geometry (Using a brownish/orange color for Drought)
  geom_col(fill = "orange", width = 0.6) + 
  
  geom_text(aes(label = paste0(round(drought_exp_ext_alt), "%")), 
            hjust = -0.1, size = 3, col = "#333333") +
  
  labs(
    title = "Riesgo a Sequías: 10 Distritos con Mayor\nAfectación Poblacional en Porcentaje",
    x = "% de población en zonas de amenaza extrema y alta a sequía (%)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(limits = c(0, max(top_risk_dr$drought_exp_ext_alt) * 1.1),
                     labels = function(x) paste0(x, "%"))

top_10_risk_chart_dr
# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_per_Drought.jpg"), 
       plot = top_10_risk_chart_dr, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.6 Top 10 Drought risk districts Population counts ---- 
top_risk_pop_dr <- data_bivariate_dr %>% 
  mutate(pop_risk_ext_alt = drought_extremo + drought_alto) %>% # Ensure these columns exist
  arrange(desc(pop_risk_ext_alt)) %>% 
  slice(1:10)

top_10_risk_pop_chart_dr <- ggplot(top_risk_pop_dr, 
                                   aes(x = pop_risk_ext_alt, 
                                       y = fct_reorder(NAM, pop_risk_ext_alt))) +
  
  geom_col(fill = "orange", width = 0.6) + 
  
  geom_label(aes(label = scales::comma(round(pop_risk_ext_alt,0), accuracy = 1)), 
             hjust = -0.1, size = 3, col = "#333333", fill = 'white', label.size = 0) +
  
  labs(
    title = "Riesgo a Sequías: 10 Distritos con Mayor \nAfectación Poblacional Total",
    x = "Población en zonas de amenaza extrema y alta a sequía (hab.)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(labels = scales::comma, 
                     limits = c(0, max(top_risk_pop_dr$pop_risk_ext_alt) * 1.15)
                     )
top_10_risk_pop_chart_dr

# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_pop_Drought.jpg"), 
       plot = top_10_risk_pop_chart_dr, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.7 Top Drought Risk Districts Vulnerability factors display ----
top_risk_dr_vulnprof <- data_bivariate_dr %>% 
  filter(bi_class == "3-3") %>% 
  mutate(drought_exp_ext_alt = drought_exp_ext_alt * 100) %>% 
  arrange(desc(drought_exp_ext_alt)) %>% 
  slice(1:10) %>% 
  pivot_longer(
    cols = c(P_SENS_3, P_ADAP_1, P_DEMO_3),
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  mutate(Category = factor(Category, 
                           levels = c("P_SENS_3", "P_ADAP_1", "P_DEMO_3"), 
                           labels = c("% pobl. en alta sensibilidad (D1)", 
                                      "% pobl. en baja capacidad adaptativa (D2)", 
                                      "% pobl. en alta presión/diferencial demográfico (D3)")))

plot_top10_Risk_dr_vulnprof <- ggplot(top_risk_dr_vulnprof, 
                                      aes(x = Value, y = fct_reorder(NAM, drought_exp_ext_alt), fill = Category)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = ifelse(Value > 1, paste0(round(Value, 0), "%"), "")), 
            position = position_dodge(width = 0.7), hjust = -0.2, size = 2, color = "black") +
  
  scale_fill_manual(
    values = c("% pobl. en alta sensibilidad (D1)" = "#d95f02",
               "% pobl. en baja capacidad adaptativa (D2)" = "#7570b3",
               "% pobl. en alta presión/diferencial demográfico (D3)" = "#1b9e77")) +
  
  labs(
    title = "Perfil de Vulnerabilidad en Distritos de Mayor Riesgo a Sequías",
    subtitle = "Composición de factores sociales, demográficos y de capacidad",
    x = "Porcentaje de Población (%)", y = "Distrito", fill = NULL
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))
                     )
plot_top10_Risk_dr_vulnprof

# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Vulnprofile_Drought.jpg"), 
       plot = plot_top10_Risk_dr_vulnprof, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.8 Population by Risk to Drought Class ----
# Reusing the custom_pal_red defined in your original script
risk_class_summary_dr <- data_bivariate_dr %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bi_class = factor(bi_class, levels = names(custom_pal_red)))

pop_by_risk_class_dr <- ggplot(risk_class_summary_dr, aes(x = bi_class, y = Total_Population, fill = bi_class)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(Total_Population, accuracy = 1)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = custom_pal_red, guide = "none") +
  scale_x_discrete(labels = c(
    "1-1" = "Vul Baja\nExp Baja",   "2-1" = "Vul Baja\nExp Media",   "3-1" = "Vul Baja\nExp Alta",
    "1-2" = "Vul Media\nExp Baja",  "2-2" = "Vul Media\nExp Media",  "3-2" = "Vul Media\nExp Alta",
    "1-3" = "Vul Alta\nExp Baja",   "2-3" = "Vul Alta\nExp Media",   "3-3" = "Vul Alta\nExp Alta"
  )) +
  labs(
    title = "Población Total por Clase de Riesgo a Sequías",
    subtitle = "Exposición a sequías (Exp.) vs Vulnerabilidad (Vul.)",
    x = "Clase de Riesgo (Exposición - Vulnerabilidad)",
    y = "Población Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))
                     )
pop_by_risk_class_dr

# Save
ggsave(filename = paste0(dir, "maps/Pop_by_risk_class_Drought.jpg"), 
       plot = pop_by_risk_class_dr, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")



### 4.3.9 Top 10 Landslide Risk % population ----
top_risk_ls <- data_bivariate_ls %>% 
  filter(bi_class == "3-3") %>% 
  mutate(landslide_exp_ext_alt = landslide_exp_ext_alt * 100) %>% 
  arrange(desc(landslide_exp_ext_alt)) %>% 
  slice(1:10)

top_10_risk_chart_ls <- ggplot(top_risk_ls, 
                               aes(x = landslide_exp_ext_alt, 
                                   y = fct_reorder(NAM, landslide_exp_ext_alt))) +
  
  # Bar geometry (Using a brown/earthy color for Landslides)
  geom_col(fill = "#8c510a", width = 0.6) + 
  
  geom_text(aes(label = paste0(round(landslide_exp_ext_alt), "%")), 
            hjust = -0.1, size = 3, col = "#333333") +
  
  labs(
    title = "Riesgo a Deslizamientos: 10 Distritos con Mayor\nAfectación Poblacional en Porcentaje",
    x = "% de población en zonas de amenaza extrema y alta a deslizamientos (%)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(limits = c(0, max(top_risk_ls$landslide_exp_ext_alt) * 1.1),
                     labels = function(x) paste0(x, "%"))
top_10_risk_chart_ls
# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_per_Landslide.jpg"), 
       plot = top_10_risk_chart_ls, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.10 Top 10 Landslide risk districts Population counts ---- 
top_risk_pop_ls <- data_bivariate_ls %>% 
  mutate(pop_risk_ext_alt = landslide_extremo + landslide_alto) %>% # Ensure these columns exist
  arrange(desc(pop_risk_ext_alt)) %>% 
  slice(1:10)

top_10_risk_pop_chart_ls <- ggplot(top_risk_pop_ls, 
                                   aes(x = pop_risk_ext_alt, 
                                       y = fct_reorder(NAM, pop_risk_ext_alt))) +
  
  geom_col(fill = "#8c510a", width = 0.6) + 
  
  geom_label(aes(label = scales::comma(round(pop_risk_ext_alt,0), accuracy = 1)), 
             hjust = -0.1, size = 3, col = "#333333", fill = 'white', label.size = 0) +
  
  labs(
    title = "Riesgo a Deslizamientos: 10 Distritos con Mayor \nAfectación Poblacional Total",
    x = "Población en zonas de amenaza extrema y alta a deslizamientos (hab.)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(labels = scales::comma, 
                     limits = c(0, max(top_risk_pop_ls$pop_risk_ext_alt) * 1.15))
top_10_risk_pop_chart_ls

# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_pop_Landslide.jpg"), 
       plot = top_10_risk_pop_chart_ls, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.11 Top Landslide Risk Districts Vulnerability factors display ----
top_risk_ls_vulnprof <- data_bivariate_ls %>% 
  filter(bi_class == "3-3") %>% 
  mutate(landslide_exp_ext_alt = landslide_exp_ext_alt * 100) %>% 
  arrange(desc(landslide_exp_ext_alt)) %>% 
  slice(1:10) %>% 
  pivot_longer(
    cols = c(P_SENS_3, P_ADAP_1, P_DEMO_3),
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  mutate(Category = factor(Category, 
                           levels = c("P_SENS_3", "P_ADAP_1", "P_DEMO_3"), 
                           labels = c("% pobl. en alta sensibilidad (D1)", 
                                      "% pobl. en baja capacidad adaptativa (D2)", 
                                      "% pobl. en alta presión/diferencial demográfico (D3)")))

plot_top10_Risk_ls_vulnprof <- ggplot(top_risk_ls_vulnprof, 
                                      aes(x = Value, y = fct_reorder(NAM, landslide_exp_ext_alt), fill = Category)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = ifelse(Value > 1, paste0(round(Value, 0), "%"), "")), 
            position = position_dodge(width = 0.7), hjust = -0.2, size = 2, color = "black") +
  
  scale_fill_manual(
    values = c("% pobl. en alta sensibilidad (D1)" = "#d95f02",
               "% pobl. en baja capacidad adaptativa (D2)" = "#7570b3",
               "% pobl. en alta presión/diferencial demográfico (D3)" = "#1b9e77")) +
  
  labs(
    title = "Perfil de Vulnerabilidad en Distritos de Mayor Riesgo a Deslizamientos",
    subtitle = "Composición de factores sociales, demográficos y de capacidad",
    x = "Porcentaje de Población (%)", y = "Distrito", fill = NULL
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15)))

plot_top10_Risk_ls_vulnprof
# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Vulnprofile_Landslide.jpg"), 
       plot = plot_top10_Risk_ls_vulnprof, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.12 Population by Risk to Landslide Class ----
risk_class_summary_ls <- data_bivariate_ls %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bi_class = factor(bi_class, levels = names(custom_pal_red)))

pop_by_risk_class_ls <- ggplot(risk_class_summary_ls, aes(x = bi_class, y = Total_Population, fill = bi_class)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(Total_Population, accuracy = 1)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = custom_pal_red, guide = "none") +
  scale_x_discrete(labels = c(
    "1-1" = "Vul Baja\nExp Baja",   "2-1" = "Vul Baja\nExp Media",   "3-1" = "Vul Baja\nExp Alta",
    "1-2" = "Vul Media\nExp Baja",  "2-2" = "Vul Media\nExp Media",  "3-2" = "Vul Media\nExp Alta",
    "1-3" = "Vul Alta\nExp Baja",   "2-3" = "Vul Alta\nExp Media",   "3-3" = "Vul Alta\nExp Alta"
  )) +
  labs(
    title = "Población Total por Clase de Riesgo a Deslizamientos",
    subtitle = "Exposición a deslizamientos (Exp.) vs Vulnerabilidad (Vul.)",
    x = "Clase de Riesgo (Exposición - Vulnerabilidad)",
    y = "Población Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))
pop_by_risk_class_ls
# Save
ggsave(filename = paste0(dir, "maps/Pop_by_risk_class_Landslide.jpg"), 
       plot = pop_by_risk_class_ls, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.13Top 10 Seismic Risk % population ----
# Note: I am assuming your exposure column is named 'seismic_exp_ext_alt' 
# and total risk columns are 'seismic_extremo' and 'seismic_alto'.
# If your columns are named 'eq_exp...' or similar, please swap "seismic" for "eq".

top_risk_eq <- data_bivariate_eq %>% 
  filter(bi_class == "3-3") %>% 
  mutate(equake_exp_ext_alt   = equake_exp_ext_alt   * 100) %>% 
  arrange(desc(equake_exp_ext_alt  )) %>% 
  slice(1:10)

top_10_risk_chart_eq <- ggplot(top_risk_eq, 
                               aes(x = equake_exp_ext_alt  , 
                                   y = fct_reorder(NAM, equake_exp_ext_alt))) +
  
  # Bar geometry (Using a Deep Red/Brick color for Seismic to distinguish from Drought)
  geom_col(fill = "#810f7c", width = 0.6) + 
  
  geom_text(aes(label = paste0(round(equake_exp_ext_alt), "%")), 
            hjust = -0.1, size = 3, col = "#333333") +
  
  labs(
    title = "Riesgo Sísmico: 10 Distritos con Mayor\nAfectación Poblacional en Porcentaje",
    x = "% de población en zonas de amenaza sísmica extrema y alta (%)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(limits = c(0, max(top_risk_eq$equake_exp_ext_alt) * 1.1),
                     labels = function(x) paste0(x, "%"))

top_10_risk_chart_eq
# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_per_Seismic.jpg"), 
       plot = top_10_risk_chart_eq, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.14 Top 10 Seismic risk districts Population counts ---- 
top_risk_pop_eq <- data_bivariate_eq %>% 
  # Ensure 'seismic_extremo' and 'seismic_alto' exist in your data_bivariate_eq
  mutate(pop_risk_ext_alt = eq_extremo + eq_alto) %>% 
  arrange(desc(pop_risk_ext_alt)) %>% 
  slice(1:10)

top_10_risk_pop_chart_eq <- ggplot(top_risk_pop_eq, 
                                   aes(x = pop_risk_ext_alt, 
                                       y = fct_reorder(NAM, pop_risk_ext_alt))) +
  
  geom_col(fill = "#810f7c", width = 0.6) + 
  
  geom_label(aes(label = scales::comma(round(pop_risk_ext_alt,0), accuracy = 1)), 
             hjust = -0.1, size = 3, col = "#333333", fill = 'white', label.size = 0) +
  
  labs(
    title = "Riesgo Sísmico: 10 Distritos con Mayor \nAfectación Poblacional Total",
    x = "Población en zonas de amenaza sísmica extrema y alta (hab.)",
    y = "Distrito"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_continuous(labels = scales::comma, 
                     limits = c(0, max(top_risk_pop_eq$pop_risk_ext_alt) * 1.15)
  )
top_10_risk_pop_chart_eq

# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Chart_pop_Seismic.jpg"), 
       plot = top_10_risk_pop_chart_eq, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.15 Top Seismic Risk Districts Vulnerability factors display ----
top_risk_eq_vulnprof <- data_bivariate_eq %>% 
  filter(bi_class == "3-3") %>% 
  mutate(equake_exp_ext_alt = equake_exp_ext_alt * 100) %>% 
  arrange(desc(equake_exp_ext_alt)) %>% 
  slice(1:10) %>% 
  pivot_longer(
    cols = c(P_SENS_3, P_ADAP_1, P_DEMO_3),
    names_to = "Category",
    values_to = "Value"
  ) %>% 
  mutate(Category = factor(Category, 
                           levels = c("P_SENS_3", "P_ADAP_1", "P_DEMO_3"), 
                           labels = c("% pobl. en alta sensibilidad (D1)", 
                                      "% pobl. en baja capacidad adaptativa (D2)", 
                                      "% pobl. en alta presión/diferencial demográfico (D3)")))

plot_top10_Risk_eq_vulnprof <- ggplot(top_risk_eq_vulnprof, 
                                      aes(x = Value, y = fct_reorder(NAM, equake_exp_ext_alt), fill = Category)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = ifelse(Value > 1, paste0(round(Value, 0), "%"), "")), 
            position = position_dodge(width = 0.7), hjust = -0.2, size = 2, color = "black") +
  
  scale_fill_manual(
    values = c("% pobl. en alta sensibilidad (D1)" = "#d95f02",
               "% pobl. en baja capacidad adaptativa (D2)" = "#7570b3",
               "% pobl. en alta presión/diferencial demográfico (D3)" = "#1b9e77")) +
  
  labs(
    title = "Perfil de Vulnerabilidad en Distritos de Mayor Riesgo Sísmico",
    subtitle = "Composición de factores sociales, demográficos y de capacidad",
    x = "Porcentaje de Población (%)", y = "Distrito", fill = NULL
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))
  )
plot_top10_Risk_eq_vulnprof

# Save
ggsave(filename = paste0(dir, "maps/Top10_Risk_Vulnprofile_Seismic.jpg"), 
       plot = plot_top10_Risk_eq_vulnprof, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


### 4.3.16 Population by Risk to Seismic Class ----
# Reusing custom_pal_red. 
risk_class_summary_eq <- data_bivariate_eq %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bi_class = factor(bi_class, levels = names(custom_pal_red)))

pop_by_risk_class_eq <- ggplot(risk_class_summary_eq, aes(x = bi_class, y = Total_Population, fill = bi_class)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(Total_Population, accuracy = 1)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = custom_pal_red, guide = "none") +
  scale_x_discrete(labels = c(
    "1-1" = "Vul Baja\nExp Baja",   "2-1" = "Vul Baja\nExp Media",   "3-1" = "Vul Baja\nExp Alta",
    "1-2" = "Vul Media\nExp Baja",  "2-2" = "Vul Media\nExp Media",  "3-2" = "Vul Media\nExp Alta",
    "1-3" = "Vul Alta\nExp Baja",   "2-3" = "Vul Alta\nExp Media",   "3-3" = "Vul Alta\nExp Alta"
  )) +
  labs(
    title = "Población Total por Clase de Riesgo Sísmico",
    subtitle = "Exposición sísmica (Exp.) vs Vulnerabilidad (Vul.)",
    x = "Clase de Riesgo (Exposición - Vulnerabilidad)",
    y = "Población Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))
  )
pop_by_risk_class_eq

# Save
ggsave(filename = paste0(dir, "maps/Pop_by_risk_class_Seismic.jpg"), 
       plot = pop_by_risk_class_eq, device = "jpeg", width = 8, height = 6, dpi = 300, bg = "white")


## 4.3.17 Mosaic Population by risk class by scenario
# --- Step 1: Create Summary Tables for ALL 4 Hazards ---

risk_class_summary_dr <- data_bivariate_dr %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Hazard = "Sequías") # Add label

risk_class_summary_eq <- data_bivariate_eq %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Hazard = "Sismos")

risk_class_summary_ls <- data_bivariate_ls %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Hazard = "Deslizamientos")

risk_class_summary_fl <- data_bivariate_fl %>%
  st_drop_geometry() %>%
  mutate(bi_class = gsub("\\s+", "", as.character(bi_class))) %>% 
  group_by(bi_class) %>%
  summarise(Total_Population = sum(wpop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Hazard = "Inundaciones")


# 1. Combine data (Ensure this step from the previous code is run)
all_risk_data <- bind_rows(
  risk_class_summary_dr %>% mutate(Hazard = "Sequías"),
  risk_class_summary_eq %>% mutate(Hazard = "Sismos"),
  risk_class_summary_ls %>% mutate(Hazard = "Deslizamientos"),
  risk_class_summary_fl %>% mutate(Hazard = "Inundaciones")
) %>%
  mutate(bi_class = factor(bi_class, levels = names(custom_pal_red)))

# 2. Plot with FIXED scales
mosaic_plot_fixed <- ggplot(all_risk_data, aes(x = bi_class, y = Total_Population, fill = bi_class)) +
  
  geom_col(width = 0.7) +
  
  # CHANGE HERE: Removed 'scales = "free_y"'. 
  # Now all panels share the same Y-axis range automatically.
  facet_wrap(~Hazard, ncol = 2, scales = "free_x") + 
  
  geom_text(aes(label = scales::comma(Total_Population, accuracy = 1)), 
            vjust = -0.5, size = 2.5) +
  
  scale_fill_manual(values = custom_pal_red, guide = "none") +
  
  scale_x_discrete(labels = c(
    "1-1" = "Vul Naja\nExp Baja",   "2-1" = "Vul Baja\nExp Media",   "3-1" = "Vul Baja\nExp Alta",
    "1-2" = "Vul Media\nExp Baja",   "2-2" = "Vul Media\nExp Media",   "3-2" = "Vul Media\nExp Alta",
    "1-3" = "Vul Alta\nExp Baja",  "2-3" = "Vul Alta\nExp Media",  "3-3" = "Vul Alta\nExp Alta"
  )) +
  
  labs(
    title = "Comparación Directa de Población en Riesgo en El Salvador",
    x = NULL,
    y = "Población Total"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    strip.text = element_text(face = "bold", size = 12, color = "#333333"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "cm")
  ) +
  
  # Set a single upper limit for ALL charts based on the absolute maximum value found in the data
  scale_y_continuous(
    labels = scales::comma, 
    limits = c(0, max(all_risk_data$Total_Population) * 1.15), # Adds 15% headroom universally
    expand = c(0,0)
  )

mosaic_plot_fixed

# Save
ggsave(filename = paste0(dir, "maps/Mosaic_Pop_by_Risk_Class.jpg"), 
       plot = mosaic_plot_fixed, 
       width = 14, height = 10, dpi = 300, bg = "white")
 
# TO DO

# Sacar tablas de resultados y graficos para exportar en imagen

# export data_bivariate into gpkg to create maps on qGIS, it is not gonna work better produce maps on R directly
# Generar tablas de resultados en excel con nombres de variables limpios
# Generar maps para los otros subniveles de vulnerabilidad
# Hablar con J Luis a ver como van los otros escenarios
