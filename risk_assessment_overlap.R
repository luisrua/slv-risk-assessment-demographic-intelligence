### RISK ASSESSMENT CALCULATION - SIF Project El Salvador ###


## Luis de la Rua - Dec 2025

# SETTINGS =====================================================================

# libraries loaded from the script below that also deals with conflicts

source("setup.R")

# Additional libraries
library(tmap)
library(biscale)
library(cowplot)
library(stringi) # to clean latin encoding

# Paths

dir <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/SLV/" # main folder
layers <- paste0(dir,"layers/") # GIS input
temp <- paste0(dir, "temp/") # temporary files and trash

# 1. IMPORT ALL THE ELEMENTS ==================================================
# Vulnerability by District with all the 3 dimensions and composed index IVMC
vul <- vect(paste0(layers,"indices de vulnerabilidad slv/indices VMC.shp"))

vul<- vul %>% 
mutate(across(where(is.character), function(x) {
  # Try to repair common Spanish encoding issues (Latin1 -> UTF-8)
  fixed_text <- iconv(x, from = "latin1", to = "UTF-8")
  # If iconv fails (returns NA), keep the original text but strip bad bytes
  ifelse(is.na(fixed_text), iconv(x, to = "UTF-8", sub = ""), fixed_text)
}))

# Population input - use Worldpop 2025 for the moment
# https://hub.worldpop.org/geodata/summary?id=73247
pop <- rast(paste0(layers,"slv_pop_2025_CN_100m_R2025A_v1.tif"))

# Hazard Areas from Google embedding and AI to determine 
haz_flood <- vect(paste0(layers,"capas susceptibilidad/Susceptibilidad_Inundación_SV_dc05251724.shp"))

# Check CRS is the same for the 3 layers
crs(vul) == crs(pop)
crs(haz_flood) == crs(pop)
crs(haz_flood) == crs(vul)

# Reproject everything to the raster projection as it is easier to reproject vector layers
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
    name = "Nivel de Amenaza" # Updated legend title to Spanish
  ) +
  theme_minimal() +
  labs(title = "Evaluación de Amenaza de Inundación",
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

# Merge with district layer to 
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

# writeVector(risk_layer_dist,
#             paste0(layers,"risk_assessment/slv_risk_assessment_districts.gpkg"),
#             layer ='floods',
#             overwrite=T)

# Layer at tile level (for hotspots mapping)
# I can use this to highlight spots where there is population exposed that
# included in vulnerable districts
risk_layer_tile <- haz_flood_pop %>% 
  rename(pop_exp_flood = wpop_2025)

# writeVector(risk_layer_tile, 
#             paste0(layers,"risk_assessment/slv_risk_assessment_tile.gpkg"),
#             layer ='floods_tile',
#             overwrite=T)

# Export table in excel
# Clean the data encoding

# risk_layer_clean <- risk_layer_dist %>%
#   st_drop_geometry() %>%
#   as.data.frame() %>%
#   mutate(across(where(is.character), function(x) {
#     # Try to repair common Spanish encoding issues (Latin1 -> UTF-8)
#     fixed_text <- iconv(x, from = "latin1", to = "UTF-8")
#     # If iconv fails (returns NA), keep the original text but strip bad bytes
#     ifelse(is.na(fixed_text), iconv(x, to = "UTF-8", sub = ""), fixed_text)
#   }))

write.xlsx(as.data.frame(risk_layer_tile),
           file = paste0(dir,"tables/slv_risk_assessment_districts.xlsx"),
           sheetName = "flood_risk",
           overwrite = TRUE)

# 3. MAPPING RESULTS ===========================================================

## 3.1 Flood Risk Bivariate Map ----
# --- Prepare Data & Palette ---

# Importing lac admin boundaries 
ab <- vect(paste0(dir,"layers/lac_ab_pol_4326.gpkg"), layer = "lac_ab_pol_4326" )
ab <-  st_make_valid(st_as_sf(ab))
plot(ab)

# Testing custom palette
# This palette moves from Light Grey (Low-Low) to Blue (High Flood) and Orange (High Vuln),
# meeting at Red (High Risk) in the top-right corner (3-3).
custom_pal_red <- c(
  "1-1" = "#ffe0c4", # Low Flood, Low Vuln (Grey)
  "2-1" = "#fdae61",
  "3-1" = "#ed771d", # High Flood, Low Vuln (Blue)
  "1-2" = "#fecbae",
  "2-2" = "#f8926a",
  "3-2" = "#ef5d2e",
  "1-3" = "#f49a9a", # Low Flood, High Vuln (Orange/Pink)
  "2-3" = "#ee6867",
  "3-3" = "#eb090b"  # High Flood, High Vuln (RED - High Risk)
)

# --- 1. Prepare Data & Palette (Same as before) ---
risk_layer_biv <- risk_layer_dist %>% 
  mutate(flood_exp_ext_alt = per_flood_extremo + per_flood_alto,
         IVMC = replace_na(IVMC, 0))

# Add a tiny random number to the problematic columns
risk_layer_biv <- risk_layer_biv %>%
  mutate(
    # factor = 0.001 adds microscopic noise to separate identical values
    IVMC_jitter = jitter(IVMC, factor = 0.001), 
    flood_jitter = jitter(flood_exp_ext_alt, factor = 0.001)
  )

data_bivariate <- bi_class(st_as_sf(risk_layer_biv),
                           x = flood_jitter,
                           y = IVMC_jitter,
                           style = "fisher",
                           dim = 3)

# --- 2. Create High-Res Legend (Same as before) ---
legend <- bi_legend(pal = custom_pal_red,
                    dim = 3,
                    xlab = "Higher Flood Exposure",
                    ylab = "Higher Vulnerability",
                    size = 12) + # Base size
  theme(
    axis.title.x = element_text(size = 18), # Customize X label
    axis.title.y = element_text(size = 18, angle = 90)  # Customize Y label
  )

ggsave(filename = "flood_biv_legend.png", plot = legend, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

# --- 3. Plot Map  ---
tmap_mode("plot")

# Turning it off switches R to a "flat" mode (GEOS) which is much more forgiving of minor topology errors.
sf::sf_use_s2(FALSE) 

# Remove El Salvador from bkground layer
ab <- ab %>%
  filter(GID_0 != "SLV")

# Layers for high risk districts
high_risk_labels <- data_bivariate %>% 
  filter(bi_class == "3-3")
# Increase bbox to fit labels at the edges of the map# 1. Create a slightly larger bounding box (e.g., 5% bigger)
bbox_new <- st_bbox(data_bivariate) # Get original box

# Manually expand the limits (xmin, ymin, xmax, ymax)
# This calculates the width/height and subtracts/adds 5% to the edges
x_range <- bbox_new["xmax"] - bbox_new["xmin"]
y_range <- bbox_new["ymax"] - bbox_new["ymin"]

bbox_new["xmin"] <- bbox_new["xmin"] - (0.05 * x_range)
bbox_new["xmax"] <- bbox_new["xmax"] + (0.05 * x_range)
bbox_new["ymin"] <- bbox_new["ymin"] - (0.05 * y_range)
bbox_new["ymax"] <- bbox_new["ymax"] + (0.05 * y_range)

# Set the map
flood_bivar_map <-
  # Background map
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  # Main bivariate map
  tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(values = custom_pal_red, value.na = "grey"),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) + 
  # Labels for high risk districts
  tm_shape(high_risk_labels) +
  tm_labels_highlighted(
    text = "NAM",   # <--- Make sure this matches your column name (e.g., NAM_ADM2)
    size = 0.7,
    col = "black",
      bgcol = "white",  
      bgcol_alpha = 0.7,
    options = opt_tm_labels(just = "center") 
    ) +
  
  tm_title("Flood Exposure vs Vulnerability") + 
  tm_logo("flood_biv_legend.png", height = 5.5, position = c("left", "bottom")) +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

flood_bivar_map

tmap_save(
  tm = flood_bivar_map, 
  filename = paste0(dir,"maps/Bivariate_fl_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)


# Display
print(flood_bivar_map)
print(legend)


## 3.2 Vulnerability IVMC and Exposure Maps ----

# We use 'unname' so tmap maps the 1st color to "1", 2nd to "2", etc.
clean_risk_pal <- unname(c(
  custom_pal_red["1-1"], 
  custom_pal_red["2-1"], 
  custom_pal_red["3-1"]
))

clean_vuln_pal <- unname(c(
  custom_pal_red["1-1"], 
  custom_pal_red["1-2"], 
  custom_pal_red["1-3"]
))

# 1. Use the labels that biscale already created for you
# We need these separate columns to make the side-by-side maps
data_bivariate$bi_x <- str_sub(data_bivariate$bi_class, 1, 1)
data_bivariate$bi_y <- str_sub(data_bivariate$bi_class, 3, 3)

# 1. Get the breaks object
breaks <- bi_class_breaks(data_bivariate, 
                          x = flood_jitter,   # Your actual Risk column name
                          y = IVMC_jitter, # Your actual Vuln column name
                          style = "fisher",        # Must match your previous code
                          dim = 3)

exp_labels_text <- breaks$bi_x 
vuln_labels_text <- breaks$bi_y

# Trim label texts to remove negative values
exp_labels_text[1] <- 0 
vuln_labels_text[1] <- 0 

# Verify they look right
print(exp_labels_text) 
print(vuln_labels_text)
# Should output: [1] "0-0.0821" "0.0821-0.327" "0.327-0.67"

# 2. Map Flood Risk (using the auto-generated labels)
map_flood_exp <- 
  tm_shape(ab, bbox = data_bivariate) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_x",
    fill.scale = tm_scale_categorical(
      values = clean_risk_pal,         
      labels = exp_labels_text        # <--- Use the text directly
    ),
    fill.legend = tm_legend(title = "Flood Exposure"),
    col = "black", lwd = 0.1
  ) +
  tm_title("Component 1: Flood Exposure (pop ratio)") +
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

# 3. Map Vulnerability (using the auto-generated labels)
map_vuln <- 
  tm_shape(ab, bbox = data_bivariate) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_y",
    fill.scale = tm_scale_categorical(
      values = clean_vuln_pal,         
      labels = vuln_labels_text        # <--- Use the text directly
    ),
    fill.legend = tm_legend(title = "Vulnerability Score - IVMC"),
    col = "black", lwd = 0.1
  ) +
  tm_title("Component 2: Vulnerability - IVMC") + 
  tm_layout(
    frame = FALSE,
    bg.color = "#dbf1ff"  # Light blue "Sea" color
    # inner.margins = c(0.1, 0.1, 0.1, 0.1) # Optional: Adds space between map and edge
  )

# 4. Display Side-by-Side
final_comparison <- tmap_arrange(map_flood_exp, map_vuln, ncol = 2)
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

## 3.3 Map the 3 Vulnerability Dimensions 
dist_to_map <- c("D1", "D2", "D3")

palettes <- c(c("Purples", "Greens", "Oranges"))

titles <- c("Dimensión 1: Vulnerabilidad", 
            "Dimensión 2: Adaptabilidad",
            "Dimensión 3: Diferencial Demográfico")

for (i in 1:length(dims_to_map)) {
  
  # Current variables for this iteration
  var_name <- dims_to_map[i]
  pal_name <- palettes[i]
  map_title <- titles[i]
  
  print(paste("Mapping:", var_name))
  
  # Build the Map
  vulnerability_map <- 
    # --- Context Layer (Sea & Neighbors) ---
    tm_shape(ab, bbox = bbox_new) + # Uses your expanded bbox
    tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
    
    # --- Main Data Layer ---
    tm_shape(data_bivariate) +
    tm_polygons(
      fill = var_name,
      
      # Quantile Scale with Specific Palette
      fill.scale = tm_scale_intervals(
        style = "quantile", 
        n = 5,                 # 5 classes (Quintiles)
        values = pal_name,     # The unique color ramp
        value.na = "transparent"
      ),
      
      col = "black", 
      lwd = 0.1, 
      col_alpha = 0.5,
      fill.legend = tm_legend(title = "") # Clean legend
    ) +
    
    # --- Layout ---
    tm_title(map_title) +
    tm_layout(
      frame = FALSE, 
      bg.color = "#dbf1ff",
      legend.position = c("left", "bottom")
    )
}


### 3.3.1 D1 Vulnerabilidad
map_viln_d1 <-  
  tm_shape(ab, bbox = data_bivariate) +
  tm_polygons(
    fill = "#f0f0f0",  
    col = "black",      
    lwd = 0.3           
  ) +
  tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_y",
    fill.scale = tm_scale_categorical(
      values = clean_vuln_pal,         
      labels = vuln_labels_text        # <--- Use the text directly
    ),
    fill.legend = tm_legend(title = "Dimensión1: Vulnerabilidad"),
    col = "black", lwd = 0.1
  ) 

# 4. WORKING AROUND OUTPUTS ===================================================
## 4.1 Export layers to map risk in QGIS ----
writeVector(vect(data_bivariate), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'floods_dist_bivar',
            overwrite = T)

# TO DO
# Mejorar mapa bivariable. Incluir fronteras paises alrededor, bkground azul para visualizar mar
# anadir etiquetas en distritos alto riesgo

# Sacar tablas de resultados y graficos para exportar en imagen

# export data_bivariate into gpkg to create maps on qGIS, it is not gonna work better produce maps on R directly
# Generar tablas de resultados en excel con nombres de variables limpios
# Generar maps para los otros subniveles de vulnerabilidad
# Hablar con J Luis a ver como van los otros escenarios
