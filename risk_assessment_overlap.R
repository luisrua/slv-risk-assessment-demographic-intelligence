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

# Check CRS is the same for the 3 layers
crs(vul) == crs(pop)
crs(haz) == crs(pop)
crs(haz) == crs(vul)

# Reproject everything to the raster projection as it is easier to reproject vector layers
vul <- project(vul, crs(pop))
haz <- project(haz, crs(pop))

# Check again
crs(vul) == crs(pop)
crs(haz) == crs(pop)
crs(haz) == crs(vul)

# Strip down haz layer removing unnecessary fields
haz <- haz %>% 
  select(c(risk_flood ,risk_lands, risk_droug, RiskFloodN,
           RiskLandN, RiskDrouN))

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

# 2. PROCESS HAZARD INFORMATION ===============================================

## 2.1 Plot hazard data using categories ----

# Force the specific order of the categories
haz$fl_risk_class <- factor(haz$fl_risk_class, 
                               levels = c("Bajo", "Medio", "Alto", "Extremo"))

haz$ls_risk_class <- factor(haz$ls_risk_class, 
       levels = c("Bajo", "Medio", "Alto", "Extremo"))

haz$dr_risk_class <- factor(haz$ls_risk_class, 
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

landslide_drought <- ggplot(data = haz) +
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

landslide_drought

# Save the three maps
ggsave(filename = paste0(dir,"maps/Flood_Hazard_Categories.png"), plot = flood_hazard, 
       width = 8, height = 6, units = "in", dpi = 300)
ggsave(filename = paste0(dir,"maps/Landslide_Hazard_Categories.png"), plot = landslide_hazard, 
       width = 8, height = 6, units = "in", dpi = 300)
ggsave(filename = paste0(dir,"maps/Drought_Hazard_Categories.png"), plot = landslide_drought, 
       width = 8, height = 6, units = "in", dpi = 300)

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

# Calculate population within each of the tiles intersected with the district layer 
risk_pop <- exactextractr::exact_extract(raster(pop),st_as_sf(risk),
                                              fun ='sum')

# Merge the population counts with the tile layer
risk_pop <- risk_pop %>% 
  cbind(risk,.) %>% 
  rename(wpop_2025 = y)

global(pop, fun = "sum", na.rm = TRUE)
sum(risk_pop$wpop_2025, na.rm = T)

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

## 2.3 Join table to spatial ----

# THESE ARE THE BASE FOR THE LAYERS AND THE EXCEL TABLES.
# They Contain exposure and vulnerability information at district level
risk_layer_fl_dist <- risk_dist_fl_wide
risk_layer_dr_dist <- risk_dist_dr_wide
risk_layer_ls_dist <- risk_dist_ls_wide

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


### --- 3.1.3. Plot Maps  ----
tmap_mode("plot")

# Turning it off switches R to a "flat" mode (GEOS) which is much more forgiving of minor topology errors.
sf::sf_use_s2(FALSE) 

# Remove El Salvador from bkground layer
ab <- ab %>%
  filter(GID_0 != "SLV")

# Layers for high risk districts
high_risk_labels_fl <- data_bivariate_fl %>% 
  select(c(bi_class,NA3, NAM)) %>% 
  filter(bi_class == "3-3")

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

# data_bivariate <- data_bivariate %>% 
#   filter(!is.na(bi_class)) %>% 
#   filter(!grepl("NA", bi_class))

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
    text = "NAM",   # <--- Make sure this matches your column name (e.g., NAM_ADM2)
    size = 0.7,
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
  filename = paste0(dir,"maps/Bivariate_fl_Risk_Map.jpg"), 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)


# Display
print(flood_bivar_map)
print(legend)


## 3.2 Vulnerability IVMC + Exposures ----

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

## 3.3 Map the Vulnerability Dimensions ----
dims_to_map <- c("D1", "D2", "D3", "IVMC")

palettes <- c(c("brewer.purples", "brewer.greens", "brewer.blues", "brewer.oranges"))

titles <- c("Dimensión 1: Vulnerabilidad", 
            "Dimensión 2: Adaptabilidad",
            "Dimensión 3: Diferencial Demográfico",
            "Indice de Vulnerabilidad Climática y Medioambiental - IVMC")


for (i in 1:length(dims_to_map)) {
  
  # Current variables for this iteration
  var_name <- dims_to_map[i]
  pal_name <- palettes[i]
  map_title <- titles[i]
  
  print(paste("Mapping:", var_name))
  
  # Calculate Breaks & Labels Manually ---
    # Get the data column for this dimension
    vals <- data_bivariate[[var_name]]
  
  # Calculate 3 quantile breaks (33%, 66%)
  breaks <- quantile(vals, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  b      <- round(breaks, 3) # Round to 2 decimals
  
  # Create clean labels with symbols
  # 1. Low:  "<= X"
  # 2. Mid:  "X - Y"
  # 3. High: "> Y"
  custom_labels <- c(
    paste0("< ", b[2]),
    paste0(b[2], " - ", b[3]),
    paste0("> ", b[3])
  )
  
  # build the map
  vulnerability_map <- 
    # Context
    tm_shape(ab, bbox = bbox_new) +
    tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
    
    # Main Data
    tm_shape(data_bivariate) +
    tm_polygons(
      fill = var_name,
      
      # Use FIXED style with our custom logical labels
      fill.scale = tm_scale_intervals(
        style = "fixed", 
        breaks = breaks,
        labels = custom_labels, # <--- Insert the custom text here
        values = pal_name,
        value.na = "darkgrey"
      ),
      
      col = "black", 
      lwd = 0.1, 
      col_alpha = 0.5,
      fill.legend = tm_legend(title = "")
    ) +
    
    # Layout
    tm_title(map_title) +
    tm_layout(
      frame = FALSE, 
      bg.color = "#dbf1ff",
      legend.position = c("left", "bottom")
    )
  
  # --- Step C: Save ---
  tmap_save(
    tm = vulnerability_map,
    filename = paste0(dir, "maps/Vulnerability_Map_", var_name, ".jpg"),
    dpi = 300,
    width = 10,
    height = 8
  )
}

## 3.4 Map Flood Hazard Zones ----

flood_colors <- c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c")

# 2. Create the map
flood_hazard_map <- 
  
  # Background
  tm_shape(ab, bbox = bbox_new) +
  tm_polygons(fill = "#f0f0f0", col = "black", lwd = 0.3) +
  
  # Map
  tm_shape(haz_flood) +
  tm_polygons(
    fill = "risk_class",
    col  = "risk_class",   # <--- The Trick: Border matches Fill
    lwd  = 0.5,            # Small width to bridge the gap
    
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
  filename = paste0(dir, "maps/Flood_Hazard_SLV.jpg"),
  dpi = 300,
  width = 10,
  height = 8
)

# Para el IVMC a lo mejor necesito los limites para los tramos de las leyendas y representacion

# 4. WORKING AROUND OUTPUTS ===================================================
## 4.1 Export layers to map risk in QGIS ----

writeVector(vect(data_bivariate), paste0(dir,"layers/risk_assessment/slv_risk_assessment_districts.gpkg"),
            layer = 'floods_dist_bivar',
            overwrite = T)

## 4.2 Export Flood table into excel but clean names and fields ----
names(data_bivariate)

flood_risk_dist_table <- data_bivariate %>% 
  as_data_frame() %>% 
  st_drop_geometry() %>% 
  arrange(NA3) %>% 
  select(-c(bi_x, bi_y, IVMC_jitter, flood_jitter, geometry)) %>% 
  rename(
    `District Code` = NA3,
    `District Name` = NAM,
    `Población Wpop 2025` = wpop_risk,
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
    `Clase Riesgo Inundacion - IVMC` = bi_class)


# Create a blank Workbook
wb <- createWorkbook()

# Add a Sheet
sheet_name <- "Riesgo_Inundaciones_SLV"
addWorksheet(wb, sheet_name)

# Write the Data
writeData(wb, sheet_name, flood_risk_dist_table, startRow = 1, startCol = 1)

# Styles for excel table

# Style 1: Header (Blue background, White Bold Text)
header_style <- createStyle(
  fontSize = 11, 
  fontColour = "white", 
  fgFill = "#08519c", # Blue color from your map
  halign = "center", 
  valign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

# Style 2: Percentage Columns 
# "0.0%" format converts 0.12 to 12.0%
pct_style <- createStyle(numFmt = "0.0%") 

# Style 3: Population/Number Columns (Comma separator)
num_style <- createStyle(numFmt = "#,##0")

# Style 4: Population ratios for vulnerability
rat_style <- createStyle(numFmt = "0.000")

# --- APPLY STYLES ---

# Apply Header Style
addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(flood_risk_dist_table), gridExpand = TRUE)

# Apply Percent Style to the specific columns (indices 11 to 14)
addStyle(wb, sheet_name, pct_style, rows = 2:(nrow(flood_risk_dist_table)+1), cols = 12:16, gridExpand = TRUE)

# Apply Number Style to Population columns (indices 7, 8, 9, 10)
addStyle(wb, sheet_name, num_style, rows = 2:(nrow(flood_risk_dist_table)+1), cols =  7:11, gridExpand = TRUE)

# Apply Ratio Style to Vulnerability columns (indices ., 4, 5, 6)
addStyle(wb, sheet_name, rat_style, rows = 2:(nrow(flood_risk_dist_table)+1), cols = 3:6, gridExpand = TRUE)

# Freeze the top row so it stays visible when scrolling
freezePane(wb, sheet_name, firstRow = TRUE)

# Auto-adjust column widths to fit the text
setColWidths(wb, sheet_name, cols = 1:ncol(flood_risk_dist_table), widths = "auto")

# Save the file
saveWorkbook(wb, paste0(dir, "tables/SLV_Risk_Assessment.xlsx"), overwrite = TRUE)


## 4.3 Graphics to include in report ----

### 4.3.1 Top 10 Flood Risk % population ----
# Filter for the top 10 districts by percentage risk
top_risk <- data_bivariate %>% 
  filter(bi_class %in% c("3-3","2-3","3-2")) %>% 
  mutate(flood_exp_ext_alt = flood_exp_ext_alt*100)
  
top_10_risk_chart <- ggplot(top_risk, 
       # Reorder the District Name (Y-axis) based on the risk percentage (X-axis) 
       # so the highest risk is at the top.
       aes(x = flood_exp_ext_alt , 
           y = fct_reorder(NAM, flood_exp_ext_alt))
       )+
  
  # Create the bar geometry
  geom_col(fill = "#0072B2", width = 0.7) + 
  
  # Add risk percentage labels to the end of the bars
  geom_text(aes(label = paste0(round(flood_exp_ext_alt), "%")), 
            hjust = -0.1, 
            size = 3,
            col = "#333333") +
  
  # Set chart title and axis labels
  labs(
    title = "% de Población en zona de amenaza extrema y alta de inundaciones \nen los 10 distritos con mayor riesgo",
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
  scale_x_continuous(limits = c(0, max(top_risk$flood_exp_ext_alt) * 1.1),
                     labels = function(x) paste0(x, "%"))

top_10_risk_chart

ggsave(
  filename = paste0(dir, "maps/Top10_Risk_Chart.jpg"), # Saving to your existing maps folder
  plot = top_10_risk_chart,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
)

### 4.3.2 Top 10 flood risk districts Population counts ---- 
top_risk_pop <- top_risk %>% 
  mutate(pop_risk_ext_alt = flood_extremo   + flood_alto )

top_10_risk_pop_chart <- ggplot(top_risk_pop, 
                            # Reorder the District Name (Y-axis) based on the risk percentage (X-axis) 
                            # so the highest risk is at the top.
                            aes(x = pop_risk_ext_alt , 
                                y = fct_reorder(NAM, pop_risk_ext_alt))
)+
  
  # Create the bar geometry
  geom_col(fill = "orange", width = 0.7) + 
  
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
    title = "Población en zona de amenaza extrema y alta de inundaciones \nen los 10 distritos con mayor riesgo",
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
  filename = paste0(dir, "maps/Top10_Risk_Chart_pop.jpg"), # Saving to your existing maps folder
  plot = top_10_risk_pop_chart,
  device = "jpeg",
  width = 8,    # Width in inches
  height = 6,   # Height in inches
  dpi = 300,    # High resolution for reports
  bg = "white"  # CRITICAL: Ensures white background instead of transparent/black
) 

### 4.3.3 Top Flood Risk Districts Vulnerability factors display.


# Graph 
# TO DO

# Sacar tablas de resultados y graficos para exportar en imagen

# export data_bivariate into gpkg to create maps on qGIS, it is not gonna work better produce maps on R directly
# Generar tablas de resultados en excel con nombres de variables limpios
# Generar maps para los otros subniveles de vulnerabilidad
# Hablar con J Luis a ver como van los otros escenarios
