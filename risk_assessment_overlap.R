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


# 3. MAPPING RESULTS ===========================================================
## 3.1 Trying Bivariate choropleth map for districts ----
# Load additional libraries
library(cowplot)
# install.packages("biscale")
library(biscale)
library(cowplot)

# --- 1. Prepare Data & Palette ---
# Field exposure we are merging % of pop in Extremo and Alto tiles
risk_layer_biv <- risk_layer_dist %>% 
  mutate(flood_exp_ext_alt = per_flood_extremo + per_flood_alto,
         IVMC = replace_na(IVMC,0))

# Create Bivariate field to represent both vuln and exposure trends
data_bivariate <- bi_class(st_as_sf(risk_layer_biv),
                           x = flood_exp_ext_alt,
                           y = IVMC,
                           style = "fisher",
                           dim = 3)
  
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

# --- 2. Create and Save High-Res Legend ---

# Generate the legend with biscale
legend <- bi_legend(pal = custom_pal_red,
                    dim = 3,
                    xlab = "Higher Flood Exposure",
                    ylab = "Higher Vulnerability",
                    size = 8)

# Save legend as a high-res transparent PNG
# We use a tempfile so we don't clutter your working directory
legend_path <- tempfile(fileext = ".png")

ggsave(filename = legend_path, 
       plot = legend, 
       bg = "transparent", 
       width = 4, height = 4, units = "in", 
       dpi = 500) # dpi=500 ensures it is crisp when zoomed/printed

# --- 3. Plot Map with tmap Native Options ---
tmap_mode("plot") # Use "view" for interactive, "plot" for static

It looks like you have upgraded to tmap v4, which was a major release that changed many function arguments (e.g., palette is now values, main.title is now tm_title()).

Here is the updated code rewritten for tmap v4 syntax while keeping the high-resolution legend fix.

3.1 Corrected Code (tmap v4)
R

## 3.1 Bivariate choropleth map (tmap v4 compatible) ----
library(tmap)
library(biscale)
library(ggplot2)
library(dplyr)
library(sf)

# --- 1. Prepare Data & Palette (Same as before) ---
risk_layer_biv <- risk_layer_dist %>% 
  mutate(flood_exp_ext_alt = per_flood_extremo + per_flood_alto,
         IVMC = replace_na(IVMC, 0))

data_bivariate <- bi_class(st_as_sf(risk_layer_biv),
                           x = flood_exp_ext_alt,
                           y = IVMC,
                           style = "fisher",
                           dim = 3)

custom_pal_red <- c(
  "1-1" = "#ffe0c4", "2-1" = "#fdae61", "3-1" = "#ed771d",
  "1-2" = "#fecbae", "2-2" = "#f8926a", "3-2" = "#ef5d2e",
  "1-3" = "#f49a9a", "2-3" = "#ee6867", "3-3" = "#eb090b"
)

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

legend_path <- tempfile(fileext = ".png")
ggsave(filename = legend_path, plot = legend, bg = "transparent", 
       width = 4, height = 4, units = "in", dpi = 500)

# --- 3. Plot Map (Updated for v4) ---
tmap_mode("plot")

bivar_map <- tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_class",
    fill.scale = tm_scale_categorical(values = custom_pal_red, value.na = "grey"),
    fill.legend = tm_legend(show = FALSE),
    col = "black",
    col_alpha = 0.5,
    lwd = 0.1
  ) +
  tm_title("Flood Exposure vs Vulnerability") + 
  tm_logo(legend_path, height = 5.5, position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

tmap_save(
  tm = bivar_map, 
  filename = "Bivariate_Risk_Map.jpg", 
  dpi = 300,        # 300 is standard print quality. Use 600 for high-res.
  width = 10,       # Width in inches
  height = 8        # Height in inches
)




# 3. Create the Main Map
# We use the 'bi_class' column as a Categorical variable.
# We map the colors manually using palette = custom_pal_red.
flood_bivar_map <- tm_shape(data_bivariate) +
  tm_polygons("bi_class",
              style = "cat",                  # Treat as categories
              palette = custom_pal_red,       # Use your specific hex codes
              border.col = "black",           # Border color
              border.alpha = 0.5,             # Make borders slightly transparent
              lwd = 0.1,                      # Line width (equivalent to size=0.01)
              title = "Risk Class",
              showNA = TRUE,                  # Show NAs
              colorNA = "grey") +             # Color for NAs
  tm_layout(main.title = "Flood Exposure vs Vulnerability",
            frame = FALSE,                    # Remove the box around the map
            legend.show = FALSE)              # Hide the default categorical legend

# 4. Create the Bivariate Legend using biscale
# tmap doesn't have a native "bivariate matrix" legend generator, 
# so we stick with the excellent one from biscale.
legend <- bi_legend(pal = custom_pal_red,
                    dim = 3,
                    xlab = "Higher Flood Exposure",
                    ylab = "Higher Vulnerability",
                    size = 8)

# 5. Combine them
# tmap objects can be converted to grid objects (grobs) using tmap_grob()
# allowing them to be used with cowplot just like ggplot objects.
final_plot <- ggdraw() +
  draw_plot(tmap_grob(flood_bivar_map), 0, 0, 1, 1) +
  draw_plot(legend, x = 0.05, y = 0.05, width = 0.20, height = 0.20)

# Display
print(final_plot)

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
                          x = flood_exp_ext_alt,   # Your actual Risk column name
                          y = IVMC, # Your actual Vuln column name
                          style = "fisher",        # Must match your previous code
                          dim = 3)

risk_labels_text <- breaks$bi_x
vuln_labels_text <- breaks$bi_y

# Verify they look right
print(risk_labels_text) 
# Should output: [1] "0-0.0821" "0.0821-0.327" "0.327-0.67"

# 2. Map Flood Risk (using the auto-generated labels)
map_risk <- tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_x",
    fill.scale = tm_scale_categorical(
      values = clean_risk_pal,         
      labels = risk_labels_text        # <--- Use the text directly
    ),
    fill.legend = tm_legend(title = "Flood Exposure"),
    col = "black", lwd = 0.1
  ) +
  tm_title("Component 1: Flood Exposure (pop ratio)")

# 3. Map Vulnerability (using the auto-generated labels)
map_vuln <- tm_shape(data_bivariate) +
  tm_polygons(
    fill = "bi_y",
    fill.scale = tm_scale_categorical(
      values = clean_vuln_pal,         
      labels = vuln_labels_text        # <--- Use the text directly
    ),
    fill.legend = tm_legend(title = "Vulnerability Score"),
    col = "black", lwd = 0.1
  ) +
  tm_title("Component 2: Vulnerability")

# 4. Display Side-by-Side
final_comparison <- tmap_arrange(map_risk, map_vuln, ncol = 2)

# 2. Save it using tmap_save
tmap_save(
  tm = final_comparison, 
  filename = "Risk_vs_Vuln_Comparison.jpg", 
  width = 12,      # Width in inches
  height = 6,      # Height in inches
  units = "in",    # Unit for width/height
  dpi = 300        # Resolution (300 is print quality)
)

# save bivariate map
ggsave(
  filename = "Risk_Vulnerability_Map.jpg",
  plot = final_plot, 
  width = 10,        # Width in inches
  height = 8,        # Height in inches
  dpi = 300,         # High resolution for print/zooming
  bg = "white"       # Ensures the background isn't transparent
)




## Create legend in case we need id later
# 1. Create the legend object (same as before)
legend_plot <- bi_legend(
  pal = custom_pal_red,
  dim = 3,
  xlab = "Higher Flood Risk",
  ylab = "Higher Vuln",
  size = 10  # Increase size slightly for readability
)

# 2. Save it as a temporary transparent PNG
# bg = "transparent" is crucial so it floats nicely over the map
ggsave("legend_overlay.png", plot = legend_plot, 
       width = 3, height = 3, dpi = 300, bg = "transparent")
