


# ---------------------------------------------------------------------------
#
#      SCRIPT: Interactive Map
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   Upload Libraries
# ---------------------------------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(leafsync)
library(RColorBrewer)
library(mapview)

# INFO about plotting: https://r-spatial.github.io/mapview/articles/

# ---------------------------------------------------------------------------
#  Read in Reach Data
# ---------------------------------------------------------------------------

# ---------------------- reaches data ------------
reaches_path = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/Data/Reaches/Reaches.shp"
reaches <- sf::st_read(reaches_path) # this shapefile does not show up properly
reaches <- sf::st_transform(reaches, 4326)
# ------------------- combine with Habitat Quality Scores data -----------
Habitat_Quality_Scores_factors = Habitat_Quality_Scores
# ------------------- convert scores to factors -----------
#"Spring.Chinook.Reach", "Steelhead.Reach", "Bull.Trout.Reach"
# factor_vars= c( "BankStability_score" , "ChannelStability_score" ,  "Stability_Mean" ,  "CoarseSubstrate_score","Cover-Wood_score","Flow-SummerBaseFlow_score"  ,"Off-Channel-Floodplain_score","Off-Channel-Side-Channels_score", "PoolQuantity&Quality_score" , "Riparian-Disturbance_score","Riparian-CanopyCover_score" ,"Riparian_Mean" ,   "Temperature-Rearing_score", "HQ_Score_Restoration", "HQ_Score_Protection")
#for(factor_x in factor_vars){ Habitat_Quality_Scores_factors[,factor_x] = as.factor( Habitat_Quality_Scores_factors[,factor_x]) }

Habitat_Quality_Scores_factors$Spring.Chinook.Reach = as.factor( Habitat_Quality_Scores_factors$Spring.Chinook.Reach )
Habitat_Quality_Scores_factors$Steelhead.Reach = as.factor( Habitat_Quality_Scores_factors$Steelhead.Reach )
Habitat_Quality_Scores_factors$Bull.Trout.Reach = as.factor( Habitat_Quality_Scores_factors$Bull.Trout.Reach )
Habitat_Quality_Scores_factors$BankStability_score = as.factor( Habitat_Quality_Scores_factors$BankStability_score )
Habitat_Quality_Scores_factors$ChannelStability_score = as.factor( Habitat_Quality_Scores_factors$ChannelStability_score )
Habitat_Quality_Scores_factors$Stability_Mean = as.factor( Habitat_Quality_Scores_factors$Stability_Mean )
Habitat_Quality_Scores_factors$CoarseSubstrate_score = as.factor( Habitat_Quality_Scores_factors$CoarseSubstrate_score )
Habitat_Quality_Scores_factors$`Cover-Wood_score` = as.factor( Habitat_Quality_Scores_factors$`Cover-Wood_score` )
Habitat_Quality_Scores_factors$`Flow-SummerBaseFlow_score` = as.factor( Habitat_Quality_Scores_factors$`Flow-SummerBaseFlow_score` )
Habitat_Quality_Scores_factors$`Off-Channel-Floodplain_score` = as.factor( Habitat_Quality_Scores_factors$`Off-Channel-Floodplain_score` )
Habitat_Quality_Scores_factors$`Off-Channel-Side-Channels_score` = as.factor( Habitat_Quality_Scores_factors$`Off-Channel-Side-Channels_score` )
Habitat_Quality_Scores_factors$`PoolQuantity&Quality_score` = as.factor( Habitat_Quality_Scores_factors$`PoolQuantity&Quality_score` )
Habitat_Quality_Scores_factors$`Riparian-Disturbance_score` = as.factor( Habitat_Quality_Scores_factors$`Riparian-Disturbance_score` )
Habitat_Quality_Scores_factors$`Riparian-CanopyCover_score` = as.factor( Habitat_Quality_Scores_factors$`Riparian-CanopyCover_score` )
Habitat_Quality_Scores_factors$Riparian_Mean = as.factor( Habitat_Quality_Scores_factors$Riparian_Mean )
Habitat_Quality_Scores_factors$`Temperature-Rearing_score` = as.factor( Habitat_Quality_Scores_factors$`Temperature-Rearing_score` )
Habitat_Quality_Scores_factors$HQ_Score_Restoration = as.factor( Habitat_Quality_Scores_factors$HQ_Score_Restoration )
Habitat_Quality_Scores_factors$HQ_Score_Protection = as.factor( Habitat_Quality_Scores_factors$HQ_Score_Protection )

# -------- merge reach spatial data with habitat data --------------
reaches_data = merge(reaches, Habitat_Quality_Scores_factors, by = "ReachName") 
# ------- remove columns we don't want ------
reaches_data = subset (reaches_data, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR,
                                             Length_mi,Length_m,Basin.y))

# ---------------------- color palettes for display --------------
color_palette_x = c("red", "yellow","forestgreen")
color_palette_continuous = brewer.pal(9, 'YlGnBu')
color_palette_x_YES_NO = brewer.pal(3, 'BuPu')
color_palette_x_YES_NO = c("#6C0586", "#FBFF68")

# ---------------------------------------------------------------------------
#
#  PLOT SINGLE Plots 
#
# ---------------------------------------------------------------------------

# THIS prints all the attributes you can map
print(names(reaches_data))

# ----------------------------------------------------------
#     plot FACTOR/SCORE variable 
# ----------------------------------------------------------

# ------ ENTER the attribute to print here ------
attribute_1 = "Riparian-CanopyCover_score"
attribute_1 = "Cover-Wood_score"

# --- simple version ---:
mapview(reaches_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# --- version where you can turn 1, 3, 5 on and off ---:
mapview(reaches_data, zcol = attribute_1,   burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))

# ----------------------------------------------------------
#     plot TWO FACTOR/SCORE variable 
# ----------------------------------------------------------

# --- simple version ---:
attribute_1 = "HQ_Score_Restoration"
attribute_2 = "HQ_Score_Protection"

mapview(reaches_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap")) +
  mapview(reaches_data, zcol = attribute_2, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
          color= color_palette_x) 
# Note: helpful for plotting factors: https://github.com/r-spatial/mapview/issues/240


# ----------------------------------------------------------
#     plot CONTINUOUS variable 
# ----------------------------------------------------------
# ENTER the attribute to print here
attribute_1 = "HQ_Pct"
mapview(reaches_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), 
        color= color_palette_continuous, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))


# ---------------------------------------------------------------------------
#
#  PLOT Multiple Plots and Compare
#
# ---------------------------------------------------------------------------

# --------------- CHOOSE the four attributres to plot ----------
attribute_1 = "HQ_Score_Restoration"
attribute_2 = "HQ_Score_Protection"
attribute_3 = "Spring.Chinook.Reach"
attribute_4 = "Steelhead.Reach"

m1 <- mapview(reaches_data, zcol = attribute_1, lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m2 <- mapview(reaches_data, zcol = attribute_2,  lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m3 <- mapview(reaches_data, zcol = attribute_3, burst = TRUE, color = color_palette_x_YES_NO, legend=TRUE,
              map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m4 <- mapview(reaches_data, zcol = attribute_4, burst = TRUE, color = color_palette_x_YES_NO,
              map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery")   )
# -------- orientation of map tiles ------
print( paste(attribute_1,attribute_2, sep="     "))
print( paste(attribute_3,attribute_4, sep="     "))


sync(m1, m2, m3, m4) # 4 panels synchronised

# --------------- CHOOSE the four attributres to plot ----------
attribute_1 = "CoarseSubstrate_score"
attribute_2 = "Cover-Wood_score"
attribute_3 = "PoolQuantity&Quality_score"
attribute_4 = "Off-Channel-Floodplain_score"

m1 <- mapview(reaches_data, zcol = attribute_1,  legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m2 <- mapview(reaches_data, zcol = attribute_2,   legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m3 <- mapview(reaches_data, zcol = attribute_3,  legend=TRUE,
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m4 <- mapview(reaches_data, zcol = attribute_4,
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))


# -------- orientation of map tiles ------
print( paste(attribute_1,attribute_2, sep="     "))
print( paste(attribute_3,attribute_4, sep="     "))


sync(m1, m2, m3, m4) # 4 panels synchronised

# --------------- CHOOSE the four attributres to plot ----------
attribute_1 = "CoarseSubstrate_score"
attribute_2 = "Cover-Wood_score"
attribute_3 = "PoolQuantity&Quality_score"
attribute_4 = "Off-Channel-Floodplain_score"

m1 <- mapview(reaches_data, zcol = attribute_1, lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m2 <- mapview(reaches_data, zcol = attribute_2,  lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m3 <- mapview(reaches_data, zcol = attribute_3, burst = TRUE,  legend=TRUE,
              color = color_palette_x,  map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m4 <- mapview(reaches_data, zcol = attribute_4,  burst='STUSPS',
              color = color_palette_x,  map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery")   )


