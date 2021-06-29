
# -------------------------------------------------------------------
#
#              Spatial Merging
#
# -------------------------------------------------------------------

library(sf)
library(sp)
library(raster)
library(rgdal)
library(leafsync)
library(RColorBrewer)
library(mapview)

# INFO about plotting: https://r-spatial.github.io/mapview/articles/

# ---------------------------------------------------------------------------
#
#   UPdate Reach Layer
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#  Read in Reach Data
# ---------------------------------------------------------------------------

# ---------------------- reaches data ------------
reaches_path = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/Data/Reaches/Reaches.shp"
#reaches <- st_read(reaches_path, stringsAsFactors = TRUE) # this shapefile does not show up properly
#reaches <- sf::st_transform(reaches, 4326)

# ------ updated with Okanogan EDT data layers -----------------
reaches_path = "Y:/UCRTT/Prioritization/Step 2/Data/GIS/Reaches/Reaches.shp"
reaches <- sf::st_read(reaches_path) # this shapefile does not show up properly
reaches <- sf::st_transform(reaches, 4326)

matrix(unname(unlist(reaches$geometry[144])), ncol = 2)


ogrListLayers(reaches_path) #will show you available layers for the above dataset

reaches=readOGR(reaches_path, layer="Reaches") #will load the shapefile to your dataset.


# ---------------------- Okanogan reaches data ------------
GIS_Okanogan_path = "P:/GIS/Reaches/US_EDT_Reaches_12_5_16_shared_by_CCT_on_March2021/US_EDT_Reaches_12_5_16.shp"
#GIS_Okanogan <- sf::st_read(GIS_Okanogan_path) # this shapefile does not show up properly
#GIS_Okanogan <- sf::st_transform(GIS_Okanogan, 4326)

ogrListLayers(GIS_Okanogan_path) #will show you available layers for the above dataset

GIS_Okanogan=readOGR(GIS_Okanogan_path, layer="US_EDT_Reaches_12_5_16") #will load the shapefile to your dataset.

# ---------------------------------------------------------------------------
#  Update the Okanogan Reach Layer
# ---------------------------------------------------------------------------

reaches_Okanogan = GIS_Okanogan[which(GIS_Okanogan$type == "Reach"),]
# ------------ update Salmon 16-10a to Salmon 16-10 --------
x = which(reaches_Okanogan$new_name == "Salmon 16-10a")
if(length(x) > 0){ reaches_Okanogan$new_name[x] = "Salmon 16-10" }

# ------- add Reach Information-----------
reaches_Okanogan$Length_mi = reaches_Okanogan$length_km * 0.6213712
reaches_Okanogan$Length_m = reaches_Okanogan$METERS
reaches_Okanogan$Basin = "Okanogan"
reaches_Okanogan$Assessment = NA
reaches_Okanogan$RM_Start = NA
reaches_Okanogan$RM_End = NA
reaches_Okanogan$SpringChin = NA
reaches_Okanogan$SteelheadR = NA
reaches_Okanogan$BullTroutR = NA

for(reach_x in reaches_Okanogan$new_name){
  
  # --------------- rows in Reach Informatoin and reaches_Okanogan ---------
  reach_x_i = which(Reach_Information_data$ReachName == reach_x)
  reach_x_new_i = which(reaches_Okanogan$new_name == reach_x)
  
  # --------------- add Assessment Unit ------
  AU_x = Reach_Information_data$Assessment.Unit[reach_x_i]
  reaches_Okanogan$Assessment[reach_x_new_i] = AU_x
  # ------------- RM Start --------------
  RM_Start_x = Reach_Information_data$Reach_start_river_miles[reach_x_i]
  reaches_Okanogan$RM_Start[reach_x_new_i] = RM_Start_x
  # ------------- RM End --------------
  RM_End_x = Reach_Information_data$Reach_end_river_miles[reach_x_i]
  reaches_Okanogan$RM_Start[reach_x_new_i] = RM_Start_x
  # ------------- SpringChin  --------------
  SpringChin_x = Reach_Information_data$Spring.Chinook.Reach[reach_x_i]
  reaches_Okanogan$SpringChin[reach_x_new_i] = SpringChin_x
  # ------------- SpringChin  --------------
  SteelheadR_x = Reach_Information_data$Steelhead.Reach[reach_x_i]
  reaches_Okanogan$SteelheadR[reach_x_new_i] = SteelheadR_x
  # ------------- SpringChin  --------------
  BullTroutR_x = Reach_Information_data$Bull.Trout.Reach[reach_x_i]
  reaches_Okanogan$BullTroutR[reach_x_new_i] = BullTroutR_x
  
}

# ---------- update names --------------
reaches_Okanogan$ReachName =reaches_Okanogan$new_name
reaches_Okanogan_new = reaches_Okanogan[,c("ReachName", "Basin","Assessment", "RM_Start" ,  "RM_End", 
                                           "SpringChin", "SteelheadR", "BullTroutR", "Length_mi",  "Length_m")] 

# ---------------------------------------------------------------------------
#  Combine Okanogan with other reach layers
# ---------------------------------------------------------------------------
# ---- exclude Okanogan -----
reaches_new = reaches[-which(reaches$Basin == "Okanogan"),]

reaches_Okanogan_new2 = reaches_Okanogan_new
reaches_new2 = reaches_new

reaches_Okanogan_new2 <-spTransform(reaches_Okanogan_new2,  CRS("+proj=utm +zone=11+datum=WGS84") )
reaches_new2 <-spTransform(reaches_new2,  CRS("+proj=utm +zone=11+datum=WGS84") )

#-------- add the new Okanogan --------
reaches_new3 = rbind(reaches_new2 , reaches_Okanogan_new2)


# ---------------------------------------------------------------------------
#  Export 
# ---------------------------------------------------------------------------

export_path_x = "Y:/UCRTT/Prioritization/Step 2/Data/GIS/Reaches"
#st_write(reaches_new3, export_path_x)

writeOGR(obj=reaches_new3, dsn=export_path_x, layer="Reaches",  driver = "ESRI Shapefile") # this is in geographical projection



# ---------------------------------------------------------------------------
#
#    Update the Assessment Units
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#  Read in Assessment Unit (HUC 12) data
# ---------------------------------------------------------------------------

AU_RTT_path = "P:/GIS/Prioritization/Step 2/Assessment_Units/AUPrioritization_Tiers.shp"

ogrListLayers(AU_RTT_path) #will show you available layers for the above dataset
AUs_RTT=readOGR(AU_RTT_path, layer="AUPrioritization_Tiers") #will load the shapefile to your dataset.

# --------------- update the projection ------
AUs_RTT <-spTransform(AUs_RTT,  CRS("+proj=utm +zone=11+datum=WGS84") )

# ---------- update names (the Protection or Restoration columns are not labeled) -----
# ---- Restoration ------------
AUs_RTT$Spring_Chinook_AU_Restoration_Tier = AUs_RTT$SpringChin 
AUs_RTT$Steelhead_AU_Restoration_Tier = AUs_RTT$Steelhead_ 
AUs_RTT$Bull_Trout_AU_Restoration_Tier = AUs_RTT$BullTrout_ 

# ---- Protection ------------
AUs_RTT$Spring_Chinook_AU_Protection_Tier = AUs_RTT$SpringCh_1 
AUs_RTT$Steelhead_AU_Protection_Tier = AUs_RTT$Steelhead1
AUs_RTT$Bull_Trout_AU_Protection_Tier = AUs_RTT$BullTrout1 

# ------ update shape length name ----
colx = which(names(AUs_RTT) == "Shape__Are")
names(AUs_RTT)[colx] = "Shape_Area_sq_meters"

#------- clean up --------------
AUs_RTT = AUs_RTT[, c("AU","Subbasin","Spring_Chinook_AU_Restoration_Tier", "Steelhead_AU_Restoration_Tier",  "Bull_Trout_AU_Restoration_Tier",
               "Spring_Chinook_AU_Protection_Tier" , "Steelhead_AU_Protection_Tier", "Bull_Trout_AU_Protection_Tier", "Shape_Area_sq_meters" )]

# ---------------------------------------------------------------------------
#  Read in Okanogan 
# ---------------------------------------------------------------------------

AU_Okanogan_path = "P:/GIS/Prioritization/Step 2/Assessment_Units/Okanogan/okanogan_assess_units_10JUN21.shp"

ogrListLayers(AU_Okanogan_path) #will show you available layers for the above dataset
AU_Okanogan=readOGR(AU_Okanogan_path, layer="okanogan_assess_units_10JUN21") #will load the shapefile to your dataset.

# --------------- update the projection ------
AU_Okanogan <-spTransform(AU_Okanogan,  CRS("+proj=utm +zone=11+datum=WGS84") )

# ---------------------------------------------------------------------------
#  Prepare to merge
# ---------------------------------------------------------------------------

# ------------ remove Okanogan AUs from RTT AUs --------
x_OK = which(AUs_RTT$Subbasin == "Okanogan")
AUs_RTT = AUs_RTT[-x_OK,]

# ------------- get Okanogan ready to merge -----------
AU_Okanogan$AU = AU_Okanogan$name
AU_Okanogan$Subbasin = "Okanogan"
AU_Okanogan$Spring_Chinook_AU_Restoration_Tier = NA
AU_Okanogan$Steelhead_AU_Restoration_Tier = NA
AU_Okanogan$Bull_Trout_AU_Restoration_Tier = NA
AU_Okanogan$Spring_Chinook_AU_Protection_Tier = NA
AU_Okanogan$Steelhead_AU_Protection_Tier = NA
AU_Okanogan$Bull_Trout_AU_Protection_Tier = NA
AU_Okanogan$Shape_Area_sq_meters =  AU_Okanogan$areasqkm*1000000


for(AU_x in AU_Okanogan$AU){
  
  # --------------- rows in AU Ranks Okanogan or AU_Okanogan ---------
  AU_x_i = which(AU_Ranks_Okanogan$`EDT AU` == AU_x)
  AU_x_new_i = which(AU_Okanogan$AU == AU_x)
  
  # ------------- Restoration AU Ranks  --------------
  SteelheadR_x = AU_Ranks_Okanogan$`AU Restoration Rank`[AU_x_i]
  AU_Okanogan$Steelhead_AU_Restoration_Tier[AU_x_new_i] = SteelheadR_x
  # ------------- Protection AU Ranks  --------------
  SteelheadR_x = AU_Ranks_Okanogan$`AU Protection Rank`[AU_x_i]
  AU_Okanogan$Steelhead_AU_Protection_Tier[AU_x_new_i] = SteelheadR_x
  
}

# ----------------- prep the Okanogan AU data --
AU_Okanogan = AU_Okanogan[, c("AU","Subbasin","Spring_Chinook_AU_Restoration_Tier", "Steelhead_AU_Restoration_Tier",  "Bull_Trout_AU_Restoration_Tier",
                                        "Spring_Chinook_AU_Protection_Tier" , "Steelhead_AU_Protection_Tier", "Bull_Trout_AU_Protection_Tier", "Shape_Area_sq_meters" )]

# ------------- remove the AUs in Canada ---------
mapview(AU_Okanogan)

Canadian_AUs = c("Okanogan-Long Joe Creek", "Haynes Creek DS", "Testalinden Creek", "Inkaneep Creek-Lower DS",
                 "Okanagan-Reed Creek", "Vaseux Creek-Lower DS", "Okanagan-Vaseux Lake", "Shuttleworth Creek US",
                 "Shuttleworth Creek DS", "McLean Creek DS", "Okanagan-Skaha Lake", "Ellis Creek",
                 "Shingle Creek-Lower", "Shatford Creek")
AU_x_remove = c()
for(AU_x in Canadian_AUs){
  AU_x_remove = c(AU_x_remove, which(AU_Okanogan$AU == AU_x))
}
AU_Okanogan2 = AU_Okanogan[ -AU_x_remove, ]

# ---------------------------------------------------------------------------
#  Merge the two
# ---------------------------------------------------------------------------
AUs_RTT_new = rbind(AUs_RTT, AU_Okanogan2)

# ---------------------------------------------------------------------------
#  Export 
# ---------------------------------------------------------------------------

export_path_x = "Y:/UCRTT/Prioritization/Step 2/Data/GIS/Assessment_Units_RTT_June2021"
#st_write(reaches_new3, export_path_x)

writeOGR(obj=AUs_RTT_new, dsn=export_path_x, layer="Assessment_Units",  driver = "ESRI Shapefile") # this is in geographical projection







# ---------------------------------------------------------------------------
#
#      OLD Code 
#
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#     Map 
# ---------------------------------------------------------------------------

color_palette_x = c("red", "blue")
color_palette_continuous_LARGE = brewer.pal(9, 'Set1')
# 
# 
mapview(reaches_Okanogan, lwd=4, zcol="length_km", legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_continuous_LARGE, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))



# -------------------------------------------------------------------
#   Adjust Reaches
# -------------------------------------------------------------------

Okanogan_16_15_1 = which(reaches$ReachName == "Okanogan 16-15")
Okanogan_16_16_2 = which(reaches$ReachName == "Okanogan 16-16")
okanogan_bridge = c( -119.578085, 48.363442)

# ----------------- prepare the two rows  ------------

#------------ get line -----------
Okanogan_16_15_1_line = reaches$geometry[Okanogan_16_15_1]
Okanogan_16_16_2_line = reaches$geometry[Okanogan_16_16_2]

Okanogan_16_15_1_line_df = Okanogan_16_15_1_line[[1]][[1]]
Okanogan_16_16_2_line_df = Okanogan_16_16_2_line[[1]][[1]]

# ----------- plot to compare --------
xlimx = c(-119.605, -119.540)
ylimx = c(48.348, 48.400)
xlimx = c(-119.590, -119.570)
ylimx = c(48.36, 48.37)
plot(Okanogan_16_15_1_line[[1]][[1]], xlim = xlimx, ylim=ylimx  )
points(Okanogan_16_16_2_line[[1]][[1]], col="purple")
points(Okanogan_16_16_2_line[[1]][[1]][16:63,], col="blue")
points(okanogan_bridge[1], okanogan_bridge[2], col="red", pch=16, cex=1)

# ---------------- update Okanogan 16-15 to be longer -----------
Okanogan_16_15_1_geometry_new = st_multilinestring(x = list(as.matrix(as.data.frame( rbind(Okanogan_16_15_1_line_df[,1:2],  Okanogan_16_16_2_line_df[1:16,1:2]  )  )))  )
Okanogan_16_16_2_geometry_new = st_multilinestring(x = list(as.matrix(as.data.frame( Okanogan_16_16_2_line_df[16:63,1:2])))  )

# ----------------- update reach geometry -------------
reaches$geometry[Okanogan_16_15_1] = Okanogan_16_15_1_geometry_new
reaches$geometry[Okanogan_16_16_2] = Okanogan_16_16_2_geometry_new


# -------------------------------------------------------------------
#   Reaches to remove
# -------------------------------------------------------------------


# ---------------------- reaches to remove ----------------

reaches_to_remove = c("Loup Loup 16-4.1 (falls)")


