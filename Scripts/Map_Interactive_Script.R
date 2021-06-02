


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

# -------------- update reach names ------------
Okanogan_Reach_Crosswalk =  read_excel(  paste(Okanogan_EDT_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )
for(reach_x in reaches$ReachName){
  if(any(Okanogan_Reach_Crosswalk$ReachName_Old == reach_x)){
    x_old = which(reaches$ReachName == reach_x)
    x_new = which(Okanogan_Reach_Crosswalk$ReachName_Old == reach_x)
    reaches$ReachName[x_old]  = Okanogan_Reach_Crosswalk$ReachName_New[x_new]
  }
}


# ---------------- Reaches from Jan 2021 ------------
#reaches_path = "C:/Users/Ryan/Downloads/Reaches_(All_Jan2021)/Reaches_(All).shp"
#reaches_jan2021 <- sf::st_read(reaches_path) # this shapefile does not show up properly
#reaches_jan2021 <- sf::st_transform(reaches_jan2021, 4326)



# ---------------------------------------------------------------------------
#  Read in Habitat Quality Score
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
#  Read in Habitat Quality Score
# ---------------------------------------------------------------------------
habitat_attributes_indiv_x = unique(Habitat_Attribute_Scores$Habitat_Attribute)
Habitat_Attribute_Scores = as.tibble(Habitat_Attribute_Scores)
Habitat_Attribute_Score_Final_COMBINE =  Reach_Information_data[,c("ReachName","Basin","Assessment.Unit")]
for(habitat_attribute_x in habitat_attributes_indiv_x ){
  
  habitat_attribute_vector_x = Habitat_Attribute_Scores %>%
    filter(Habitat_Attribute == habitat_attribute_x) 
  habitat_attribute_vector_x =  habitat_attribute_vector_x[,c("ReachName", "Habitat_Attribute_Score")]
  
  Habitat_Attribute_Score_Final_COMBINE = merge(Habitat_Attribute_Score_Final_COMBINE, 
                                           habitat_attribute_vector_x, by="ReachName")
  colnamesx = colnames(Habitat_Attribute_Score_Final_COMBINE)
  colnamesx[length(colnamesx)] = habitat_attribute_x
  colnames(Habitat_Attribute_Score_Final_COMBINE) = colnamesx
  Habitat_Attribute_Score_Final_COMBINE[,habitat_attribute_x] = as.factor(Habitat_Attribute_Score_Final_COMBINE[,habitat_attribute_x])
}


Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']]
Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Protection']]

# ---------------------------------------------------------------------------
#  Merge the data
# ---------------------------------------------------------------------------

# -------------------------------------
#  Habitat Quality 
# -------------------------------------
# -------- merge reach spatial data with habitat data --------------
reaches_HQ_data = merge(reaches, Habitat_Quality_Scores_factors, by = "ReachName") 
# ------- remove columns we don't want ------
reaches_HQ_data = subset (reaches_HQ_data, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR,
                                             Length_mi,Length_m,Basin.y))

# -------------------------------------
#  Habitat Attributes (Limiting Factor) 
# -------------------------------------
# -------- merge reach spatial data with habitat data --------------
reaches_LF_data = merge(reaches, Habitat_Attribute_Score_Final_COMBINE, by = "ReachName") 
# ------- remove columns we don't want ------
reaches_LF_data = subset (reaches_LF_data, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR,
                                                 Length_mi,Length_m,Basin.y))
# ----------------------------------
# 
# -------------------------------------------

# -------- merge reach spatial data with habitat data --------------
reaches_PROTECTION_data = merge(reaches, Protection_Prioritization_Output, by = "ReachName") 
# ------- remove columns we don't want ------
reaches_PROTECTION_data = subset (reaches_PROTECTION_data, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR,
                                                       Length_mi,Length_m,Basin.y))

# -------------------------------------
# Add Projects
# -------------------------------------
# -------- merge reach spatial data with habitat data --------------
reaches_Projects = merge(reaches, Reach_Asessment_Project_Data, by = "ReachName") 
#----------------- make reach assessment a factor -----------
colnames(reaches_Projects)[colnames(reaches_Projects) == "Reach Assessment"] = "Reach_Assessment"
reaches_Projects$Reach_Assessment = as.factor(reaches_Projects$Reach_Assessment)

# -------------------------------------
#   Reach Ranks (Restoration and Protection) Scores
# -------------------------------------
output_path_x =  paste(output_path,'Restoration_Reach_Ranking_Scores_Output.xlsx', sep="")
#Restoration_Rank_Scores_Output =  read_excel( output_path_x) 
Restoration_Rank_Scores_Output = Reach_Rankings_Output_Restoration
output_path_x =  paste(output_path,'Protection_Reach_Ranking_Scores_Output.xlsx', sep="")
#Protection_Rank_Scores_Output =  read_excel( output_path_x) 
Protection_Rank_Scores_Output = Reach_Rankings_Output_Protection

# -------- merge reach spatial data with habitat data --------------
Restoration_Ranks_data = merge(reaches, Restoration_Rank_Scores_Output, by = "ReachName") 
Protection_Ranks_data = merge(reaches, Protection_Rank_Scores_Output, by = "ReachName") 
# ------- remove columns we don't want ------
#Restoration_Ranks_data = subset (Restoration_Ranks_data, select = -c("RM_End","RM_Start","Length_mi","Length_m","Assessment.Unit","Basin.y"))
#Protection_Ranks_data = subset (Protection_Ranks_data, select = -c("RM_End","RM_Start","Length_mi","Length_m","Assessment.Unit","Basin.y"))
# ------------------- make it numeric ---------------
# ------- Restoration ----------
Restoration_Ranks_data$Score_Total = as.numeric(as.character(Restoration_Ranks_data$Reach_Rank_Total_Score))
Restoration_Ranks_data$Rank_Total = as.numeric(as.character(Restoration_Ranks_data$AU_level_Reach_Rank))
# ------------- Protection ------
Protection_Ranks_data$Score_Total = as.numeric(as.character(Protection_Ranks_data$Reach_Rank_Total_Score))
Protection_Ranks_data$Rank_Total = as.numeric(as.character(Protection_Ranks_data$AU_level_Reach_Rank))
Protection_Ranks_data$Protected_90_100_Pct = as.numeric(as.character(Protection_Ranks_data$Protected_90_100_Pct))

# ---------------- set "Missing_Data" to NA ------------
restoration_rank_missing = which(Restoration_Ranks_data$AU_level_Reach_Rank == "Missing_Data")
Restoration_Ranks_data$AU_level_Reach_Rank[restoration_rank_missing] = NA
protection_rank_missing = which(Protection_Ranks_data$AU_level_Reach_Rank == "Missing_Data")
Protection_Ranks_data$AU_level_Reach_Rank[protection_rank_missing] = NA

# --------------------------------
#   WebMap output (read in)
# --------------------------------------
Restoration_Prioritization_Output_for_WebMap_updated = Restoration_Prioritization_Output_for_WebMap
Restoration_Prioritization_Output_for_WebMap_updated$ReachName = Restoration_Prioritization_Output_for_WebMap_updated$'Reach Name'
# -------- merge reach spatial data with habitat data --------------
reaches_Restoration_WebMap_data = merge(reaches, Restoration_Prioritization_Output_for_WebMap_updated, by = "ReachName") 
# ------- remove columns we don't want ------
reaches_Restoration_WebMap_data = subset(reaches_Restoration_WebMap_data, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR,
                                                                                      Length_mi,Length_m))


# -------------------------------------
#  Create composite scores
# -------------------------------------
floodplain_dif = as.numeric(as.character( reaches_HQ_data$`Off-Channel-Floodplain_score` )) -  as.numeric(as.character( reaches_LF_data$`Off-Channel- Floodplain` ))
floodplain_dif = as.data.frame(cbind(Habitat_Quality_Scores$ReachName,floodplain_dif ))
colnames(floodplain_dif) = c("ReachName","Floodplain_Dif")
#floodplain_dif[,2] = as.factor(floodplain_dif[,2])
reaches_HQ_data = merge(reaches_HQ_data, floodplain_dif, by = "ReachName") 
reaches_HQ_data$Floodplain_Dif = as.factor(reaches_HQ_data$Floodplain_Dif)


floodplain_dif_both = cbind(floodplain_dif[,1], 
                            as.numeric(as.character( reaches_HQ_data$`Off-Channel-Floodplain_score` )), 
                            as.numeric(as.character( reaches_LF_data$`Off-Channel- Floodplain` )),
                            floodplain_dif[,2],
                            as.data.frame(as.numeric(as.character( reaches_HQ_data$`Off-Channel-Floodplain_score` )) -  as.numeric(as.character( reaches_LF_data$`Off-Channel- Floodplain` ))) )  
colnames(floodplain_dif_both) = c("ReachName", "HQ_score(REI)","LF_score_old","difference","difference_calc")

x = which(reaches_HQ_data$ReachName = "Peshastin Creek Lower 03")
as.numeric(as.character( reaches_HQ_data$`Off-Channel-Floodplain_score` ))[x]
as.numeric(as.character( reaches_LF_data$`Off-Channel- Floodplain` ))[x]
as.numeric(as.character( reaches_HQ_data$`Off-Channel-Floodplain_score` ))[x] -  as.numeric(as.character( reaches_LF_data$`Off-Channel- Floodplain` ))[x]


reaches$color_reach = 0
reaches$color_reach[which(reaches$ReachName == "Johnson 16-1")] = 1

color_palette_x = c("red", "blue")
color_palette_continuous_LARGE = brewer.pal(9, 'Set1')
# 
mapview(reaches, lwd=4, zcol="RM_End", legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_continuous_LARGE, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# ---------------------------------------------------------------------------
#     Generate the Colors
# ---------------------------------------------------------------------------

# ---------------------- color palettes for display --------------
color_palette_x = c("red", "yellow","forestgreen")
color_palette_continuous = brewer.pal(9, 'YlGnBu')
color_palette_continuous_LARGE = brewer.pal(9, 'Set1')
color_palette_x_YES_NO = brewer.pal(3, 'BuPu')
color_palette_x_YES_NO = c("#6C0586", "#FBFF68")
color_neg_to_pos = brewer.pal(11, 'RdBu')
color_qualitative = brewer.pal(9, 'Set1')

# ---------------------------------------------------------------------------
#
#  PLOT SINGLE Plots (single window)
#
# ---------------------------------------------------------------------------

# THIS prints all the attributes you can map
print(names(reaches_HQ_data))

reaches_HQ_data$random_number = round(runif(nrow(reaches_HQ_data),1,9))

# --- simple version ---:
mapview(reaches_HQ_data, zcol= "random_number", lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_continuous_LARGE, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))


# ----------------------------------------------------------
#     plot FACTOR/SCORE variable 
# ----------------------------------------------------------

# ------ ENTER the attribute to print here ------
attribute_1 = "Riparian-Disturbance_score"
attribute_1 = "Off-Channel-Side-Channels_score"
attribute_1 = "HQ_Score_Restoration"
attribute_1 = "HQ_Score_Protection"
attribute_1 = "Riparian-CanopyCover_score"

# --- simple version ---:
mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# --- version where you can turn 1, 3, 5 on and off ---:
mapview(reaches_HQ_data, zcol = attribute_1,   burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))

# ----------------------------------------------------------
#     plot Qualitative
# ----------------------------------------------------------

# ---------------- Just map the reaches
mapview(reaches_Projects, zcol="Action_Category" ,lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_qualitative, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# ----------------------------------------------------------
#     plot TWO FACTOR/SCORE variable 
# ----------------------------------------------------------

# --- simple version ---:
attribute_1 = "HQ_Score_Restoration"
attribute_2 = "HQ_Score_Protection"

mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap")) +
mapview(reaches_HQ_data, zcol = attribute_2, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
          color= color_palette_x) 
# Note: helpful for plotting factors: https://github.com/r-spatial/mapview/issues/240

# ----------------------------------------------------------
#     plot TWO FACTOR/SCORE variable - HQ and LF
# ----------------------------------------------------------

# --- simple version ---:
attribute_1 = "Off-Channel-Floodplain_score"
attribute_2 = "Off-Channel- Floodplain"

mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap")) +
  mapview(reaches_LF_data, zcol = attribute_2, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
          color= color_palette_x) 
# Note: helpful for plotting factors: https://github.com/r-spatial/mapview/issues/240



# ----------------------------------------------------------
#     plot CONTINUOUS variable 
# ----------------------------------------------------------
# ENTER the attribute to print here
attribute_1 = "HQ_Pct"
mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), 
        color= color_palette_continuous, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# ----------------------------------------------------------
#     plot Reach Ranks
# ----------------------------------------------------------

# ------------- for total score -------------
attribute_1 = "AU_level_Reach_Rank"
color_palette_x = c("red", "yellow","forestgreen")
color_palette_x = brewer.pal(3, 'YlGnBu')
color_palette_x = c(color_palette_x[3],color_palette_x[2],color_palette_x[1]) # reverse the order


mapview(Restoration_Ranks_data , zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))
mapview(Protection_Ranks_data , zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

attribute_1 = "Protected_Percent"
color_palette_x = brewer.pal(9, 'YlGnBu')
mapview(Protection_Ranks_data , zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# ------------- for Reach Ranks -------------
attribute_1 = "Rank_AUs"
color_palette_continuous = rev(brewer.pal(5, 'YlGnBu'))

mapview(reaches_reach_ranks_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), 
        color= color_palette_continuous, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

# ------------- for Reach Ranks -------------
attribute_1 = "Score_Total"
color_palette_continuous = brewer.pal(7, 'YlGnBu')

mapview(reaches_reach_ranks_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), 
        color= color_palette_continuous, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))



# ----------------------------------------------------------
#     plot PROTECTION
# ----------------------------------------------------------

mapview(reaches_PROTECTION_data, lwd=4, legend = mapviewGetOption("legend"), 
        color= 'blue', map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))

attribute_1 = "HQ_Score_Protection"
mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), 
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))


# ----------------------------------------------------------
#     plot WebMap Output
# ----------------------------------------------------------

mapview(reaches_Restoration_WebMap_data, lwd=4, legend = mapviewGetOption("legend"), 
        color= 'blue', map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))



# ---------------------------------------------------------------------------
#
#  PLOT Multiple Plots and Compare (multiple windows)
#
# ---------------------------------------------------------------------------

# --------------- CHOOSE the four attributes to plot ----------
attribute_1 = "HQ_Score_Restoration"
attribute_2 = "HQ_Score_Protection"
attribute_3 = "Spring.Chinook.Reach"
attribute_4 = "Steelhead.Reach"

m1 <- mapview(reaches_HQ_data, zcol = attribute_1, lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m2 <- mapview(reaches_HQ_data, zcol = attribute_2,  lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m3 <- mapview(reaches_HQ_data, zcol = attribute_3, burst = TRUE, color = color_palette_x_YES_NO, legend=TRUE,
              map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m4 <- mapview(reaches_HQ_data, zcol = attribute_4, burst = TRUE, color = color_palette_x_YES_NO,
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

m1 <- mapview(reaches_HQ_data, zcol = attribute_1,  legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m2 <- mapview(reaches_HQ_data, zcol = attribute_2,   legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m3 <- mapview(reaches_HQ_data, zcol = attribute_3,  legend=TRUE,
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"),
              popup = popupTable(reaches_b2, zcol = c("ReachName","Basin.x","Assessment.Unit")))
m4 <- mapview(reaches_HQ_data, zcol = attribute_4,
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

m1 <- mapview(reaches_HQ_data, zcol = attribute_1, lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m2 <- mapview(reaches_HQ_data, zcol = attribute_2,  lwd=5,  burst=TRUE,legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m3 <- mapview(reaches_HQ_data, zcol = attribute_3, burst = TRUE,  legend=TRUE,
              color = color_palette_x,  map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m4 <- mapview(reaches_HQ_data, zcol = attribute_4,  burst='STUSPS',
              color = color_palette_x,  map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery")   )


# --- simple version ---:
attribute_1 = "Off-Channel-Floodplain_score"
attribute_2 = "Off-Channel- Floodplain"
attribute_3 = "Floodplain_Dif"

m1 <- mapview(reaches_HQ_data, zcol = attribute_1,burst=TRUE, lwd=3, legend = mapviewGetOption("legend"), 
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m2 <- mapview(reaches_LF_data, zcol = attribute_2, burst=TRUE, lwd=3, legend = mapviewGetOption("legend"),
              color = color_palette_x, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))
m3 <- mapview(reaches_HQ_data, zcol = attribute_3, burst=TRUE,  lwd=5,  legend = mapviewGetOption("legend"),
              color = color_neg_to_pos, map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery"))



sync(m1, m2, m3)

reaches_HQ_data$Floodplain_Dif_Continuous = as.numeric(as.character(reaches_HQ_data$Floodplain_Dif))
color_neg_to_pos = brewer.pal(9, 'RdBu')
attribute_1 = "Floodplain_Dif_Continuous"
mapview(reaches_HQ_data, zcol = attribute_1, lwd=2, legend = mapviewGetOption("legend"), 
        color= color_neg_to_pos, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap")) +
mapview(reaches_HQ_data, zcol = attribute_1, burst = TRUE, lwd=6, legend = mapviewGetOption("legend"), 
        color= "white", map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))



# ---------------------------------------------------------------------------
#
#  HISTOGRAM Plots
#
# ---------------------------------------------------------------------------

hist( as.numeric(as.character(reaches_LF_data$`Off-Channel- Floodplain`) ), col="orange", ylim=c(0,220),
      main=" ", ylab=" ", xlab=" ")
par(new=T)
hist( as.numeric(as.character(reaches_HQ_data$`Off-Channel-Floodplain_score`) ), col="blue", ylim=c(0,220),
      main=" Histogram of Floodplain scores", xlab="Score")
legend(1,215, c("Habitat Quality Pathway (REI value)", "Limiting Factor Pathway (lowest of 4 habitat attribute scores)"), col=c("blue", "orange"), pch=15)


hist(  as.numeric(as.character(reaches_HQ_data$Floodplain_Dif) )  )
abline(v = median(as.numeric(as.character(reaches_HQ_data$Floodplain_Dif) ), na.rm=T ), col='red', lwd=3)















join(reaches$geometry[x1], reaches$geometry[x2])



x = st_sf(a = 1:2, geom = st_sfc(st_point(c(0,0)), st_point(c(1,1))))
y = data.frame(a = 2:3)
merge(x, y)
