# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   Prepare Data to A) read in to ArcGIS Pro then B) push to AGO
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Upload Library
# -----------------------------------------------------------------------------------------------------------------------------------------------
library(sp)
library(rgdal)


# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Read in Reaches
# -----------------------------------------------------------------------------------------------------------------------------------------------

reaches_path2 = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/Data/Reaches"
reaches2 = readOGR(dsn = reaches_path2, layer = "Reaches")


# -----------------------------------------------------------------------------------------------------------------------------------------------
#   Convert Okanogan Names
# -----------------------------------------------------------------------------------------------------------------------------------------------
#  NOTE - once the reach names/geography are updated - won't need to run this


# -------------- update reach names ------------
Okanogan_Reach_Crosswalk =  read_excel(  paste(Okanogan_EDT_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )
for(reach_x in reaches$ReachName){
  if(any(Okanogan_Reach_Crosswalk$ReachName_Old == reach_x)){
    x_old = which(reaches2$ReachName == reach_x)
    x_new = which(Okanogan_Reach_Crosswalk$ReachName_Old == reach_x)
    reaches2$ReachName[x_old]  = Okanogan_Reach_Crosswalk$ReachName_New[x_new[1]]
    
    
  }
}


# ------------------------------------------------
#   Merge with Reach ranks
# ------------------------------------------------

# ------------------------- reduce -------------------------
Reach_Rankings_Output_Restoration_x = Reach_Rankings_Output_Restoration[,-c(2,3)]
Reach_Rankings_Output_Protection_x = Reach_Rankings_Output_Protection[,-c(2,3)]

# ------------- do the same for the SpatialDataFrame -------
reaches2_Rank_Restoration = merge(reaches2,Reach_Rankings_Output_Restoration_x , by = "ReachName") 
reaches2_Rank_Protection = merge(reaches2,Reach_Rankings_Output_Protection_x , by = "ReachName") 

# ------- remove columns we don't want ------
reaches2_Rank_Restoration = subset (reaches2_Rank_Restoration, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR, Length_mi,Length_m))
reaches2_Rank_Protection = subset (reaches2_Rank_Protection, select = -c(Assessment,RM_Start,RM_End, SpringChin, SteelheadR,BullTroutR, Length_mi,Length_m))

# ------------------------------------------------
#    Output as Shapefile (to upload into ArcGIS to then push to AGO for WebMap)
# ------------------------------------------------

# ------------ export the shapefile -----------
export_path_restoration = "Y:/UCRTT/Prioritization/Step 2/Data/Restoration_AU_Ranks"
export_path_protection = "Y:/UCRTT/Prioritization/Step 2/Data/Protection_AU_Ranks"
writeOGR(reaches2_Rank_Restoration, dsn = export_path_restoration, layer = "Restoration_AU_Ranks_2",
         driver = "ESRI Shapefile" )
writeOGR(reaches2_Rank_Protection, dsn = export_path_protection, layer = "Protection_AU_Ranks_2",
         driver = "ESRI Shapefile" )



