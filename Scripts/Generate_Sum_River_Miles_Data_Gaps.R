

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   Calculate Mileage of missing data for habitat attributes
#      June 2021
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------- parameters -------
Basins_to_Exclude = c("Okanogan")
Reaches_to_Exclude = c("Lake Wenatchee 01")

# ------------- filter to remove those reaches -------
Habitat_Quality_Scores_for_Data_Gaps = filter(Habitat_Quality_Scores, 
                                  !ReachName %in%  Reaches_to_Exclude)
# ----------- calculate total miles in each Basin and AU --------
Reach_Information_data_Data_gaps = filter(Reach_Information_data, 
                                          !ReachName %in%  Reaches_to_Exclude) 
Reach_Information_data_Data_gaps = filter(Reach_Information_data_Data_gaps, !Basin %in% Basins_to_Exclude)

# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Pull the NAs
# -----------------------------------------------------------------------------------------------------------------------------------------------

coarse_substrate_na_x = which(is.na(Habitat_Quality_Scores_for_Data_Gaps$CoarseSubstrate_score))
cover_wood_na_x = which(is.na(Habitat_Quality_Scores_for_Data_Gaps$`Cover-Wood_score`))
pool_na_x = which(is.na(Habitat_Quality_Scores_for_Data_Gaps$`PoolQuantity&Quality_score`))


# ------------------------------------------------
#     Pull Reach Information for Reaches with Data Gaps
# ------------------------------------------------

coarse_substrate_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores_for_Data_Gaps[coarse_substrate_na_x, c("ReachName","CoarseSubstrate_score")], by="ReachName")
cover_wood_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores_for_Data_Gaps[cover_wood_na_x, c("ReachName","Cover-Wood_score")], by="ReachName")
pool_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores_for_Data_Gaps[pool_na_x, c("ReachName","PoolQuantity&Quality_score")], by="ReachName")

# ------------------------------------------------
#   Exclude any basins
# ------------------------------------------------

# ------------- filter to remove those reaches -------
coarse_substrate_Reach_Info = filter(coarse_substrate_Reach_Info, 
                                                !Basin %in%  Basins_to_Exclude )
cover_wood_Reach_Info = filter(cover_wood_Reach_Info, 
                                          !Basin %in%  Basins_to_Exclude )
pool_Reach_Info = filter(pool_Reach_Info, 
                                    !Basin %in%  Basins_to_Exclude )

# ------------------------------------------------
#     Get Sum of River Miles with Data Gap
# ------------------------------------------------

coarse_substrate_Reach_Info_Summarized = coarse_substrate_Reach_Info %>% group_by(Assessment.Unit) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters = sum(Length..meters., na.rm=TRUE))
cover_wood_Reach_Info_Summarized = cover_wood_Reach_Info %>% group_by(Assessment.Unit) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters = sum(Length..meters., na.rm=TRUE))
pool_Reach_Info_Summarized = pool_Reach_Info %>% group_by(Assessment.Unit) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters = sum(Length..meters., na.rm=TRUE))

# ------------------------------------------------
#    Add Basin
# ------------------------------------------------
Reach_Info_Basin = Reach_Information_data[which(!duplicated(Reach_Information_data$Assessment.Unit)),c("Assessment.Unit", "Basin")]

coarse_substrate_Reach_Info_Summarized = merge(coarse_substrate_Reach_Info_Summarized, Reach_Info_Basin, by="Assessment.Unit", all.x=TRUE, all.y=FALSE)
cover_wood_Reach_Info_Summarized = merge(cover_wood_Reach_Info_Summarized, Reach_Info_Basin, by="Assessment.Unit", all.x=TRUE, all.y=FALSE)
pool_Reach_Info_Summarized = merge(pool_Reach_Info_Summarized, Reach_Info_Basin, by="Assessment.Unit", all.x=TRUE, all.y=FALSE)

# ------ re-organize columns ------------
coarse_substrate_Reach_Info_Summarized = coarse_substrate_Reach_Info_Summarized[,c("Assessment.Unit","Basin","Total_River_Miles_Data_Gaps_meters")]
cover_wood_Reach_Info_Summarized = cover_wood_Reach_Info_Summarized[,c("Assessment.Unit","Basin","Total_River_Miles_Data_Gaps_meters")]
pool_Reach_Info_Summarized = pool_Reach_Info_Summarized[,c("Assessment.Unit","Basin","Total_River_Miles_Data_Gaps_meters")]

# ------------------------------------------------
#    List Reach Names
# ------------------------------------------------

FUNCTION_pull_reach_names = function(AU_data_frame,reach_data_frame){
  
  AU_data_frame$ReachNames = NA
  for(AU_i in 1:nrow(AU_data_frame )){
    # ----------- find where AU name overlaps ----------
    reaches_x_i = which(reach_data_frame$Assessment.Unit == AU_data_frame$Assessment.Unit[AU_i])
    # ------------- generate reach names --------------
    reach_names_x_i = reach_data_frame$ReachName[reaches_x_i]
    reach_names_x_i = paste(reach_names_x_i, collapse=",")
    # ---------- add reach names --------
    AU_data_frame$ReachNames[AU_i] = reach_names_x_i
  }
  
  return(AU_data_frame)
  
}

coarse_substrate_Reach_Info_Summarized = FUNCTION_pull_reach_names(coarse_substrate_Reach_Info_Summarized, coarse_substrate_Reach_Info)
cover_wood_Reach_Info_Summarized = FUNCTION_pull_reach_names(cover_wood_Reach_Info_Summarized, cover_wood_Reach_Info)
pool_Reach_Info_Summarized = FUNCTION_pull_reach_names(pool_Reach_Info_Summarized, pool_Reach_Info)

# ------------------------------------------------
#    Add Restoration and Protection Tiers
# ------------------------------------------------
AU_Ranks_Okanogan2 = AU_Ranks_Okanogan[,c("EDT AU", "AU Restoration Rank", "AU Protection Rank" )]
colnames(AU_Ranks_Okanogan2) = c("Assessment.Unit", "STLTier_Restoration","STLTier_Protection")  # we are using EDT AU names

AU_Ranks_data2 = AU_Ranks_data
colnames(AU_Ranks_data2)[1] = "Assessment.Unit"
AU_Ranks_data2 = AU_Ranks_data2[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",
                                   "BTTier_Restoration","SPCHNTier_Protection","STLTier_Protection","BTTier_Protection")]

# ----------- add Okanogan AU data to Wen-Ent-Met AU data --------
AU_Ranks_Okanogan3 =AU_Ranks_Okanogan2[,"Assessment.Unit"]
for(colx in colnames(AU_Ranks_data2)[2:ncol(AU_Ranks_data2)]){
  if( any(colnames(AU_Ranks_Okanogan2) == colx) ){
    AU_Ranks_Okanogan3[, colx] = AU_Ranks_Okanogan2[,colx]
  }else{
    AU_Ranks_Okanogan3[, colx] =  NA
  }
}

AU_Ranks_data3 = rbind(AU_Ranks_data2, AU_Ranks_Okanogan3)
AU_Ranks_data3 = as.data.frame(AU_Ranks_data3)
# set "NA" to "Not a Priority"
for(colx in colnames(AU_Ranks_data3)){
  class(AU_Ranks_data3[,colx])
  x_NA = which( is.na(AU_Ranks_data3[,colx]) )
  AU_Ranks_data3[x_NA,colx] = "Not a Priority"
}

# ----------- add  AU priorities ---------
coarse_substrate_Reach_Info_Summarized = merge(coarse_substrate_Reach_Info_Summarized, 
                                               AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",  "SPCHNTier_Protection","STLTier_Protection")], by="Assessment.Unit", all.x=TRUE)
cover_wood_Reach_Info_Summarized = merge(cover_wood_Reach_Info_Summarized, 
                                         AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",  "SPCHNTier_Protection","STLTier_Protection")], by="Assessment.Unit", all.x=TRUE)
pool_Reach_Info_Summarized = merge(pool_Reach_Info_Summarized, 
                                   AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration", "SPCHNTier_Protection","STLTier_Protection")], by="Assessment.Unit", all.x=TRUE)

# ------------------------------------------------
#   Summarize missing miles 
# ------------------------------------------------

Basin_total_river_length_meters = Reach_Information_data_Data_gaps %>% group_by(Basin) %>%
  dplyr::summarize(Total_river_length_meters = sum(Length..meters., na.rm=TRUE))
AU_total_river_length_meters = Reach_Information_data_Data_gaps %>% group_by(Assessment.Unit) %>%
  dplyr::summarize(Total_river_length_meters = sum(Length..meters., na.rm=TRUE))


# ------------------------ Calculate Percent of each Basin ------------------------
Basin_gap_coarse_substrate =  coarse_substrate_Reach_Info_Summarized %>% group_by(Basin) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters2 = sum(Total_River_Miles_Data_Gaps_meters, na.rm=TRUE)) 
Basin_gap_coarse_substrate$Total_Basin_river_meters =  Basin_total_river_length_meters$Total_river_length_meters
Basin_gap_coarse_substrate$Portion_Gap = Basin_gap_coarse_substrate$Total_River_Miles_Data_Gaps_meters2/ Basin_gap_coarse_substrate$Total_Basin_river_meters 

Basin_gap_cover_wood =  cover_wood_Reach_Info_Summarized %>% group_by(Basin) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters2 = sum(Total_River_Miles_Data_Gaps_meters, na.rm=TRUE)) 
Basin_gap_cover_wood$Total_Basin_river_meters =  Basin_total_river_length_meters$Total_river_length_meters
Basin_gap_cover_wood$Portion_Gap = Basin_gap_cover_wood$Total_River_Miles_Data_Gaps_meters2/ Basin_gap_cover_wood$Total_Basin_river_meters 

Basin_gap_pools =  pool_Reach_Info_Summarized %>% group_by(Basin) %>%
  dplyr::summarize(Total_River_Miles_Data_Gaps_meters2 = sum(Total_River_Miles_Data_Gaps_meters, na.rm=TRUE)) 
Basin_gap_pools$Total_Basin_river_meters =  Basin_total_river_length_meters$Total_river_length_meters
Basin_gap_pools$Portion_Gap = Basin_gap_pools$Total_River_Miles_Data_Gaps_meters2/ Basin_gap_pools$Total_Basin_river_meters 

# ------------------------ Calculate Total ------------------------
sum(Basin_gap_coarse_substrate$Total_River_Miles_Data_Gaps_meters2)
sum(Basin_gap_coarse_substrate$Total_River_Miles_Data_Gaps_meters2)/sum(Basin_gap_coarse_substrate$Total_Basin_river_meters)
sum(Basin_gap_cover_wood$Total_River_Miles_Data_Gaps_meters2)
sum(Basin_gap_cover_wood$Total_River_Miles_Data_Gaps_meters2)/sum(Basin_gap_cover_wood$Total_Basin_river_meters)
sum(Basin_gap_pools$Total_River_Miles_Data_Gaps_meters2)
sum(Basin_gap_pools$Total_River_Miles_Data_Gaps_meters2)/sum(Basin_gap_pools$Total_Basin_river_meters)

# ------------------------------------------------
#    Write to excel 
# ------------------------------------------------

output_path_x =  paste(output_path,'Generate_River_Miles_Data_Gaps.xlsx', sep="")
write.xlsx(coarse_substrate_Reach_Info_Summarized, file = output_path_x , sheetName="Coarse_Substrate", row.names=FALSE)
write.xlsx(cover_wood_Reach_Info_Summarized, file=output_path_x, sheetName="Cover_Wood", append=TRUE, row.names=FALSE)
write.xlsx(pool_Reach_Info_Summarized, file=output_path_x, sheetName="Pool_Quality_and_Quantity", append=TRUE, row.names=FALSE)

# -------------------------------------------------------------------------------------------
#
#    Generate Maps
#
# -------------------------------------------------------------------------------------------

# !!! NOTE !!!!: first run the "Map_Interactive_Script.R" - just top - 
#       where Habitat_Quality_Scores_for_Data_Gaps are read in and prepped

# --------------------------------------------------- 
#       add presence/absence 
# --------------------------------------------------- 

# ------ change the name so it looks better in the legend ---------
Upper_Columbia_Habitat_Data = reaches2_HQ_data
# ----------- calculate total miles in each Basin and AU --------
for(reach_x in Reaches_to_Exclude){
  Upper_Columbia_Habitat_Data = Upper_Columbia_Habitat_Data[-which(Upper_Columbia_Habitat_Data$ReachName == reach_x),]
}
for(basin_x in Basins_to_Exclude){
  Upper_Columbia_Habitat_Data = Upper_Columbia_Habitat_Data[-which(Upper_Columbia_Habitat_Data$Basin == basin_x),]
}

# ---------- Coarse Substrate --------
Upper_Columbia_Habitat_Data$Coarse_Substrate_score_Present = "no"
Upper_Columbia_Habitat_Data$Coarse_Substrate_score_Present[which(!is.na(Upper_Columbia_Habitat_Data$CoarseSubstrate_score))] = "yes"
Upper_Columbia_Habitat_Data$Coarse_Substrate_score_Present = as.factor(Upper_Columbia_Habitat_Data$Coarse_Substrate_score_Present)
# ------------ Cover- Wood ----
Upper_Columbia_Habitat_Data$Cover_Wood_score_Present = "no"
Upper_Columbia_Habitat_Data$Cover_Wood_score_Present[which(!is.na(Upper_Columbia_Habitat_Data$`Cover-Wood_score`))] = "yes"
Upper_Columbia_Habitat_Data$Cover_Wood_score_Present = as.factor(Upper_Columbia_Habitat_Data$Cover_Wood_score_Present)
# ------------ Pool Quality & Quantity ----
Upper_Columbia_Habitat_Data$Pool_Quality_and_Quantity_score_Present = "no"
Upper_Columbia_Habitat_Data$Pool_Quality_and_Quantity_score_Present[which(!is.na(Upper_Columbia_Habitat_Data$`PoolQuantity&Quality_score`))] = "yes"
Upper_Columbia_Habitat_Data$Pool_Quality_and_Quantity_score_Present = as.factor(Upper_Columbia_Habitat_Data$Pool_Quality_and_Quantity_score_Present)

# --------------------------------------------------- 
#      Map it
# --------------------------------------------------- 

# ---------------- COLORS -----------
#color_palette_x_YES_NO = brewer.pal(1, 'BuPu')
color_palette_x_YES_NO = c("#005CAB", "#CBE1E8")

# --- Coarse Substrate---:
mapview(Upper_Columbia_Habitat_Data, zcol = "Coarse_Substrate_score_Present", lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x_YES_NO, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap") )

# --- Cover- Wood ---:
mapview(Upper_Columbia_Habitat_Data, zcol = "Cover_Wood_score_Present", lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x_YES_NO, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap") )

# --- Pool Quantity & Quality ---:
mapview(Upper_Columbia_Habitat_Data, zcol = "Pool_Quality_and_Quantity_score_Present", lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x_YES_NO, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap") )



# ------------------------------------------------
#    Output as Shapefile (to upload into ArcGIS to then push to AGO for WebMap)
# ------------------------------------------------

# ---------------- filter out ------------
Upper_Columbia_Habitat_Data_for_AGO = Upper_Columbia_Habitat_Data[,c("ReachName","Basin","Spring.Chinook.Reach", "Steelhead.Reach", 
                                                                     "CoarseSubstrate_score","Cover-Wood_score" , "PoolQuantity&Quality_score",
                                                                     "Coarse_Substrate_score_Present", "Cover_Wood_score_Present" , "Pool_Quality_and_Quantity_score_Present")]
# ------------ export the shapefile -----------
export_path = "Y:/UCRTT/Prioritization/Step 2/Data/Data_Gaps"
writeOGR(Upper_Columbia_Habitat_Data_for_AGO, dsn = export_path, layer = "Reaches_Data_Gaps",
         driver = "ESRI Shapefile" )



# ------------------------------------------------
#    Double Checking Habitat_Quality_Scores_for_Data_Gaps and Habitat_Attributes_Scores values are all the same
# ------------------------------------------------


output_compare_x = c()
for(row_x in 1:nrow(Habitat_Quality_Scores_for_Data_Gaps)){
  reach_x = Habitat_Quality_Scores_for_Data_Gaps$ReachName[row_x]
  # ----------- Cover- Wood -------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Cover- Wood"  )
  wood_x = c(Habitat_Quality_Scores_for_Data_Gaps$`Cover-Wood_score`[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
             as.character( Habitat_Quality_Scores_for_Data_Gaps$`Cover-Wood_score`[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i]  ) )

  # ----------- Pool Quality and Quantiaty-------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Pool Quantity & Quality"   )
  pool_x = c( Habitat_Quality_Scores_for_Data_Gaps$`PoolQuantity&Quality_score`[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
              as.character( Habitat_Quality_Scores_for_Data_Gaps$`PoolQuantity&Quality_score`[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i] ) )
  

  # ----------- Coarse Substrate -------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Coarse Substrate"  )
  coarse_substrate_x = c( Habitat_Quality_Scores_for_Data_Gaps$CoarseSubstrate_score[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
                          as.character( Habitat_Quality_Scores_for_Data_Gaps$CoarseSubstrate_score[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i] ) )
  
  output_x = t( as.data.frame( c(reach_x, wood_x, pool_x, coarse_substrate_x) ) )
  colnames(output_x) = c("ReachName", "Cover_Wood_HQ","Cover_Wood_LF", "Cover_Wood_TF",
                         "Pool_HQ","Pool_LF","Pool_TF",
                         "Coarse_Substrate_HQ","Coarse_Substrate_LF","Coarse_Substrate_TF")
  output_compare_x = rbind(output_compare_x, output_x)
  
}

rownames(output_compare_x) = seq(1,nrow(output_compare_x))
output_compare_x = as.data.frame(output_compare_x)
