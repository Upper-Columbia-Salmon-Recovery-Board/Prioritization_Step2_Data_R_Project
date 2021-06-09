

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   Calculate Mileage of missing data for habitat attributes
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Pull the NAs
# -----------------------------------------------------------------------------------------------------------------------------------------------

coarse_substrate_na_x = which(is.na(Habitat_Quality_Scores$CoarseSubstrate_score))
cover_wood_na_x = which(is.na(Habitat_Quality_Scores$`Cover-Wood_score`))
pool_na_x = which(is.na(Habitat_Quality_Scores$`PoolQuantity&Quality_score`))


# ------------------------------------------------
#     Pull Reach Information for Reaches with Data Gaps
# ------------------------------------------------

coarse_substrate_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores[coarse_substrate_na_x, c("ReachName","CoarseSubstrate_score")], by="ReachName")
cover_wood_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores[cover_wood_na_x, c("ReachName","Cover-Wood_score")], by="ReachName")
pool_Reach_Info = merge(Reach_Information_data, Habitat_Quality_Scores[pool_na_x, c("ReachName","PoolQuantity&Quality_score")], by="ReachName")

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
cover_wood_Reach_Info_Summarized = coarse_substrate_Reach_Info_Summarized[,c("Assessment.Unit","Basin","Total_River_Miles_Data_Gaps_meters")]
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

# ----------- add  AU priorities ---------
coarse_substrate_Reach_Info_Summarized = merge(coarse_substrate_Reach_Info_Summarized, AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",
                                                                                                              "BTTier_Restoration","SPCHNTier_Protection","STLTier_Protection","BTTier_Protection")], by="Assessment.Unit", all.x=TRUE)
cover_wood_Reach_Info_Summarized = merge(cover_wood_Reach_Info_Summarized, AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",
                                                                                                         "BTTier_Restoration","SPCHNTier_Protection","STLTier_Protection","BTTier_Protection")], by="Assessment.Unit", all.x=TRUE)
pool_Reach_Info_Summarized = merge(pool_Reach_Info_Summarized, AU_Ranks_data3[,c("Assessment.Unit","SPCHNTier_Restoration","STLTier_Restoration",
                                                                                                         "BTTier_Restoration","SPCHNTier_Protection","STLTier_Protection","BTTier_Protection")], by="Assessment.Unit", all.x=TRUE)

# ------------------------------------------------
#    Write to excel 
# ------------------------------------------------

output_path_x =  paste(output_path,'Generate_River_Miles_Data_Gaps.xlsx', sep="")
write.xlsx(coarse_substrate_Reach_Info_Summarized, file = output_path_x , sheetName="Coarse_Substrate", row.names=FALSE)
write.xlsx(cover_wood_Reach_Info_Summarized, file=output_path_x, sheetName="Cover_Wood", append=TRUE, row.names=FALSE)
write.xlsx(pool_Reach_Info_Summarized, file=output_path_x, sheetName="Pool_Quality_and_Quantity", append=TRUE, row.names=FALSE)

# ------------------------------------------------
#    Generate Maps
# ------------------------------------------------

# --- Cover- Wood ---:
attribute_1 = "Cover-Wood_score"

# --- Pool Quantity & Quality ---:
attribute_1 = "PoolQuantity&Quality_score"

mapview(reaches_HQ_data, zcol = attribute_1, lwd=4, legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_x, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap") )
  
if(TRUE){
  print("YES!")
}
  
if (FALSE) {
  m = mapview(breweries)
  
  ## create standalone .png; temporary .html is removed automatically unless
  ## 'remove_url = FALSE' is specified
  output_path_x = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/map.png"
  mapshot(m, file = output_path_x )
  
  mapshot(m, file = paste0(getwd(), "/map.png"),
          remove_controls = c("homeButton", "layersControl"))
  
  ## create .html and .png
  mapshot(m, url = paste0(getwd(), "/map.html"),
          file = paste0(getwd(), "/map.png"))
}


mapviewOptions(fgb = FALSE)
m = mapview(breweries)
mapshot(m, file = "~/Rplot.png")

# ------------------------------------------------
#    Double Checking Habitat_Quality_Scores and Habitat_Attributes_Scores values are all the same
# ------------------------------------------------


output_compare_x = c()
for(row_x in 1:nrow(Habitat_Quality_Scores)){
  reach_x = Habitat_Quality_Scores$ReachName[row_x]
  # ----------- Cover- Wood -------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Cover- Wood"  )
  wood_x = c(Habitat_Quality_Scores$`Cover-Wood_score`[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
             as.character( Habitat_Quality_Scores$`Cover-Wood_score`[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i]  ) )

  # ----------- Pool Quality and Quantiaty-------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Pool Quantity & Quality"   )
  pool_x = c( Habitat_Quality_Scores$`PoolQuantity&Quality_score`[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
              as.character( Habitat_Quality_Scores$`PoolQuantity&Quality_score`[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i] ) )
  

  # ----------- Coarse Substrate -------------
  hab_attr_x_i = which(Habitat_Attribute_Scores$ReachName == reach_x & 
                         Habitat_Attribute_Scores$Habitat_Attribute =="Coarse Substrate"  )
  coarse_substrate_x = c( Habitat_Quality_Scores$CoarseSubstrate_score[row_x], Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i],
                          as.character( Habitat_Quality_Scores$CoarseSubstrate_score[row_x] == Habitat_Attribute_Scores$Habitat_Attribute_Score[hab_attr_x_i] ) )
  
  output_x = t( as.data.frame( c(reach_x, wood_x, pool_x, coarse_substrate_x) ) )
  colnames(output_x) = c("ReachName", "Cover_Wood_HQ","Cover_Wood_LF", "Cover_Wood_TF",
                         "Pool_HQ","Pool_LF","Pool_TF",
                         "Coarse_Substrate_HQ","Coarse_Substrate_LF","Coarse_Substrate_TF")
  output_compare_x = rbind(output_compare_x, output_x)
  
}

rownames(output_compare_x) = seq(1,nrow(output_compare_x))
output_compare_x = as.data.frame(output_compare_x)
