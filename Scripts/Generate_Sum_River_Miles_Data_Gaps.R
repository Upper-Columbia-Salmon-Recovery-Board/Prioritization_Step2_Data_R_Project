

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



