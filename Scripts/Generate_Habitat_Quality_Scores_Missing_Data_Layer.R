
# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Script to Generate Data Gaps Layer for Webmap  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# COLUMNS: Reach Name, Basin, Assessment Unit, Data Gaps, (Primary) Data Source

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Start the Data Frame
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Quality_Data_Gaps = Habitat_Quality_Scores[, c("ReachName","Basin","Assessment.Unit")]

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      go through each habitat attribute and list whether missing or not
# -----------------------------------------------------------------------------------------------------------------------------------------------

# --------- NEW - pull core metrics AND HQ metrics -------------
HQ_metrics = c("Bank Stability",  "Channel Stability", "Coarse Substrate", "Cover- Wood", "Flow- Summer Base Flow" ,     
               "Off-Channel- Floodplain" , "Off-Channel- Side-Channels" ,"Pool Quantity & Quality" ,    
               "Riparian- Canopy Cover", "Riparian-Disturbance" , "Temperature- Rearing" )

# --------- pull core metrics for specific species ---------
rows_pull = c()
for(species_x in core_metric_missing_data_species){
  x = which(Attribute_LifeStage_Crosswalk$Species == species_x)
  rows_pull = c(rows_pull,x)
}
rows_pull = rows_pull[order(rows_pull)]
Attribute_LifeStage_Crosswalk_updated = Attribute_LifeStage_Crosswalk[rows_pull,]
# ------------ pull core metrics only ----------
Attribute_LifeStage_Crosswalk_updated = Attribute_LifeStage_Crosswalk_updated[which(Attribute_LifeStage_Crosswalk_updated$`Life Stage Core Metric?` == "x"), ]

Core_metrics = unique(Attribute_LifeStage_Crosswalk_updated$`Habitat Attribute`)

# ------------ combine HQ and Core metrics -------------
missing_data_metrics = c(HQ_metrics, Core_metrics)
missing_data_metrics = unique(missing_data_metrics)[order(unique(missing_data_metrics))]       

# ------- OLD - just pulled HQ scores (June 25, 2021) --------
#habitat_attributes_for_missing_layer = c("BankStability_score",  "ChannelStability_score", "CoarseSubstrate_score", "Cover-Wood_score", "Flow-SummerBaseFlow_score" ,     
#                                        "Off-Channel-Floodplain_score" , "Off-Channel-Side-Channels_score" ,"PoolQuantity&Quality_score" ,    
#                                        "Riparian-CanopyCover_score", "Riparian-Disturbance_score" , "Temperature-Rearing_score" )
#habitat_attributes_for_missing_layer_COLUMN_NAMES = c("Bank Stability",  "Channel Stability", "Coarse Substrate", "Cover-Wood", "Flow-Summer Base Flow" ,     
#                                                      "Off-Channel-Floodplain" , "Off-Channel-Side-Channels" ,"Pool Quantity & Quality" ,    
#                                                      "Riparian-Canopy Cover", "Riparian-Disturbance" , "Temperature-Rearing" )

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Loop through to identify missing data 
# -----------------------------------------------------------------------------------------------------------------------------------------------

data_gap_df = as.data.frame( Habitat_Quality_Data_Gaps$ReachName ) 
colnames(data_gap_df) = c("ReachName")

for(habitat_attribute_x in missing_data_metrics){

  # ------------------- add to Habitat_Quality_Data_Gaps --------------
  # 
  # ---------------- pull only rows with the habitat_attribute_x in Habitat_Attribute_Scores --------
  Habitat_Attribute_Scores_x = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x), ]
  # -------------------- pull if present -----------------
  if(nrow(Habitat_Attribute_Scores_x)>0){
    present_data_x = which(Habitat_Attribute_Scores_x$Habitat_Attribute_Score > 0)
    Habitat_Attribute_Scores_x = Habitat_Attribute_Scores_x[ ,c("ReachName", "Assessment.Unit") ]
    Habitat_Attribute_Scores_x$Data_Presence = "missing"
    Habitat_Attribute_Scores_x$Data_Presence[present_data_x] = "present"
    Habitat_Attribute_Scores_x = Habitat_Attribute_Scores_x[,c("ReachName", "Data_Presence")]
    colnames(Habitat_Attribute_Scores_x) = c("ReachName",habitat_attribute_x )
    
    # ----------------- merge ---------------
    data_gap_df = merge(data_gap_df, Habitat_Attribute_Scores_x, by="ReachName", all.x=TRUE)
  }else{
    print("Data not in Habitat Attribute Scores: ")
    print(habitat_attribute_x)
  }

}


# -----------------------------------------------------------------------------------------------------------------------------------------------
#      List habitat attributes that are missing
# -----------------------------------------------------------------------------------------------------------------------------------------------

colnames_habitat_attributes = colnames(data_gap_df)
all_missing_data = as.data.frame(data_gap_df[,c("ReachName")])
all_missing_data$missing_data_x = NA

for(row_x in 1:nrow(data_gap_df) ){
  # ----------- identify which attributes are missing -----
  missing_x = which(data_gap_df[row_x, ] == "missing")
  missing_habitat_attribute_x = colnames_habitat_attributes[missing_x]
  # ------------ add to other reaches -----------
  row_cell_x = paste(missing_habitat_attribute_x, collapse=", ")
  all_missing_data$missing_data_x[row_x] = row_cell_x
  
}
colnames(all_missing_data) = c("ReachName","Data_Gap")

# -------------- add column --------
Habitat_Quality_Data_Gaps = merge(Habitat_Quality_Data_Gaps,  all_missing_data, by="ReachName", all.x=TRUE)

# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Add Data Source
# -----------------------------------------------------------------------------------------------------------------------------------------------
habitat_raw_data_x = habitat_raw_data[,c("ReachName","Data_Source")]
colnames(habitat_raw_data_x) = c("ReachName", "Primary_Data_Source")
Habitat_Quality_Data_Gaps = merge(Habitat_Quality_Data_Gaps, habitat_raw_data_x, by="ReachName", all.x=TRUE)


# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Save Data Layer
# -----------------------------------------------------------------------------------------------------------------------------------------------

output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing.xlsx', sep="")
write_xlsx(Habitat_Quality_Data_Gaps,output_path_x )

# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Just Double Checking
# -----------------------------------------------------------------------------------------------------------------------------------------------

reach_x = "Wenatchee River Beaver 01"

# -------- Data PRESENT for reach data in Habitat Attribute Scores ----------
print("Data PRESENT for this reach")
Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & Habitat_Attribute_Scores$Habitat_Attribute_Score > 0 ) ] [order( Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & Habitat_Attribute_Scores$Habitat_Attribute_Score > 0 ) ] ) ] 
# -------- Data MISSING for reach data in Habitat Attribute Scores ----------
# NOTE: will have more in this list then the last list - SINCE data gaps is only for Habitat Quality metrics and Core Metrics
print("Data MISSING for this reach (from Habtat Attribute Scores")
Habitat_Attribute_Scores$Habitat_Attribute[ which(Habitat_Attribute_Scores$ReachName == reach_x & is.na(Habitat_Attribute_Scores$Habitat_Attribute_Score)  ) ] [order( Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & is.na(Habitat_Attribute_Scores$Habitat_Attribute_Score) ) ] ) ] 
# ----------------- Data listed as missing for Data Gap layer --------
print("Data MISSING for this reach (from data gaps layer)")
Habitat_Quality_Data_Gaps$Data_Gap[Habitat_Quality_Data_Gaps$ReachName == reach_x]
