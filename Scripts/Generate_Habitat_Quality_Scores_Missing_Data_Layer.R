
# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Script to Generate Data Gaps Layer for Webmap  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# COLUMNS: Reach Name, Basin, Assessment Unit, Data Gaps, (Primary) Data Source
# Data Gaps/Missing data - of HQ metrics and core metrics

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#          Data Gaps
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Start the Data Frame
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Quality_Data_Gaps = Habitat_Quality_Scores[, c("ReachName","Basin","Assessment.Unit")]

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      go through each habitat attribute and list whether missing or not
# -----------------------------------------------------------------------------------------------------------------------------------------------

# --------- NEW - pull core metrics AND HQ metrics -------------
HQ_metrics = c("Bank Stability",  "Channel Stability", "Coarse Substrate", "Cover- Wood", "Flow- Summer Base Flow" ,     
               "Floodplain Connectivity" , "Off-Channel/Side-Channels" ,"Pool Quantity & Quality" ,    
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
#      Loop through OKANOGAN to identify missing data 
# -----------------------------------------------------------------------------------------------------------------------------------------------

for(habitat_attribute_x in missing_data_metrics){
  
  #col_data_gap_df = which(colnames(data_gap_df) == habitat_attribute_x)
  # ------------------- add to Habitat_Quality_Data_Gaps --------------
  # 
  # ---------------- pull only rows with the habitat_attribute_x in Habitat_Attribute_Scores --------
  Habitat_Attribute_Scores_x = Habitat_Attribute_Scores_Okanogan[which(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute == habitat_attribute_x), ]
  # -------------------- pull if present -----------------
  if( nrow(Habitat_Attribute_Scores_x)>0 ){
    for(rowx in 1:nrow(Habitat_Attribute_Scores_x)){
      reach_x = Habitat_Attribute_Scores_x$ReachName[rowx]
      row_data_gap_df = which(data_gap_df$ReachName == reach_x)
      
      # -------------- if okanogan data is missing (NA) for this habitat attribute and reach -----------
      if( is.na(Habitat_Attribute_Scores_x$Habitat_Attribute_Score[rowx]) ){ 
        data_gap_df[row_data_gap_df,habitat_attribute_x] = "missing" 
        
      # ----------------- IF okanogan data is present for this habitat attribute and reach ---------
      }else if(Habitat_Attribute_Scores_x$Habitat_Attribute_Score[rowx] > 0){
        data_gap_df[row_data_gap_df,habitat_attribute_x] = "present"
        
      # -------------- IF okanogan data is not present for this habitat attribute and reach -----  
      }else{data_gap_df[row_data_gap_df,habitat_attribute_x] = "missing" }
    }

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
#      IF no data is missing - add "no data are missing"
# -----------------------------------------------------------------------------------------------------------------------------------------------
no_data_missing_x = which(Habitat_Quality_Data_Gaps$Data_Gap == "")
Habitat_Quality_Data_Gaps$Data_Gap[no_data_missing_x] = "no data are missing"

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      IF okanogan - add EDT 
# -----------------------------------------------------------------------------------------------------------------------------------------------
Okanogan_Basin_x = which(Habitat_Quality_Data_Gaps$Basin == "Okanogan")
Habitat_Quality_Data_Gaps$Primary_Data_Source[Okanogan_Basin_x] = "Okanogan EDT"

# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Remove the layers where no data are missing
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Quality_Data_Gaps = Habitat_Quality_Data_Gaps[which(Habitat_Quality_Data_Gaps$Data_Gap != "no data are missing"),]

# -----------------------------------------------------------------------------------------------------------------------------------------------
#    Update Attribute Names
# -----------------------------------------------------------------------------------------------------------------------------------------------
# -------------------- Fines/Embeddedness ------------------
Habitat_Quality_Data_Gaps$Data_Gap  = gsub("%Fines/Embeddedness", "PRCNT Fines and Embeddedness", Habitat_Quality_Data_Gaps$Data_Gap )
Habitat_Quality_Data_Gaps$Data_Gap  = gsub("% Fines/Embeddedness", "PRCNT Fines and Embeddedness", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Pool Quantity and Quality ------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("PoolQuantity&Quality", "Pool Quantity and Quality", Habitat_Quality_Data_Gaps$Data_Gap )
Habitat_Quality_Data_Gaps$Data_Gap = gsub("Pool Quantity & Quality", "Pool Quantity and Quality", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Entrainment and Stranding ------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("Entrainment/Stranding", "Entrainment and Stranding", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Floodplain Connectivity ------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Off-Channel/Side-Channels------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Floodplain Connectivity ------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", Habitat_Quality_Data_Gaps$Data_Gap )
# -------------------- Floodplain Connectivity ------------------
Habitat_Quality_Data_Gaps$Data_Gap = gsub("Off-Channel- Floodplain", "Floodplain Connectivity", Habitat_Quality_Data_Gaps$Data_Gap )

# -----------------------------------------------------------------------------------------------------------------------------------------------
#    Add whether a reach was a potential reach layer (yes/no)  potential_priority_reach_yes_no
# -----------------------------------------------------------------------------------------------------------------------------------------------
Output_ALL_species_and_reaches_SLIM = Output_ALL_species_and_reaches[,c("ReachName","Potential_Priority_Reach_all_species_restoration_or_protection", "Priority_Tiers_all_species_restoration_or_protection")]
#colnames(Output_ALL_species_and_reaches_SLIM)[1] = "Reach Name"
Habitat_Quality_Data_Gaps2  = merge(Habitat_Quality_Data_Gaps, Output_ALL_species_and_reaches_SLIM, by="ReachName", all.x=TRUE)

colnames(Habitat_Quality_Data_Gaps2) = c("Reach Name", "Basin", "Assessment Unit", "Missing Data", "Primary Data Source", "Potential Priority Reach (yes or no)" , "Priority AU Tiers all species restoraiton or protection (yes or no)")
 

  
# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Save Data Layer
# -----------------------------------------------------------------------------------------------------------------------------------------------

output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing.xlsx', sep="")
write.xlsx(Habitat_Quality_Data_Gaps2,output_path_x )

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#    Prepare and Output for BPA
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------------
#    Add whether a reach was a potential reach layer (yes/no)  potential_priority_reach_yes_no
# -----------------------------------------------------------------------------------------------------------------------------------------------
Output_ALL_species_and_reaches_SLIM_SprChn_Stld = Output_ALL_species_and_reaches[,c("ReachName","Potential_Priority_Reach_all_species_restoration_or_protection", "Spring_Chinook.AU.Restoration.Rank", "Spring_Chinook.AU.Protection.Rank",
                                                                        "Steelhead.AU.Restoration.Rank", "Steelhead.AU.Protection.Rank" )]
#colnames(Output_ALL_species_and_reaches_SLIM)[1] = "Reach Name"
Habitat_Quality_Data_Gaps_SprChn_Stld = merge(Habitat_Quality_Data_Gaps, Output_ALL_species_and_reaches_SLIM_SprChn_Stld, by="ReachName", all.x=TRUE)

colnames(Habitat_Quality_Data_Gaps_SprChn_Stld) = c("Reach Name", "Basin", "Assessment Unit", "Missing Data", "Primary Data Source", "Potential Priority Reach (yes or no)" , 
                                        "Tier 1 Spring Chinook - Restoration", "Tier 1 Spring Chinook - Protection", "Tier 1 Steelhead - Restoration", "Tier 1 Steelhead - Protection")

# ---------------- only pull Tier 1 ------------------

tier1_x = which(Habitat_Quality_Data_Gaps_SprChn_Stld$`Tier 1 Spring Chinook - Restoration` == 1 |
                  Habitat_Quality_Data_Gaps_SprChn_Stld$`Tier 1 Spring Chinook - Protection` == 1 |
                  Habitat_Quality_Data_Gaps_SprChn_Stld$`Tier 1 Steelhead - Restoration` == 1 |
                  Habitat_Quality_Data_Gaps_SprChn_Stld$`Tier 1 Steelhead - Protection` == 1 )

Habitat_Quality_Data_Gaps_SprChn_Stld = Habitat_Quality_Data_Gaps_SprChn_Stld[tier1_x, ]
remove_col_x = "Missing Data"
columns_to_pull = colnames(Habitat_Quality_Data_Gaps_SprChn_Stld)
columns_to_pull = columns_to_pull[-which(columns_to_pull==remove_col_x)]

# ---------------------- Pool Quality and Quantity ----------------
Habitat_Quality_Data_Gaps_Pool_True_False = grepl("Pool Quantity and Quality", Habitat_Quality_Data_Gaps_SprChn_Stld$`Missing Data`)
Habitat_Quality_Data_Gaps_Pool = Habitat_Quality_Data_Gaps_SprChn_Stld[Habitat_Quality_Data_Gaps_Pool_True_False, ] 
Habitat_Quality_Data_Gaps_Pool = Habitat_Quality_Data_Gaps_Pool[columns_to_pull]
output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing_Pool_Quant_and_Qual.xlsx', sep="")
write.xlsx(Habitat_Quality_Data_Gaps_Pool,output_path_x )

# --------------------- Coarse Substrate -----------------
Habitat_Quality_Data_Gaps_Coarse_Substrate_True_False = grepl("Coarse Substrate", Habitat_Quality_Data_Gaps_SprChn_Stld$`Missing Data`)
Habitat_Quality_Data_Gaps_Coarse_Substrate = Habitat_Quality_Data_Gaps_SprChn_Stld[Habitat_Quality_Data_Gaps_Coarse_Substrate_True_False, ] 
Habitat_Quality_Data_Gaps_Coarse_Substrate = Habitat_Quality_Data_Gaps_Coarse_Substrate[columns_to_pull]
output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing_Coarse_Substrate.xlsx', sep="")
write.xlsx(Habitat_Quality_Data_Gaps_Coarse_Substrate,output_path_x )

# -------------------- Cover- Wood ------------------
Habitat_Quality_Data_Gaps_Cover_Wood_True_False = grepl("Cover- Wood", Habitat_Quality_Data_Gaps_SprChn_Stld$`Missing Data`)
Habitat_Quality_Data_Gaps_Cover_Wood = Habitat_Quality_Data_Gaps_SprChn_Stld[Habitat_Quality_Data_Gaps_Cover_Wood_True_False, ] 
Habitat_Quality_Data_Gaps_Cover_Wood = Habitat_Quality_Data_Gaps_Cover_Wood[columns_to_pull]
output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing_Cover_Wood.xlsx', sep="")
write.xlsx(Habitat_Quality_Data_Gaps_Cover_Wood,output_path_x )


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#          Tier 1 Restoration/Protectoin OR data collected before 2011
#
# -----------------------------------------------------------------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Generate layer that is just A) Tier 1s (already prepped) and B) has data OLDER than 2011 (10 years old or older)
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan = c()

for(reach_x in habitat_raw_data$ReachName){
  i = which(habitat_raw_data$ReachName == reach_x)
  
  if( habitat_raw_data$Basin[i] != "Okanogan" ){
    
    # ------------ identify year of data collection ----------------
    year_x = habitat_raw_data$Year.Data.Collection[i]
    year_x_split = strsplit(year_x, ",")[[1]]
    if(length(year_x_split) > 1){
      year_x = max(as.numeric(year_x_split))
    }else{
      year_x = as.numeric(year_x)
    }
    # ----------------- tier 1 protection or restoration --------------
    Priority_Tiers_all_species_restoration_or_protection_x = Output_ALL_species_and_reaches$Priority_Tiers_all_species_restoration_or_protection[which(Output_ALL_species_and_reaches$ReachName == reach_x)]
    
    # --------------------- list data gaps if present ----------
    data_gaps_index_x = which(Habitat_Quality_Data_Gaps$`Reach Name` == reach_x)
    if(length(data_gaps_index_x) > 0){
      missing_data_x = Habitat_Quality_Data_Gaps[data_gaps_index_x, c("Missing Data")]
    }else{
      missing_data_x = "no missing data"
    }
    # ----------------- output ------------
    output_x = c( habitat_raw_data[i,c("ReachName" )],
                  habitat_raw_data[i,c("Basin" )],
                  habitat_raw_data[i,c("Assessment.Unit" )],
                  habitat_raw_data[i,c("Data_Source" )], 
                  year_x,   Priority_Tiers_all_species_restoration_or_protection_x,  missing_data_x  )
    output_x =  t( as.data.frame(unlist(output_x)) )
    output_x = as.data.frame(output_x)
    colnames(output_x) = c("ReachName", "Basin", "Assessment.Unit", "Data_Source","Survey_Year", 
                           "Priority_Tier_all_species_restoration_or_protection","Missing_Data")
    
    # --------------- if A) a Tier 1 and B) data survey older than 2011 -----------
    if( is.na(output_x$Survey_Year)){year_x = 0}
    if( (year_x < 2011 | output_x$Missing_Data != "no missing data" ) & output_x$Priority_Tier_all_species_restoration_or_protection == "yes"){
      output_x$Priority_Tier_and_missing_data_OR_survey_before_2011 = "yes"
    }else{
      output_x$Priority_Tier_and_missing_data_OR_survey_before_2011 = "no"
    }
    # -------------- combine -------------
    Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan = rbind( Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan, output_x)
      
  }
  
}
Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan$Survey_Year = as.numeric(Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan$Survey_Year)


# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Save Output
# -----------------------------------------------------------------------------------------------------------------------------------------------



output_path_x =  paste(output_path,'ALL_Data_Wenatchee_Entiat_Methow,list_potential_data_gaps_Tier1_and_survey_year.xlsx', sep="")
write.xlsx(Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan,output_path_x )


# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Calculate total length of missing data (for all AND Priority)
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------------- only pull reaches taht pass the filter -------------
Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE = Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan[which(Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan$Priority_Tier_and_missing_data_OR_survey_before_2011 == "yes"),]
# remove Brender Creek - since year was not in there (will add in later)
# Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE[-which(Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$ReachName == "Brender Creek 01"),]


unique_AUs = unique(Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$Assessment.Unit)
table_missing_data = c()
for(AUx in unique_AUs){
  
  # ---------------- basin -------------
  basin_x = Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$Basin[Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$Assessment.Unit == AUx]
  basin_x = basin_x[1]
  # ------------ pull reaches -------------
  reaches_x = Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$ReachName[Habitat_Quality_Data_Gaps_10_years_older_no_Okanogan_TRUE$Assessment.Unit == AUx]
  # ------------- add up length -----------------
  length_x = c()
  for(reach_x in reaches_x){
    length_x = rbind(length_x, habitat_raw_data$Length..miles.[which(habitat_raw_data$ReachName == reach_x)] )
  }
  length_x = as.numeric(length_x)
  length_total = sum(length_x, na.rm = TRUE)
  
  
  output_x = t(as.data.frame(c(AUx, basin_x , paste(reaches_x, sep=",", collapse=","), length_total) ) )
  row.names(output_x) = AUx
  table_missing_data = rbind(table_missing_data, output_x)
                           
}

colnames(table_missing_data) = c("Assessment Unit", "Basin", "ReachName", "total_length_miles")
table_missing_data = as.data.frame(table_missing_data)
table_missing_data$total_length_miles = as.numeric(table_missing_data$total_length_miles)
table_missing_data$total_length_miles = round(table_missing_data$total_length_miles, digits=1)
# aggregate(x$Frequency, by=list(Group=x$Category), FUN=sum)


output_path_x =  paste(output_path,'Data_Gap_Wenatchee_Entiat_Methow,Tier1_and_survey_before_2011.xlsx', sep="")
write.xlsx(table_missing_data,output_path_x )


any(AU_Ranks_data[AU_Ranks_data$`Assessment Unit` == "Rock Creek",] == 1)



# -----------------------------------------------------------------------------------------------------------------------------------------------
#     Just Double Checking
# -----------------------------------------------------------------------------------------------------------------------------------------------

reach_x = "Salmon 16-4"

# -------- Data PRESENT for reach data in Habitat Attribute Scores ----------
print("Data PRESENT for this reach")
Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & Habitat_Attribute_Scores$Habitat_Attribute_Score > 0 ) ] [order( Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & Habitat_Attribute_Scores$Habitat_Attribute_Score > 0 ) ] ) ] 
# -------- Data MISSING for reach data in Habitat Attribute Scores ----------
# NOTE: will have more in this list then the last list - SINCE data gaps is only for Habitat Quality metrics and Core Metrics
print("Data MISSING for this reach (from Habtat Attribute Scores")
Habitat_Attribute_Scores$Habitat_Attribute[ which(Habitat_Attribute_Scores$ReachName == reach_x & is.na(Habitat_Attribute_Scores$Habitat_Attribute_Score)  ) ] [order( Habitat_Attribute_Scores$Habitat_Attribute[which(Habitat_Attribute_Scores$ReachName == reach_x & is.na(Habitat_Attribute_Scores$Habitat_Attribute_Score) ) ] ) ] 
Habitat_Attribute_Scores_Okanogan$Habitat_Attribute[ which(Habitat_Attribute_Scores_Okanogan$ReachName == reach_x & is.na(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute_Score)  ) ] [order( Habitat_Attribute_Scores_Okanogan$Habitat_Attribute[which(Habitat_Attribute_Scores_Okanogan$ReachName == reach_x & is.na(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute_Score) ) ] ) ] 

# ----------------- Data listed as missing for Data Gap layer --------
print("Data MISSING for this reach (from data gaps layer)")
Habitat_Quality_Data_Gaps$`Missing Data`[Habitat_Quality_Data_Gaps$`Reach Name` == reach_x]

# ----------------- double check Okanogan -------------
reach_x = "Salmon 16-4"
Habitat_Quality_Data_Gaps[which(Habitat_Quality_Data_Gaps$`Reach Name` == reach_x),"Missing Data"]
Habitat_Quality_Scores[which(Habitat_Quality_Scores$ReachName == reach_x), 7:15]
Habitat_Quality_Scores_Okanogan[which(Habitat_Quality_Scores_Okanogan$ReachName == reach_x), 7:15]
Habitat_Quality_Scores[which(Habitat_Quality_Scores$ReachName == reach_x), 16:21]
Habitat_Quality_Scores_Okanogan[which(Habitat_Quality_Scores_Okanogan$ReachName == reach_x), 16:21]

Habitat_Attribute_Scores_Okanogan[which(Habitat_Attribute_Scores_Okanogan$ReachName == reach_x),c(1,4,12)]
Habitat_Quality_Scores_Okanogan[which(Habitat_Quality_Scores_Okanogan$ReachName == reach_x),]
Habitat_Quality_Scores_Okanogan$`Riparian-Disturbance_score`[which(Habitat_Quality_Scores_Okanogan$ReachName == reach_x)]

reach_x = "Salmon 16-4"
Habitat_Quality_Data_Gaps[which(Habitat_Quality_Data_Gaps$`Reach Name` == reach_x),"Missing Data"]
for(HQ_metrics_x in HQ_metrics){
  print(HQ_metrics_x)
  print(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute_Score[which(Habitat_Attribute_Scores_Okanogan$ReachName == reach_x & Habitat_Attribute_Scores_Okanogan$Habitat_Attribute == HQ_metrics_x)])
  
  
}
