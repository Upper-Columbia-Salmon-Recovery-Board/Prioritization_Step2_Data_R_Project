




# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Script to Generate Data Gaps Layer for Webmap  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------




# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Start the Data Frame
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Quality_Data_Gaps = Habitat_Quality_Scores[, c("ReachName","Basin","Assessment.Unit")]

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      go through each habitat attribute and list whether missing or not
# -----------------------------------------------------------------------------------------------------------------------------------------------

habitat_attributes_for_missing_layer = c("BankStability_score",  "ChannelStability_score", "CoarseSubstrate_score", "Cover-Wood_score", "Flow-SummerBaseFlow_score" ,     
                                        "Off-Channel-Floodplain_score" , "Off-Channel-Side-Channels_score" ,"PoolQuantity&Quality_score" ,    
                                        "Riparian-CanopyCover_score", "Riparian-Disturbance_score" , "Temperature-Rearing_score" )

habitat_attributes_for_missing_layer_COLUMN_NAMES = c("Bank Stability",  "Channel Stability", "Coarse Substrate", "Cover-Wood", "Flow-Summer Base Flow" ,     
                                                      "Off-Channel-Floodplain" , "Off-Channel-Side-Channels" ,"Pool Quantity & Quality" ,    
                                                      "Riparian-Canopy Cover", "Riparian-Disturbance" , "Temperature-Rearing" )

i = 0
for(habitat_attribute_x in habitat_attributes_for_missing_layer){
  i = i + 1
  # ------------------- add to Habitat_Quality_Data_Gaps --------------
  Habitat_Quality_Data_Gaps[,habitat_attributes_for_missing_layer_COLUMN_NAMES[i]] = "missing"
  # -------------------- pull if present -----------------
  present_data_x = which(Habitat_Quality_Scores[,habitat_attribute_x]>0)
  Habitat_Quality_Data_Gaps[present_data_x,habitat_attributes_for_missing_layer_COLUMN_NAMES[i]] = "present"
}

# -----------------------------------------------------------------------------------------------------------------------------------------------
#      Save Data Layer
# -----------------------------------------------------------------------------------------------------------------------------------------------

output_path_x =  paste(output_path,'Habitat_Quality_Data_Missing.xlsx', sep="")
write_xlsx(Habitat_Quality_Data_Gaps,output_path_x )


