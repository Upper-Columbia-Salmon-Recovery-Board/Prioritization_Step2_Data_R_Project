

# ---------------------------------------------------------------------------
#
#      SCRIPT: Compare EDT and RTT data
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


# ------------------------------------------------------------------------------------ 
#
#                          Compare EDT flow and RTT data 
#
# ------------------------------------------------------------------------------------ 

EDT_RTT_habitat_attribute_comparison = c()
# for(reach_x in Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']]$ReachName){

for(reach_x in unique(HabitatAttribute_Ratings_level_2$Reach)){
  
  # --------- if EDT reach is in RTT reach network ---------
  if(any(Habitat_Attribute_Scores$ReachName == reach_x)){
    
    #print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ")
    #print(paste("REACH:", reach_x))
    # --------- get EDT and Habitat Attribute rows -------------- 
    x_EDT = which(HabitatAttribute_Ratings_level_2$Reach == reach_x)
    EDT_reach_x = HabitatAttribute_Ratings_level_2[x_EDT,]
    x_RTT = which(Habitat_Attribute_Scores$ReachName == reach_x)
    RTT_reach_x = Habitat_Attribute_Scores[x_RTT,]
    RTT_HQ_reach_x = Habitat_Quality_Scores[which(Habitat_Quality_Scores$ReachName == reach_x), ]
    
    # unique(EDT_reach_x$`RTT Habitat Attribute`)[order( unique(EDT_reach_x$`RTT Habitat Attribute`))]
    # unique(RTT_reach_x$Habitat_Attribute)[order( unique(RTT_reach_x$Habitat_Attribute))]
    
    
    # -------------------  Contaminants -------------------
    EDT_contaminants = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Contaminants"]
    #print("EDT Contaminants: ")
    if(length(EDT_contaminants) > 1){ EDT_contaminants = min(EDT_contaminants) }
    if(length(EDT_contaminants) == 0){  EDT_contaminants = NA}
    
    
    RTT_contaminants = RTT_reach_x$Habitat_Attribute_Score[ RTT_reach_x$Habitat_Attribute   == "Contaminants"]
    #print("RTT Contaminants: ")
    if(length(RTT_contaminants) > 1){ RTT_contaminants = min(RTT_contaminants) }
    if(length(RTT_contaminants) == 0){  RTT_contaminants = NA}
    
    contaminants_dif = EDT_contaminants - RTT_contaminants
    
    # -------------------  Flow- Scour -------------------
    EDT_flow_scour = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Flow- Scour"]
    #print("EDT Flow- Scour: ")
    if(length(EDT_flow_scour) > 1){ EDT_flow_scour = min(EDT_flow_scour) }
    if(length(EDT_flow_scour) == 0){  EDT_flow_scour = NA}
    
    RTT_flow_scour = RTT_reach_x$Habitat_Attribute_Score[ RTT_reach_x$Habitat_Attribute   == "Flow- Scour"]
    #print("RTT Flow- Scour: ")
    if(length(RTT_flow_scour) > 1){ RTT_flow_scour = min(RTT_flow_scour) }
    if(length(RTT_flow_scour) == 0){  RTT_flow_scour = NA}
    
    flow_scour_dif = EDT_flow_scour - RTT_flow_scour
    
    # -------------------  Flow- Summer Base Flow -------------------
    EDT_flow_base_flow = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Flow- Summer Base Flow"]
    #print("EDT Flow- Summer Base Flow: ")
    if(length(EDT_flow_base_flow) > 1){ EDT_flow_base_flow = min(EDT_flow_base_flow) }
    if(length(EDT_flow_base_flow) == 0){  EDT_flow_base_flow = NA}
    
    EDT_flow_water_withdrawls = EDT_reach_x$Function_Condition[EDT_reach_x$`EDT Attribute` == "Water Withdrawals"]
    #print("EDT Flow- Water Withdrawls: ")
    if(length(EDT_flow_water_withdrawls) > 1){ EDT_flow_water_withdrawls = min(EDT_flow_water_withdrawls) }
    if(length(EDT_flow_water_withdrawls) == 0){  EDT_flow_water_withdrawls = NA}
    
    RTT_flow_base_flow = RTT_reach_x$Habitat_Attribute_Score[ RTT_reach_x$Habitat_Attribute   == "Flow- Summer Base Flow"]
    #print("RTT Flow- Summer Base Flow: ")
    if(length(RTT_flow_base_flow) > 1){ RTT_flow_base_flow = min(RTT_flow_base_flow) }
    if(length(RTT_flow_base_flow) == 0){  RTT_flow_base_flow = NA}
    
    flow_base_flow_dif = min(EDT_flow_base_flow) - RTT_flow_base_flow
    
    # -------------------  Food- Food Web Resources -------------------
    EDT_food = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Food- Food Web Resources"]
    #print("EDT Food- Food Web Resources: ")
    if(length(EDT_food) > 1){ EDT_food = min(EDT_food) }
    if(length(EDT_food) == 0){  EDT_food = NA}
    
    EDT_salmon_carcass = EDT_reach_x$Function_Condition[EDT_reach_x$`EDT Attribute` == "Salmon Carcasses"]
    #print("EDT Food- Salmon Carcasses: ")
    if(length(EDT_salmon_carcass) > 1){ EDT_salmon_carcass = min(EDT_salmon_carcass) }
    if(length(EDT_salmon_carcass) == 0){  EDT_salmon_carcass = NA}
    
    RTT_food = RTT_reach_x$Habitat_Attribute_Score[ RTT_reach_x$Habitat_Attribute   == "Food- Food Web Resources"]
    #print("RTT Food- Food Web Resources: ")
    if(length(RTT_food) > 1){ RTT_food = min(RTT_food) }
    if(length(RTT_food) == 0){  RTT_food = NA}
    
    food_dif = EDT_food - RTT_food
    
    # -------------------  Temperature- Rearing -------------------
    EDT_temp = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Temperature- Rearing"]
    #print("EDT Temperature- Rearing: ")
    if(length(EDT_temp) > 1){ EDT_temp = min(EDT_temp) }
    if(length(EDT_temp) == 0){  EDT_temp = NA}
    
    RTT_temp = RTT_reach_x$Habitat_Attribute_Score[ RTT_reach_x$Habitat_Attribute   == "Temperature- Rearing"]
    #print("RTT Temperature- Rearing: ")
    if(length(RTT_temp) > 1){ RTT_temp = min(RTT_temp) }
    if(length(RTT_temp) == 0){  RTT_temp = NA}
    
    temp_dif = EDT_temp - RTT_temp
    
    # -------------------  Riparian -------------------
    EDT_riparian = EDT_reach_x$Function_Condition[EDT_reach_x$`RTT Habitat Attribute` == "Riparian"]
    #print("EDT Riparian: ")
    if(length(EDT_riparian) > 1){ EDT_riparian = min(EDT_riparian) }
    if(length(EDT_riparian) == 0){  EDT_riparian = NA}
    
    RTT_riparian = RTT_HQ_reach_x$`Riparian-CanopyCover_score`
    #print("RTT Riparian: ")
    if(length(RTT_riparian) > 1){ RTT_riparian = min(RTT_riparian) }
    if(length(RTT_riparian) == 0){  RTT_riparian = NA}
    
    riparian_dif = EDT_riparian - RTT_riparian
    
    # ------------------- combine into one row ----------
    x_row = t( as.data.frame(c(reach_x, 
                               EDT_contaminants, RTT_contaminants, contaminants_dif,
                               EDT_flow_scour, RTT_flow_scour, flow_scour_dif,
                               EDT_flow_base_flow, EDT_flow_water_withdrawls, RTT_flow_base_flow, flow_base_flow_dif,
                               EDT_food, EDT_salmon_carcass, RTT_food,  food_dif,
                               EDT_temp,  RTT_temp, temp_dif,
                               EDT_riparian, RTT_riparian, riparian_dif
    )) )
    
    colnames(x_row) = c("ReachName",  
                        "EDT_contaminants", "RTT_contaminants", "contaminants_dif",
                        "EDT_flow_scour", "RTT_flow_scour", "flow_scour_dif",
                        "EDT_flow_base_flow_width", 'EDT_flow_base_flow_water_withdrawls', 'RTT_flow_base_flow', "flow_base_flow_dif",
                        'EDT_food_benthic',  'EDT_food_salmon_carcass',"RTT_food", "food_web_dif",
                        "EDT_temperature_rearing",  "RTT_temperature_rearing", "temperature_rearing_dif",
                        "EDT_riparian", "RTT_riparian", "riparian_dif")
    
    
    
    # -------------- combine ----------
    EDT_RTT_habitat_attribute_comparison = rbind(EDT_RTT_habitat_attribute_comparison, x_row) 
    
  }
  
}

rownames(EDT_RTT_habitat_attribute_comparison) = seq(1, nrow(EDT_RTT_habitat_attribute_comparison))
EDT_RTT_habitat_attribute_comparison = as.data.frame(EDT_RTT_habitat_attribute_comparison)

output_path_x =  paste(output_path,'EDT_RTT_habitat_attribute_score_comparison_REACHES.xlsx', sep="")
write_xlsx(EDT_RTT_habitat_attribute_comparison,output_path_x )

# ---------------------- summarize the EDT and RTT difference findings -----------------
dif_column = colnames(EDT_RTT_habitat_attribute_comparison)[c(4,7,11,15,18,21)]
EDT_RTT_dif_summary = c()
for(colx in dif_column){
  
  # ------------- get column data --------
  datax = as.numeric( as.character( EDT_RTT_habitat_attribute_comparison[,colx] ))
  # ---------- identify number of habitat attribute scores that are over and under -------
  EDT_over_x = length( which(datax > 0) )
  EDT_under_x = length( which(datax < 0) )
  # -------------- combine data ---------
  output_x = t( as.data.frame( c(substr(colx, 1,nchar(colx)-4), EDT_over_x, EDT_under_x )) )
  rownames(output_x) = substr(colx, 1,nchar(colx)-4)
  colnames(output_x) = c("habitat_attribute", "EDT_score_over_RTT", "EDT_score_under_RTT")
  EDT_RTT_dif_summary = rbind(EDT_RTT_dif_summary, output_x)
  
}
# -------------- summarize data ---------------
EDT_RTT_dif_summary = as.data.frame(EDT_RTT_dif_summary)
colnames(EDT_RTT_dif_summary) = c("habitat_attribute", "EDT_greater_than_RTT", "RTT_greater_than_EDT")
EDT_RTT_dif_summary$PRCNT_Okanogan_Reaches_EDT_greater_than_RTT = round( (as.numeric(EDT_RTT_dif_summary$EDT_greater_than_RTT) / nrow(EDT_RTT_habitat_attribute_comparison) ), 2)
EDT_RTT_dif_summary$PRCNT_Okanogan_Reaches_RTT_greater_than_EDT = round( (as.numeric(EDT_RTT_dif_summary$RTT_greater_than_EDT) / nrow(EDT_RTT_habitat_attribute_comparison) ), 2)

# -------------- output data ---------------
output_path_x =  paste(output_path,'EDT_RTT_habitat_attribute_score_comparison_SUMMARY.xlsx', sep="")
write_xlsx(EDT_RTT_dif_summary,output_path_x )

