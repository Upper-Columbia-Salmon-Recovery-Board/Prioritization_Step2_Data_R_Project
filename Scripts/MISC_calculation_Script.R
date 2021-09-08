
# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#     Misc scrpit
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------
#      Get unique Limiting Factors (Unacceptable and At Risk)
# -----------------------------------------------------------------

habitat_attributes_all_x = unique(Habitat_Attribute_Scores$Habitat_Attribute)
priority_reach_all_attributes_unacceptable = paste( Restoration_Prioritization_Output_for_WebMap$`Unacceptable Limiting Factors`,  collapse=",")
priority_reach_all_attributes_at_risk= paste( Restoration_Prioritization_Output_for_WebMap$`At-Risk Limiting Factors`,  collapse=",")

unnaceptable_habitat_attributes_x = c()
for(habitat_attribute_x in habitat_attributes_all_x){
  if( str_detect(priority_reach_all_attributes_unacceptable, habitat_attribute_x, negate = FALSE) ){
    unnaceptable_habitat_attributes_x = c(unnaceptable_habitat_attributes_x,habitat_attribute_x )
  }
}
at_risk_habitat_attributes_x = c()
for(habitat_attribute_x in habitat_attributes_all_x){
  
  if( str_detect(priority_reach_all_attributes_at_risk, habitat_attribute_x, negate = FALSE) ){
    at_risk_habitat_attributes_x = c(at_risk_habitat_attributes_x,habitat_attribute_x )
  }
  
  
}

unlist(strsplit(Restoration_Prioritization_Output_for_WebMap$`Unacceptable Limiting Factors`[100], ",", fixed=TRUE))


output_x = c()
for(i in 1:nrow(Restoration_Prioritization_Output_for_WebMap)){
  x1 = unlist(strsplit(Restoration_Prioritization_Output_for_WebMap$`Unacceptable Limiting Factors`[i], ",", fixed=TRUE))
  x2 = unlist(strsplit(Restoration_Prioritization_Output_for_WebMap$`At-Risk Limiting Factors`[i], ",", fixed=TRUE))
  x = c(x1,x2)
  output_x = c(output_x, x)
}
unique(output_x)[order(unique(output_x))]

# -----------------------------------------------------------------
#      Get unique Action Categories
# -----------------------------------------------------------------

action_categories_all_x = unique(Crosswalk_Habitat_Attributes_and_Actions$`Action Category`)

action_categories_x = c()
for(habitat_attribute_x in action_categories_all_x){
  
  if( str_detect(priority_reach_all_attributes_at_risk, habitat_attribute_x, negate = FALSE) ){
    action_categories_x = c(action_categories_x,habitat_attribute_x )
  }
  
  
}



output_x2 = c()
for(i in 1:nrow(Restoration_Prioritization_Output_Spring_Chinook)){
  x1 = unlist(strsplit(Restoration_Prioritization_Output_Spring_Chinook$`Action Categories`[i], ",", fixed=TRUE))
  output_x2 = c(output_x, x1)
}
unique(output_x2)[order(unique(output_x2))]


# -----------------------------------------------------------------
#     Get Tier 1 (Restoration, Spring Chinook and Steelhead)
#         w/ no riparian data 
# -----------------------------------------------------------------

# ------------- pull reaches with NO riparian data --------------
riparian_canopy_cover_habitat_attribute_scores = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == "Riparian- Canopy Cover"),]
riparian_disturbance_habitat_attribute_scores = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == "Riparian-Disturbance"),]
riparian_canopy_cover_habitat_attribute_scores = riparian_canopy_cover_habitat_attribute_scores[which( is.na( riparian_canopy_cover_habitat_attribute_scores$Habitat_Attribute_Score) ), c("ReachName","Assessment.Unit", "Basin")]
riparian_disturbance_habitat_attribute_scores = riparian_disturbance_habitat_attribute_scores[which( is.na( riparian_disturbance_habitat_attribute_scores$Habitat_Attribute_Score) ), c("ReachName","Assessment.Unit", "Basin")]

# no_riparian_data = habitat_raw_data[which(is.na(habitat_raw_data$UCSRB_RiparianDisturbancePct)), c("ReachName", "Assessment.Unit","Basin"  )]
# no_canopy_cover = habitat_raw_data[which(is.na(habitat_raw_data$UCSRB_CanopyCoverPct)), c("ReachName", "Assessment.Unit" ,"Basin"  )]

# ---------------- get list of Tier 1 restoration AUs (Spring Chinook and Steelhead) ---------
tier_1_reaches = c("SPCHNTier_Restoration","STLTier_Restoration"  )

AU_Ranks_data_tie1_restoration_sprch_stld = c()
for(i in 1:nrow(AU_Ranks_data)){
  tier_1_true_false = FALSE
  if( !is.na(AU_Ranks_data[i,tier_1_reaches[1]]) ){
    if(AU_Ranks_data[i,tier_1_reaches[1]] == 1 ){
      tier_1_true_false = TRUE
    }else{
      tier_1_true_false = FALSE
    }
  }
  
  if( !is.na(AU_Ranks_data[i,tier_1_reaches[2]]) &  tier_1_true_false == FALSE ){
    if(AU_Ranks_data[i,tier_1_reaches[2]] == 1 ){
      tier_1_true_false = TRUE
    }
  }
  
  if(tier_1_true_false){
    AU_Ranks_data_tie1_restoration_sprch_stld = c(AU_Ranks_data_tie1_restoration_sprch_stld,AU_Ranks_data$`Assessment Unit`[i])
  }
    

}

# ------------- pull reaches in these Assessment Units -------------
tier1_sprchn_stld_no_riparian = c()
for(i in 1:nrow(riparian_disturbance_habitat_attribute_scores) ){
  if( any( AU_Ranks_data_tie1_restoration_sprch_stld == riparian_disturbance_habitat_attribute_scores$Assessment.Unit[i] )  ){
    tier1_sprchn_stld_no_riparian = rbind(tier1_sprchn_stld_no_riparian,  riparian_disturbance_habitat_attribute_scores[i,] )
  }
}

tier1_sprchn_stld_no_canopy = c()
for(i in 1:nrow(riparian_canopy_cover_habitat_attribute_scores) ){
  if( any( AU_Ranks_data_tie1_restoration_sprch_stld == riparian_canopy_cover_habitat_attribute_scores$Assessment.Unit[i] )  ){
    tier1_sprchn_stld_no_canopy = rbind(tier1_sprchn_stld_no_canopy,  riparian_canopy_cover_habitat_attribute_scores[i,] )
  }
}

# ------------------ identify if any gaps in canopy or disturbance ---------
unique_Reaches = unique(c(tier1_sprchn_stld_no_riparian$ReachName, tier1_sprchn_stld_no_canopy$ReachName))
tier1_sprchn_stld_no_riparian_all = c()
for(reach_x in unique_Reaches){
  output_x = Reach_Information_data[which(Reach_Information_data$ReachName == reach_x), c("ReachName","Assessment.Unit","Basin")]
  # ---------- Riparian data Disturbance ---------
    if( any(tier1_sprchn_stld_no_riparian$ReachName == reach_x)){
    output_x$riparian_disturbance_missing = "yes"
  }else{
    output_x$riparian_disturbance_missing = "no"
  }
  # ---------- Riparian data Canopy Cover ---------
  if( any(tier1_sprchn_stld_no_canopy$ReachName == reach_x)){
    output_x$riparian_canopy_cover_missing = "yes"
  }else{
    output_x$riparian_canopy_cover_missing = "no"
  }
  # ---------- combine ---------
  tier1_sprchn_stld_no_riparian_all = rbind(tier1_sprchn_stld_no_riparian_all, output_x)
}



output_path_x =  paste(output_path,'Tier1_SprChn_Stld_Restoration_Riparian_Missing_Data.xlsx', sep="")
write_xlsx(tier1_sprchn_stld_no_riparian_all,output_path_x )
