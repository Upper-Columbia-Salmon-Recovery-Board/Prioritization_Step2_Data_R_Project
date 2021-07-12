


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#     Script to compare AU tiers with a given output (based on Reach Name)
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

species_restoration_x = c( "SPCHNTier_Restoration", "STLTier_Restoration") # WHICH Tier 1 to pull
# species_priorities_x = c("SPCHNTier_Protection","STLTier_Protection", "BTTier_Protection")

species_protection_x = c( "SPCHNTier_Protection","STLTier_Protection") # WHICH Tier 1 to pull
#  "SPCHNTier_Protection","STLTier_Protection", "BTTier_Protection"


test_x = TRUE
if(test_x){
  reach_names_x = Reach_Rankings_Output_Protection$ReachName
  restoration_or_protection = "protection"
}

run_x = TRUE
if(run_x){
  Restoration_Rank_Tier_1_comparison = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Reach_Rankings_Output_Restoration$ReachName, species_restoration_x, species_protection_x )
  Protection_Rank_Tier_1_comparison = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("protection", Reach_Rankings_Output_Protection$ReachName, species_restoration_x, species_protection_x)
}

run_x = TRUE
if(run_x){
  Restoration_Rank_Tier_1_comparison2 = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Restoration_Prioritization_Output_for_WebMap$`Reach Name`, species_restoration_x, species_protection_x)
  Protection_Rank_Tier_1_comparison2 = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("protection", Protection_Prioritization_Output_for_WebMap$`Reach Name`, species_restoration_x, species_protection_x)
}

# ---------------- Run JUST for Bull Trout
run_x = TRUE
if(run_x){
  Restoration_WebMap_Tier_1_comparison_Bull_Trout = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Restoration_Prioritization_Output_for_WebMap$`Reach Name`, "BTTier_Restoration", "BTTier_Protection")
  Protection_WebMap_Tier_1_comparison_Bull_Trout = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("protection", Protection_Prioritization_Output_for_WebMap$`Reach Name`, "BTTier_Restoration", "BTTier_Protection")
}

# ---------------- Run JUST for Bull Trout
run_x = TRUE
if(run_x){
  Restoration_Rank_Tier_1_comparison_Bull_Trout = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Reach_Rankings_Output_Restoration$ReachName, "BTTier_Restoration", "BTTier_Protection")
  Protection_Rank_Tier_1_comparison_Bull_Trout = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("protection", Reach_Rankings_Output_Protection$ReachName, "BTTier_Restoration", "BTTier_Protection")
}

# ---------- print restoration and protection AUs not with priority reaches ------------
Restoration_Rank_Tier_1_comparison_NO_PRIORITY_REACH = Restoration_Rank_Tier_1_comparison[which(Restoration_Rank_Tier_1_comparison$Priority_Reaches == ""), c(1,3)]
Protection_Rank_Tier_1_comparison_NO_PRIORITY_REACH = Protection_Rank_Tier_1_comparison[which(Protection_Rank_Tier_1_comparison$Priority_Reaches == ""), c(1,4)]

# ---------- print restoration and protection AUs not with priority reaches ------------
Restoration_Bull_Trout_Tier_1_comparison_LF = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]$ReachName, species_restoration_x, species_protection_x)
Restoration_Bull_Trout_Tier_1_comparison_HQ = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]$ReachName, species_restoration_x, species_protection_x)
#Protection_Bull_Trout_Tier_1_comparison_NO_PRIORITY_REACH = FUNCTION_compare_tier_1_reaches_to_an_output_ranks("restoration", Reach_Rankings_Output_Restoration$ReachName)

# species_restoration_x = c( "SPCHNTier_Restoration", "STLTier_Restoration")
# reach_names_x = Reach_Rankings_Output_Protection$ReachName
#  restoration_or_protection = "protection"
FUNCTION_compare_tier_1_reaches_to_an_output_ranks = function(restoration_or_protection,  reach_names_x, species_restoration_x, species_protection_x){
  
  
  if( restoration_or_protection == "restoration" ){
    
      tier_AUs = AU_Ranks_data[ , c("Assessment Unit",species_restoration_x)]
      AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
      colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
      if( any(species_restoration_x == "SPCHNTier_Restoration") ){ AU_Ranks_Okanogan_to_merge$SPCHNTier_Restoration = NA }
      if( any(species_restoration_x == "STLTier_Restoration") ){ AU_Ranks_Okanogan_to_merge$STLTier_Restoration = AU_Ranks_Okanogan$`AU Restoration Rank` }
      if( any(species_restoration_x == "BTTier_Restoration") ){ AU_Ranks_Okanogan_to_merge$BTTier_Restoration = NA }
      tier_AUs = rbind(tier_AUs, AU_Ranks_Okanogan_to_merge)
      
  }else if(restoration_or_protection == "protection"){

      tier_AUs = AU_Ranks_data[, c("Assessment Unit",species_protection_x)]
      AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
      colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
      if( any(species_protection_x == "SPCHNTier_Protection") ){ AU_Ranks_Okanogan_to_merge$SPCHNTier_Protection = NA }
      if( any(species_protection_x == "STLTier_Protection") ){ AU_Ranks_Okanogan_to_merge$STLTier_Protection = AU_Ranks_Okanogan$`AU Protection Rank` }
      if( any(species_protection_x == "BTTier_Protection") ){ AU_Ranks_Okanogan_to_merge$BTTier_Protection = NA }
      tier_AUs = rbind(tier_AUs, AU_Ranks_Okanogan_to_merge)
      
  }else{
    print("incorrect tier name entered")
  }
  
  # ------------ filter out for Tier 1 ----------
  # NOTE: just rows with Tier 1 for any species
  if(exclude_bull_trout == "no" & any(species_protection_x == "BTTier_Protection") ){
    tier_AUs = tier_AUs %>% filter_all(any_vars(. %in% c(1)))
  }else{
    tier_AUs = tier_AUs[,c(1:3)]
    tier_AUs = tier_AUs %>% filter_all(any_vars(. %in% c(1)))
  }
  
  
  # ------------------- Pull reaches for AUs ----------
  rows_in_tier_1 = c()
  output_combined = c()
  for(AU_x in tier_AUs$`Assessment Unit`){
    
    # --------------- pull reaches for AU -------
    reaches_in_AU_x = Reach_Information_data$ReachName[which(Reach_Information_data$Assessment.Unit == AU_x)]
    
    # --------------- identify which reaches are in the AU_x --------------
    reaches_in_AU_also_in_priority_results = intersect(reaches_in_AU_x, reach_names_x)
    reaches_in_AU_also_in_priority_results_for_output = paste(reaches_in_AU_also_in_priority_results, collapse=",")
    
    # --------------- identify which reaches are in the AU_x --------------
    reaches_in_AU_NOT_in_priority_results = setdiff(reaches_in_AU_x, reach_names_x)
    reaches_in_AU_NOT_in_priority_results_for_output = paste(reaches_in_AU_NOT_in_priority_results, collapse=",")
    
    # -------------- combine output ------------
    output_x = t(as.data.frame(c(AU_x,reaches_in_AU_also_in_priority_results_for_output, reaches_in_AU_NOT_in_priority_results_for_output )))
    output_combined = rbind(output_combined, output_x)
    
    # ------------- identify index of reaches ------
    if(length(reaches_in_AU_also_in_priority_results) > 0){
      for(reach_x_in in reaches_in_AU_also_in_priority_results){
        rows_in_tier_1 = c(rows_in_tier_1, which(reach_names_x == reach_x_in ))
      }
      #  -------------------- print AU and ----------------------------
      print(c("-----------------",AU_x,"------------------"))
      print(reaches_in_AU_also_in_priority_results)
      print(tier_AUs[which(tier_AUs$`Assessment Unit` == AU_x),])
      print("  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ")
      print(" ")
    }

  }
  print(" ")
  print("REACHES NOT IN tier 1 AU:  ")
  # ------------- print reaches not in priority AUs ---------
  rows_in_tier_1 = rows_in_tier_1[order(rows_in_tier_1)]
  rows_NOT_in_tier_1 = seq(1,length(reach_names_x))
  rows_NOT_in_tier_1 = rows_NOT_in_tier_1[-rows_in_tier_1]
  reaches_NOT_in_tier_1 = reach_names_x[rows_NOT_in_tier_1]
  
  print(reaches_NOT_in_tier_1[order(reaches_NOT_in_tier_1)])
  print(reach_names_x[rows_in_tier_1])
  
  colnames(output_combined) = c("Assessment.Unit","Priority_Reaches","NOT_priority_reaches")
  rownames(output_combined) = seq(1,nrow(output_combined))
  
  output_combined = as.data.frame(output_combined)
  
  # ---------- add Basin ---------
  Reach_Info_x = Reach_Information_data[which( !duplicated(Reach_Information_data$Assessment.Unit) ) ,c("Assessment.Unit","Basin")]
  output_combined = merge(output_combined, Reach_Info_x, by="Assessment.Unit", all.x=TRUE, all.y=FALSE)
  output_combined = output_combined[,c(1,4,2,3)]
  
  return(output_combined)
}
