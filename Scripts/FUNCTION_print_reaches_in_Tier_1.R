


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#     Script to compare AU tiers with a given output (based on Reach Name)
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

reach_names_x = Reach_Rankings_Output_Protection$ReachName
restoration_or_protection = "protection"

FUNCTION_compare_tier_1_reaches_to_an_output("protection", Reach_Rankings_Output_Protection$ReachName)
FUNCTION_compare_tier_1_reaches_to_an_output("restoration", Reach_Rankings_Output_Restoration$ReachName)


FUNCTION_compare_tier_1_reaches_to_an_output = function(restoration_or_protection,  reach_names_x){
  
  
  if( restoration_or_protection == "restoration" ){
    
      tier_AUs = AU_Ranks_data[ , c("Assessment Unit","SPCHNTier_Restoration", "STLTier_Restoration","BTTier_Restoration")]
      AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
      colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
      AU_Ranks_Okanogan_to_merge$SPCHNTier_Restoration = NA
      AU_Ranks_Okanogan_to_merge$STLTier_Restoration = AU_Ranks_Okanogan$`AU Restoration Rank`
      AU_Ranks_Okanogan_to_merge$BTTier_Restoration = NA
      tier_AUs = rbind(tier_AUs, AU_Ranks_Okanogan_to_merge)
      
  }else if(restoration_or_protection == "protection"){

      tier_AUs = AU_Ranks_data[, c("Assessment Unit","SPCHNTier_Protection","STLTier_Protection", "BTTier_Protection")]
      AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
      colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
      AU_Ranks_Okanogan_to_merge$SPCHNTier_Protection = NA
      AU_Ranks_Okanogan_to_merge$STLTier_Protection = AU_Ranks_Okanogan$`AU Protection Rank`
      AU_Ranks_Okanogan_to_merge$BTTier_Protection = NA
      tier_AUs = rbind(tier_AUs, AU_Ranks_Okanogan_to_merge)
      
  }else{
    print("incorrect tier name entered")
  }
  
  # ------------ filter out for Tier 1 ----------
  tier_AUs = tier_AUs %>% filter_all(any_vars(. %in% c(1)))
  
  # ------------------- Pull reaches for AUs ----------
  rows_in_tier_1 = c()
  for(AU_x in tier_AUs$`Assessment Unit`){
    
    # --------------- pull reaches for AU -------
    reaches_in_AU_x = Reach_Information_data$ReachName[which(Reach_Information_data$Assessment.Unit == AU_x)]
    # --------------- identify which reaches are in the AU_x --------------
    reaches_in_AU_from_data = intersect(reaches_in_AU_x, reach_names_x)
    # ------------- identify index of reaches ------
    if(length(reaches_in_AU_from_data) > 0){
      for(reach_x_in in reaches_in_AU_from_data){
        rows_in_tier_1 = c(rows_in_tier_1, which(reach_names_x == reach_x_in ))
      }
      #  -------------------- print AU and ----------------------------
      print(c("-----------------",AU_x,"------------------"))
      print(reaches_in_AU_from_data)
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
  
}
