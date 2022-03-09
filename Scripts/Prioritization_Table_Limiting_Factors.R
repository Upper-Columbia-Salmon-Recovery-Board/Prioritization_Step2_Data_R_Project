
# -----------------------------------------------------------------------------------------------
#      
#          AU Output of A) Restoration Habitat Attributes for Priority Reaches (i.e. everything that made it through the filters)
#               and B) All values for all habitat attributes
#
# -----------------------------------------------------------------------------------------------

# NEED to run this script AFTER all the other scripts


columns_limiting_factors = c("PRCNT Fines and Embeddedness", "Bank Stability", "Brook Trout" , "Channel Stability",  
                             "Coarse Substrate" , "Contaminants", "Cover- Boulders", "Cover- Undercut Banks", "Cover- Wood", 
                             "Entrainment and Stranding", "Floodplain Connectivity" , "Flow- Scour", "Flow- Summer Base Flow", 
                             "Food- Food Web Resources", "Harassment", "Icing", "Off-Channel- Side-Channels", "Pool Quantity and Quality", 
                             "Predators- Adult", "Riparian", "Riparian-Canopy Cover", "Riparian-Disturbance", "Stability", "Superimposition", 
                             "Temperature- Adult Holding", "Temperature- Adult Spawning", "Temperature- Rearing")


# ------------------------------------------------------------------------------------------------------
#      
#          Restoration Habitat Attributes for Priority Reaches (i.e. everything that made it through the filters)
#
# ------------------------------------------------------------------------------------------------------

Restoration_Limiting_Factor_Table = c()

for(reach_x in Restoration_Prioritization_Output_for_WebMap$`Reach Name` ){
  row_x = Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_x),]
  
  # ---------- pull basic data ---------
  AU_x = row_x$`Assessment Unit`
  Basin_x = row_x$Basin
  species_x = row_x$`Priority Species`
  reach_rank_x = row_x$`Reach Rank`
  
  # ------ start data frame ----------
  df_x = t( as.data.frame(c(reach_x, AU_x, Basin_x, species_x, reach_rank_x)) )
  rownames(df_x) = reach_x
  colnames(df_x) = c("ReachName","Assessment_Unit","Basin","Priority_Species","Reach_Rank")
  
  # ----------- pull limiting factor into columns ------
  limiting_factors_x = c()
  for(factor_x in columns_limiting_factors){
    
    # --------- if limiting factor is UNACCEPTABLE --------
    if( grepl(factor_x, row_x$`Unacceptable Limiting Factors`) ){
      factor_x2 = 1
    }else if(  grepl(factor_x, row_x$`At-Risk Limiting Factors`) ){
      factor_x2 = 3
    }else{
      factor_x2 = NA
    }
   
    limiting_factors_x = cbind(limiting_factors_x, factor_x2)

  }
  #------- prep limiting factors ------
  colnames(limiting_factors_x) = columns_limiting_factors
  # --------------add limiting factors to data frame ------
  df_x = cbind(df_x, limiting_factors_x)
  Restoration_Limiting_Factor_Table = rbind(Restoration_Limiting_Factor_Table, df_x)
}

# ------------------------ Output Excel--------------------------------------------------------------
output_path_x =  paste(output_path,'Restoration_Prioritization_Limiting_Factor_Table.csv', sep="")
write.csv(Restoration_Limiting_Factor_Table, output_path_x, row.names = FALSE )


# ------------------------------------------------------------------------------------------------------
#      
#         All values for all habitat attributes
#
# ------------------------------------------------------------------------------------------------------

# ------------------------ get all the unique habitat attributes -------------
habitat_attributes_all = unique(Habitat_Attribute_Scores$Habitat_Attribute)

# ------------- Habitat Attribute Scores Names ----------
habitat_attribute_indiv_score_col_names = c("HabitatAttributeScore1", "HabitatAttributeScore2","HabitatAttributeScore3",         
                                    "HabitatAttributeScore4","HabitatAttributeScore5","HabitatAttributeScore6"  )

Restoration_ALL_Limiting_Factor_Table = c()

reaches_list_all = unique(Habitat_Attribute_Scores$ReachName)

for(reach_x in reaches_list_all ){
  data_x = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$ReachName == reach_x),]
  
  # ---------- pull basic data ---------
  AU_x = data_x$Assessment.Unit[1]
  Basin_x = data_x$Basin[1]
  
  # ------ start data frame ----------
  df_x = t( as.data.frame(c(reach_x, AU_x, Basin_x)) )
  rownames(df_x) = reach_x
  colnames(df_x) = c("ReachName","Assessment_Unit","Basin")
  
  # ----------- pull limiting factor into columns ------
  habitat_attributes_scores_data_sources_x = c()
  for(factor_x in habitat_attributes_all){
    
    # --------- pull the habitat attribute --------
    row_x = data_x[which(data_x$Habitat_Attribute == factor_x),]
    
    # ------------ separate out data sources ---------
    data_sources_x = unlist( strsplit(row_x$Data_Sources,  ",")  )
    
    # --------------- pull score -----------------
    score_x = row_x$Habitat_Attribute_Score
    
    # ----------------- pull data source ------------
    if( !is.na(score_x) ){
      
      # --------------- IF default to REI ---------------
      if( any(factor_x == REI_Default_List)  ){
        data_source_x = paste(data_sources_x[1], " (detault to REI score)", sep="")
        
      # ------ loop through each habitat attribute - see if it is the minimum -------
      }else{
        data_source_x = " "
        
        i = 0
        for(data_source_loop_x in data_sources_x){
          i = i + 1
          if( !is.na(row_x[,habitat_attribute_indiv_score_col_names[i]]) ){  # if there is a score for this habitat attribute
            if(row_x[,habitat_attribute_indiv_score_col_names[i]] == score_x){
              data_source_x = paste(data_source_x, data_sources_x[i], sep=", " )   # pull data source if the scores are the same
            }
          }
        }
        # --------- remove leading space and comma ---------
        data_source_x = substring(data_source_x, 4,nchar(data_source_x))
        
      }
      
    # ------------------ IF no habitat attribute score ------------  
    }else{
      data_source_x = "no score"
    }
    
    # ------------- combine scores and data sources --------
    row_score_output_x = t( as.data.frame(c(score_x, data_source_x)) )
    colnames(row_score_output_x) = c( paste(factor_x,"Habitat_Attribute_Score",sep=" - "),
                                            paste(factor_x, "Data_Source",sep=" - ") )
    
    rownames(row_score_output_x) = reach_x
    habitat_attributes_scores_data_sources_x = cbind(habitat_attributes_scores_data_sources_x, row_score_output_x)
    
  }

  # --------------add limiting factors to data frame ------
  df_x = cbind(df_x, habitat_attributes_scores_data_sources_x)
  Restoration_ALL_Limiting_Factor_Table = rbind(Restoration_ALL_Limiting_Factor_Table, df_x)
}

# ------------------------ Output Excel--------------------------------------------------------------
#output_path_x = "Downloads"
output_path_x =  paste(output_path,'Habitat_Attribute_Scores_Data_Sources_ALL_Table.csv', sep="")
write.csv(Restoration_ALL_Limiting_Factor_Table, output_path_x, row.names = FALSE )






