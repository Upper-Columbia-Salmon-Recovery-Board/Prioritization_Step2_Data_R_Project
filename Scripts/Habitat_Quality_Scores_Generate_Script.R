
# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Habitat Quality Scores for Restoration AND Protection
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#   Read in List of Data sources for Habitat Attributes
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'Data_Sources_List_for_Habitat_Attributes.R', sep=""))

# ---------------------------------------------------------------------------
#
#   Call Functions for Habitat Attribute Filters
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'FUNCTIONS_for_Habitat_Quality_Filters.R', sep=""))


# ---------------------------------------------------------------------------
#
#
#        Generate Habitat Quality Scores (1, 3, 5) for each Habitat Attribute
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#  Loop through each habitat attribute and data source to generate score
#
# ---------------------------------------------------------------------------

#reach_names = unique(Reach_Information_data$ReachName)

# --------------------------------------
#   Loop through each Reach to generate score
# --------------------------------------

# --------------------------------------
#   Loop through each Habitat Attribute
# --------------------------------------
Habitat_Quality_Scores = as.tibble(Reach_Information_data[,c('ReachName','Basin', 'Assessment.Unit',
                                                             'Spring.Chinook.Reach','Steelhead.Reach','Bull.Trout.Reach')])
colnames(Habitat_Quality_Scores) = c('ReachName','Basin', 'Assessment.Unit',
                                     'Spring.Chinook.Reach','Steelhead.Reach','Bull.Trout.Reach')

ptm <- proc.time()[3]
for(habitat_attribute_x in names(Habitat_Quality_Habitat_Attributes_List)){
  print(habitat_attribute_x)
  
  # --------------------------------------------------------------------
  #   Loop through each Data Source for this specific habitat attribute
  # --------------------------------------------------------------------
  data_sources_list =  Habitat_Quality_Habitat_Attributes_List[habitat_attribute_x]
  
  # --------------------------------------------------------------------
  #   Only one data source for this habitat attribute
  # --------------------------------------------------------------------
  if(length(data_sources_list[[1]]) == 1){
    
    output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(data_sources_list[[1]][1])
    
    
    # --------------------------------------------------------------------
    #  MULTIPLE data source for this habitat attribute (need to get optimum)
    # --------------------------------------------------------------------
    # NOTE this is only true for Temperature- Rearing AND	Flow- Summer Base Flow
    
  }else{
    
    # ------------ data frame to record habitat attributes --------
    habitat_attribute_x_data_frame = data.frame()
    
    for( location_x in 1:length(data_sources_list[[1]]) ){
      
      # ------------------- skip reading PROFESSOINAL JUDGEMENT -------------
      if(data_sources_list[[1]][location_x] == "PROFESSIONAL JUDGEMENT"){ next }else{
        # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
        # outputs both metric value and score
        output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(data_sources_list[[1]][location_x] )
        
        # -------- data frame for this specific reach and habitat attribute --------
        habitat_attribute_x_data_frame = rbind(habitat_attribute_x_data_frame, output_x$score)
      }
      
      
    }
    
    
    # ------------ data frame to record habitat attributes --------
    habitat_attribute_x_data_frame = t(habitat_attribute_x_data_frame)
    habitat_attribute_x_data_frame = as_data_frame(habitat_attribute_x_data_frame)
    
    # ------------------- get minimum score for each row ----------
    habitat_attribute_x_data_frame = habitat_attribute_x_data_frame%>%
      rowwise() %>%
      mutate(minimum_score = min(c_across(), na.rm=T) )
    
    # -------- adding NA column (for metric colum, it needs to be NA to be multiple) ------
    habitat_attribute_x_data_frame$na_column = NA
    output_x =  habitat_attribute_x_data_frame[,c("na_column","minimum_score")]
    colnames(output_x) = c("metric_data", "score")
  }
  
  # --------------------------------------------------------------------
  #      Combine data with data frame
  # --------------------------------------------------------------------
  
  # ---------------------------- add new columns to summary data frame -----------
  #column_metric = paste(gsub(" ", "", habitat_attribute_x, fixed = TRUE), "metric", sep="_")
  column_score = paste(gsub(" ", "", habitat_attribute_x, fixed = TRUE), "score", sep="_")
  #Habitat_Quality_Scores[,column_metric] = output_x$metric_data
  Habitat_Quality_Scores[,column_score] = as.numeric(output_x$score)
  
  # --------------------------------------------------------------------
  #      Add Riparian Mean score
  # --------------------------------------------------------------------
  
  if(habitat_attribute_x == 'Riparian- Canopy Cover'){
    Habitat_Quality_Scores = Habitat_Quality_Scores%>%
      rowwise() %>%
      mutate(Riparian_Mean = mean(c_across(c('Riparian-Disturbance_score',
                                             'Riparian-CanopyCover_score')), na.rm=T) )
  }
  
  
  # --------------------------------------------------------------------
  #      Add Stability mean score
  # --------------------------------------------------------------------
  
  if(habitat_attribute_x == 'Channel Stability'){
    Habitat_Quality_Scores = Habitat_Quality_Scores%>%
      rowwise() %>%
      mutate(Stability_Mean = mean(c_across(c('BankStability_score',
                                              'ChannelStability_score')), na.rm=T) )
  }
}
print(paste("Time to complete loop: ", paste(round((proc.time()[3] - ptm)/60, 2), " minutes")    ))

# ------------------------------------------------------------------------------------- 
#                       calculate the HQ Sum and Percent
# ------------------------------------------------------------------------------------- 

# -------------------- Calculate the HQ Sum ----------------------
Habitat_Quality_Scores = Habitat_Quality_Scores%>%
  rowwise() %>%
  mutate(HQ_Sum = sum(c_across( habitat_quality_scores_colnames_for_sum  ), na.rm=F) )

# -------------------- Calculate percent (0 - 100%) ------------------
Habitat_Quality_Scores$HQ_Pct = Habitat_Quality_Scores$HQ_Sum/45

# ------------------------------------------------------------------------------------- 
#                 calculate HQ Restoration and Protection Score
# ------------------------------------------------------------------------------------- 

Habitat_Quality_Scores = Habitat_Quality_Scores  %>%
  mutate(HQ_Score_Restoration = ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                         HQ_Pct  <= Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                       ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[2] & 
                                                HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                              ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                       HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                     NA))))
Habitat_Quality_Scores = Habitat_Quality_Scores  %>%
  mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                        HQ_Pct  <= Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                      ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[2] & 
                                               HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                             ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                      HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                    NA))))



# ------------------ output data -------------------------
Habitat_Quality_Scores = as.data.frame(Habitat_Quality_Scores)
output_path_x =  paste(output_path,'Habitat_Quality_Scores.xlsx', sep="")
write.xlsx(
  Habitat_Quality_Scores,
  output_path_x,
  col.names = TRUE,
  row.names = FALSE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)





