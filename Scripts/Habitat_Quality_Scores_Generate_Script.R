
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

# Habitat Attributes in HQ but not in LF pathway: "Bank Stability", "Channel Stability" "Riparian- Canopy Cover", "Riparian-Disturbance"   

# --------------------------------------
#   Loop through each Reach to generate score
# --------------------------------------

# --------------------------------------
#   Loop through each Habitat Attribute
# --------------------------------------

# --------------------- Establish Habitat_Quality_Scores data frame ---------------------------------
Habitat_Quality_Scores = as.tibble(Reach_Information_data[,c('ReachName','Basin', 'Assessment.Unit',
                                                             'Spring.Chinook.Reach','Steelhead.Reach','Bull.Trout.Reach')])
colnames(Habitat_Quality_Scores) = c('ReachName','Basin', 'Assessment.Unit',
                                     'Spring.Chinook.Reach','Steelhead.Reach','Bull.Trout.Reach')
habitat_attribute_x = names(Habitat_Quality_Habitat_Attributes_List)[1] # just to print out

# --------------------- Establish data frame to establish data source ---------------------------------
Habitat_Quality_Scores_DATA_SOURCES = as.tibble(Reach_Information_data[,c('ReachName','Basin', 'Assessment.Unit')])


ptm <- proc.time()[3]
for( habitat_attribute_x in names(Habitat_Quality_Habitat_Attributes_List) ){
  print(habitat_attribute_x)
  
  # --------------------------------------------------------------------
  #   Pull Habitat Attribute Score from Habitat Attributes
  # --------------------------------------------------------------------
  output_x = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x),]
  
  # --------------------------------------------------------------------
  #   IF Score for Habitat Attribute is present in Habitat_Attribute_Scores
  # --------------------------------------------------------------------
  
  if( nrow(output_x) > 0 ){
    # ------- pull score from Habitat Attribute data --------
    output_x_add_to_HQ = output_x[  , c("ReachName", "Habitat_Attribute_Score")]
    colnames(output_x_add_to_HQ) = c("ReachName","final_score")
    
  # --------------------------------------------------------------------
  #   IF NO Score for Habitat Attribute is present in Habitat_Attribute_Scores
  # --------------------------------------------------------------------
  }else{
    
    print("--------- (for above and below attribute) No habitat attribute in Habitat_Attribute_Scores (calculate from HQ Pathway) --------")
    print(habitat_attribute_x)
    # --------------------------------------------------------------------
    #   Loop through each Data Source for this specific habitat attribute
    # --------------------------------------------------------------------
    data_sources_list =  Habitat_Quality_Habitat_Attributes_List[habitat_attribute_x]
    
    # --------------------------------------------------------------------
    #   Only one data source for this habitat attribute
    # --------------------------------------------------------------------
    if( length(data_sources_list[[1]]) == 1){
      
      output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x, data_sources_list[[1]][1], "HQ")
      
      
    # --------------------------------------------------------------------
    #  MULTIPLE data source for this habitat attribute (need to get optimum)
    # --------------------------------------------------------------------
      # NOTE this is only true for Temperature- Rearing AND	Flow- Summer Base Flow
      
    }else{
      
      # ------------ data frame to record habitat attributes --------
      
      i = 0
      for( location_x in 1:length(data_sources_list[[1]]) ){

        # ------------------- skip reading PROFESSOINAL JUDGEMENT -------------
        if(data_sources_list[[1]][location_x] == "PROFESSIONAL JUDGEMENT"){ next }else{
          # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
          i = i + 1 # to include reach name if first 
          
          # outputs both metric value and score
          output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x,  data_sources_list[[1]][location_x],  "HQ" )
          
          # -------- data frame for this specific reach and habitat attribute --------
          if(i == 1){
            habitat_attribute_x_data_frame = output_x[,c("ReachName","score")]
          }else{
            habitat_attribute_x_data_frame = cbind(habitat_attribute_x_data_frame, output_x$score)
          }
          
        }
      }
      
      # ------------ data frame to record habitat attributes --------
      #habitat_attribute_x_data_frame = t(habitat_attribute_x_data_frame)
      #habitat_attribute_x_data_frame = as_data_frame(habitat_attribute_x_data_frame)
      # ---------- pull just the scores -------
      habitat_attribute_x_data_frame_numeric = habitat_attribute_x_data_frame[,2:ncol(habitat_attribute_x_data_frame)]
      
      # ------------------- get minimum score for each row ----------
      habitat_attribute_x_data_frame_numeric= habitat_attribute_x_data_frame_numeric %>%
        rowwise() %>%
        mutate(minimum_score = min(c_across(), na.rm=T) )
      
      # ------- convert minimum score to numeric ---------------
      habitat_attribute_x_data_frame_numeric$minimum_score2 = as.numeric(habitat_attribute_x_data_frame_numeric$minimum_score) 
      
      # -------- adding NA column (for metric column, it needs to be NA to be multiple) ------
      habitat_attribute_x_data_frame_numeric$na_column = NA
      output_x =  cbind(habitat_attribute_x_data_frame$ReachName, habitat_attribute_x_data_frame_numeric[,c("na_column","minimum_score2")])
      colnames(output_x) = c("ReachName", "metric_data", "final_score")
      # ------ generate output to add to HQ Score data frames -------
      output_x_add_to_HQ = output_x[,c("ReachName","final_score")]
    }

  }
  
  # --------------------------------------------------------------------
  #      Combine data with data frame
  # --------------------------------------------------------------------
  
  # ---------------------------- add new columns to summary data frame -----------
  # --------- generate column name ---------
  column_score = paste(gsub(" ", "", habitat_attribute_x, fixed = TRUE), "score", sep="_")
  colnames(output_x_add_to_HQ)[2] = column_score
  # ---------- add to HQ score ------------
  Habitat_Quality_Scores = merge(Habitat_Quality_Scores, output_x_add_to_HQ, by="ReachName")
  
  # --------------------------------------------------------------------
  #      Add Riparian Mean score
  # --------------------------------------------------------------------
  if(habitat_attribute_x == 'Riparian-Disturbance'){
    Habitat_Quality_Scores = Habitat_Quality_Scores%>%
      rowwise() %>%
      mutate(Riparian_Mean = mean( c_across(c('Riparian-Disturbance_score',
                                             'Riparian-CanopyCover_score')),  na.rm=T)   )
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
                                         HQ_Pct  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                       ifelse(HQ_Pct  >= Restoration_Scoring$Category_Lower[2] & 
                                                HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                              ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                       HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                     NA))))
Habitat_Quality_Scores = Habitat_Quality_Scores  %>%
  mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                        HQ_Pct  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                      ifelse(HQ_Pct  >= Protection_Scoring$Category_Lower[2] & 
                                               HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                             ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                      HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                    NA))))


# ------------------ output data -------------------------
if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
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
}


# --------------------------------------------------------------------------------------------------
#
#              Calculate HQ Score by pulling out habitat attribute
#
# --------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------
#
#                Sensitivity Analysis by removing HQ Score
#
# --------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------
#           Calculate new HQ_Pct and HQ_Sum by pulling out one variable
# --------------------------------------------------------------------------------------------------

if(HQ_sensitivity_analysis_true_false){
  
  # -------------- names of new HQ_Pct columns data frame --------
  HQ_sensitivity_analysis_true_false_HQ_PCT_COLUMN_NAMES = c()
 
  # ------------------- remove original HQ Sum, HQ_Pct, and Restoration and Protection scores ------
  Habitat_Quality_Scores_for_sensitivity = Habitat_Quality_Scores
  
  # ------------- loop through habitat attributes to NOT use to calculate HQ  ---------------
  for(habitat_attribute_x in habitat_quality_scores_colnames_for_sum){
    
    # --------------------------- remove column for sensitivity analysis ----------------------
    habitat_quality_scores_colnames_for_sum_X = habitat_quality_scores_colnames_for_sum[-which(habitat_quality_scores_colnames_for_sum == habitat_attribute_x)]
    
    # ------------------------------------------------------------------------------------- 
    #                       calculate the HQ Sum and Percent
    # ------------------------------------------------------------------------------------- 
    
    # -------------------- Calculate the HQ Sum ----------------------
    Habitat_Quality_Scores_for_sensitivity = Habitat_Quality_Scores_for_sensitivity%>%
      rowwise() %>%
      mutate(HQ_Sum2 = sum(c_across( habitat_quality_scores_colnames_for_sum_X  ), na.rm=F) )

    # -------------------- Calculate percent (0 - 100%) ------------------
    Habitat_Quality_Scores_for_sensitivity$HQ_Pct2 = Habitat_Quality_Scores_for_sensitivity[,"HQ_Sum2"] /40

    # ------------------------------------------------------------------------------------- 
    #                 calculate HQ Restoration and Protection Score
    # ------------------------------------------------------------------------------------- 
    
    Habitat_Quality_Scores_for_sensitivity = Habitat_Quality_Scores_for_sensitivity  %>%
      mutate(HQ_Score_Restoration2 = ifelse(HQ_Pct2  > Restoration_Scoring$Category_Lower[1] & 
                                             HQ_Pct2  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                           ifelse(HQ_Pct2  >= Restoration_Scoring$Category_Lower[2] & 
                                                    HQ_Pct2  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                  ifelse(HQ_Pct2  > Restoration_Scoring$Category_Lower[3] & 
                                                           HQ_Pct2  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                         NA))))
    Habitat_Quality_Scores_for_sensitivity = Habitat_Quality_Scores_for_sensitivity  %>%
      mutate(HQ_Score_Protection2 = ifelse(HQ_Pct2  > Protection_Scoring$Category_Lower [1] & 
                                            HQ_Pct2  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                          ifelse(HQ_Pct2  >= Protection_Scoring$Category_Lower[2] & 
                                                   HQ_Pct2  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                                 ifelse(HQ_Pct2  > Protection_Scoring$Category_Lower[3] & 
                                                          HQ_Pct2  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                        NA))))
    
    # ---------------- update column names -----------
    colnames(Habitat_Quality_Scores_for_sensitivity)[ncol(Habitat_Quality_Scores_for_sensitivity)-3] = paste("HQ_Sum_NO",habitat_attribute_x, sep="_")
    colnames(Habitat_Quality_Scores_for_sensitivity)[ncol(Habitat_Quality_Scores_for_sensitivity)-2] = paste("HQ_Pct_NO",habitat_attribute_x, sep="_")
    colnames(Habitat_Quality_Scores_for_sensitivity)[ncol(Habitat_Quality_Scores_for_sensitivity)-1] = paste("HQ_Score_Restoration_NO",habitat_attribute_x, sep="_")
    colnames(Habitat_Quality_Scores_for_sensitivity)[ncol(Habitat_Quality_Scores_for_sensitivity)] = paste("HQ_Score_Protection_NO",habitat_attribute_x, sep="_")
    
    # ----------------- output name --------------
    HQ_sensitivity_analysis_true_false_HQ_PCT_COLUMN_NAMES=rbind(HQ_sensitivity_analysis_true_false_HQ_PCT_COLUMN_NAMES , paste("HQ_Pct_NO",habitat_attribute_x, sep="_"))
  }
   
}

if(HQ_sensitivity_analysis_true_false){
  # test for NA in original, but present in updated 
  Habitat_Quality_Scores_for_sensitivity$ReachName[which(  is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct) & !is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct_NO_Stability_Mean) )]
  Habitat_Quality_Scores_for_sensitivity$ReachName[which(  is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct) & !is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct_NO_CoarseSubstrate_score) )]
  Habitat_Quality_Scores_for_sensitivity$ReachName[which(  is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct) & !is.na(Habitat_Quality_Scores_for_sensitivity$`HQ_Pct_NO_PoolQuantity&Quality_score`) )]
  length( which(  !is.na(Habitat_Quality_Scores_for_sensitivity$HQ_Pct) ) )
  
}

# --------------------------------------------------------------------------------------------------
#          Analysis of new HQ_Pct values
# --------------------------------------------------------------------------------------------------

if(HQ_sensitivity_analysis_true_false){
  par(mfrow=c(3,3))
  i = 0
  for(HQ_Pct_column_x in HQ_sensitivity_analysis_true_false_HQ_PCT_COLUMN_NAMES){
    i = i + 1
    # ------------ establish data frame with old and new HQ Pct ----------
    HQ_new = Habitat_Quality_Scores_for_sensitivity[, c("HQ_Pct",HQ_Pct_column_x)]
    HQ_new = as.data.frame(as.matrix(HQ_new))
    colnames(HQ_new) = c("HQ_Pct_Orig","HQ_Pct_Updated")
    
    # ----------------- calculate the reaches gained ------------
    HQ_new_reach_added = which( is.na(HQ_new$HQ_Pct_Orig) & !is.na(HQ_new$HQ_Pct_Updated) )
    HQ_new_reach_added_total = length(HQ_new_reach_added)
    #if(HQ_Pct_column_x == "HQ_Pct_NO_Temperature-Rearing_score" ){
    #  HQ_new_reach_added_total = 0
    #}
    # ------------------ Plot -------------------
    plot(HQ_new$HQ_Pct_Orig, HQ_new$HQ_Pct_Updated, 
         main=paste("Attribute removed: ",habitat_quality_scores_colnames_for_sum[i], " \n ","additional reaches: ",HQ_new_reach_added_total, sep=""),
         ylab="HQ Pct UPDATED", xlab="HQ Pct ORIGINAL")
    abline(0,1,col="black")
    
  }
  
}



