


# ---------------------------------------------------------------------------
#
#      SCRIPT: FUNCTIONS for Habitat Quality filters
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
#   Create A Function to output habitat attribute data FROM Habitat Data Raw
#
# ---------------------------------------------------------------------------

# ------ just for testing functions -----

# Channel_Unit_Raw_data_use[habitat_attribute_x] == location_x

# habitat_attribute_x =  '% Fines/Embeddedness' 
# location_x = 2
# reach_name_x  = "Beaver Creek 01"
# raw_data_col_name = Habitat_Attributes_List[habitat_attribute_x ][[1]][location_x]

# Channel_Unit_Raw[raw_data_col_name]


# Channel_Unit_Raw_data_use[habitat_attribute_x]

habitat_attribute_x = "Coarse Substrate"
habitat_attribute_x = "Pool Quantity & Quality"
reach_name_x  = "Big Meadow Creek 01"
# NOTE: "location_x" is merely the numeric location of data source in the individual
#        habitat_attribute list of data source (1, 2, 3, or 4)
#  'Pool Quantity & Quality' = c( 'Pools_CATEGORY_1', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2'), 
#   'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' ), 
habitat_attribute_x = "Temperature- Adult Holding"
data_col_name = 'NORWEST_Temperature'
LF_or_HQ = "HQ"

test_x = TRUE
if(test_x){
  data_col_name = data_source_x
  LF_or_HQ = "HQ"
}



FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw = function(habitat_attribute_x, data_col_name, LF_or_HQ){
  
  
  # -----------------------------------
  #    Generate Metric Criteria
  # -----------------------------------
  if(LF_or_HQ == "HQ"){
    metric_criteria_x = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name)
  }else if(LF_or_HQ == "LF"){
    metric_criteria_x = Habitat_Limiting_Factor_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name)
  }else{
    print("Pathway entered incorrectly")
  }
  
  # ------------- habitat attribute metrics have gone between LF and HQ (need to merge) -----
  if( nrow(metric_criteria_x) == 0 & LF_or_HQ == "LF"){
    metric_criteria_x = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name)
  }
  
  # -----------------------------------
  #    Generate Metric value from Raw (primary) data table
  # -----------------------------------
  if( nrow(metric_criteria_x) == 0 ){ 
    print(paste("no data for: ",habitat_attribute_x, "and ",data_col_name )  )
    data_output_x = cbind(  habitat_raw_data$ReachName,
                            as.data.frame(rep(NA, length.out = nrow(habitat_raw_data))),
                            as.data.frame(rep(NA, length.out = nrow(habitat_raw_data)))  )
    colnames(data_output_x) = c("ReachName", "metric_data", "score")
  
  }else{
    data_output_x = habitat_raw_data %>%
      dplyr::select(ReachName,data_col_name)
    
    colnames(data_output_x) = c("ReachName",  "metric_data")
    
    # -----------------------------------
    #    Generate Score for FACTOR criteria
    # -----------------------------------
    
    if( metric_criteria_x$Category_Type[1]== 'factor' ){
      
      # -----------------------------------
      #    Contaminants ONLY
      # -----------------------------------
      # NOTE: if anything is listed, it gets a 1, otherwise a 5
      if( data_col_name == "Contaminants_303d"  ){
        
        data_output_x$score = "NA"
        # ------ IF ANY contaminant listed - set as 1 ----------
        data_output_x$score[data_output_x$metric_data != "NA"] = 1
        # ------ IF no contaminant listed - set as 5 ----------
        data_output_x$score[data_output_x$metric_data == "NA"] = 5

      # -----------------------------------
      #    ALL other factors (REI scores mainly)
      # ----------------------------------- 
      }else{
        
        # ------------ set scores for factor metrics ---------------
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  == metric_criteria_x$Category[1], metric_criteria_x$Score[1],
                                ifelse(metric_data  == metric_criteria_x$Category[2], metric_criteria_x$Score[2],
                                       ifelse(metric_data  == metric_criteria_x$Category[3], metric_criteria_x$Score[3],
                                              ifelse(metric_data  == metric_criteria_x$Category[4], metric_criteria_x$Score[4],
                                                     ifelse(metric_data  == metric_criteria_x$Category[5], metric_criteria_x$Score[5],
                                                            ifelse(metric_data  == metric_criteria_x$Category[6], metric_criteria_x$Score[6],
                                                                   ifelse(metric_data  == metric_criteria_x$Category[7], metric_criteria_x$Score[7],
                                                                          ifelse(metric_data  == metric_criteria_x$Category[8], metric_criteria_x$Score[8],
                                                                                 NA)))))))))
        
        
      }
        
        

      # -----------------------------------
      #    Generate Score for NUMERIC criteria 
      # -----------------------------------
    }else if( metric_criteria_x$Category_Type[1]== 'numeric' ){
      
      # ------------------- Convert tometric_data to numeric ----------
      data_output_x$metric_data = as.numeric(as.character(data_output_x$metric_data))
      
      # -----------------------------
      #    IF Cover- Wood OR Pool Quantity & Quality, filter by stream width
      # -----------------------------
      
      if(metric_criteria_x$Habitat_Attribute[1] == 'Cover- Wood' |
         metric_criteria_x$Data_Sources[1] ==  'Pools_per_mile_INDICATOR_2' |
         metric_criteria_x$Data_Sources[1] ==  'CCT_OBMEP_LWM_pieces_per_mile'|
         metric_criteria_x$Data_Sources[1] ==  'CCT_OBMEP_pools_per_mile'){
        
        # ----------- pull stream width for this reach -------------
        stream_width_m = Reach_Information_data %>%
          dplyr::select(ReachName,PFC_Channel_Width_BINS_5_10_15_20_25_50_75_100_feet)
        
        # as of June 10th, 2021 - we used the PFC width bins as the stream widths
        stream_width_m$PFC_Channel_Width_meters = stream_width_m$PFC_Channel_Width_BINS_5_10_15_20_25_50_75_100_feet * 0.3048
        
        # --------- create blank NA column ------
        data_output_x$score = NA
        # habitat_filter_type = unique(metric_criteria_x$Habitat_Type_Filter)[8]
        for( habitat_filter_type in unique(metric_criteria_x$Habitat_Type_Filter) ){
          
          # ------------ get criteria specific to this stream width -----
          metric_criteria_x_i = metric_criteria_x %>%
            filter(metric_criteria_x$Habitat_Type_Filter == habitat_filter_type)
          
          # ------------ find streams with this width ----------
          if( metric_criteria_x$Data_Sources[1] ==  'Pools_per_mile_INDICATOR_2' |
              metric_criteria_x$Data_Sources[1] ==  'CCT_OBMEP_pools_per_mile' ){
            
            # ---------- "upper" of 100 foot bin was set high --------
            if( habitat_filter_type == "100 ft stream width bin"  ){
              stream_width_m_i = which( abs(stream_width_m$PFC_Channel_Width_meters - 30.480)<0.1) 
              
            }else{
              stream_width_m_i = which( abs( stream_width_m$PFC_Channel_Width_meters - as.numeric(metric_criteria_x_i$Filter_value_upper_meters[1]) )<0.1) 
            }
            
          }else if(metric_criteria_x$Habitat_Attribute[1] == 'Cover- Wood'){
            stream_width_m_i = which( stream_width_m$PFC_Channel_Width_meters >= metric_criteria_x_i$Filter_value_lower_meters[1] &
                                        stream_width_m$PFC_Channel_Width_meters <  metric_criteria_x_i$Filter_value_upper_meters[1]  ) 
            
          }
          
          # ------------------- pull metrics for those stream widths -------------------
          data_output_x_overlap_i = which(data_output_x$ReachName %in% stream_width_m$ReachName[stream_width_m_i])
          data_output_x_i = data_output_x[data_output_x_overlap_i,]
          
          # ----------------- generate scores ------------
          if( nrow(metric_criteria_x_i) == 2 ){
            data_output_x_i = data_output_x_i  %>%
              mutate(score = ifelse(metric_data  >= metric_criteria_x_i$Category_lower[1] & 
                                      metric_data  <= metric_criteria_x_i$Category_upper[1] , metric_criteria_x_i$Score[1],
                                    ifelse(metric_data  > metric_criteria_x_i$Category_lower[2] & 
                                             metric_data  <= metric_criteria_x_i$Category_upper[2] , metric_criteria_x_i$Score[2],
                                                  NA)))
          }else if( nrow(metric_criteria_x_i) == 3 ){
            data_output_x_i = data_output_x_i  %>%
              mutate(score = ifelse(metric_data  >= metric_criteria_x_i$Category_lower[1] & 
                                      metric_data  <= metric_criteria_x_i$Category_upper[1] , metric_criteria_x_i$Score[1],
                                    ifelse(metric_data  > metric_criteria_x_i$Category_lower[2] & 
                                             metric_data  <= metric_criteria_x_i$Category_upper[2] , metric_criteria_x_i$Score[2],
                                           ifelse(metric_data  > metric_criteria_x_i$Category_lower[3] & 
                                                    metric_data  <= metric_criteria_x_i$Category_upper[3] , metric_criteria_x_i$Score[3],
                                                  NA))))
          }

          
          # --------------- add score data for appropriate stream width ---------
          data_output_x$score[data_output_x_overlap_i] = data_output_x_i$score
        }
        
      }else if( metric_criteria_x$Habitat_Type[1] == 'Temperature' ){
        
        # --------------- get metric data ONLY for this habitat attribute --------
        metric_criteria_UPDATED_x = metric_criteria_x %>%
          filter(Habitat_Attribute   == habitat_attribute_x ) 
        
        # ----------------- generate scores ------------
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_UPDATED_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_UPDATED_x$Category_upper[1] , metric_criteria_UPDATED_x$Score[1],
                                ifelse(metric_data  > metric_criteria_UPDATED_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_UPDATED_x$Category_upper[2] , metric_criteria_UPDATED_x$Score[2],
                                       ifelse(metric_data  > metric_criteria_UPDATED_x$Category_lower[3] & 
                                                metric_data  <= metric_criteria_UPDATED_x$Category_upper[3] , metric_criteria_UPDATED_x$Score[3],
                                              NA))))
        
        
        # -----------------------------
        #    IF no stream width filter for criteria
        # -----------------------------
      }else if(length(metric_criteria_x$Category) == 2){
        data_output_x2 = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                                ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                       NA)))
        
      }else if(length(metric_criteria_x$Category) == 3){
        data_output_x$metric_data = as.numeric(data_output_x$metric_data)  # convert metric data to numeric
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                                ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                       ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                                metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                              NA))))
      }else if(length(metric_criteria_x$Category) == 4){
        data_output_x$metric_data = as.numeric(data_output_x$metric_data)  # convert metric data to numeric
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                                ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                       ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                                metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                              ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                       metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],  
                                                     NA)))))
      }else if(length(metric_criteria_x$Category) == 5){
        data_output_x$metric_data = as.numeric(data_output_x$metric_data)  # convert metric data to numeric
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                                ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                       ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                                metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                              ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                       metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],
                                                     ifelse(metric_data  > metric_criteria_x$Category_lower[5] & 
                                                              metric_data  <= metric_criteria_x$Category_upper[5] , metric_criteria_x$Score[5], 
                                                            NA))))))
      }else if(length(metric_criteria_x$Category) == 6){
        data_output_x = data_output_x  %>%
          mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                  metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                                ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                         metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                       ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                                metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                              ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                       metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],
                                                     ifelse(metric_data  > metric_criteria_x$Category_lower[5] & 
                                                              metric_data  <= metric_criteria_x$Category_upper[5] , metric_criteria_x$Score[5], 
                                                            ifelse(metric_data  > metric_criteria_x$Category_lower[6] & 
                                                                     metric_data  <= metric_criteria_x$Category_upper[6] , metric_criteria_x$Score[6],
                                                                   NA)))))))
      }else if(nrow(metric_criteria_x) == 0){
        
        data_output_x = cbind(  as.data.frame(rep(NA, length.out = nrow(habitat_raw_data))),as.data.frame(rep(NA, length.out = nrow(habitat_raw_data)))  )
        colnames(data_output_x) = c("metric_data", "score")
        
      }
      
    }else{
      
      print('criteria not numeric OR factor')
      # GENERATE NA
    }
    
  }

  return(data_output_x)
  
}

# ----------- TEST -----------
# output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw('PROSPER')




# ---------------------------------------------------------------------------
#
#   Create A Function to output habitat attribute data FROM CHAMP or Channel Unit Data
#
# ---------------------------------------------------------------------------

# ------ just for testing functions -----

# Channel_Unit_Raw_data_use[habitat_attribute_x] == location_x

# habitat_attribute_x =  '% Fines/Embeddedness' 
# location_x = 2
# reach_name_x  = "Beaver Creek 01"
# raw_data_col_name = Habitat_Attributes_List[habitat_attribute_x ][[1]][location_x]

# Channel_Unit_Raw[raw_data_col_name]


# Channel_Unit_Raw_data_use[habitat_attribute_x]

habitat_attribute_x = "Flow- Summer Base Flow"
habitat_attribute_x = "Pool Quantity & Quality"
reach_name_x  = "Big Meadow Creek 01"
# NOTE: "location_x" is merely the numeric location of data source in the individual
#        habitat_attribute list of data source (1, 2, 3, or 4)

data_col_name = 'PROSPER'
habitat_attribute_x = "Off-Channel- Side-Channels"
data_col_name = 'Side_Channel_Habitat_Prcnt_INDICATOR_6'
data_col_name = data_source_x
# x = FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit(habitat_attribute_x, data_col_name, "LF")
data_col_name = data_source_x
LF_or_HQ = "LF"

FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit = function(habitat_attribute_x, data_col_name, LF_or_HQ){
  
  
  # -----------------------------------
  #    Generate Metric Criteria
  # -----------------------------------
  if(LF_or_HQ == "HQ"){
    metric_criteria_x = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name)
  }else if(LF_or_HQ == "LF"){
    metric_criteria_x = Habitat_Limiting_Factor_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name)
    
  }else{
    print("Pathway entered incorrectly")
  }
 
  
  
  
  # -----------------------------------
  #    Generate Metric value IF in CHAMP Data
  # -----------------------------------
  if( any(CHAMP_data_per_reach_data_sources[habitat_attribute_x] == data_col_name )  ){
    
    data_output_x = CHAMP_data_Updated %>%
      dplyr::select(ReachName,data_col_name)
    colnames(data_output_x) = c("ReachName", "metric_data")
    
  }
  
  # -----------------------------------
  #    Generate Metric value IF in Channel Data
  # -----------------------------------
  if( any(Channel_Unit_Raw_data_sources[habitat_attribute_x] == data_col_name )  ){
    
    data_output_x = Channel_Unit_Raw %>%
      dplyr::select(ReachName,data_col_name)
    colnames(data_output_x) = c("ReachName", "metric_data")
  } 
  


  # -----------------------------------
  #    Generate Score for FACTOR criteria
  # -----------------------------------
  
  if(metric_criteria_x$Category_Type[1]== 'factor'){
    
    data_output_x = data_output_x  %>%
      mutate(score = ifelse(metric_data  == metric_criteria_x$Category[1], metric_criteria$Score[1],
                            ifelse(metric_data  == metric_criteria_x$Category[2], metric_criteria$Score[2],
                                   ifelse(metric_data  == metric_criteria_x$Category[3], metric_criteria$Score[3],
                                          ifelse(metric_data  == metric_criteria_x$Category[4], metric_criteria$Score[4],
                                                 NA)))))
    
    # -----------------------------------
    #    Generate Score for NUMERIC criteria 
    # -----------------------------------
  }else if(metric_criteria_x$Category_Type[1]== 'numeric'){
    
    if(length(metric_criteria_x$Category) == 2){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     NA)))
      
    }else if(length(metric_criteria_x$Category) == 3){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            NA))))
    }else if(length(metric_criteria_x$Category) == 4){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                     metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],  
                                                   NA)))))
    }else if(length(metric_criteria_x$Category) == 5){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                     metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],
                                                   ifelse(metric_data  > metric_criteria_x$Category_lower[5] & 
                                                            metric_data  <= metric_criteria_x$Category_upper[5] , metric_criteria_x$Score[5], 
                                                          NA))))))
    }else if(length(metric_criteria_x$Category) == 6){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  >= metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            ifelse(metric_data  > metric_criteria_x$Category_lower[4] & 
                                                     metric_data  <= metric_criteria_x$Category_upper[4] , metric_criteria_x$Score[4],
                                                   ifelse(metric_data  > metric_criteria_x$Category_lower[5] & 
                                                            metric_data  <= metric_criteria_x$Category_upper[5] , metric_criteria_x$Score[5], 
                                                          ifelse(metric_data  > metric_criteria_x$Category_lower[6] & 
                                                                   metric_data  <= metric_criteria_x$Category_upper[6] , metric_criteria_x$Score[6],
                                                                 NA)))))))
    }
    
    
    
  }else{
    
    print('criteria not numeric OR factor')
    # GENERATE NA
  }
  
  
  return(data_output_x)
  
}










