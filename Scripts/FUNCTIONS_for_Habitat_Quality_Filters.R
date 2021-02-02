


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

habitat_attribute_x = "Flow- Summer Base Flow"
habitat_attribute_x = "Pool Quantity & Quality"
reach_name_x  = "Big Meadow Creek 01"
# NOTE: "location_x" is merely the numeric location of data source in the individual
#        habitat_attribute list of data source (1, 2, 3, or 4)

data_col_name = 'PROSPER'

FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw = function(data_col_name){
  
  
  # -----------------------------------
  #    Generate Metric Criteria
  # -----------------------------------
  metric_criteria_x = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated %>%
    filter(Data_Sources   == data_col_name) 
  
  # -----------------------------------
  #    Generate Metric value from Raw (primary) data table
  # -----------------------------------
  data_output_x = habitat_raw_data %>%
    select(data_col_name)
  
  colnames(data_output_x) = "metric_data"
  
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                                   NA)))
      
    }else if(length(metric_criteria_x$Category) == 3){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            NA))))
    }else if(length(metric_criteria_x$Category) == 4){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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

# ----------- TEST -----------
output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw('PROSPER')





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

FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit = function(habitat_attribute_x, data_col_name){
  
  
  # -----------------------------------
  #    Generate Metric Criteria
  # -----------------------------------
  metric_criteria_x = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated %>%
    filter(Data_Sources   == data_col_name) 
  
  # -----------------------------------
  #    Generate Metric value IF in Channel Data
  # -----------------------------------
  if( !is.null(Channel_Unit_Raw_data_use[habitat_attribute_x][[1]]) ){
    
    data_output_x = Channel_Unit_Raw %>%
      select(data_col_name)
    
    colnames(data_output_x) = "metric_data"
    
    
    
    
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     NA)))
      
    }else if(length(metric_criteria_x$Category) == 3){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
                                metric_data  <= metric_criteria_x$Category_upper[1] , metric_criteria_x$Score[1],
                              ifelse(metric_data  > metric_criteria_x$Category_lower[2] & 
                                       metric_data  <= metric_criteria_x$Category_upper[2] , metric_criteria_x$Score[2],
                                     ifelse(metric_data  > metric_criteria_x$Category_lower[3] & 
                                              metric_data  <= metric_criteria_x$Category_upper[3] , metric_criteria_x$Score[3],
                                            NA))))
    }else if(length(metric_criteria_x$Category) == 4){
      data_output_x = data_output_x  %>%
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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
        mutate(score = ifelse(metric_data  > metric_criteria_x$Category_lower[1] & 
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





# ---------------------------------------------------------------------------
#
#   Create A Function when multiple Habitat Attributes 
#
# ---------------------------------------------------------------------------



FUNCTION_generate_habitat_attribute_score_from_multiple_habitat_attributes = function(habitat_attribute_x){
  
  
  
  FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw()
  
  
  
}










