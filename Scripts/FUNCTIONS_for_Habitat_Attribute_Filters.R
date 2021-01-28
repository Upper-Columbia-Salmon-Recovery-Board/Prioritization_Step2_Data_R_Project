

# ---------------------------------------------------------------------------
#
#      SCRIPT: FUNCTIONS for Habitat Attribute filters
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
#   Create A Function to output habitat attribute data
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

habitat_attribute_x = "% Fines/Embeddedness"
habitat_attribute_x = "Pool Quantity & Quality"
# NOTE: "location_x" is merely the numeric location of data source in the individual
#        habitat_attribute list of data source (1, 2, 3, or 4)

FUNCTION_Return_Habitat_Data = function(habitat_attribute_x, location_x, reach_name_x){
  
  # ---------------------------------------------------------------------- 
  #
  #   Prepare input variables
  #
  # ---------------------------------------------------------------------- 
  
  # ---------------------------------------------------------------------- 
  #   column name in data frame (any of the data frames)
  # ---------------------------------------------------------------------- 
  
  data_col_name = Habitat_Attributes_List[habitat_attribute_x ][[1]][location_x]
  
  # ---------------------------------------------------------------------- 
  #   set column location (set to 0 if habitat attribute on in Chanel Unit data)
  # ---------------------------------------------------------------------- 
  
  if( !is.null(Channel_Unit_Raw_data_use[habitat_attribute_x][[1]])  ){
    Channel_Unit_Raw_data_use_location_x = Channel_Unit_Raw_data_use[habitat_attribute_x][[1]]
  }else{
    Channel_Unit_Raw_data_use_location_x = 0
  }
  
  # ---------------------------------------------------------------------- 
  #   set column location (set to 0 if habitat attribute on in CHAMP data data)
  # ---------------------------------------------------------------------- 
  
  if( !is.null(CHAMP_data_per_reach_data_use[habitat_attribute_x][[1]])  ){
    CHAMP_data_per_reach_data_use_location_x = CHAMP_data_per_reach_data_use[habitat_attribute_x][[1]]
  }else{
    CHAMP_data_per_reach_data_use_location_x = 0
  }
  
  # ---------------------------------------------------------------------- 
  #
  #   Loop to generate metric value based on A) Habitat Attribute, B) location
  #
  # ---------------------------------------------------------------------- 
  
  if( any(colnames(habitat_raw_data) == data_col_name) ){
    
    # -----------------------------------
    #    Generate Metric value from Raw (primary) data table
    # -----------------------------------
    data_output_x = habitat_raw_data %>%
      filter(ReachName   == reach_name_x, ) %>%
      select(data_col_name)
    
  }else if( !is.null(Channel_Unit_Raw_data_use[habitat_attribute_x][[1]])  &
            Channel_Unit_Raw_data_use_location_x == location_x  ){
    
    # ------------ pull vector data --------
    #data_vector_x = Channel_Unit_Raw[data_col_name]
    data_output_x = Channel_Unit_Raw %>%
      filter(ReachName   == reach_name_x, ) %>%
      select(data_col_name)
    # if no value, make value NA
    if( nrow(data_output_x) == 0 ){
      data_output_x = NA
    }
    
    
    # ---------------------------------------------------------------------- 
    #    if data source for this habitat attribute is from the CHAMP data table 
    # ---------------------------------------------------------------------- 
    
  }else if(!is.null(CHAMP_data_per_reach_data_use[habitat_attribute_x][[1]])  &
           CHAMP_data_per_reach_data_use_location_x == location_x){
    
    
    # data_vector_x = Channel_Unit_Raw[data_col_name]
    data_output_x = CHAMP_data_per_reach %>%
      filter(ReachName   == reach_name_x, ) %>%
      select(data_col_name)
    # if no value, make value NA
    if( nrow(data_output_x) == 0 ){
      data_output_x = NA
    }
    
    
    # ---------------------------------------------------------------------- 
    #    if data source is from the primary Raw Habitat data table
    # ---------------------------------------------------------------------- 
    
  }else{
    
    
    #print('error - no data matching habitat attribute:')
    #print(habitat_attribute_x)
    #print('     for data source name (column name: ')
    #print( Habitat_Attributes_List[habitat_attribute_x ][[1]][location_x])
    #print('     data_col_name: ')
    #print(data_col_name)
    #print("                ")
    
    data_output_x = "NA"
    
    
  }
  
  # ---------------------------------------------------------------------- 
  #
  #  Generate Score (1,3,5) - IF Metric data is present - based on Criteria
  #
  # ---------------------------------------------------------------------- 
  
  # ----------- if metric value is present (is NOT NA) -----
  
  # ---------- if any rows present --------
  if(!is.null(dim(data_output_x)[1] )){
    
    # ----------- if any rows present -----------
    if(dim(data_output_x)[1] >  0){
      
      # ---------- if metric data is not NA -----
      if( !is.na(data_output_x[[1]])   & data_output_x[[1]] != "NA"  ){
        
        # -------------------- Identify rows with the data source (will be multiple if multiple criteria) -----
        metric_criteria_x = Habitat_Limiting_Factor_Rating_Criteria_Updated %>%
          filter(Data_Sources   == data_col_name) 

        # -----------------------------
        #    IF score based on category (e.g. "Adequate" or "3d" score)
        # -----------------------------
        if(metric_criteria_x$Category_Type[1]== 'factor'){
          
          
          # -----------------------------
          #    IF habitat attribute is Contaminants (just being listed gets a 1)
          # -----------------------------
          if(habitat_attribute_x == 'Contaminants'){
            
            # ----------- if no contaminants listed -------
            if(data_output_x[[1]] == "NA"){
              score_output_x = 5
              
              # ---------- if any contaminants listed ------------  
            }else{
              score_output_x = 1
            }
            
            
            # -----------------------------
            #    IF score is simple REI value
            # ----------------------------- 
          }else{
            # ------------------- identify specific score for the metric -----------
            score_output_x = metric_criteria_x %>%
              filter(Category   == data_output_x[[1]], ) 
            score_output_x = score_output_x$Score
          }
          
          
          
          # -----------------------------
          #    IF score based on numeric value
          # -----------------------------
        }else if(metric_criteria_x$Category_Type[1]== 'numeric'){
          
          
          # -----------------------------
          #    IF Cover- Wood OR Pool Quantity & Quality, filter by stream width
          # -----------------------------
          
          if(metric_criteria_x$Habitat_Attribute[1] == 'Cover- Wood' |
             metric_criteria_x$Data_Sources[1] ==  'Pools_per_mile_INDICATOR_2'){
            
            # ----------- pull stream width for this reach -------------
            stream_width_m = Reach_Information_data %>%
              filter(ReachName == reach_x) %>%
              select(Length_AvgWettedWidth_Meters)
            
            # ----------------- identify criteria based on stream width --------
            metric_criteria_UPDATED_x = metric_criteria_x %>%
              filter(Filter_value_upper_meters   > stream_width_m[[1]], ) %>%
              filter(Filter_value_lower_meters   <= stream_width_m[[1]] ) 
            
            # ------------ identify category based on lower and upper bounds -------
            data_output_filter_x = metric_criteria_UPDATED_x %>%
              filter(Category_upper   > data_output_x[[1]], ) %>%
              filter(Category_lower   <= data_output_x[[1]] ) 
            
            score_output_x = data_output_filter_x$Score
            
            
            # -----------------------------
            #    IF stream temperature (uses same data source, but different different habitat attributes)
            # -----------------------------
          }else if(metric_criteria_x$Habitat_Type[1] == 'Temperature'){
            
            # --------------- get metric data ONLY for this habitat attribute --------
            metric_criteria_UPDATED_x = metric_criteria_x %>%
              filter(Habitat_Attribute   == habitat_attribute_x ) 
            
            # ------------ identify category based on lower and upper bounds -------
            data_output_filter_x = metric_criteria_UPDATED_x %>%
              filter(Category_upper   > data_output_x[[1]], ) %>%
              filter(Category_lower   <= data_output_x[[1]] ) 
            
            score_output_x = data_output_filter_x$Score
            
            
            # -----------------------------
            #    IF no stream width filter for criteria
            # -----------------------------
          }else{
            
            # ------------ identify category based on lower and upper bounds -------
            data_output_filter_x = metric_criteria_x %>%
              filter(Category_upper   > data_output_x[[1]], ) %>%
              filter(Category_lower   <= data_output_x[[1]] ) 
            
            score_output_x = data_output_filter_x$Score
            
          }
          
          
        }else{
          score_output_x = NA
        }  
        
        # --------------- IF contaminants but cell is "NA"  - set to 5 -----------------  
      }else if(habitat_attribute_x == 'Contaminants'){
        
        # ----------- if no contaminants listed -------
        if(data_output_x[[1]] == "NA"){
          score_output_x = 5
        }
        
      }else{
        score_output_x = NA
      }
    }else{
      score_output_x = NA
    }
    
  }else{
    score_output_x = NA
  }
  # ---------------------------------------------------------------------- 
  #
  #    Return both the metric and score values
  #
  # ---------------------------------------------------------------------- 
  
  # --------- get Basin and Assessment Unit name --------
  basin_x = Reach_Information_data %>%
    filter(ReachName == reach_name_x) %>%
    select(Basin)
  AU_x = Reach_Information_data %>%
    filter(ReachName == reach_name_x) %>%
    select(Assessment.Unit)
  
  output_all = t( as.data.frame(c(reach_name_x, basin_x, AU_x, habitat_attribute_x, data_col_name, data_output_x[[1]], score_output_x ) )  )
  if(nrow(output_all)>1){
    output_all = t(output_all)
  }
  #print(output_all)
  #print(dim(output_all))
  colnames(output_all) = c('ReachName', 'Basin', 'Assessment.Unit',
                           'Habitat_Attribute','data_source', 'metric_value','score')
  
  return(output_all)
  
  
  
}






# ---------------------------------------------------------------------- 
#
#    if Professional Judgment and REI value over-rides data 
#      NOTE: in this code Professional Judgment ALWAYS over-rides previous data
#            Also note if "NA" is generated, in loop, the previous score will not
#            be replaced.
#
# ---------------------------------------------------------------------- 

FUNCTION_Update_REI_value_OR_Profession_Judgment = function(habitat_attribute_x, reach_name_x){
  
  
  # ---------------------------------------------------------------------- 
  #
  #  Generate data for this Reach Name
  #
  # ---------------------------------------------------------------------- 
  
  
  # ----- identify if reach and habitat attribute has REI value over-ride ------
  data_rei_x = Habitat_Attribute_Notes_and_Professional_Judgement %>%
    filter(ReachName   == reach_name_x, ) %>%                # ONLY for this reach
    filter(Habitat_Attribute == habitat_attribute_x ) %>%    # ONLY for this habitat attribute
    filter(Use_REI_Value == "yes")                           # ONLY if indicated to replace w/ REI value
  
  # --------------- data - where REI value will not over-ride ----------------
  data_output_x = Habitat_Attribute_Notes_and_Professional_Judgement %>%
    filter(ReachName   == reach_name_x) %>%
    filter(Habitat_Attribute == habitat_attribute_x ) %>%    # ONLY for this habitat attribute
    filter(Use_REI_Value == "no") 
  
  
  # ---------------------------------------------------------------------- 
  #
  #  Update Value based on REI-value
  #
  # ---------------------------------------------------------------------- 
  
  if(nrow(data_rei_x)==1){
    
    # ------ name of REI category score for this habitat attribute (always first location) ----
    data_col_name = Habitat_Attributes_List[habitat_attribute_x ][[1]][1]
    
    # ------------ get metric value (Unacceptable, At Risk, or Adequate) ------
    metric_value_x = habitat_raw_data %>%
      filter(ReachName   == reach_name_x, ) %>%
      select(data_col_name)
    
    # -------------------- Identify rows with the data source (will be multiple if multiple criteria) -----
    metric_criteria_x = Habitat_Limiting_Factor_Rating_Criteria_Updated %>%
      filter(Data_Sources   == data_col_name) 
    
    # ------------------- identify specific score for the metric -----------
    score_output_x = metric_criteria_x %>%
      filter(Category   == metric_value_x[[1]], ) 
    score_x = score_output_x$Score
    
    # ------------------ generate notes ------------
    notes_output_x = data_rei_x$Notes_or_Professional_Judgement[1]
    
    
    # ---------------------------------------------------------------------- 
    #
    #  Update Value based on Professional Judgment
    #
    # ---------------------------------------------------------------------- 
    
  }else if(nrow(data_output_x) > 0){
    
    
    # ---------------- if reach AND habitat attribute is present in Professional Judgment -------------
    
    
    # ---------------- if reach AND habitat attribute is present in Professional Judgment -------------
    score_x = data_output_x$Updated_Value
    # ----------- take minimum value -----------
    score_x = min(score_x, na.rm=T)
    
    # ------------------ generate notes ------------
    notes_output_x = data_output_x$Notes_or_Professional_Judgement[1]
    
    
  }else{
    
    score_x = NA
    notes_output_x = NA
    
  }
  
  output_x = c(score_x, notes_output_x)
  
  return(output_x)
  
}






