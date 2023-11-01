
# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Habitat Attribute Scores Table
#            (for Habitat Quality AND Limiting Factor Analysis)
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
# generates: Habitat_Attributes_List

source(paste(script_path, 'Data_Sources_List_for_Habitat_Attributes.R', sep=""))

# ---------------------------------------------------------------------------
#
#   Call Functions for Habitat Attribute Filters
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'FUNCTIONS_for_Habitat_Attribute_Filters.R', sep=""))

# ---------------------------------------------------------------------------
#
#
#        Generate Habitat Attributes Scores (1, 3, 5) for each Habitat Attribute
#
#
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
#
#   Go through by habitat attribute (instead of by reach)
#
# ---------------------------------------------------------------------------

# -------------- to test ------------------------
test = TRUE
if(test){
  habitat_attribute_x = names(Habitat_Attributes_List)[5]
  data_sources_list =  Habitat_Attributes_List[habitat_attribute_x]
  data_source_x = data_sources_list[[1]][1]
}


# ----- empty data frame to write to -----------
Habitat_Attribute_Scores = data.frame()


ptm <- proc.time()[3]
for(habitat_attribute_x in names(Habitat_Attributes_List) ){
  print(paste("-----------------Habitat Attribute----------------------: ", habitat_attribute_x,sep="") )
  
  # --------------------------------------------------------------------
  #   Loop through each Data Source for this specific habitat attribute
  # --------------------------------------------------------------------
  data_sources_list =  Habitat_Attributes_List[habitat_attribute_x]
  data_sources_list_Okanogan =  Habitat_Attributes_List_OKANOGAN[habitat_attribute_x]
  
  # --------------------------------------------------------------------
  #   Loop through each data source
  # --------------------------------------------------------------------
  # ------------ data frame to record habitat attributes --------
  habitat_attribute_x_data_frame = data.frame()
  # ------------ prep data source name -----------
  data_source_output_list_per_row = c('a') # create a nchar = 1 data frame
  column_names = c("(HabitatAttributeScore1)",	"(HabitatAttributeScore2)",	"(HabitatAttributeScore3)",
                   "(HabitatAttributeScore4)", "(HabitatAttributeScore5)", "(HabitatAttributeScore6)")

  # ------------------------------------------------------------------
  #         For non-Okanogan sub-basins (Methow, Entiat, Wenatchee)
  # ------------------------------------------------------------------
  
  i = 0
  
  for( data_source_x in data_sources_list[[1]] ){
    print(paste("Data Source (column in Habitat Raw, CHAMP, or Channel Unit): ", data_source_x,sep="") )
    
    # --------------- generate data source name --------------
    i = i + 1
    if( nchar(data_source_output_list_per_row) == 1){
      data_source_output_list_per_row = paste(data_source_x, column_names[i], sep=" ")
    }else{
      data_source_output_list_per_row = paste(data_source_output_list_per_row,
                                              paste(data_source_x, column_names[i], sep=" "), sep=",") 
    }

    
    # ------------------- skip reading PROFESSOINAL JUDGEMENT -------------
    if( data_source_x== "PROFESSIONAL JUDGEMENT" ){ 
      
      # ------------ Professional judgment gets "5" unless dictated otherwise -----------
      output_x = cbind( as.data.frame( habitat_raw_data$ReachName ),
                        as.data.frame(rep("NA", length.out=dim(habitat_raw_data)[1] )),
                        as.data.frame(rep(5, length.out=dim(habitat_raw_data)[1] )))
      colnames(output_x ) = c('metric_data', 'score')
    
    # ------------------ read in from Channel Unit data  -----------
    }else if( any(Channel_Unit_Raw_data_sources[habitat_attribute_x] == data_source_x) ){
      
      # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
      # outputs both metric value and score
      output_x = FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit(habitat_attribute_x, data_source_x, "LF" )
      
    # ------------------ read in from CHAMP  -----------
    }else if(  any(CHAMP_data_per_reach_data_sources[habitat_attribute_x] == data_source_x )   ){
      
      output_x = FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit(habitat_attribute_x, data_source_x, "LF" )
      
      # ----------------- read in from Habitat Raw data ---------
    }else{
      
      # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
      # outputs both metric value and score
      output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x, data_source_x , "LF" )

    }
    
    # ------------------ Add Column Names ----------------
    colnames(output_x) = c("ReachName","metric_data","score")
    
    # -------- data frame for this specific reach and habitat attribute --------
    output_x_2 = as.data.frame(output_x[,c("ReachName","score")])
    colnames(output_x_2) = c( "ReachName",paste("score",i,sep="_") )
    scores_x = as.data.frame(output_x$score)
    
    if(nrow(habitat_attribute_x_data_frame) == 0){
      habitat_attribute_x_data_frame = as.data.frame(output_x_2)
    }else{
      habitat_attribute_x_data_frame = merge(habitat_attribute_x_data_frame, output_x_2, by="ReachName")
    }

  }

  # ------------------------------------------------------------------
  #         For Okanogan sub-basins 
  # ------------------------------------------------------------------
  
  # ------------------ over-write data in Okanogan to all be NA ----------
  for(reachx_okanogan in Reach_Information_data_Okanogan$ReachName){
    x_OK = which(habitat_attribute_x_data_frame$ReachName == reachx_okanogan)
    habitat_attribute_x_data_frame[x_OK,2:ncol(habitat_attribute_x_data_frame)] = rep(NA,length.out=(ncol(habitat_attribute_x_data_frame)-1))
  }

  data_source_output_list_per_row_Okanogan = c('a') # create a nchar = 1 data frame
  
  i = 0 # for the habitat attribute column - first is reach name
  
  for( data_source_x in data_sources_list_Okanogan[[1]] ){
    print(paste("Data Source - Okanogan (column in Habitat Raw, CHAMP, or Channel Unit): ", data_source_x,sep="") )
    
    # --------------- generate data source name --------------
    i = i + 1 # for the habitat attribute column
    if( nchar(data_source_output_list_per_row_Okanogan) == 1){
      data_source_output_list_per_row_Okanogan = paste(data_source_x, column_names[i], sep=" ")
    }else{
      data_source_output_list_per_row_Okanogan = paste(data_source_output_list_per_row_Okanogan,
                                              paste(data_source_x, column_names[i], sep=" "), sep=",") 
    }
    
    # ------------------- skip reading PROFESSOINAL JUDGEMENT -------------
    if( data_source_x== "PROFESSIONAL JUDGEMENT" ){ 
      
      # ------------ Professional judgment gets "5" unless dictated otherwise -----------
      output_x = cbind( as.data.frame( habitat_raw_data$ReachName ),
                        as.data.frame(rep("NA", length.out=dim(habitat_raw_data)[1] )),
                        as.data.frame(rep(5, length.out=dim(habitat_raw_data)[1] )))
      colnames(output_x ) = c('metric_data', 'score')
      
      # ------------------ read in from Channel Unit data  -----------
    }else if( any(Channel_Unit_Raw_data_sources[habitat_attribute_x] == data_source_x) ){
      
      # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
      # outputs both metric value and score
      output_x = FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit(habitat_attribute_x, data_source_x, "LF" )
      
      # ------------------ read in from CHAMP  -----------
    }else if(  any(CHAMP_data_per_reach_data_sources[habitat_attribute_x] == data_source_x )   ){
      
      output_x = FUNCTION_generate_habitat_attribute_score_from_CHAMP_or_Channel_Unit(habitat_attribute_x, data_source_x, "LF" )
      
      # ----------------- read in from Habitat Raw data ---------
    }else{
      
      # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
      # outputs both metric value and score
      output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x, data_source_x , "LF" )
      
    }
    
    # ------------------ Add Column Names ----------------
    colnames(output_x) = c("ReachName","metric_data","score")
    
    # -------- data frame for this specific reach and habitat attribute --------
    output_x_2 = as.data.frame(output_x[,c("ReachName","score")])
    colnames(output_x_2) = c( "ReachName",paste("score",i,sep="_") )
    
    # ------------- loop through new Okanogan data and add to habitat_attribute_x_data_frame --------
    numeric_rows_x = which(  !is.na(output_x_2$score_1) )  # which rows/reaches have metric
    for(i_x in numeric_rows_x){
      # ----- row in habitat_attribute_x_data_frame that has i_x ReachName ---------
      i_x_habitat = which(habitat_attribute_x_data_frame$ReachName == output_x_2$ReachName[i_x])
      # ----------- pull new Okanogan data and put in habitat_attribute_x_data_frame -------
      habitat_attribute_x_data_frame[i_x_habitat ,(i+1)] = output_x_2$score_1[i_x]
    }
    
  }
  
  # ------------------------------------------------------------------
  #          For all four sub-basins
  # ------------------------------------------------------------------
  
  # -------------- add NA columns so there are four columns -----------------
  na_column = as.data.frame(rep(NA, length.out=nrow(habitat_attribute_x_data_frame)))
  if( ncol(habitat_attribute_x_data_frame) < 7){
    for(i in 1:(7-ncol(habitat_attribute_x_data_frame) ) )
      habitat_attribute_x_data_frame = cbind(habitat_attribute_x_data_frame, na_column)
  }
  
  # --------------------------------------------------------------------
  #  Prepare Habitat data frame 
  # --------------------------------------------------------------------
  
  # ---------------------------  prepare habitat data frame ----------------
  colnames(habitat_attribute_x_data_frame) = c("ReachName", "HabitatAttributeScore1",	"HabitatAttributeScore2",	"HabitatAttributeScore3",	"HabitatAttributeScore4",
                                               "HabitatAttributeScore5","HabitatAttributeScore6")
  habitat_attribute_x_data_frame = as.tibble(habitat_attribute_x_data_frame)
  cols.num <- c("HabitatAttributeScore1",	"HabitatAttributeScore2",	"HabitatAttributeScore3",	"HabitatAttributeScore4",  "HabitatAttributeScore5",  "HabitatAttributeScore6")
  habitat_attribute_x_data_frame[cols.num] <- sapply(habitat_attribute_x_data_frame[cols.num],as.numeric)
  
  # ------------------- get minimum score for each row ----------
  habitat_attribute_x_data_frame_CALC = habitat_attribute_x_data_frame[,c("HabitatAttributeScore1",	"HabitatAttributeScore2",	"HabitatAttributeScore3",	"HabitatAttributeScore4", "HabitatAttributeScore5", "HabitatAttributeScore6")]
  
  habitat_attribute_x_data_frame_CALC = habitat_attribute_x_data_frame_CALC %>% 
    rowwise() %>%
    mutate(minimum_score = min(c_across(), na.rm=T) )
  
  # ------------------- which row is the minimum score  ----------
  column_data_sources_x = c()
  for(i2 in 1:nrow(habitat_attribute_x_data_frame_CALC)){
    # ----------- pull basin --------
    basin_x = Reach_Information_data$Basin[which(Reach_Information_data$ReachName == habitat_attribute_x_data_frame$ReachName[i2])]
    # --------------- identify which columns were the minimum ---------
    min_x = which( as.numeric(habitat_attribute_x_data_frame_CALC[i2,1:6]) == as.numeric(habitat_attribute_x_data_frame_CALC[i2,7]) )
   
    # ------ for Methow, Entiat, Wenatchee ------
    if(basin_x != "Okanogan"){
      # ------------pull the data source(s) -------------
      data_sources_row_x = data_sources_list[[1]][min_x]
      # if multiple data sources are equal, collapse into one name
      if(length(data_sources_row_x)>1){
        data_sources_row_x = paste(data_sources_row_x, collapse=", ")
      }
    # ------------- for Okanogan ----------------
    }else{
      # ------------pull the data source(s) -------------
      data_sources_row_x = data_sources_list_Okanogan[[1]][min_x]
      # if multiple data sources are equal, collapse into one name
      if(length(data_sources_row_x)>1){
        data_sources_row_x = paste(data_sources_row_x, collapse=", ")
      }
    }

    # ------------ if no data ---------
    if(length(min_x) == 0){data_sources_row_x = NA}
    # --------- combine -----
    column_data_sources_x = rbind(column_data_sources_x, t(as.data.frame(data_sources_row_x)))
  }
  # ------- add data sources column ---------
  colnames(column_data_sources_x) = "Data_Source_of_metric_prioritized"
  habitat_attribute_x_data_frame_CALC = cbind(habitat_attribute_x_data_frame_CALC, column_data_sources_x)
  
  # ------- add the reach name back in ---------
  habitat_attribute_x_data_frame_CALC = cbind(habitat_attribute_x_data_frame$ReachName,habitat_attribute_x_data_frame_CALC)
  
  # ---------------- over-ride with REI values -------
  if( any(REI_Default_List == habitat_attribute_x) ){
    # ----------- pull REI values if REI value is present and over-ride minimum value ------
    REI_present_x = which( !is.na(habitat_attribute_x_data_frame_CALC$HabitatAttributeScore1) )
    habitat_attribute_x_data_frame_CALC$minimum_score[REI_present_x] = habitat_attribute_x_data_frame_CALC$HabitatAttributeScore1[REI_present_x]
  }
  # ------------ update first column name ---------
  colnames(habitat_attribute_x_data_frame_CALC)[1] = "ReachName"
  
  # --------------------- add data source ----------------
  data_source_output_list_per_row = rep(data_source_output_list_per_row, length.out=nrow(habitat_attribute_x_data_frame))
  data_source_output_list_per_row = as.data.frame(data_source_output_list_per_row)
  # --------------------- add habitat attribute ----------------
  habitat_attribute_name = rep(habitat_attribute_x, length.out=nrow(habitat_attribute_x_data_frame))
  habitat_attribute_name = as.data.frame(habitat_attribute_name)
  # -------------------- match Basin and Assessment Unit ----------------
  habitat_attribute_x_data_frame_Reach_Name = habitat_attribute_x_data_frame[,"ReachName"]
  habitat_attribute_x_data_frame_Reach_Name = as.data.frame(habitat_attribute_x_data_frame_Reach_Name)
  colnames(habitat_attribute_x_data_frame_Reach_Name) = "ReachName"
  habitat_attribute_x_data_frame = merge(habitat_attribute_x_data_frame_Reach_Name,   Reach_Information_data[, c("ReachName","Basin","Assessment.Unit")],
                                         by="ReachName")
  # ----------------- add habitat attribute name and data source list -----------
  habitat_attribute_x_data_frame = cbind( habitat_attribute_x_data_frame, habitat_attribute_name,  data_source_output_list_per_row)
  colnames(habitat_attribute_x_data_frame) = c('ReachName',	'Basin',	'Assessment.Unit',	'Habitat_Attribute',	'Data_Sources')
  # --------------- merge Habitat Attribute Scores data with reach basic data ----------------
  habitat_attribute_x_data_frame = merge(habitat_attribute_x_data_frame, habitat_attribute_x_data_frame_CALC, by="ReachName")
  colnames(habitat_attribute_x_data_frame) = c('ReachName',	'Basin',	'Assessment.Unit',	'Habitat_Attribute',	'Data_Sources',	'HabitatAttributeScore1',
                                               'HabitatAttributeScore2',	'HabitatAttributeScore3',	'HabitatAttributeScore4',
                                               'HabitatAttributeScore5',	'HabitatAttributeScore6', 'Habitat_Attribute_Score','Data_Source_of_Metric_Prioritized')
  habitat_attribute_x_data_frame$Notes_or_Professional_Judgement = NA
  
  # --------------------------------------------------------------------
  #   Insert Professional Judgment where indicated
  # --------------------------------------------------------------------
  
  # ------------ Where indicated, Over-ride the score with REI OR Professional Judgment -------------------
  # ---------------- generate professional judgment score -----------------
  prof_judgement_score_notes_x = FUNCTION_Update_REI_value_OR_Profession_Judgment(habitat_attribute_x)

  # --------------- only replace professional judgment where new score (1,3,5) is generated --------------
  if( !is.na(prof_judgement_score_notes_x$score[1]) ){
    
    # ---------------- only use professional judgement reaches WITH a score -----------
    prof_judgement_score_notes_x = prof_judgement_score_notes_x[which( as.numeric(prof_judgement_score_notes_x$score) > 0) ,  ]
    
    # --------- merge habitat data frame with professional judgement ----------
    habitat_attribute_x_data_frame_Merge = merge(habitat_attribute_x_data_frame,prof_judgement_score_notes_x, by="ReachName", all.x=TRUE)
    # ------------------- remove duplicate rows -------------
    
    # ------------- identify rows where over-ride score is present ----------
    over_ride_score_x = which(habitat_attribute_x_data_frame_Merge$score >= 0)
    # --------------- updated scores with the over-ride score 
    habitat_attribute_x_data_frame_Merge$Habitat_Attribute_Score[over_ride_score_x] = habitat_attribute_x_data_frame_Merge$score[over_ride_score_x]
    # --------------- over ride notes where scores were over-ridden ----------
    habitat_attribute_x_data_frame_Merge$Notes_or_Professional_Judgement.x[over_ride_score_x] = habitat_attribute_x_data_frame_Merge$Notes_or_Professional_Judgement.y[over_ride_score_x]
    
    # -------------------- generate new habitat_attribute_x_data_frame ---------
    habitat_attribute_x_data_frame = habitat_attribute_x_data_frame_Merge[, c("ReachName" , "Basin","Assessment.Unit", "Habitat_Attribute", "Data_Sources" ,"HabitatAttributeScore1",           
                                  "HabitatAttributeScore2","HabitatAttributeScore3","HabitatAttributeScore4" ,
                                  'HabitatAttributeScore5',	'HabitatAttributeScore6',          
                                   "Habitat_Attribute_Score" ,"Data_Source_of_Metric_Prioritized", "Notes_or_Professional_Judgement.x")]
    colnames(habitat_attribute_x_data_frame) = c("ReachName" , "Basin","Assessment.Unit", "Habitat_Attribute", "Data_Sources" ,"HabitatAttributeScore1",           
                                                 "HabitatAttributeScore2","HabitatAttributeScore3","HabitatAttributeScore4" ,
                                                 'HabitatAttributeScore5',	'HabitatAttributeScore6',          
                                                 "Habitat_Attribute_Score" ,"Data_Source_of_Metric_Prioritized", "Notes_or_Professional_Judgement")
  }
  
  # ------------- if it's infinite, convert to NA -----------
  habitat_attribute_x_data_frame$Habitat_Attribute_Score[which(habitat_attribute_x_data_frame$Habitat_Attribute_Score == "Inf")] = NA 
  
  # ---------------------------------------
  #    combine metrics with data frame
  # ---------------------------------------
  Habitat_Attribute_Scores_ORIG = Habitat_Attribute_Scores
  Habitat_Attribute_Scores = rbind( Habitat_Attribute_Scores, habitat_attribute_x_data_frame )
  print(paste("number of rows for Output : ", nrow(Habitat_Attribute_Scores)))
    
}


# ------------------ output data -------------------------
Habitat_Attribute_Scores = as.data.frame(Habitat_Attribute_Scores)
Habitat_Attribute_Scores$Habitat_Attribute_Score = as.numeric(as.character(Habitat_Attribute_Scores$Habitat_Attribute_Score))

# --------- only write data if "yes" indicated (since this adds ~1 minute to script writing time) ------------
if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  output_path_x =  paste(output_path,'Habitat_Attribute_Scores.xlsx', sep="")
  write.xlsx(
    Habitat_Attribute_Scores,
    output_path_x,
    col.names = TRUE,
    row.names = FALSE,
    append = FALSE,
    showNA = TRUE,
    password = NULL
  )
}

print(paste("Time to complete Habitat Attributes Output: ", paste(round((proc.time()[3] - ptm)/60, 2), " minutes")    ))



