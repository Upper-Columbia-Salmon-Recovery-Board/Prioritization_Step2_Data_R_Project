
# -------------------------------------------------------------------
#
#   R Script to take old reach names and update with new Reach Names
#
# -------------------------------------------------------------------


# -------------------------------------------------------------------
#
#   Read in Crosswalk 
#
# -------------------------------------------------------------------

Okanogan_Crosswalk =  read_excel(  paste(Okanogan_EDT_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )


# ------------------------------------------------------------------------------------------------------------
#
#       Function to update reach names 
#
# ------------------------------------------------------------------------------------------------------------

# ------------- to test it out -------------
#updated_column_old = Okanogan_Crosswalk$ReachName_Old
#update_column_new = Okanogan_Crosswalk$ReachName_New
input_data_frame = habitat_raw_data
input_column = as.data.frame(habitat_raw_data[,"ReachName"])
#input_column = input_column[,1]
input_column_name = "ReachName"
new_and_old_data_frame = Okanogan_Reach_Crosswalk[,c("ReachName_Old", "ReachName_New", "Duplicate_RTT_Names")]
output_path_x = paste(Okanogan_EDT_path,'Habitat_Data_Raw.xlsx', sep="")

# -------------- the Function -----------------
FUNCTION_read_in_and_update_file_reach_names = function(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x){
  
  # ------ which reaches have a single name for each --------------
  one_to_one_crossover = which(Okanogan_Crosswalk$identical_reaches_RTT_Okanogan == "yes" |
                                 Okanogan_Crosswalk$`Multiple_RTT_Reaches?` == "yes")
  # ---------------- which reaches have multiple Okanogan names for single RTT reach ----------
  duplicate_RTT_names =  which(Okanogan_Crosswalk$`Multiple_Okanogan_Reaches?` == "yes")
  
  # -----------------------------------------------
  #   Update names with direct 1:1 crossover
  # -----------------------------------------------
  
  # --------------- update names in column -----------------
  for(i in 1:nrow(new_and_old_data_frame)){
    if( any(one_to_one_crossover == i)  ){
      
      x_replace = which(input_column[,1] == as.character(new_and_old_data_frame[i,1]) )
      input_column[x_replace,1] = new_and_old_data_frame[i,2]
      
    }else if( any(duplicate_RTT_names == i) ){
      
      
    }

  }
  
  # ------------------ update column in data frame ------------
  input_data_frame[,input_column_name] = input_column
  
  # -----------------------------------------------
  #   Update names with 
  # -----------------------------------------------
  
  
  # ----------------- write excel ------------
  write_xlsx(input_data_frame,output_path_x )
  
}

# -------------------------------------------------------------------
#    Read and Write Data - FIRST time updating
# -------------------------------------------------------------------

# --------------- Habitat_Data_Raw ---------
FUNCTION_read_in_and_update_file_reach_names(habitat_raw_data, as.data.frame(habitat_raw_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Reach_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="") )

# --------------- Channel_Unit_Raw ---------
FUNCTION_read_in_and_update_file_reach_names(Channel_Unit_Raw, as.data.frame(Channel_Unit_Raw[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Channel_Unit_Raw.xlsx', sep="")  )

# ----------------------- Confinement_Scores ----------------------
FUNCTION_read_in_and_update_file_reach_names(Confinement_Scores, 
                                             as.data.frame(Confinement_Scores[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Confinement_Scores.xlsx', sep="")  )



# ----------------------- Habitat_Attribute_Notes_and_Professional_Judgement ----------------------
FUNCTION_read_in_and_update_file_reach_names(Habitat_Attribute_Notes_and_Professional_Judgement, 
                                             as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="")  )


# ----------------------- Life_Stage_Priorities_AU_and_Reach_data ----------------------
FUNCTION_read_in_and_update_file_reach_names(Life_Stage_Priorities_AU_and_Reach_data, 
                                             as.data.frame(Life_Stage_Priorities_AU_and_Reach_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")  )

# ------------------- Reach_Information_data ------------------
FUNCTION_read_in_and_update_file_reach_names(Reach_Information_data, 
                                             as.data.frame(Reach_Information_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'ReachInfo.xlsx', sep="")  )




# ------------------------------------------------------------------------------------------------------------
#
#        the Function to update data frames that have already been updated
#
# ------------------------------------------------------------------------------------------------------------



input_data_frame = Habitat_Attribute_Notes_and_Professional_Judgement
input_column = as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"])
input_column_name = "ReachName"
new_and_old_data_frame = Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")]
output_path_x = paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="") 


FUNCTION_read_in_and_update_file_reach_names_already_updated_once = function(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x){
  
  # ------ which reaches have a single name for each --------------
  one_to_one_crossover = which(Okanogan_Crosswalk$identical_reaches_RTT_Okanogan == "yes" |
                                 Okanogan_Crosswalk$`Multiple_RTT_Reaches?` == "yes")
  # ---------------- which reaches have multiple Okanogan names for single RTT reach ----------
  duplicate_RTT_names =  which(Okanogan_Crosswalk$`Multiple_Okanogan_Reaches?` == "yes")
  
  # -----------------------------------------------
  #   Update names with direct 1:1 crossover
  # -----------------------------------------------
  
  # --------------- update names in column -----------------
  for(i in 1:nrow(Okanogan_Crosswalk)){
    
    # ------- if name is already been updated or is same ---------
    if( any(input_column$ReachName == Okanogan_Crosswalk$ReachName_New[i])   ){
      
      # NO need to replace anything, because no need
      
    # ----------- if new name for reach ----------
    }else{
      
    # ------------- IF one to one between RTT and Okanogan reach name (replace directly) -------
      if(  any(one_to_one_crossover == i)  ){
        
        # -------------- which reach to replace --------------
        x_replace = which(input_column$ReachName == Okanogan_Crosswalk$ReachName_Old[i])
        # ----------- replace it ------------
        input_column[x_replace,1] = Okanogan_Crosswalk$ReachName_New[i]
        
    # --------------- IF multiple Okanogan reaches for a single RTT reach ------------  
      }else if(   any(duplicate_RTT_names == i)    ){
        
        # ---------------  which RTT reach name is in the overlap -----
        reach_row_x = unlist(strsplit(Okanogan_Crosswalk$ReachName_New[i], ","))
        first_row_x = TRUE
        # ------ which row has duplicate Okanogan reaches -----------
        row_with_duplicate_i = which(input_data_frame$ReachName == Okanogan_Crosswalk$ReachName_Old[i])
        
        for(reach_x_i in reach_row_x){
          
          # ------ if first instance of reach ---------
          if(first_row_x){
            # -------------- which reach to replace --------------
            x_replace = which(input_column$ReachName == Okanogan_Crosswalk$ReachName_Old[i])
            # ----------- replace it ------------
            input_column[x_replace,1] = reach_x_i
            
            # ------ set as not first row --------
            first_row_x = FALSE
            
            
          # ---------- if NOT first instance of reach ----------
          }else{
            
            # -------------- which reach to replace --------------
            reach_x_i2 = as.data.frame(reach_x_i)
            colnames(reach_x_i2) = colnames(input_column)
            # ----------- add column at the bottom ------------
            input_column = rbind(input_column, reach_x_i2 )
            # ----------------- add new row --------
            new_row_x = input_data_frame[row_with_duplicate_i, ]
            new_row_x$ReachName = reach_x_i
            input_data_frame = rbind(input_data_frame , new_row_x)
          }
        } # end of multiple reaches loop
        
      } # end of if multiple reaches if/else
    } # end of if new name
  }
  
  # ------------------ update column in data frame ------------
  input_data_frame[,input_column_name] = input_column
  
  # ------------ redo order --------------
  input_data_frame  = input_data_frame[order(input_data_frame$ReachName),]
  
  # -----------------------------------------------
  #   Update names with 
  # -----------------------------------------------
  
  
  # ----------------- write excel ------------
  write_xlsx(input_data_frame,output_path_x )
  
}

# -------------------------------------------------------------------
#    Read and Write Data - FIRST time updating
# -------------------------------------------------------------------

# --------------- Habitat_Data_Raw ---------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(habitat_raw_data, as.data.frame(habitat_raw_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Reach_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="") )

# --------------- Channel_Unit_Raw ---------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Channel_Unit_Raw, as.data.frame(Channel_Unit_Raw[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Channel_Unit_Raw.xlsx', sep="")  )

# ----------------------- Confinement_Scores ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Confinement_Scores, 
                                             as.data.frame(Confinement_Scores[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Confinement_Scores.xlsx', sep="")  )



# ----------------------- Habitat_Attribute_Notes_and_Professional_Judgement ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Habitat_Attribute_Notes_and_Professional_Judgement, 
                                             as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="")  )


# ----------------------- Life_Stage_Priorities_AU_and_Reach_data ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Life_Stage_Priorities_AU_and_Reach_data, 
                                             as.data.frame(Life_Stage_Priorities_AU_and_Reach_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")  )

# ------------------- Reach_Information_data ------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Reach_Information_data, 
                                             as.data.frame(Reach_Information_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'ReachInfo.xlsx', sep="")  )


