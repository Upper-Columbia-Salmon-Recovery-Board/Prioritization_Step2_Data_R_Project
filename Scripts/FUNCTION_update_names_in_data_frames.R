

# -------------------------------------------------------------------
#    Function to update reach names 
# -------------------------------------------------------------------


Okanogan_Crosswalk =  read_excel(  paste(data_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )

# ------------- to test it out -------------
#updated_column_old = Okanogan_Crosswalk$ReachName_Old
#update_column_new = Okanogan_Crosswalk$ReachName_New
input_data_frame = habitat_raw_data
input_column = as.data.frame(habitat_raw_data[,"ReachName"])
#input_column = input_column[,1]
input_column_name = "ReachName"
new_and_old_data_frame = Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")]
output_path_x = paste(data_path,'Habitat_Data_Raw.xlsx', sep="")

# -------------- the Function -----------------
FUNCTION_read_in_and_update_file_reach_names = function(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x){
  
  # --------------- update names in column -----------------
  for(i in 1:nrow(new_and_old_data_frame)){
    x_replace = which(input_column[,1] == as.character(new_and_old_data_frame[i,1]) )
    input_column[x_replace,1] = new_and_old_data_frame[i,2]
  }
  
  # ------------------ update column in data frame ------------
  input_data_frame[,input_column_name] = input_column
  
  # ----------------- write excel ------------
  write_xlsx(input_data_frame,output_path_x )
  
}

# -------------------------------------------------------------------
#    Read and Write Data
# -------------------------------------------------------------------

FUNCTION_read_in_and_update_file_reach_names(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x)

# --------------- Channel_Unit_Raw ---------
FUNCTION_read_in_and_update_file_reach_names(Channel_Unit_Raw, as.data.frame(Channel_Unit_Raw[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(data_path,'Channel_Unit_Raw.xlsx', sep="")  )

# ----------------------- Confinement_Scores ----------------------
FUNCTION_read_in_and_update_file_reach_names(Confinement_Scores, 
                                             as.data.frame(Confinement_Scores[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(data_path,'Confinement_Scores.xlsx', sep="")  )



# ----------------------- Habitat_Attribute_Notes_and_Professional_Judgement ----------------------
FUNCTION_read_in_and_update_file_reach_names(Habitat_Attribute_Notes_and_Professional_Judgement, 
                                             as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="")  )


# ----------------------- Life_Stage_Priorities_AU_and_Reach_data ----------------------
FUNCTION_read_in_and_update_file_reach_names(Life_Stage_Priorities_AU_and_Reach_data, 
                                             as.data.frame(Life_Stage_Priorities_AU_and_Reach_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")  )

# ------------------- Reach_Information_data ------------------
FUNCTION_read_in_and_update_file_reach_names(Reach_Information_data, 
                                             as.data.frame(Reach_Information_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(data_path,'ReachInfo.xlsx', sep="")  )








