
# ----------------------------------------------------------------------------------------
#
#      Compare Habitat data raw versions
#
# ----------------------------------------------------------------------------------------


habitat_raw_data_new = read_excel(  paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="")   )
habitat_raw_data_old = read_excel(  'C:/Users/Ryan/Downloads/Habitat_Data_Raw_Apr10.xlsx'   )

# ----------------- Run Function ---------
habitat_data_raw_diff_df = FUNCTION_output_dif_new_versus_old(habitat_raw_data_new, habitat_raw_data_old)
#  ------------ output path ------------
output_path_x = paste(output_path,'Compare_Apr10_and_Apr23_habitat_data_raw.xlsx', sep="")
write_xlsx(habitat_data_raw_diff_df, output_path_x)


# ----------------------------------------------------------------------------------------
#
#     Function to identify changes in reaches
#
# ----------------------------------------------------------------------------------------


FUNCTION_output_dif_new_versus_old = function(reach_name_habitat_raw_data_new, reach_name_habitat_raw_data_old){
  
  habitat_data_raw_diff = c()
  reach_name_habitat_raw_data_old = habitat_raw_data_old$ReachName
  reach_name_habitat_raw_data_new = habitat_raw_data_new$ReachName
  
  for(i in 1:nrow(habitat_raw_data_new) ){
    
    reach_x = habitat_raw_data_new$ReachName[i]
    
    i_old = which(habitat_raw_data_old$ReachName == reach_x)
    
    # ------------- IF there is no old reach with the same name ---------
    if( length(i_old) == 0){
      outputx = t(as.data.frame(c(reach_x,  "------ new reach has no matching old reach ---","NA","NA" )))
      colnames(outputx) = c("ReachName","Habitat_Attribute","data_old", "data_new")
      rownames(outputx) = paste(reach_x)
      habitat_data_raw_diff = rbind(habitat_data_raw_diff, outputx)
      
    }else{
      for(j in 1:ncol(habitat_raw_data_new)){
        newx = as.character(habitat_raw_data_new[i,j])
        oldx = as.character(habitat_raw_data_old[i_old,j])
        
        if(is.na(newx)){newx = "NA"}
        if(is.na(oldx)){oldx = "NA"}
        
        if( newx != oldx  ){
          
          
          if( oldx != "NA" & newx != "NA" ){
            
            if( (as.numeric(newx) - as.numeric(oldx)) > 0.1  ){
              #print(reach_x)
              #print(colnames(habitat_raw_data_new)[j])
              #print(j)
              #print(paste("OLD: ", habitat_raw_data_old[i_old,j] ))
              #print(paste("NEW: ", habitat_raw_data_new[i,j] ))
              #habitat_raw_data_diff[i,j] = habitat_raw_data_new[i,j]
              outputx = as.data.frame(c(reach_x,  colnames(habitat_raw_data_new)[j],  habitat_raw_data_old[i_old,j],  habitat_raw_data_new[i,j] ))
              colnames(outputx) = c("ReachName","Habitat_Attribute","data_old", "data_new")
              rownames(outputx) = paste(reach_x,j, sep="_")
              habitat_data_raw_diff = rbind(habitat_data_raw_diff, outputx)
              
              # --------- if difference is rounding error, don't copy ------------
            }else{
              
              # difference is a rounding error 
              
            }
          }else{
            
            #print(reach_x)
            #print(colnames(habitat_raw_data_new)[j])
            #print(j)
            #print(paste("OLD: ", habitat_raw_data_old[i_old,j] ))
            #print(paste("NEW: ", habitat_raw_data_new[i,j] ))
            #habitat_raw_data_diff[i,j] = habitat_raw_data_new[i,j]
            outputx = as.data.frame(c(reach_x,  colnames(habitat_raw_data_new)[j],  habitat_raw_data_old[i,j],  habitat_raw_data_new[i,j] ))
            colnames(outputx) = c("ReachName","Habitat_Attribute","data_old", "data_new")
            rownames(outputx) = paste(reach_x,j, sep="_")
            habitat_data_raw_diff = rbind(habitat_data_raw_diff, outputx)
          }
          
          
          
          
        }else{
          #habitat_raw_data_diff[i,j] = "-"
        }
        
      }
    }
    
    
  }
  
  return(habitat_data_raw_diff)
  
}
