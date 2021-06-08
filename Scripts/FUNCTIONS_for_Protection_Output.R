


# ---------------------------------------------------------------------------
#
#      SCRIPT: Function to generate Protection Output
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
#   Function to prep Protection Output
#
# ---------------------------------------------------------------------------
test_x = TRUE
if(test_x){
  HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']]
  HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]
  HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']]
  LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']]
  LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']]
  LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']]
  
}


FUNCTION_Combine_Protection_Output = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout, LF_spring_chinook, LF_steelhead, LF_bull_trout, exclude_bull_trout){
  
  # -------------- identify reaches across all protection
  reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName, 
                          LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName))
  # -------- remove NA reach ---------
  xNA = which(is.na(reaches_unique))
  if(length(xNA)>0){
    reaches_unique = reaches_unique[-xNA]
  }
  
  # 	Life_Stage	Action

  # --------------------------------------------------------------
  #           Loop through each reach and prep for output
  # --------------------------------------------------------------
  Protection_Output = c()
  for(reach_x in reaches_unique){
    
    # ------------------------------------
    #      Get Basin and Assessment Unit
    # -----------------------------------
    basin_x = Reach_Information_data$Basin[which(Reach_Information_data$ReachName == reach_x)]
    assessment_unit_x = Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)]
    
    # ------------------------------------
    #     Pathway
    # -----------------------------------
    pathway_x = c()
    if( any(HQ_spring_chinook$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "HQ_spring_chinook", sep=",")
    }
    if( any(HQ_steelhead$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "HQ_steelhead", sep=",")
    }
    
    if( any(LF_spring_chinook$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "LF_spring_chinook", sep=",")
    }
    if( any(LF_steelhead$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "LF_steelhead", sep=",")
    }
    
    if(exclude_bull_trout == "no"){
      if( any(HQ_bull_trout$ReachName == reach_x) ){
        pathway_x = paste(pathway_x, "HQ_bull_trout", sep=",")
      }
      if( any(LF_bull_trout$ReachName == reach_x) ){
        pathway_x = paste(pathway_x, "LF_bull_trout", sep=",")
      }
      
    }

    # ---------------- remove leading comma -------
    pathway_x = substr(pathway_x,2,nchar(pathway_x))
    
    # ------------- Number of Pathways -------------------
    number_of_pathways_x =  length( unlist(strsplit(pathway_x, ",")) )
    
    # ------------------------------------
    #      Life Stage
    # -----------------------------------
    life_stage_x = c()
    if( any(LF_spring_chinook$ReachName == reach_x) ){
      life_stages_LF_x = LF_spring_chinook$life_stage[which(LF_spring_chinook$ReachName == reach_x)]
      # make into one element if more than one life stage 
      if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
      life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",") 
    }
    if( any(LF_steelhead$ReachName == reach_x) ){
      life_stages_LF_x = LF_steelhead$life_stage[which(LF_steelhead$ReachName == reach_x)]
      # make into one element if more than one life stage 
      if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
      life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",") 
    }
    if(exclude_bull_trout == "no"){
      if( any(LF_bull_trout$ReachName == reach_x) ){
        life_stages_LF_x = LF_bull_trout$life_stage[which(LF_bull_trout$ReachName == reach_x)]
        # make into one element if more than one life stage 
        if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
        life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",") 
      }
    }

    # ------------- Number of Life Stages -------------------
    if( !is.null(life_stage_x) ){
      life_stage_x = substr(life_stage_x,2,nchar(life_stage_x))   # remove leading comma
      life_stage_x = unique(unlist(strsplit(life_stage_x, ","))) # get unique life stages (don't double count)
      number_of_life_stage_x =  length( life_stage_x )  # number of life stages
      life_stage_x = paste(life_stage_x, collapse=",") # put all life stages into a single element

    }else{
      number_of_life_stage_x = "NA"
    }
    # ----- make NA if no life stages -----------
    if(is.null(life_stage_x)){life_stage_x = "NA"}

    
    
    # ------------------------------------
    #     Action
    # -----------------------------------
    action_x = "Habitat Protection"
    
    
    # ------------------------------------
    #    Combine into one row, then output
    # -----------------------------------
    output_x = t(as.data.frame( c(reach_x, assessment_unit_x, basin_x, pathway_x, number_of_pathways_x,
                                life_stage_x, number_of_life_stage_x, action_x )))
    rownames(output_x) = reach_x
    colnames(output_x) = c("ReachName","Assessment.Unit", "Basin","Pathway", "Number_of_Pathways",
                           "Life_Stages","Number_of_Life_Stages","Action")
    Protection_Output = rbind(Protection_Output, output_x)
  }
  
  # ------------ prep data to process ------
  Protection_Output = as.data.frame(Protection_Output)
  rownames(Protection_Output) = seq(1,nrow(Protection_Output))
  # ------------ make numeric output numeric ----------------
  Protection_Output$Number_of_Pathways = as.numeric(Protection_Output$Number_of_Pathways )
  Protection_Output$Number_of_Life_Stages = as.numeric(Protection_Output$Number_of_Life_Stages )
  # -------- make blank cells to NA -------
  x_na = which(is.na(Protection_Output$Number_of_Life_Stages))
  Protection_Output$Number_of_Life_Stages[x_na] = 0
  
  return(Protection_Output)
}




