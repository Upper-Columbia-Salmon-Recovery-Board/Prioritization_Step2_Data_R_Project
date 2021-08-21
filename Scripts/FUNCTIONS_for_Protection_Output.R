


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
  exclude_bull_trout = "no"
  
}


FUNCTION_Combine_Protection_Output = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout, LF_spring_chinook, LF_steelhead, LF_bull_trout, exclude_bull_trout){
  
  # -------------- identify reaches across all protection
  if(exclude_bull_trout == "no"){
    reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName, 
                               LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName))
  }else{
    reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName, 
                               LF_spring_chinook$ReachName))
  }

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
    pathway_spring_chinook_x = c()
    pathway_steelhead_x = c()
    pathway_bull_trout_x = c()
    species_x = c()
    if( any(HQ_spring_chinook$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "HQ_spring_chinook", sep=",")
      pathway_spring_chinook_x = paste(pathway_spring_chinook_x, "HQ_spring_chinook", sep=",") 
      species_x = paste(species_x, "Spring Chinook", sep=",")
    }
    if( any(HQ_steelhead$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "HQ_steelhead", sep=",")
      pathway_steelhead_x = paste(pathway_steelhead_x, "HQ_steelhead", sep=",")
      species_x = paste(species_x, "Steelhead", sep=",")
    }
    
    if( any(LF_spring_chinook$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "LF_spring_chinook", sep=",")
      pathway_spring_chinook_x = paste(pathway_spring_chinook_x, "LF_spring_chinook", sep=",")
      if( any(HQ_spring_chinook$ReachName == reach_x) == FALSE ){
        species_x = paste(species_x, "Spring Chinook", sep=",")} # to not duplicate species names
    }
    
    if( any(LF_steelhead$ReachName == reach_x) ){
      pathway_x = paste(pathway_x, "LF_steelhead", sep=",")
      pathway_steelhead_x = paste(pathway_steelhead_x, "LF_steelhead", sep=",")
      if( any(HQ_steelhead$ReachName == reach_x) == FALSE ){
        species_x = paste(species_x, "Steelhead", sep=",")
      } # to not duplicate species names
    }
    
    if(exclude_bull_trout == "no"){
      if( any(HQ_bull_trout$ReachName == reach_x) ){
        pathway_x = paste(pathway_x, "HQ_bull_trout", sep=",")
        pathway_bull_trout_x = paste(pathway_bull_trout_x, "HQ_bull_trout", sep=",")
        species_x = paste(species_x, "Bull Trout", sep=",")
      }
      if( any(LF_bull_trout$ReachName == reach_x) ){
        pathway_x = paste(pathway_x, "LF_bull_trout", sep=",")
        pathway_bull_trout_x = paste(pathway_bull_trout_x, "LF_bull_trout", sep=",")
        if( any(HQ_bull_trout$ReachName == reach_x) == FALSE ){ 
          species_x = paste(species_x, "Bull Trout", sep=",") } # to not duplicate species names
      }
      
    }

    # ---------------- remove leading comma -------
    pathway_x = substr(pathway_x,2,nchar(pathway_x))
    species_x = substr(species_x,2,nchar(species_x))
    
    # ------------- Number of Pathways -------------------
    number_of_pathways_x =  length( unlist(strsplit(pathway_x, ",")) )
    
    # ------------------------------------
    #      Life Stage
    # -----------------------------------
    life_stage_x = c()
    life_stage_spring_chinook = c()
    life_stage_steelhead = c()
    life_stage_bull_trout = c()
    if( any(LF_spring_chinook$ReachName == reach_x) ){
      life_stages_LF_x = LF_spring_chinook$life_stage[which(LF_spring_chinook$ReachName == reach_x)]
      # make into one element if more than one life stage 
      if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
      life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",")
      life_stage_spring_chinook = paste(life_stage_spring_chinook, life_stages_LF_x, sep=",")
    }
    if( any(LF_steelhead$ReachName == reach_x) ){
      life_stages_LF_x = LF_steelhead$life_stage[which(LF_steelhead$ReachName == reach_x)]
      # make into one element if more than one life stage 
      if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
      life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",") 
      life_stage_steelhead = paste(life_stage_steelhead, life_stages_LF_x, sep=",")
    }
    if(exclude_bull_trout == "no"){
      if( any(LF_bull_trout$ReachName == reach_x) ){
        life_stages_LF_x = LF_bull_trout$life_stage[which(LF_bull_trout$ReachName == reach_x)]
        # make into one element if more than one life stage 
        if(length(life_stages_LF_x) > 1){   life_stages_LF_x = paste(life_stages_LF_x, collapse=",")}
        life_stage_x = paste(life_stage_x, life_stages_LF_x, sep=",") 
        life_stage_bull_trout = paste(life_stage_bull_trout, life_stages_LF_x, sep=",")
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
    # ---------------- Prep species-specific life stages ------------
    if( !is.null(life_stage_spring_chinook) ){
      life_stage_spring_chinook = substr(life_stage_spring_chinook,2,nchar(life_stage_spring_chinook))   # remove leading comma
    }else{
      life_stage_spring_chinook = "NA"
    }
    if( !is.null(life_stage_steelhead) ){
      life_stage_steelhead = substr(life_stage_steelhead,2,nchar(life_stage_steelhead))   # remove leading comma
    }else{
      life_stage_steelhead = "NA"
    }
    if( !is.null(life_stage_bull_trout) ){
      life_stage_bull_trout = substr(life_stage_bull_trout,2,nchar(life_stage_bull_trout))   # remove leading comma
    }else{
      life_stage_bull_trout = "NA"
    }
    # ----- make NA if no life stages -----------
    if(is.null(life_stage_x)){life_stage_x = "NA"}

    # ------------- make NA if pathways not present in individual species
    if( !is.null(pathway_spring_chinook_x) ){
      pathway_spring_chinook_x = substr(pathway_spring_chinook_x,2,nchar(pathway_spring_chinook_x))
    }else{
      pathway_spring_chinook_x = "NA"
    }
    if( !is.null(pathway_steelhead_x) ){
      pathway_steelhead_x = substr(pathway_steelhead_x,2,nchar(pathway_steelhead_x))
    }else{
      pathway_steelhead_x = "NA"
    }
    if( !is.null(pathway_bull_trout_x) ){
      pathway_bull_trout_x = substr(pathway_bull_trout_x,2,nchar(pathway_bull_trout_x))
    }else{
      pathway_bull_trout_x = "NA"
    }
    
    # ------------------------------------
    #     Action Categories (based on Protection)
    # -----------------------------------
    protection_pcnt = Protected_Percentage_Data$ProtectedPercent[which(Protected_Percentage_Data$ReachName == reach_x)]
    
    action_x = Crosswalk_Protection_Action_Categories$`Action Category`[ which(Crosswalk_Protection_Action_Categories$Percent_Protected_Lower <= protection_pcnt &
            Crosswalk_Protection_Action_Categories$Percent_Protected_Upper > protection_pcnt ) ]

    
    # ------------------------------------
    #    Combine into one row, then output
    # -----------------------------------
    output_x = t(as.data.frame( c(reach_x, assessment_unit_x, basin_x, species_x, pathway_x,  number_of_pathways_x,  pathway_spring_chinook_x, pathway_steelhead_x, pathway_bull_trout_x ,
                                life_stage_x, number_of_life_stage_x, life_stage_spring_chinook, life_stage_steelhead, life_stage_bull_trout, action_x )))
    rownames(output_x) = reach_x
    colnames(output_x) = c("ReachName","Assessment.Unit", "Basin","Species", "Pathway", "Number_of_Pathways","Spring_Chinook_Pathways", "Steelhead_Pathways","Bull_Trout_Pathways",
                           "Life_Stages","Number_of_Life_Stages","Spring_Chinook_Life_Stages","Steelhead_Life_Stages","Bull_Trout_Life_Stages",  "Action")
    Protection_Output = rbind(Protection_Output, output_x)
  }
  
  # ------------ prep data to process ------
  Protection_Output = as.data.frame(Protection_Output)
  rownames(Protection_Output) = seq(1,nrow(Protection_Output))
  # ------------ make numeric output numeric ----------------
  Protection_Output$Number_of_Pathways = as.numeric(Protection_Output$Number_of_Pathways )
  Protection_Output$Number_of_Life_Stages[which(Protection_Output$Number_of_Life_Stages  == "NA")] = 0
  Protection_Output$Number_of_Life_Stages = as.numeric(as.character(Protection_Output$Number_of_Life_Stages ))
  # -------- make blank cells to NA -------
  x_na = which(is.na(Protection_Output$Number_of_Life_Stages))
  Protection_Output$Number_of_Life_Stages[x_na] = 0
  
  return(Protection_Output)
}




