

# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Scripts for each Reach
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
#      FUNCTION to generate projects for each reach 
#
# ---------------------------------------------------------------------------
# project_df = Reach_Assessment_Project_Data
# crosswalk_df = Crosswalk_Habitat_Attributes_and_Actions
# column_order = c("ReachName", "Reach Assessment", "ProjectName", "Reach_ID_in_assessment","Action_Type","Action_Category", "Habitat_Attribute", "Action_Description")
FUNCTION_add_habitat_attributes_to_Projects = function(crosswalk_df, project_df, column_order){
  
  # ---------------- create Habitat Attribute column with NAs ----------
  project_df$Habitat_Attribute = NA
  
  # --------------------- go through reach list and add habitat attribute ------
  unique_habitat_attributes = unique(crosswalk_df$`Habitat Attribute`)
  
  for(habitat_attribute_x in unique_habitat_attributes){

    # ------- generate action categories for this habitat attribute ------
    action_categories_x = crosswalk_df$`Action Category`[ which(crosswalk_df$`Habitat Attribute` == habitat_attribute_x) ]

    # ------------- identify projects with this action category ---------
    for(action_cat_x in action_categories_x){

      # ---------- identify action category ---------
      action_cat_x2 = Action_Category_Name_Crosswalk_Simple[action_cat_x]
      # ------------ identify rows with this action category -------
      rows_with_action = which(project_df$Action_Category == action_cat_x2)

      # ------------ add habitat attributes ----------
      project_df$Habitat_Attribute[rows_with_action] = 
        paste(project_df$Habitat_Attribute[rows_with_action],habitat_attribute_x,sep=",")
    }
    
  }
  
  
  # --------------------- remove "NA," at the beginning of habitat attributes  ----------------
  rows_with_habitat_attributes = which( !is.na(project_df$Habitat_Attribute ))
  project_df$Habitat_Attribute[rows_with_habitat_attributes] = substr(project_df$Habitat_Attribute[rows_with_habitat_attributes], 4,nchar(project_df$Habitat_Attribute[rows_with_habitat_attributes]))
  
  # --------------- remove any duplicate habitat attributes -----------
  project_df$Habitat_Attribute[rows_with_habitat_attributes] = paste(unique(unlist(strsplit(project_df$Habitat_Attribute[rows_with_habitat_attributes], ","))), collapse=",")
  
  # -------------------- re-order the columns --------------
  project_df = project_df[,column_order]
  
  return(project_df)
}





# ---------------------------------------------------------------------------
#
#      FUNCTION to Output Projects/Action Categories/Habitat Attributes
#
# ---------------------------------------------------------------------------

actions_df = Reach_Assessment_Project_Data_Habitat_Attributes
priority_reaches_df = Restoration_Prioritization_Output_for_WebMap
row_i = 60 # Methow River Fawn 08 - has projects
FUNCTION_output_actions_for_priority_reaches = function(actions_df, priority_reaches_df){
  
  
  reaches_combined_projects_list = c()
  # ---------------------- Go through each PRIORITY reach -----------------
  for(row_i in 1:nrow(priority_reaches_df)){
    
    # ---------------- reach -----------------
    reach_x = priority_reaches_df$`Reach Name`[row_i]
    # ---------------------- impaired habitat attributes ------
    habitat_attributes_x = priority_reaches_df$`Limiting Factor`[row_i]
    habitat_attributes_x = unique(unlist(strsplit(habitat_attributes_x, ",")))
    
    # ------------------------------------------
    #   if reach is present in projects
    # ------------------------------------------ 
    if( any( actions_df$ReachName == reach_x ) ){
      
      # ------------- data frame in Action/Projects table for this reach
      df_projects_habitat_attribute = actions_df[ which(actions_df$ReachName == reach_x), ]
      # ------------ reach assessment -----------
      reach_assessment_x = paste(unique(df_projects_habitat_attribute$`Reach Assessment`), collapse=",")
      
      # ---------- get unique habitat attributes in project list for this reach -----------
      projects_unique_habitat_attributes = df_projects_habitat_attribute$Habitat_Attribute
      projects_unique_habitat_attributes = unique(unlist(strsplit(paste(unlist(projects_unique_habitat_attributes),collapse=","),",")))
      projects_unique_habitat_attributes = projects_unique_habitat_attributes[ -which(projects_unique_habitat_attributes=="NA")  ]   # remove NA
      # ---------------- habitat attributes that overlap between projects and impaired attributes ---------------
      habitat_attributes_overlap = intersect(habitat_attributes_x, projects_unique_habitat_attributes)
      
      # ------------------------------------------
      #   if reach is present AND projects overlap with impaired habitat attributes
      # ------------------------------------------ 
      if( length(habitat_attributes_overlap) > 0 ){
        
        reach_present_in_projects = "yes"
        
        # -----------------------------------------
        #  Get unique rows that address habitat attributes
        # -----------------------------------------
        project_rows_with_habitat_attributes_x = c()
        for(habitat_attributre_overlap_x in habitat_attributes_overlap){
          # ------------ get unique rows with this impaired  habitat attribute -------------
          project_rows_with_habitat_attributes_x_i = grep("Stability", df_projects_habitat_attribute$Habitat_Attribute )
          project_rows_with_habitat_attributes_x = c(project_rows_with_habitat_attributes_x, project_rows_with_habitat_attributes_x_i)
        }
        project_rows_with_habitat_attributes_x = unique(project_rows_with_habitat_attributes_x)
        
        # -----------------------------------------
        #  Loop through unique rows
        # -----------------------------------------
        projects_combined_x = c()
        i = 0
        for(project_row_x in project_rows_with_habitat_attributes_x){
          i = i + 1
          proj_output_x = paste(df_projects_habitat_attribute[project_row_x, c("ProjectName","Action_Type","Action_Category","Action_Description")], collapse=", ")
          proj_output_x = paste(paste(as.character(i), ")", sep=""),proj_output_x)
          projects_combined_x = rbind(projects_combined_x, proj_output_x)
        }
        
        projects_combined_x = paste(projects_combined_x, collapse=",  ")
        
        df_output_x = t( as.data.frame( c(reach_x, reach_assessment_x, reach_present_in_projects, projects_combined_x ) ) )
        
        
      # ------------------------------------------
      #   if reach is present BUT NO projects overlap with impaired habitat attributes
      # ------------------------------------------ 
      }else{
        
        reach_present_in_projects = "yes"
        df_output_x = t(as.data.frame( c(reach_x, reach_assessment_x, reach_present_in_projects, "NA" ) ))
        
      }
      
      
      
      # ------------------------------------------
      #   if reach NOT present in action list
      # ------------------------------------------  
    }else{
      
      reach_present_in_projects = "no"
      reach_assessment_x = "NA"
      df_output_x = t(as.data.frame( c(reach_x, reach_assessment_x, reach_present_in_projects, "NA" ) ))
      
    }
    
    
    reaches_combined_projects_list = rbind(reaches_combined_projects_list, df_output_x)
    
  }
  
  colnames(reaches_combined_projects_list) = c("Reach Name","Reach Assessment", "Reach Present in Projects", "List of Projects addressing Impaired Habitat Attributes")
  rownames(reaches_combined_projects_list) = c(seq(1,nrow(reaches_combined_projects_list)))
  reaches_combined_projects_list = as.data.frame(reaches_combined_projects_list)
  
  return(reaches_combined_projects_list)
  
  
  
}
