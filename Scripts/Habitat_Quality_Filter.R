

# ---------------------------------------------------------------------------
#
#      SCRIPT:  Habitat Quality FILTER
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
#
#
# ---------------------------------------------------------------------------


basins_to_include = c("Methow",  "Entiat","Wenatchee")

Generate_Limiting_Factor_Output_Table = function(species, basins, output_path){
  
  
  
  # ------------------------------------------------------------------------------
  #       Establish species name variable
  # ------------------------------------------------------------------------------
  
  if(species == "Spring Chinook"){
    # ---------------- species reach ---------------
    species_reach = 'Spring.Chinook.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name = 'SPCHNTier_Restoration'
    
    
    
    # ------- establish species variable name -----
    variable_name = 'variable_SPCH'
    # -------------- species data frame --------
    Species_data_frame = Spring_Chinook_data_frame
    # ------------- habitat attribute data frame -----------
    Species_Life_Stage_Habitat_Attributes = Spring_Chinook_Life_Stage_Habitat_Attributes
    # ----------- Use Spring Chinook Reaches-Life Stage data --------
    reach_life_stage_presence = reach_life_stage_presence_Spring_Chinook
    # ---------------- Output correct column names -------------
    output_column_AU_column = 'AU Tier Spring CHN'
  }else if("Steelhead"){
    # ---------------- species reach ---------------
    species_reach = 'Steelhead.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name = 'STLTier_Restoration'
    
    
    
    
    # ------- establish species variable name -----
    variable_name = 'variable_SH'
    # -------------- species data frame --------
    Species_data_frame = Steelhead_data_frame
    # ------------- habitat attribute data frame -----------
    Species_Life_Stage_Habitat_Attributes = Steelhead_Life_Stage_Habitat_Attributes
    # ----------- Use Steelhead Reaches-Life Stage data --------
    reach_life_stage_presence = reach_life_stage_presence_Steelhead
    # ---------------- Output correct column names -------------
    output_column_AU_column = 'AU Tier Steelhead' 
  }else{
    print('Incorrectly entered species name - re-type species name')
    
  }
  
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data
  # ---------- AU Ranks data frame for this species ---------
  Species_AU_Ranks_data = AU_Ranks_data
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach]
  # ----------------------- filter out for only reaches with this species --------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Basins desired for this analysis
  #  ---------------------------------------------------------------------------------
  
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Basin  %in% basins_to_include)
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data$`Assessment Unit`)
  
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality score
  #  ---------------------------------------------------------------------------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Habitat_Quality_score)
  
  
  
  
  
  
  
  
  
  
  
  
  
  Spring_Chinook_data_frame = Reach_Information_data[habitat_attribute_scores[species_reach].isin(spring_chinook_reaches_true)] 
  print('initial number of reaches/rows: ' + str(habitat_attribute_scores.shape[0]) )
  print('number of reaches after including only Spring Chinook reaches: ' + str(Spring_Chinook_data_frame.shape[0]) )
  
  
  
  # --------------------------------- life stage index variable --------------------------------      
  life_stage_val = int(index_vals[life_stages_all.life_stages.isin([life_stage])])
  life_stage_val_list = [life_stage_val]
  
  # ------------------------------------------------------------------------------
  #       AU Life Stage Priority for this life stage
  # ------------------------------------------------------------------------------
  
  AU_Life_Stage_Priority_Criteria = AU_life_stage_priorities.loc[AU_life_stage_priorities[Life_Stage_Priority_vars[variable_name][life_stage_val]].isin(Life_Stage_Priority)]
  AU_Life_Stage_Priority_Criteria_AUs = AU_Life_Stage_Priority_Criteria.AU
  Species_data_frame_Life_Stage = Species_data_frame.loc[Species_data_frame['Assessment Unit'].isin(AU_Life_Stage_Priority_Criteria_AUs)] 
  print('number of reaches/row after AU Life Stage Priority criteria: ' + str(Species_data_frame_Life_Stage.shape[0]) )
  
  # ------------------------------------------------------------------------------
  #       Pull reaches with this Life Stage presence
  # ------------------------------------------------------------------------------
  
  Reach_Life_Stage_Presence_Criteria = reach_life_stage_presence.loc[reach_life_stage_presence[Reach_Life_Stage_Presence_vars[variable_name][life_stage_val]].isin(Reach_Life_Stage_Presence)]
  Reach_Life_Stage_Presence_Criteria_ReachName = Reach_Life_Stage_Presence_Criteria.ReachName
  Species_data_frame_Life_Stage = Species_data_frame_Life_Stage.loc[Species_data_frame_Life_Stage['ReachName'].isin(Reach_Life_Stage_Presence_Criteria_ReachName)] 
  print('number of reaches/row after Life Stage Presence criteria: ' + str(Species_data_frame_Life_Stage.shape[0]) )
  
  # ------------------------------------------------------------------------------
  #       Identify the Habitat Attributes for the Specific Life Stage
  # ------------------------------------------------------------------------------
  
  # ------------------ Habitat Attributes for this Life Stage --------------------------------
  Life_Stage_Habitat_Criteria = Species_Life_Stage_Habitat_Attributes.loc[Species_Life_Stage_Habitat_Attributes['Life Stage'].isin(Life_Stage_Habitat_Attributes_vars['variable'][life_stage_val_list])]   
  
  # ----------------- list of Habitat Attributes for this specific Life Stage ---------------
  habitat_attributes_list = Life_Stage_Habitat_Criteria['Habitat Attribute'].values.tolist()
  
  # --------------- Reaches and Habitat Attributes for this Life Stage ------------------
  Life_Stage_Habitat_Criteria_2 = Species_data_frame_Life_Stage.loc[Species_data_frame_Life_Stage['Habitat Attribute'].isin(habitat_attributes_list)]
  
  # ------------------------------------------------------------------------------
  #      Prepare all the Variables for the Output Table
  # ------------------------------------------------------------------------------
  
  # Example Output Variables (for Spring Chinook - Holding and Maturation):  subbasin,  AU, AU Tier SprChn, 
  #           Spch_Holding_priority (high priority),  ReachName,  SPCH_Holding,  Species, Life_Stage, Habitat_Attribute,  
  #          DRAFT Habitat Attribute Score
  
  # Update Assessment Unit column name to be identical to Life Stage data frame
  AU_Life_Stage_Priority_Criteria = AU_Life_Stage_Priority_Criteria.rename(columns = {'AU':'Assessment Unit'})
  
  # Append Life Stage Habitat Criteria and AU life Stage Priority data frame to include AU Life Stage Priority
  Life_Stage_Habitat_Criteria_combined = pd.merge(Life_Stage_Habitat_Criteria_2, AU_Life_Stage_Priority_Criteria, how='left',  on = ['Assessment Unit'])
  
  # Append Life Stage Habitat Criteria and AU life Stage Priority data frame to include AU Life Stage Priority
  Life_Stage_Habitat_Criteria_combined = pd.merge(Life_Stage_Habitat_Criteria_combined, Reach_Life_Stage_Presence_Criteria, how='left',  on = ['ReachName'])
  
  # generate column of species name
  rows_x = Life_Stage_Habitat_Criteria_combined.shape[0]  # get number of rows in Life_Stage_Habitat_Criteria_combined
  col_x = pd.DataFrame({'Species': [species] * rows_x})  # create columns
  Life_Stage_Habitat_Criteria_combined['Species'] = col_x          # add column
  
  # add Life Stage name
  col_x = pd.DataFrame({'Species': [Life_Stage_Priority_vars['variable'][life_stage_val]] * rows_x})  # create columns
  Life_Stage_Habitat_Criteria_combined['Life_Stage'] = col_x          # add column
  
  
  # ------------------------------------------------------------------------------
  #      Create Output Table
  # ------------------------------------------------------------------------------
  
  # create output that matches the output table in Access
  Output_data_frame = Life_Stage_Habitat_Criteria_combined[['Species', 'Subbasin_x', 'Assessment Unit','ReachName', 
                                                            'Life_Stage' , 'Habitat Attribute', 'DRAFT Habitat Attribute Score (Currently MIN Score OR NA)', output_column_AU_column,  
                                                            Life_Stage_Priority_vars[variable_name][life_stage_val],   Reach_Life_Stage_Presence_vars[variable_name][life_stage_val] ]]
  
  Output_data_frame = Output_data_frame.rename(columns={'Subbasin_x':'Subbasin', 
    'DRAFT Habitat Attribute Score (Currently MIN Score OR NA)' : 'Cumulative Habitat Attribute Score'})
  
  # re-order based on ReachName
  Output_data_frame_X = Output_data_frame.sort_values(by=['ReachName'])
  # Output_data_frame_X.iloc[0:78]    # just can use to output data
  
  # ------------------------------------------------------------------------------
  #      Add individual habitat attribute score for the individual habitat attribute
  # ------------------------------------------------------------------------------
  if Output_data_frame_X.shape[0] > 0: # ONLY do this if there is data present
    individual_habitat_attributes = pd.DataFrame([])
  for index, row in Output_data_frame.iterrows():
    habitat_x = row['Habitat Attribute']
  reach_x2 = row['ReachName']
  reach_row_x = habitat_quality_scores.loc[habitat_quality_scores['ReachName'].isin([reach_x2])]
  columns_x = reach_row_x.columns
  
  
  
  if any(columns_x == habitat_x):
    reach_habitat_x = reach_row_x[habitat_x]
  
  if any(reach_habitat_x.isnull()):
    reach_habitat_x = "NA"
  elif is_number(reach_habitat_x):
    #print('----------------------reach_habitat_x')
    #print(reach_habitat_x)
    reach_habitat_x = int(reach_row_x[habitat_x])
  else:
    reach_habitat_x = "NA"
  individual_habitat_attributes = individual_habitat_attributes.append([reach_habitat_x], ignore_index=True)
  #print("---------------HABITAT X")
  #print(habitat_x)
  else:
    individual_habitat_attributes = individual_habitat_attributes.append(['NA'], ignore_index=True)
  
  #print('-------------Output_data_frame shape')
  #print(Output_data_frame.shape)
  #print('-------------individual_habitat_attributes TYPE')
  #print(type(individual_habitat_attributes))
  print('---------------individual_habitat_attributes shape ')
  print(individual_habitat_attributes.shape)
  print('---------------individual_habitat_attributes COLUMNS ')
  print(individual_habitat_attributes.columns)
  
  Output_data_frame_X['Individual Habitat Scores'] = individual_habitat_attributes
  # ------------------------------------------------------------------------------
  #       Generate Columns for Output
  # ------------------------------------------------------------------------------
  
  #print('completed output for Species: ' + species + "for the Life Stage: " + life_stage)
  #print(' ----------------------------------------------------------------------------------')
  #print("      ")
  
  Output_data_frame_X.to_excel(output_path, index = False)
  return(Output_data_frame_X)
  
  