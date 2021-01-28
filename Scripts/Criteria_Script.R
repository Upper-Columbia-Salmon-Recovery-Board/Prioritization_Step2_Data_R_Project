# ---------------------------------------------------------------------------
#
#      SCRIPT: Establish Critiera for Filters 
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------------------------------
#
#             Criteria: Habitat Quality Pathway
#
# ----------------------------------------------------------------------------------------------------------------------------

# -------- Assessment Unit (AU) (HUC12) Tier rank (data in habitat_attribute_scores) ----------------
AU_Rank = c(1)                       # potential: 1, 2, or 3; 1 is high priority, 3 is low priority
# --------------- Reach Confinement (data in confinement_scores) --------------------------------
Reach_Confinement =  c(5)           # potential: 1, 3, 5; 1 is higher % confined, 5 is higher % unconfined, based on USGS Valley Confinment Algorithm
# --------------- Sum Life Stage (sum of all the life stages present in reach) ---------------------
Sum_Life_Stage = c(4, 5, 6, 7)   # potential: 1,2,3,4,5,6,7
# ----------------- Habitat Quality Score (data in habitat_attribute_scores) -----------------
Habitat_Quality_score = c(5)      # potential: 1,2,3,4,5; based on "goldilocks method" where 5 is highest proirity, 1 is lowest
# --------------------- Habitat Attribute Scores-- "DRAFT Habitat Attribute..." data (data in Cumul_Habitat_Attribute_Score)
# Oct 2020: did [1] (Unacceptable only)   OR [1, 3] (Unacceptable and At Risk)
Individual_Habitat_Attribute_Score = c(1, 3)   # possible scores - [1,3,5] 1 - unacceptable, 5 - adequate


# ----------------------------------------------------------------------------------------------------------------------------
#
#             Criteria: Limiting Factor Pathway
#
# ----------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------
#        Variables that are for ANY life stage
# -------------------------------------------------------------------------------------------------------------

# -------- Assessment Unit (AU) (HUC12) Tier rank (data in habitat_attribute_scores) ----------------
AU_Rank = c(1)                        #potential: 1,2,3; 1 is high priority, 5 is low priority

# --------------------- Habitat Attribute Scores-- "DRAFT Habitat Attribute..." data (data in Cumul_Habitat_Attribute_Score)
Cumul_Habitat_Attribute_Score = c(1)    # potential score: [1, 3, 5]; 1 is unacceptable, 5 is adequate 

# -------------------------------------------------------------------------------------------------------------
#        Variables for SPECIFIC life stages
# -------------------------------------------------------------------------------------------------------------

# -------------------- Life Stage Priority (LSP) rank is "High Priority" (data in AU_Life_Stage_Priorities)  -------------------
Life_Stage_Priority = c("High Priority") # potential: ["Life Stage Not Supported", "Low Priority", "Medium Priority", "High Priority"] 

# -------------------- Life Stage is Present is 1 (data in reach_life_stages)  -------------------
Reach_Life_Stage_Presence = c(1)   # potential: 0 or 1 (1 is present, 0 is not present) 


# ----------------------------------------------------------------------------------------------------------------------------
#
#             Criteria: Priority Actions
#
# ----------------------------------------------------------------------------------------------------------------------------



# ----------------------------- Habitat Quality Criteria ---------------------------
# ---------- put the minimum allowable score -----------
# cut off value for actions to include for individual habitat attributes in Habitat Quality analysis
habitat_quality_priority_score = c(5)  #HQ script will pull HQ_score for habitat attributes with this OR higher HQ_score values

indiv_habitat_attribute_score = c(1) # for pulling actions JUST from 
# note - this is redundant with the habitat_quality_priority_score:    limiting_factor_priority_score = 1

# ----------------------------- Limiting Factor Criteria ---------------------------

# --------------- Reach Confinement (data in confinement_scores) --------------------------------
# 1, 3, 5, where 1 is higher % confined, 5 is higher % unconfined, based on USGS Valley Confinement Algorithm
# Habitat Quality analysis filters out based on confinement during initial filters

Reach_Confinement_Limiting_Factors = list('Floodplain Reconnection' = 5) 


