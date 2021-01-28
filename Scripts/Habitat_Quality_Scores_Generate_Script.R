
# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Habitat Quality Scores for Restoration AND Protection
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

# NEXT STEP - use Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria to
#          generate HQ score

# ---------------------------------------------------------------------------
#
#     Add up all the HQ score criteria
#
# ---------------------------------------------------------------------------

#  HQ_Sum nine criteria: 
#         Temperature-Rearing (TAKE LOWEST, do PROF JUDG over-ride):    NoRWEST Temp,  303b Listing Temperature
#         Flow-Summer Base Flow (TAKE LOWEST, do PROF JUDG over-ride):  RAWatershed_Rating_Flow,  Flow_305bList,  PROSPER, 
#         Riparian:        Average( Disturbance_CATEGORY_1, Canopy_Cover_CATEGORY_1)
#         Coarse Substrate:      Dominant_Substrate_CATEGORY_1
#         Cover-Wood:       Pieces_per_mile_CATEGORY_1
#         Pool Quantity&Quality:    Pools_CATEGORY_1
#        Off-Channel-Floodplain:       Floodplain_Connectivity_CATEGORY_1
#        Off-Channel-Side-Channels:    Connectivity_CATEGORY_1
#        Stability:      Average(  Vertical_Channel_Stability_CATEGORY_1,   Bank_Stability_CATEGORY_1 )
           
          



# ---------------------------------------------------------------------------
#
#    Calculate Habitat Quality Score for Restoration
#
# ---------------------------------------------------------------------------


habitat_raw_data %>%
  filter(ReachName  == "White River Upper 01" ) %>%
  select(ReachName, PROSER)


# ---------------------------------------------------------------------------
# 
#       Calculate Habitat Quality Score for Restoration
#
# ---------------------------------------------------------------------------





# ---------------------------------------------------------------------------
#
#
#
# ---------------------------------------------------------------------------





# ---------------------------------------------------------------------------
#
#
#
# ---------------------------------------------------------------------------






