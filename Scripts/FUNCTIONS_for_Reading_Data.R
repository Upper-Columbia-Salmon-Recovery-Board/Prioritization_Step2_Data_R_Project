





# ---------------------------------------------------------------------------
#
#      SCRIPT: FUNCTIONS for reading in Data
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------





FUNCTION_update_Confinement_Scores = function(Confinement_Scores, Geomorphic_Criteria){
  
  
  Confinement_Scores =  Confinement_Scores  %>%
    mutate(Score = ifelse(Unconfined_Pct  > Geomorphic_Criteria$Category_lower[1] & 
                            Unconfined_Pct  <= Geomorphic_Criteria$Category_upper[1] , Geomorphic_Criteria$Score[1],
                                         ifelse(Unconfined_Pct  > Geomorphic_Criteria$Category_lower[2] & 
                                                  Unconfined_Pct  <= Geomorphic_Criteria$Category_upper[2] , Geomorphic_Criteria$Score[2],
                                                ifelse(Unconfined_Pct  > Geomorphic_Criteria$Category_lower[3] & 
                                                         Unconfined_Pct  <= Geomorphic_Criteria$Category_upper[3] , Geomorphic_Criteria$Score[3],
                                                       NA))))
 
  
  
}

