
# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate AU-Level Habitat Quality Score based on reach-level
#                    Habitat Quality Scores
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------



# ---------------------------------------------------------------------------
#       Prep Data Frame for AU level scores
# ---------------------------------------------------------------------------

# --------------- generate data frame with basic info -----------
AU_level_scores_df = Reach_Information_data[,c("ReachName","Basin","Assessment.Unit","Length..miles.","Length_AvgWettedWidth_Meters")]
# --------------- add reach-level HQ Scores -----------
AU_level_scores_df = merge(AU_level_scores_df,Habitat_Quality_Scores[,c("ReachName","HQ_Pct")], by="ReachName", all.x= TRUE)

# ---------------------------------------------------------------------------
#        Calculate the fraction of each reach in the total AU-level stream length
# ---------------------------------------------------------------------------

AU_unique_x = unique(AU_level_scores_df$Assessment.Unit)

AU_level_HQ_scores = c()

for(AU_x in AU_unique_x){
  
  # ----------------------- Pull data just from that AU ---------
  AU_level_scores_df_AU_x = AU_level_scores_df[which(AU_level_scores_df$Assessment.Unit == AU_x),]
  
  # ------------- only pull reaches with HQ score -----------------
  HQ_pct_true_x = which(!is.na(AU_level_scores_df_AU_x$HQ_Pct))   # pull reaches with an HQ score
  AU_level_scores_df_AU_x_HQ_score = AU_level_scores_df_AU_x[HQ_pct_true_x,]
  
  # ---------------- sum reach length for all reaches that have an HQ score --------------
  tot_reach_length_miles_x = sum(AU_level_scores_df_AU_x_HQ_score$Length..miles., na.rm=T)
  # ------------------ calculate percent of total reach length ----------
  AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO = AU_level_scores_df_AU_x_HQ_score$Length..miles./tot_reach_length_miles_x
  # ----------------- calculate reach-length adjusted average --------
  AU_level_scores_df_AU_x_HQ_score$HQ_Pct_reach_adj = AU_level_scores_df_AU_x_HQ_score$HQ_Pct * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  
  # ------------- calculate HQ Scores -----
  HQ_Pct_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$HQ_Pct_reach_adj)
  HQ_Pct_mean = mean(AU_level_scores_df_AU_x_HQ_score$HQ_Pct)
  
  # ---------- if HQ_Pct is "NA" - no HQ Pct available ----------
  if(is.na(HQ_Pct_mean)){
    HQ_Pct_reach_adj_mean = HQ_Pct_mean
    HQ_Pct_DIFF_x = NA
  }else{
    HQ_Pct_DIFF_x = HQ_Pct_mean - HQ_Pct_reach_adj_mean
  }
  
  # ----------------- append to AU level data -----
  basin_x = AU_level_scores_df$Basin[which(AU_level_scores_df$Assessment.Unit == AU_x)][1]
  output_x = t(as.data.frame( c(AU_x,basin_x,HQ_Pct_mean,HQ_Pct_reach_adj_mean, HQ_Pct_DIFF_x)  ))
  
  AU_level_HQ_scores = rbind(AU_level_HQ_scores,output_x)
  
}
AU_level_HQ_scores = as.data.frame(AU_level_HQ_scores)
colnames(AU_level_HQ_scores) = c("AU","Basin", "HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF")
rownames(AU_level_HQ_scores) = seq(1,nrow(AU_level_HQ_scores))
AU_level_HQ_scores[c("HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF")] = sapply(AU_level_HQ_scores[c("HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF")],as.numeric)   # set columns as numeric

plot_true = TRUE
if(plot_true){
  hist(AU_level_HQ_scores$HQ_Pct_DIFF, breaks=20)
  abline(v=0, col="red")
}



# ---------------------------------------------------------------------------
#       Compare to Step 1 scores: rank order
# ---------------------------------------------------------------------------

# --------------- get rank of HQ_Geomean average across species scores ------
Step1_Scores_All$HQ_Rank_Score = rank(Step1_Scores_All$HQ_Geomean_mean_all_species)
AU_level_HQ_scores$HQ_Pct_reach_Length_Adj_RANK = rank(AU_level_HQ_scores$HQ_Pct_reach_length_adj_Mean)

# ------------------- add Step 1 HQ Geomean (all species mean) and rank --------------
Step1_Scores_All_output = Step1_Scores_All[,c("AU","HQ_Geomean_mean_all_species", "HQ_Rank_Score")]
colnames(Step1_Scores_All_output) = c("AU","Step1_HQ_Geomean_mean_all_species","Step1_HQ_Rank_Score")
AU_level_HQ_scores_combo = merge(AU_level_HQ_scores, Step1_Scores_All_output, by="AU", all.x=TRUE)

# ------------------ just pull the AUs with scores for both (all Okanogan and formerly Swaram Creek AU in Methow) -------
AU_level_HQ_scores_combo2 = AU_level_HQ_scores_combo[which( !is.na(AU_level_HQ_scores_combo$Step1_HQ_Geomean_mean_all_species) ),]
# --------------- remove AUs where Step 2 scores are NA -----------
AU_level_HQ_scores_combo2 = AU_level_HQ_scores_combo2[ which( !is.na(AU_level_HQ_scores_combo2$HQ_Pct_reach_length_adj_Mean)),]

# ------------- re-rank the two sets of scores (since the rank won't be continuous) ------
AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK = rank(AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK )
AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score = rank(AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score)

# ------------------- calculate difference between the Step 1 and Step 2 HQ score -------
AU_level_HQ_scores_combo2$HQ_Rank_Score_Step2_Step1_DIFF = AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK - AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score

if(plot_true){
  # ---------- histogram ------
  hist(AU_level_HQ_scores_combo2$HQ_Rank_Score_Step2_Step1_DIFF, breaks=20)
  abline(v=0, col="red", lwd=2)
  
  # ------------ plot ------------
  plot(AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score, AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK)
  
  # ------------ plot ------------
  plot(AU_level_HQ_scores_combo2$HQ_Pct_reach_length_adj_Mean, AU_level_HQ_scores_combo2$Step1_HQ_Geomean_mean_all_species)
  
}


