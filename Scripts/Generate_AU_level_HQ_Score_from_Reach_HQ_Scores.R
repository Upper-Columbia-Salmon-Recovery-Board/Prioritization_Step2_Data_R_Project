
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
AU_level_scores_df = merge(AU_level_scores_df,Habitat_Quality_Scores[,c("ReachName","HQ_Pct","Stability_Mean","CoarseSubstrate_score","Cover-Wood_score","Flow-SummerBaseFlow_score",     
                                                  "FloodplainConnectivity_score"  ,"Off-Channel/Side-Channels_score","PoolQuantity&Quality_score","Riparian_Mean" , "Temperature-Rearing_score")], by="ReachName", all.x= TRUE)

# ---------------------------------------------------------------------------
#        Calculate the fraction of each reach in the total AU-level stream length
# ---------------------------------------------------------------------------

AU_unique_x = unique(AU_level_scores_df$Assessment.Unit)

AU_level_HQ_scores = c()

for(AU_x in AU_unique_x){
  
  # ----------------------- Pull data just from that AU ---------
  AU_level_scores_df_AU_x = AU_level_scores_df[which(AU_level_scores_df$Assessment.Unit == AU_x),]
  
  if(AU_level_scores_df_AU_x$Basin[1] == "Okanogan"){next}
  
  # ------------- only pull reaches with HQ score -----------------
  HQ_pct_true_x = which(!is.na(AU_level_scores_df_AU_x$HQ_Pct))   # pull reaches with an HQ score
  AU_level_scores_df_AU_x_HQ_score = AU_level_scores_df_AU_x[HQ_pct_true_x,]
  percent_reaches_data_present_x = nrow(AU_level_scores_df_AU_x_HQ_score)/nrow(AU_level_scores_df_AU_x)
  
  # ----------------------------------------------------------
  #    Calculate AU-level HQ score
  # ---------------------------------------------------------- 
  
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
  
  # ----------------------------------------------------------
  #    Calculate AU-level values for 9 Habitat Attributes
  # ---------------------------------------------------------- 
  
  # ----------------- calculate reach-length adjusted average --------
  AU_level_scores_df_AU_x_HQ_score$Stability_Mean_reach_adj = AU_level_scores_df_AU_x_HQ_score$Stability_Mean * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Coarse_Substrate_reach_adj = AU_level_scores_df_AU_x_HQ_score$CoarseSubstrate_score * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Cover_Wood_reach_adj       = AU_level_scores_df_AU_x_HQ_score$`Cover-Wood_score` * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Summer_Baseflow_reach_adj = AU_level_scores_df_AU_x_HQ_score$`Flow-SummerBaseFlow_score` * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Off_Channel_Floodplain_reach_adj = AU_level_scores_df_AU_x_HQ_score$FloodplainConnectivity_score * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Off_Channel_Side_Channel_reach_adj = AU_level_scores_df_AU_x_HQ_score$`Off-Channel/Side-Channels_score` * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Pools_reach_adj = AU_level_scores_df_AU_x_HQ_score$`PoolQuantity&Quality_score` * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Riparian_Mean_reach_adj = AU_level_scores_df_AU_x_HQ_score$Riparian_Mean * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  AU_level_scores_df_AU_x_HQ_score$Temperature_reach_adj = AU_level_scores_df_AU_x_HQ_score$`Temperature-Rearing_score` * AU_level_scores_df_AU_x_HQ_score$reach_level_RATIO
  
  # ----------------- calculate reach-length adjusted average --------
  Stability_Mean_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Stability_Mean_reach_adj)
  Coarse_Substrate_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Coarse_Substrate_reach_adj)
  Cover_Wood_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Cover_Wood_reach_adj)
  Summer_Baseflow_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Summer_Baseflow_reach_adj)
  Off_Channel_Floodplain_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Off_Channel_Floodplain_reach_adj)
  Off_Channel_Side_Channel_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Off_Channel_Side_Channel_reach_adj)
  Pools_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Pools_reach_adj)
  Riparian_Mean_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Riparian_Mean_reach_adj)
  Temperature_reach_adj_mean = sum(AU_level_scores_df_AU_x_HQ_score$Temperature_reach_adj)
  
  # ----------------- append to AU level data -----
  basin_x = AU_level_scores_df$Basin[which(AU_level_scores_df$Assessment.Unit == AU_x)][1]
  output_x = t(as.data.frame( c(AU_x,basin_x,percent_reaches_data_present_x,HQ_Pct_mean,HQ_Pct_reach_adj_mean, HQ_Pct_DIFF_x,
                                Stability_Mean_reach_adj_mean, Coarse_Substrate_reach_adj_mean, Cover_Wood_reach_adj_mean,
                                Summer_Baseflow_reach_adj_mean, Off_Channel_Floodplain_reach_adj_mean, Off_Channel_Side_Channel_reach_adj_mean,
                                Pools_reach_adj_mean, Riparian_Mean_reach_adj_mean, Temperature_reach_adj_mean)  ))
  
  AU_level_HQ_scores = rbind(AU_level_HQ_scores,output_x)
  
}
AU_level_HQ_scores = as.data.frame(AU_level_HQ_scores)
colnames(AU_level_HQ_scores) = c("AU","Basin","PRCNT_reaches_HQ_Present" , "HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF",
                                 "Stability_Mean_reach_adj_mean", "Coarse_Substrate_reach_adj_mean", "Cover_Wood_reach_adj_mean",
                                 "Summer_Baseflow_reach_adj_mean", "Off_Channel_Floodplain_reach_adj_mean", "Off_Channel_Side_Channel_reach_adj_mean",
                                 "Pools_reach_adj_mean", "Riparian_Mean_reach_adj_mean", "Temperature_reach_adj_mean")
rownames(AU_level_HQ_scores) = seq(1,nrow(AU_level_HQ_scores))
AU_level_HQ_scores[c("PRCNT_reaches_HQ_Present" ,"HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF", "Stability_Mean_reach_adj_mean", "Coarse_Substrate_reach_adj_mean", "Cover_Wood_reach_adj_mean",
                     "Summer_Baseflow_reach_adj_mean", "Off_Channel_Floodplain_reach_adj_mean", "Off_Channel_Side_Channel_reach_adj_mean",
                     "Pools_reach_adj_mean", "Riparian_Mean_reach_adj_mean", "Temperature_reach_adj_mean")] = sapply(AU_level_HQ_scores[c("PRCNT_reaches_HQ_Present" ,"HQ_Pct_Mean","HQ_Pct_reach_length_adj_Mean", "HQ_Pct_DIFF", "Stability_Mean_reach_adj_mean", "Coarse_Substrate_reach_adj_mean", "Cover_Wood_reach_adj_mean",
                                                                                                                 "Summer_Baseflow_reach_adj_mean", "Off_Channel_Floodplain_reach_adj_mean", "Off_Channel_Side_Channel_reach_adj_mean",
                                                                                                                 "Pools_reach_adj_mean", "Riparian_Mean_reach_adj_mean", "Temperature_reach_adj_mean")],as.numeric)   # set columns as numeric

plot_true = TRUE
if(plot_true){
  par(mfrow=c(1,1))
  hist(AU_level_HQ_scores$HQ_Pct_DIFF, breaks=20)
  abline(v=0, col="red")
}


# ---------------------------------------------------------------------------
#       Calculate AU-level scores for 9 HQ attributes
# ---------------------------------------------------------------------------

output_path_x =  paste(output_path,'AU_level_HQ_scores.xlsx', sep="")
write.xlsx(AU_level_HQ_scores,output_path_x )


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

  # --------------------- histograms of Step 1 scores---------------
  hist(Step1_Scores_All$HQ_Geomean_SprChn, breaks=10, main="Histogram of Step 1 AU Level Habitat Quality Geomean Scores", xlab="Habitat Quality Geomean")
  abline(v= median(Step1_Scores_All$HQ_Geomean_SprChn, na.rm=T), col="blue", lty=2, lwd=2.5)
  
  hist(Step1_Scores_All$HQ_Score_SprChn, breaks=6, main="Histogram of Step 1 AU Level Habitat Quality Final Scores", xlab="Habitat Quality Final Score")
  
  # ----------- histogram of Step 2 HQ Scores --------
  hist(AU_level_HQ_scores$HQ_Pct_DIFF[which(AU_level_HQ_scores$Basin!="Okanogan")], 
       xlab="Step 2 Habitat Quality Percent", main="Histogram: Step 2 Reach-level Habitat Quality Scores")
  abline(v= median(AU_level_HQ_scores$HQ_Pct_DIFF[which(AU_level_HQ_scores$Basin!="Okanogan")], na.rm=T), col="blue", lty=2, lwd=3.5)
  
  # ----------- histogram of Step 2 reach-level HQ scores --------
  hist(Habitat_Quality_Scores$HQ_Pct[which(Habitat_Quality_Scores$Basin!="Okanogan")], 
       xlab="Step 2 Habitat Quality Percent", main="Histogram: Step 2 Reach-level Habitat Quality Scores")
  abline(v= median(Habitat_Quality_Scores$HQ_Pct[which(Habitat_Quality_Scores$Basin!="Okanogan")], na.rm=T), col="blue", lty=2, lwd=3.5)
  
  # ---------- histogram of difference between two Step 2 HQ Score ------
  par(mfrow=c(1,1))
  hist(AU_level_HQ_scores_combo2$HQ_Pct_DIFF, breaks=20, main="AU-level HQ score: Difference between equal average \n and reach-length adjusted average",
       xlab=" Difference between equal average and reach-length adjusted average")
  abline(v=0, col="black", lwd=2)
  
  # ---------- histogram ------
  par(mfrow=c(1,1))
  hist(AU_level_HQ_scores_combo2$HQ_Rank_Score_Step2_Step1_DIFF, breaks=20, main="Step 2 AU HQ Score RANK - Step 1 AU HQ Score RANK",
       xlab=" Step 2 HQ RANK - Step 1 HQ RANK")
  abline(v=0, col="black", lwd=2)
  abline(v=median(AU_level_HQ_scores_combo2$HQ_Rank_Score_Step2_Step1_DIFF), col="blue", lty=2, lwd=3)
  
  # ------------ plot ------------
  plot(AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score, AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK,
       xlab="Step 1 HQ RANK", ylab="Step 2 HQ RANK", main="Step 2 HQ Rank vs Step 1 HQ Rank")
  plot(AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score, AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK,
       xlab="Step 1 HQ RANK", ylab="Step 2 HQ RANK", main="Step 2 HQ Rank vs Step 1 HQ Rank")
  abline(0,1, lty=2)
  text(AU_level_HQ_scores_combo2$Step1_HQ_Rank_Score,AU_level_HQ_scores_combo2$HQ_Pct_reach_Length_Adj_RANK, AU_level_HQ_scores_combo2$AU)
  mtext( "Step 2 > Step 1                                                                                      " , side=3, line=-1, font=2, col="blue")
  mtext( "                                                                                      Step 2 < Step 1" , side=1, line=-1, font=2, col="red")
  
  # ------------ plot ------------
  plot(AU_level_HQ_scores_combo2$Step1_HQ_Geomean_mean_all_species,AU_level_HQ_scores_combo2$HQ_Pct_reach_length_adj_Mean, 
       main="Step 1 and 2 AU Raw scores", xlab="Step 1 HQ Geomean", ylab="Step 2 AU-level reach-adjusted average HQ score")

  plot(AU_level_HQ_scores_combo2$Step1_HQ_Geomean_mean_all_species,AU_level_HQ_scores_combo2$HQ_Pct_reach_length_adj_Mean, 
       main="Step 1 and 2 AU Raw scores", xlab="Step 1 HQ Geomean", ylab="Step 2 AU-level reach-adjusted average HQ score")
  abline(0,1, lty=2)
  text(AU_level_HQ_scores_combo2$Step1_HQ_Geomean_mean_all_species,AU_level_HQ_scores_combo2$HQ_Pct_reach_length_adj_Mean, AU_level_HQ_scores_combo2$AU)
  
}



# ---------------------------------------------------------------------------
#      Correlate stream T and Riparian
# ---------------------------------------------------------------------------

colnames(Step1_Scores_All_total)[3] = "AU"

combine_Step1_Step2_Habitat_data = merge(AU_level_HQ_scores_combo2, Step1_Scores_All_total[,c(3:25)], by="AU")

combine_Step1_Step2_Habitat_data_50PRCNT_present = combine_Step1_Step2_Habitat_data[which(combine_Step1_Step2_Habitat_data$PRCNT_reaches_HQ_Present > 0.5),]

par(mfrow=c(1,2))
plot(jitter(combine_Step1_Step2_Habitat_data_50PRCNT_present$`Canopy Cover...10`), jitter(combine_Step1_Step2_Habitat_data_50PRCNT_present$Riparian_Mean_reach_adj_mean),
     xlab="Step 1 Riparian (NorWEST)", ylab="Step 2 Riparian (REI, misc)")
abline(lm( combine_Step1_Step2_Habitat_data_50PRCNT_present$Riparian_Mean_reach_adj_mean  ~ combine_Step1_Step2_Habitat_data_50PRCNT_present$`Canopy Cover...10`), col="red")

plot( jitter(combine_Step1_Step2_Habitat_data_50PRCNT_present$`Future Stream Temperature...18`),jitter(combine_Step1_Step2_Habitat_data_50PRCNT_present$Temperature_reach_adj_mean),
     xlab="Step 1 Summer Temp (NorWEST)", ylab="Step 2 Summer Temp (REI, NorWEST, misc)")
abline(lm( combine_Step1_Step2_Habitat_data_50PRCNT_present$Temperature_reach_adj_mean  ~ combine_Step1_Step2_Habitat_data_50PRCNT_present$`Future Stream Temperature...18`), col="red")


View(combine_Step1_Step2_Habitat_data_50PRCNT_present[,c(1,2,14,26)])

View(combine_Step1_Step2_Habitat_data_50PRCNT_present[,c(1,2,15,34)])

