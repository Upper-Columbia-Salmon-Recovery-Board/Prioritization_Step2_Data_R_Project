

# ---------------------------------------------------------------------------
# Count number of reaches for specific habitat attributes
# ---------------------------------------------------------------------------

output_path_x =  paste(output_path,'Restoration_Prioritization_Output_for_WebMap_Table.xlsx', sep="")
Restoration_Prioritization_Output_for_WebMap = read_excel(output_path_x)

Habitat_Attribute_Scores_columns_to_pull  = c("Bank Stability","Channel Stability",  "Coarse Substrate",
                                              "Cover- Wood", "Flow- Summer Base Flow",
                                              "Floodplain Connectivity", "Off-Channel/Side-Channels","Pool Quantity & Quality", # <- REI Values
                                              "Riparian- Canopy Cover"  ,    "Riparian-Disturbance" , 
                                              "Temperature- Rearing")

datx = c()
for(habitat_attribute_x in Habitat_Attribute_Scores_columns_to_pull){
   x = length( which( grepl( habitat_attribute_x, Restoration_Prioritization_Output_for_WebMap$`Unacceptable Limiting Factors`, fixed = TRUE) == TRUE) )
   datx = c(datx, x)
}
count_impaired_attributes = cbind(Habitat_Attribute_Scores_columns_to_pull, datx)
count_impaired_attributes = as.data.frame(count_impaired_attributes)
colnames(count_impaired_attributes) = c("habitat_attributes","count_in_prioritization")
count_impaired_attributes$count_in_prioritization = as.numeric(as.character(count_impaired_attributes$count_in_prioritization))


# ---------------------------------------------------------------------------
#  Plot Habitat Attribute Scores
# ---------------------------------------------------------------------------


# --------------------------- Pools --------------------------------------

habitat_attribute_x = "Pool Quantity & Quality"

Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x), ]
Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores_to_plot[which(Habitat_Attribute_Scores_to_plot$Basin != "Okanogan"),]

par(mfrow=c(1,2))
plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      xlab="REI pool metrics", ylab="pool habitat percent - rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1 ), col="red")

plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      xlab="pool frequency - rating", ylab="pool habitat percent - rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3 ), col="red")


# --------------------------- Wood --------------------------------------

habitat_attribute_x = "Cover- Wood"

Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x), ]
Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores_to_plot[which(Habitat_Attribute_Scores_to_plot$Basin != "Okanogan"),]

par(mfrow=c(1,2))
plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      xlab="REI wood metrics", ylab="pieces per mile - rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1 ), col="red")

plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      xlab="CHAMP LWFreq_Bf - rating", ylab="pieces per mile - rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3 ), col="red")


# --------------------------- Flow- Summer Base Flow --------------------------------------

habitat_attribute_x = "Flow- Summer Base Flow"

Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x), ]
Habitat_Attribute_Scores_to_plot = Habitat_Attribute_Scores_to_plot[which(Habitat_Attribute_Scores_to_plot$Basin != "Okanogan"),]

par(mfrow=c(1,2))
plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      xlab="Flow_305B", ylab="Reach Assess Watershed Flow Rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore2 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1 ), col="red")

plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3), jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore5), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      ylab="Atlas flow - rating", xlab="PROSPER rating")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore5 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore3 ), col="red")


plot( jitter(Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1), jitter(Habitat_Attribute_Scores_to_plot), col=rgb(0.5,0.5,0.5,0.2), pch=16, cex=2,
      ylab="Atlas flow - rating", xlab="RA Watershed")
abline(lm( Habitat_Attribute_Scores_to_plot$HabitatAttributeScore5 ~ Habitat_Attribute_Scores_to_plot$HabitatAttributeScore1 ), col="red")


# -----------------------------------------------------------------------------------------------
#         compare old and new Reach Ranks data
# -----------------------------------------------------------------------------------------------

# ------------ read in 2023 reach ranks -------
Restoration_Reaches_2023 = read_excel( "C:/Users/ryan/Downloads/MASTER_Step2_FINALDRFT_Feb2023_opens_to_Restoration.xlsx" , sheet="Restoration_Results")
Restoration_Reaches_2023$ReachName = Restoration_Reaches_2023$`Reach Name`

# ----------------- compare and plot old/new Okanogan data ----------
Restoration_Reaches_2023_OK = Restoration_Reaches_2023[which(Restoration_Reaches_2023$Basin == "Okanogan"),]
Reach_Rankings_Output_Restoration_OK = Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$Basin == "Okanogan"),]


colnames(Reach_Rankings_Output_Restoration_OK)[1] = "ReachName"
Reach_Ranks_Merged = merge(Restoration_Reaches_2023_OK, Reach_Rankings_Output_Restoration_OK, by="ReachName")
Reach_Ranks_Merged$`Reach Rank.y` = as.numeric(as.character(Reach_Ranks_Merged$`Reach Rank.y`))
plot( jitter(Reach_Ranks_Merged$`Reach Rank.x`), jitter(Reach_Ranks_Merged$`Reach Rank.y`), xlab="2023 Reach Rank (EDT HQ)", ylab="2024 Reach Rank (Prioritization HQ)",
      col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5),
      pch = 16, cex = 2)

model = lm(Reach_Ranks_Merged$`Reach Rank.y` ~  Reach_Ranks_Merged$`Reach Rank.x`)
newx <- seq(min(Reach_Ranks_Merged$`Reach Rank.x`), max(Reach_Ranks_Merged$`Reach Rank.x`), length.out=100)
preds <- predict(model, newdata = data.frame(x=newx), interval = 'confidence')

#add fitted regression line
abline(model)

#add dashed lines for confidence bands
lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')

#text(Reach_Ranks_Merged$`Reach Rank.x`+0.03, Reach_Ranks_Merged$`Reach Rank.y`-0.01 + runif(n=nrow(Reach_Ranks_Merged), min=-0.015, max=0.015), labels=Reach_Ranks_Merged$ReachName, cex=0.6)

# Reach_Rankings_Output_Restoration_OK_New = Reach_Rankings_Output_Restoration_OK

plot( jitter(Reach_Rankings_Output_Restoration$EDT_reach_rank ), jitter(Reach_Rankings_Output_Restoration$AU_level_Reach_Rank), xlab="EDT Reach Rank", ylab="Prioritizatoin Reach Rank",
      col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5),
      pch = 16, cex = 2)


View(Restoration_Scores_Output[which(Restoration_Scores_Output$Basin == "Okanogan" & Restoration_Scores_Output$AU_level_Reach_Rank>0),])
View(Restoration_Scores_Output[which(Restoration_Scores_Output$Basin == "Okanogan" ),])




