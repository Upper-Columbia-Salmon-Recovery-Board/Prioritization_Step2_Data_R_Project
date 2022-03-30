

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




