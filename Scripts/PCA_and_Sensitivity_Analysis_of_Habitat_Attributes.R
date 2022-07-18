# ---------------------------------------------------------------------------
#
#       PCA and Sensitivity Analysis for UCSRB Prioritization Habitat data
#       Summer 2022
#      Author: Ryan Niemeyer, UCSRB
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#        read in packages
# ---------------------------------------------------------------------------
# for plotting
library(devtools)
library(RTools)
library(ggplot2)
# package: https://www.datacamp.com/tutorial/pca-analysis-r
install_github("vqv/ggbiplot")
library(ggbiplot)


# -------------- for correlation plot ---------
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# habitat_raw_data

# ---------------------------------------------------------------------------
#        Run the PCA with continuous
# ---------------------------------------------------------------------------

pca_attributes_columns = c("GravelCobble_UCSRB_pct", "Pieces_per_mile_INDICATOR_1", 
                           "Pools_per_mile_INDICATOR_2","UCSRB_ChannelStability",
                           "Channel_Confinementor_or_Entrenchment_Ratio_INDICATOR_9",
                           "UCSRB_CanopyCoverPct","NORWEST_Temperature","PROSPER")

pca_attributes_columns_names = c("Gravel_Cobble","Wood_pcs_mile",
                                 "Pools_permile","Channel_Stability",
                                 "Entrenchment_Ratio","Canopy_Cover","Stream_T_NorW","Streamflow_Prosp")
habitat_raw_pca_continuous = na.omit(habitat_raw_data[,pca_attributes_columns])
colnames(habitat_raw_pca_continuous) = pca_attributes_columns_names
habitat_pca_continuous <- prcomp(habitat_raw_pca_continuous, center = TRUE,scale. = TRUE)

summary(habitat_pca_continuous)
#str(habitat_pca_continuous)
# ggbiplot(mtcars.pca)
par(mar=c(5,0,3,0))
biplot(habitat_pca_continuous, scale = 0, cex=c(0.7,0.8),
       expand = 1.2, xlabs = rep("X",nrow(habitat_raw_pca_continuous)),
       xlim=c(-5,5), ylim=c(-5, 4))

pairs(habitat_raw_pca_continuous,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 

# ------------ plot ----------

plot(habitat_raw_pca_continuous$Pools_permile,habitat_raw_pca_continuous$Gravel_Cobble )
plot(habitat_raw_data$Pools_per_mile_INDICATOR_2,habitat_raw_data$GravelCobble_UCSRB_pct )
ggplot(habitat_raw_data, aes(x = Pools_per_mile_INDICATOR_2, y = GravelCobble_UCSRB_pct)) +
  geom_hex(bins=10, color="white") +
  scale_fill_viridis_c()

# ---------------------------------------------------------------------------
#        Run the PCA with REI metrics (from HQ)
# ---------------------------------------------------------------------------

pca_attributes_columns2 = c("Bank_Stability_CATEGORY_1", "Vertical_Channel_Stability_CATEGORY_1",
                           "Dominant_Substrate_CATEGORY_1", "Pieces_per_mile_CATEGORY_1",
                          "Floodplain_Connectivity_CATEGORY_1", "Connectivity_CATEGORY_1",
                          "Pools_CATEGORY_1", "Canopy_Cover_CATEGORY_1", "Disturbance_CATEGORY_1")
pca_attributes_columns_names2 = c("Bank_Stability","Vertical_Stability",
                                 "Substrate","Woot_Category",
                                 "Floodplain_Connectivity","Channel_Connectivity","Pools",
                                 "Canopy_Cover","Disturbance")
habitat_raw_pca_categorical = na.omit(habitat_raw_data[,pca_attributes_columns2])
habitat_raw_pca_categorical = as.data.frame(habitat_raw_pca_categorical)
habitat_raw_pca_categorical[habitat_raw_pca_categorical=="Adequate"] = 5
habitat_raw_pca_categorical[habitat_raw_pca_categorical=="At Risk"] = 3
habitat_raw_pca_categorical[habitat_raw_pca_categorical=="Unacceptable"] = 1
habitat_raw_pca_categorical[habitat_raw_pca_categorical=="NA"] = NA
habitat_raw_pca_categorical = na.omit(habitat_raw_pca_categorical)
habitat_raw_pca_categorical = as.matrix(as.data.frame(habitat_raw_pca_categorical))
habitat_raw_pca_categorical = as.data.frame(habitat_raw_pca_categorical)
colnames(habitat_raw_pca_categorical) = pca_attributes_columns_names2

habitat_raw_pca_categorical[ , pca_attributes_columns_names2] <- apply(habitat_raw_pca_categorical[ , pca_attributes_columns_names2], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

habitat_pca_categorical <- prcomp(habitat_raw_pca_categorical, center = TRUE,scale. = TRUE)


summary(habitat_pca_categorical)
#str(habitat_pca_continuous)
# ggbiplot(mtcars.pca)
par(mar=c(5,3,3,3))
biplot(habitat_pca_categorical, scale = 0, cex=c(0.7,0.8),
       expand = 1.2, xlabs = rep("X",nrow(habitat_raw_pca_categorical)),
       xlim=c(-3,2), ylim=c(-2.8, 2.2))

pairs(habitat_raw_pca_categorical,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 


par(mar=c(5,5,3,3))
ggplot(habitat_raw_pca_categorical, aes(x = Pools, y = Substrate)) +
  geom_hex(bins=12, color="white") +
  scale_fill_viridis_c()
# ---------------------------------------------------------------------------
#        Run the PCA with HQ scores (pulls from mutliple data sources)
# ---------------------------------------------------------------------------

pca_attributes_columns_cat2 = c("Stability_Mean", "CoarseSubstrate_score",
                                "Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                "FloodplainConnectivity_score",    "Off-Channel/Side-Channels_score",
                                "PoolQuantity&Quality_score", "Riparian_Mean", "Temperature-Rearing_score")
pca_attributes_columns_names_cat2 = c("Stability","Coarse_Substrate","Cover_Wood", "Summer_Flow",
                                      "Floodplain_Connectivity","Off_channel_side_channel",
                                      "Pools","Riparian","Temperature")
habitat_raw_pca_categorical2 = na.omit(Habitat_Quality_Scores[,pca_attributes_columns_cat2])
habitat_raw_pca_categorical2 = as.data.frame(habitat_raw_pca_categorical2)
colnames(habitat_raw_pca_categorical2) = pca_attributes_columns_names_cat2
habitat_pca_categorical2 <- prcomp(habitat_raw_pca_categorical2, center = TRUE,scale. = TRUE)


summary(habitat_pca_categorical2)
#str(habitat_pca_continuous)
# ggbiplot(mtcars.pca)
par(mar=c(5,3,3,3))
biplot(habitat_pca_categorical2, scale = 0, cex=c(0.7,0.8),
       expand = 1.2, xlabs = rep("X",nrow(habitat_raw_pca_categorical2)),
       xlim=c(-1,4), ylim=c(-3, 2.5))

pairs(habitat_raw_pca_categorical,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 



# ---------------------------------------------------------------------------
#        
# ---------------------------------------------------------------------------



# ---------------------------------------------------------------------------
#        
# ---------------------------------------------------------------------------



# ---------------------------------------------------------------------------
#        
# ---------------------------------------------------------------------------


