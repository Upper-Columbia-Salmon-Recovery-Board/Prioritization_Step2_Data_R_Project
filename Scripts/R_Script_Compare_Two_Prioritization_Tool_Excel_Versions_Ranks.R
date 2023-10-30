# ---------------------------------------------------------------------------
#
#      R Script to Compare different Restoration/Protection Output
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

#
#  quantify how many reaches were added
#  quantify how many reach ranks were shifted
#  quantify how many reach ranks were added and shifted with floodplain data

library(openxlsx)
library(readxl)

path_comparison1_x = "Y:/UCRTT/Prioritization/Step 2/Habitat Evaluation/"
path_comparison2_x = "Output/"
#version_1 = read_excel( paste(path_comparison1_x,'MASTER_Step2_FINALDRFT_11052021_opens_to_Restoration.xlsx', sep=""), 
#                        sheet = 'Restoration_Results')

version_1 = read_excel( paste(path_comparison2_x,'Restoration_Prioritization_Output_for_WebMap_Table_Tier_1_2_edited.xlsx', sep=""), sheet='Sheet 1')

#version_2 = read_excel( paste(path_comparison2_x,'Restoration_Prioritization_Output_for_WebMap_Table_Tier1AU.xlsx', sep=""))

version_2 = read_excel( paste(path_comparison2_x,'Restoration_Prioritization_Output_for_WebMap_Table_Tier1_2_AU.xlsx', sep=""))




# ---------------------------------------------------------------------------
#  quantify how many reaches were added
# ---------------------------------------------------------------------------

# -------reaches in version 1 but NOT in version 2 ------------
diff_ver1 = setdiff(version_1$`Reach Name`, version_2$`Reach Name`)
print(diff_ver1)

# -------reaches in version 2 but NOT in version 1 ------------
diff_ver2 = setdiff(version_2$`Reach Name`, version_1$`Reach Name`)
print(diff_ver2)

# ------ get data frame of the additional version 2 reaches --------
df_diff_ver2 = c()
for(reach_x in diff_ver2){
  i = which(version_2$`Reach Name` == reach_x)
  df_diff_ver2 = rbind(df_diff_ver2, version_2[i,])
}

# ---------------------------------------------------------------------------
#  quantify how many reach ranks were shifted
# ---------------------------------------------------------------------------

# ----------- merge version 1 and version 2 -------------------
merge_version1_version2 = merge(version_1, version_2, by="Reach Name")

# --------------- calculate shifts in rank --------
merge_version1_version2$Rank_Shift = as.numeric(merge_version1_version2$`Reach Rank.y`) - as.numeric(merge_version1_version2$`Reach Rank.x`) 

# ------------- which ranks did not change -----------------
no_shift = merge_version1_version2[which(merge_version1_version2$Rank_Shift == 0), ]

# ------------- which ranks increased -----------------
increase_rank = merge_version1_version2[which(merge_version1_version2$Rank_Shift > 0), ]

# ------------- which ranks decreased -----------------
decrease_rank = merge_version1_version2[which(merge_version1_version2$Rank_Shift < 0), ]


# ---------------------------------------------------------------------------
#  quantify how many reach ranks were added and shifted with floodplain data
# ---------------------------------------------------------------------------






