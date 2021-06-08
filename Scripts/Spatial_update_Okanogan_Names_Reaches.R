
# -------------------------------------------------------------------
#
#              Spatial Merging
#
# -------------------------------------------------------------------

library(sf)
library(sp)
library(raster)
library(rgdal)
library(leafsync)
library(RColorBrewer)
library(mapview)

# INFO about plotting: https://r-spatial.github.io/mapview/articles/

# ---------------------------------------------------------------------------
#  Read in Reach Data
# ---------------------------------------------------------------------------

# ---------------------- reaches data ------------
reaches_path = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/Data/Reaches/Reaches.shp"
reaches <- sf::st_read(reaches_path) # this shapefile does not show up properly
reaches <- sf::st_transform(reaches, 4326)


# ---------------------- Okanogan reaches data ------------
GIS_Okanogan_path = "P:/GIS/Reaches/US_EDT_Reaches_12_5_16_shared_by_CCT_on_March2021/US_EDT_Reaches_12_5_16.shp"
GIS_Okanogan <- sf::st_read(GIS_Okanogan_path) # this shapefile does not show up properly
GIS_Okanogan <- sf::st_transform(GIS_Okanogan, 4326)

reaches_Okanogan = GIS_Okanogan[which(GIS_Okanogan$type == "Reach"),]

color_palette_x = c("red", "blue")
color_palette_continuous_LARGE = brewer.pal(9, 'Set1')
# 
# 
mapview(reaches_Okanogan, lwd=4, zcol="length_km", legend = mapviewGetOption("legend"), na.color='grey',
        color= color_palette_continuous_LARGE, map.types = c("CartoDB.Positron","CartoDB.DarkMatter",  "Esri.WorldImagery", "OpenStreetMap"))


# -------------------------------------------------------------------
#    Crosswalk names
# -------------------------------------------------------------------

Okanogan_Reach_Crosswalk =  read_excel(  paste(Okanogan_EDT_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )


i = 0
for(reach_x in Okanogan_Reach_Crosswalk$ReachName_Okanogan){
  i = i + 1
  
  # ------------- if reach geometry is the same ----------
  if( Okanogan_Reach_Crosswalk$identical_reaches_RTT_Okanogan[i] == "yes" ){
    x = which(reaches$ReachName == Okanogan_Reach_Crosswalk$ReachName_Old[i])
    reaches$ReachName[x] = reach_x
  }
  
}

# -------------------------------------------------------------------
#    Merge Two reaches 
# -------------------------------------------------------------------

# ------------------- merge: Okanogan River Ninemile 01 and Okanogan River Mosquito 13 -> to Okanogan 16-46  ------------

RTT_1 = which(reaches$ReachName == "Okanogan River Mosquito 13")
RTT_2 = which(reaches$ReachName == "Okanogan River Ninemile 01")

RTT_1_line = reaches$geometry[RTT_1]
RTT_2_line = reaches$geometry[RTT_2]

RTT_1_RTT_2_new  = st_union(RTT_1_line, RTT_2_line)

reaches$geometry[RTT_1] = RTT_1_RTT_2_new
reaches$ReachName[RTT_1] = "Okanogan 16-46"
reaches = reaches[-RTT_2,]

# ------------------- merge: Stapaloop Creek 01 and Stapaloop Creek 02 -> to Stapaloop 16-1 -----------

RTT_1 = which(reaches$ReachName == "Stapaloop Creek 01")
RTT_2 = which(reaches$ReachName == "Stapaloop Creek 02")

RTT_1_line = reaches$geometry[RTT_1]
RTT_2_line = reaches$geometry[RTT_2]

RTT_1_RTT_2_new  = st_union(RTT_1_line, RTT_2_line)

reaches$geometry[RTT_1] = RTT_1_RTT_2_new
reaches$ReachName[RTT_1] = "Stapaloop 16-1"
reaches = reaches[-RTT_2,]

# ------------------- merge: Okanogan River Ninemile 01 to Okanogan River Mosquito 13 -> to Okanogan 16-46

RTT_1 = which(reaches$ReachName == "Swimptkin Creek 01")
RTT_2 = which(reaches$ReachName == "Swimptkin Creek 02")

RTT_1_line = reaches$geometry[RTT_1]
RTT_2_line = reaches$geometry[RTT_2]

RTT_1_RTT_2_new  = st_union(RTT_1_line, RTT_2_line)

reaches$geometry[RTT_1] = RTT_1_RTT_2_new
reaches$ReachName[RTT_1] = "Swimptkin 16-1"
reaches = reaches[-RTT_2,]

# -------------------------------------------------------------------
#   Expand one reach to two
# -------------------------------------------------------------------

# -------- expand: Johnson Creek 01 -> Johnson 16-1 and Johnson 16-2 -----------------------

# ----------------- prepare the two rows (Johnson 16-1, Johnson 16-2) ------------
RTT_1 = which(reaches$ReachName == "Johnson Creek 01")
reaches = rbind(reaches, reaches[RTT_1,])
reaches$ReachName[RTT_1] = "Johnson 16-1"
reaches$ReachName[nrow(reaches)] = "Johnson 16-2"

#------------ get line -----------
RTT_1_line = reaches$geometry[RTT_1]
# ----------- plot to compare --------
plot(RTT_1_line[[1]][[1]])
points(RTT_1_line[[1]][[1]][36:104,],col="dark green")
points(RTT_1_line[[1]][[1]][1:35,],col="red")
points(-119.509208, 48.500622, col="red", pch=1, cex=1)

# ----- convert this to sfc_MULTILINESTRING --------
# website: https://r-spatial.github.io/sf/reference/st.html

Johnson_16_1 = RTT_1_line[[1]][[1]][1:36,1:3]
Johnson_16_2 = RTT_1_line[[1]][[1]][36:104,1:3]

Johnson_16_1_new = st_multipoint(x = as.matrix(as.data.frame(Johnson_16_1))  )
Johnson_16_2_new = st_multipoint(x = as.matrix(as.data.frame(Johnson_16_2))  )

# --------------- need to get it as a multistring --
reaches$geometry[RTT_1] = Johnson_16_1_new
reaches$geometry[nrow(reaches)] = Johnson_16_2_new
# TRY TO merge

Johnson_16_1_new = st_multilinestring(x = list(as.matrix(as.data.frame(Johnson_16_1[,1:2])))  )
Johnson_16_2_new = st_multilinestring(x = list(as.matrix(as.data.frame(Johnson_16_2[,1:2])))  )


plot(RTT_1_line)
lines(Johnson_16_1_new, col="red", lwd=2)
lines(Johnson_16_2_new, col="purple", lwd=2)
points(-119.509208, 48.500622, col="red", pch=1, cex=1)


# -------------------------------------------------------------------
#   Adjust Reaches
# -------------------------------------------------------------------

Okanogan_16_15_1 = which(reaches$ReachName == "Okanogan 16-15")
Okanogan_16_16_2 = which(reaches$ReachName == "Okanogan 16-16")
okanogan_bridge = c( -119.578085, 48.363442)

# ----------------- prepare the two rows  ------------

#------------ get line -----------
Okanogan_16_15_1_line = reaches$geometry[Okanogan_16_15_1]
Okanogan_16_16_2_line = reaches$geometry[Okanogan_16_16_2]

Okanogan_16_15_1_line_df = Okanogan_16_15_1_line[[1]][[1]]
Okanogan_16_16_2_line_df = Okanogan_16_16_2_line[[1]][[1]]

# ----------- plot to compare --------
xlimx = c(-119.605, -119.540)
ylimx = c(48.348, 48.400)
xlimx = c(-119.590, -119.570)
ylimx = c(48.36, 48.37)
plot(Okanogan_16_15_1_line[[1]][[1]], xlim = xlimx, ylim=ylimx  )
points(Okanogan_16_16_2_line[[1]][[1]], col="purple")
points(Okanogan_16_16_2_line[[1]][[1]][16:63,], col="blue")
points(okanogan_bridge[1], okanogan_bridge[2], col="red", pch=16, cex=1)

# ---------------- update Okanogan 16-15 to be longer -----------
Okanogan_16_15_1_geometry_new = st_multilinestring(x = list(as.matrix(as.data.frame( rbind(Okanogan_16_15_1_line_df[,1:2],  Okanogan_16_16_2_line_df[1:16,1:2]  )  )))  )
Okanogan_16_16_2_geometry_new = st_multilinestring(x = list(as.matrix(as.data.frame( Okanogan_16_16_2_line_df[16:63,1:2])))  )

# ----------------- update reach geometry -------------
reaches$geometry[Okanogan_16_15_1] = Okanogan_16_15_1_geometry_new
reaches$geometry[Okanogan_16_16_2] = Okanogan_16_16_2_geometry_new


# -------------------------------------------------------------------
#   Reaches to remove
# -------------------------------------------------------------------


# ---------------------- reaches to remove ----------------

reaches_to_remove = c("Loup Loup 16-4.1 (falls)")


