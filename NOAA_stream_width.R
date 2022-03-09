


# ---------------------------------------------------------------------------
#
#     Read in Data NOAA 24K stream width data
#
# ---------------------------------------------------------------------------





library(openxlsx)
#library(writexl)
library(readxl)


path_NOAA_x = "P:/GIS/Geomorphology/NOAA_24K_side_channel_predictions_v2.csv"

NOAA_stream_data = read.csv(path_NOAA_x)
