# UCSRB Prioritization Step2 Reach Prioritization Data Tool R Code

<img src="https://storage.googleapis.com/ff-storage-p01/festivals/logos/000/051/750/large/logo.jpg?1575572027" width="200">

## Overview
Scripts and data tables within this repository are for Step 2 of the [UCSRB Habitat Prioritization process](https://www.ucsrb.org/prioritization/). I (Ryan) will endeavor to update the data available on this page, however the most up-to-date results for Step 2 can be found on the [Prioritization Products Page](https://www.ucsrb.org/prioritization-products/). For each script and output, the last update is listed on the right side of the screen (e.g. "2 months ago").

## Reach and Action Prioritization
The RTT decided to use three pathways to develop prioritized reaches and actions: Habitat Quality pathway, Limiting Factors pathway, and Fish Passage/Barrier pathway.  This R code tool deals primarily with the Habitat Quality and Limiting Factors pathway. The Barrier pathway is generated from the [barrier prioritizatoin process](https://www.ucsrb.org/fish-passage/), and added to [Barrier_Pathway_Data.xlsx](https://github.com/Upper-Columbia-Salmon-Recovery-Board/Prioritization_Step2_Data_R_Project/blob/main/Data/Barriers_Pathway_Data.xlsx), which is then used by the R code to add the prioritized reaches to the results from this tool. The filters and criteria for filters (as of October 14th, 2020) are overviewd in this [google slides presentation](https://docs.google.com/presentation/d/1dEJ-A_LlW1HkxfIjOfLmjRxS1DRR_nWGOsBlZj5khss/edit?usp=sharing). 

## Downloading and Running the R Tool
This R code tool is written with [R Studio](https://rstudio.com/products/rstudio/), which make the code easier to understand and track. For an overview how to install R Studio (which allows you to run this tool), [this page](https://www.r-bloggers.com/2020/08/tutorial-getting-started-with-r-and-rstudio/) overviews how to get R and R Studio installed. Once that is done, simply download the Github repository. To do this, on this page on the upper right, click on the green "Code" button, then click "Download ZIP".
<img src="https://github.com/Upper-Columbia-Salmon-Recovery-Board/Prioritization_Step2_Data_R_Project/blob/main/Data/Screenshot_GitHub_Page_to_download.png?raw=true" width="800">

Once you do this, unzip the folder and click on the **Step2_Prioritization_R_Project** which (if R and R Studio are installed) will open R Studio. To run the tool, click **Source** (circled in red below) and the code will run (it should take about one minute). The prioritized reaches and action categories will be written to the "Output" folder. The versions of those output here on Github can be found [here for Restoration](https://github.com/Upper-Columbia-Salmon-Recovery-Board/Prioritization_Step2_Data_R_Project/blob/main/Output/Reach_Actions_Restoration_Unacceptable_and_AtRisk.xlsx?raw=true) and [here for Protection](https://github.com/Upper-Columbia-Salmon-Recovery-Board/Prioritization_Step2_Data_R_Project/blob/main/Output/Reach_Actions_Protection.xlsx?raw=true).

<img src="https://github.com/Upper-Columbia-Salmon-Recovery-Board/Prioritization_Step2_Data_R_Project/blob/main/Data/Screenshot_R_Studio_page.png?raw=true" width="800">

If you have any issues with this - please feel free to send me an email with any questions.

## Final Note
Note - some of the data or scripts in this repository are draft versions. 
As of October 14th, 2020, this R tool generates priority action categories based on the priority reaches output from the Habitat Quality, Limiting Factor, and Barrier pathways output. 
If you have any questions, please feel to reach out to me at ryan.niemeyer@ucsrb.org.

## Prioritization Sponsors
**Of course - a huge thanks to our sponsors in this process!**

<img src="https://www.ucsrb.org/wp-content/uploads/2020/05/GSRO-300x98.jpg" width="300">
<img src="https://www.ucsrb.org/wp-content/uploads/2020/05/BEF-300x121.jpg" width="250">
<img src="https://www.ucsrb.org/wp-content/uploads/2020/05/BPA-300x194.jpg" width="250">
<img src="https://www.ucsrb.org/wp-content/uploads/2020/05/yakama_logo_color_PLATINUM-300x187.png" width="200">


## Upper Columbia Salmon Recovery Board region
<img src="https://github.com/Upper-Columbia-Salmon-Recovery-Board/R_Scripts_Environmental_Conditions/blob/master/RCO_Map_v3.jpg" width="600">

