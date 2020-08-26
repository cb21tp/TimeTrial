# TimeTrial: An Interactive Application for Optimizing the Design and Analysis of Transcriptomic Time-Series Data in Circadian Biology Research
***
#### Elan Ness-Cohn, Marta Iwanaszko, William Kath, Ravi Allada, and Rosemary Braun
***
Welcome to TimeTrial!

TimeTrial is an interactive application for the design and optimization of omic 
time-series experiments to aid researchers in developing their experiment design. 
Consisting of two interactive applications using both synthetic and biological 
data, TimeTrial allows researchers to explore the effects of experimental design
on signal shape, examine cycling detection reproducibility across biological 
datasets, and optimize experimental design for cycle detection. 
TimeTrial is currently in beta.

Try TimeTrial Online powered by Rstudio's shinyapps.io:

- [TimeTrial Synthetic Data App ](https://nesscoder.shinyapps.io/TimeTrial_Synthetic/)
- [TimeTrial Real Data App ](https://nesscoder.shinyapps.io/TimeTrial_Real/)

Watch our [introductory video](https://vimeo.com/388290542) for TimeTrial training material.

[![TimeTrial](https://res.cloudinary.com/marcomontalbano/image/upload/v1580425724/video_to_markdown/images/vimeo--388290542-c05b58ac6eb4c4700831b2b3070cd403.jpg)](https://vimeo.com/388290542 "TimeTrial")

***
### Local Installation Instructions
1. Clone the TimeTrial repo to your Desktop (Mac)
 - `git clone https://github.com/nesscoder/TimeTrial.git ~/Desktop`
2. Open The TimeTrial_Synthetic.R or TimeTrial_Real.R using RStudio 
 - *files located in the TimeTrial_App folder*.
3. Click on the *Run App* button within Rstudio
 - *It may take a couple minutes for TimeTrial to install and load the necessary packages*
4. Begin designing your circadian time-series experiment!

***

The TimeTrial Git Repository includes the following:

##### Data
 - Raw/preprocessed synthetic and real datasets

##### Results
 - Results of processing all 240 synthetic and 13 real datasets with all 4 methods

##### Scripts
 - Scripts used to generate the synthetic data
 - Scripts used to generate each figure in the manuscript
 - Scripts used to process data by each method, includes masterfile of parameter choices

##### TimeTrial_Apps
 - Synthetic data version of TimeTrial
 - Real biological data version of TimeTrial
 - Datafiles and tutorial slides used by TimeTrial


***

R version 3.6.3 (2020-02-29)    

Packages:
- limma_3.42.2        
- rain_1.20.0         
- multtest_2.42.0     
- Biobase_2.46.0      
- BiocGenerics_0.32.0 gmp_0.5-13.6       
- BiocManager_1.30.10 
- gridGraphics_0.5-0  
- gridExtra_2.3       
- eulerr_6.1.0        
- UpSetR_1.4.0        
- vembedr_0.1.3      
- htmltools_0.4.0
- shinyjs_1.1         
- pROC_1.16.2         
- pastecs_1.3.21      
- shinythemes_1.1.2   
- matrixStats_0.56.0 
- gplots_3.0.3        
- forcats_0.5.0       
- stringr_1.4.0       
- dplyr_0.8.5         
- purrr_0.3.4         
- readr_1.3.1        
- tidyr_1.0.2         
- tibble_3.0.1        
- tidyverse_1.3.0     
- plyr_1.8.6          
- ggplot2_3.3.0       
- shiny_1.4.0.2  


***

TimeTrial is Currently Published in the Journal of Biological Rhythms:
[See Article](https://doi.org/10.1177/0748730420934672)

Ness-Cohn, E., Iwanaszko, M., Kath, W. L., Allada, R. & Braun, R. TimeTrial: An Interactive Application for Optimizing the Design and Analysis of Transcriptomic Time-Series Data in Circadian Biology Research. Journal of Biological Rhythms (2020). doi.org/10.1177/0748730420934672

TimeTrial 2020, Northwestern University
