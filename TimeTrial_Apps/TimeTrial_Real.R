#========================================================#
# Author: Elan Ness-Cohn                                 #
# Email: elanness-cohn2017@u.northwestern.edu            #
# Title: TimeTrial_BiologicalData                        #
# Code Produced: May 2, 2019                             #
#========================================================#

#Load Required Packages and Install if missing
reqPkgs <- c('ggplot2', 'plyr','tidyverse', 'gplots','matrixStats', 'shiny', 'shinythemes',"pastecs", "pROC", "shinyjs", "UpSetR","eulerr", "grid", 'gridExtra',"gridGraphics","htmltools","vembedr")
for(pkg in reqPkgs){
  if(!require(pkg, character.only=TRUE)){
    install.packages(pkg)
  }
}
lapply(reqPkgs,library, character.only = TRUE)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BoicPkg <- c('rain','limma')
for(pkg in BoicPkg){
  if(!require(pkg, character.only=TRUE)){
    BiocManager::install(pkg)
  }
}
lapply(BoicPkg,library, character.only = TRUE)

#setworking directory and Load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("App_Data/timeTrialRealDataComplete.RData")
load("App_Data/timeTrialAdjRealDataComplete.RData")
load("App_Data/timeTrialProcessTimeSeriesComplete.RData")

#run app
ui <- tagList(
  
  useShinyjs(),
  
  ## -------------------------------------------------- Search Bar - Gene Panel  ------------------------------------------------------
  
  absolutePanel(id = "searchGenes", class = "panel panel-default", fixed = T,
                draggable = F, top = 10, left = 1100, right = 0, bottom = "auto",
                width = 300, height = "auto",style = "border: 0px; background-color: rgba(62,63,58,1) ; z-index: 19999",
                selectizeInput(inputId = "geneSel", label = NULL, choices = rownames(data[[1]]), selected = NULL, multiple = T, options = list(maxItems = 1,maxOptions = 5, placeholder = "Search For a Gene of Interest")),
                span(h4("Selected Gene: "),style="color:white"),  
                h4(strong(span(textOutput(outputId = "geneName_method"),style="color:white")))
                ),
  
  ## ----------------------------------------- Start of Shiny App Interface Title Slide------------------------------------------------------
  navbarPage(
    title = "TimeTrial: Interactive Application for the Design and Analysis of Transcriptomic Times-Series Data in Circadian Biology Research",
    
    position = "fixed-top",
    theme = shinytheme("sandstone"),
    
    ## ----------------------------------------- Landing Page Tab Layout ------------------------------------------------------
    tabPanel("Overview",
             column(width = 8,offset = 1,
             h1(strong("Biological Data"))),
             
             fluidRow(
             column(width = 8,offset = 2,
             h2(strong("TimeTrial: Interactive Application for the Design and Analysis of Transcriptomic Times-Series Data in Circadian Biology Research")),
             h3("Elan Ness-Cohn, Marta Iwanaszko, William Kath, Ravi Allada, and Rosemary Braun"),
             hr(),
             h4(strong("Introduction:")),
             p("TimeTrial is an interactive application for the design and optimization of omic time-series experiments to aid researchers in developing 
               their experiment design. Consisting of two inter- active applications using both synthetic and real data, TimeTrial allows researchers to 
               explore the effects of experimental design on signal shape, examine cycling detection reproducibility across biological datasets, and 
               optimize experimental design for cycle detection. TimeTrial is currently in beta."),
             #Replace src file with the Final TimeTrial Video
             p(strong("Watch our introductory video for TimeTrial training material."))),
             column(width = 8,offset = 2, align = 'center',
             # tags$video(id="video1", type = "video/mp4",src = "3DEmbedding.mp4", controls = "controls"),
             embed_vimeo("388290542") %>%
               div(class = "vembedr") %>%
               div(align = "center"),
             hr()),
             
             column(width = 8,offset = 2,
                    h4(strong("The Data Sets:"))),
             column(width = 8,offset = 2, align = "center",
                    img(src='Figure6.png', align = "center", width = "75%")),
             column(width = 8,offset = 2,
                    br(),
                    p("Three mouse liver time-series expression sets (Hogenesch 2009 - ",strong("Hogenesch_1,"),"Hughes 2012 - ",strong("Hughes_2,"),"Zhang 2014 - ",strong("Zhang_2"),")
                      where analyzed from the Gene Expression Omnibus database (GEO). In each experiment, Wildtype C57BL/6J mice where entrained to a 12 hour
                      light, 12 hour dark environment for a week before being released into constant darkness, a standard practice in the field for assessing
                      circadian function. The Hogenesch study sampled mice every 1 hour for 48 hours, while the Hughes and Zhang Studies sampled every 2 
                      hours for 48 hours. Other variations in the datasets include microarray platform, ZT time of sam- pling, and number of mice sampled 
                      per timepoint. Taking a cross study concordance approach, we made the assumption that if there are true biologically relevant circadian 
                      signals in the data, they will be present in all three datasets measuring the same condition. We thus assessed a method’s robustness by 
                      the ability to consistently score genes as cycling versus non-cycling across experimental datasets."),
                      p("Given a fixed number of samples based on experimental cost, an obvious question is what is the optimal sampling scheme that can be achieved
                      by varying sampling length, resolution, and replicates. High resolution sampling presents a more clear picture of the underlying signal. We thus
                      down sampled our three datasets to test the effects of sampling resolution on cycle detection. The Hughes 2012 and Zhang 2014 studies which were 
                      sampled every 2 hours for 48 hours, were downsampled to 2 datasets sampled every 4 hour ",strong("(Hughes_4A and Hughes_4B"),"; "
                      ,strong("Zhang_4A and Zhang_4B)."),"The Hogenesch 2009 dataset which was sampled every 1 hour for 48 hours, was downsampled to 2 
                      datasets sampled every 2 hours ",strong("(Hogenesch_2A and Hogenesch_2B)")," and also into 4 datasets sampled every 4 hours ",strong("(Hogenesch_4A, Hogenesch_4B,
                      Hogenesch_4C, and Hogenesch_4D).")),
                    hr()),
             
             column(width = 8,offset = 2,
                  h4(strong("Comparison  Summary Plots:"))),
             column(width = 8,offset = 2,align = 'center',
             img(src='Figure7.png', align = "center", width = "75%")),
             column(width = 8,offset = 2,
                   br(),
                   p("The comparison summary plots allows users to examine cycling detection reproducibility across biological datasets and downsamplings. Again taking a cross concordance approach,
                     one assumes that if a gene is truly a biologically relevant cycler it should be detected as cycling across datasets and downsamplings. To aid users in investigating detected 
                     cyclers across datasets, we highlight the know circadian genes (orange triangle), user selected gene of interested (green triangle), and set of largest interaction across 
                     datasets (blue colored set). By clicking on the histograms to the left or above the set intersection plots, users can directly investigate the expression of genes 
                     that are identified as cycling in each sets or intersection, respective (top panel). Once selected, users can browse the genes expression of detected cyclers within an 
                     intersection set, and switch between datasets to look those genes expression in across all 13 subsampled datasets regardless of detection status (Bottom panel). 
                     Users can further adjust p-value thresholds and log fold change thresholds to see how this impacts cross-concordance results of specific genes. Not p-values for cycle detection
                     have been FDR corrected."),
                   hr()),
             
             column(width = 8,offset = 2,
                    h4(strong("Methods Comparison:"))),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure8.png', align = "center", width = "85%")),
             column(width = 8,offset = 2,
                    br(),
                    p("In the methods comparison  tab, users can test two cycling detection methods head-to-head across datasets by selecting the desired comparison in the parameter selection section.
                       Comparison results are displayed in the form of a scatter plot of both methods -log(p-value) results. Larger -log(p-value)'s denote more cyclic genes, where smaller -log(p-value)'s 
                       denote less cyclic genes. The gene expression of the gene of interest for the sampling scheme being analyzed - in this case 1 hour sampling for clock - is highlighted in orange.
                       Click on a different point in the scatter plot or type a gene into the search bar to change the gene of interest. It is important to note that for comparison purposes results are 
                       reported as raw p-values, and have not been adjusted for multiple hypothesis testing."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure9.png', align = "center", width = "90%")),
             column(width = 8,offset = 2,
                    br(),
                    p("To compare method’s output the spearman rank correlation, (ρ), of the resultant p-values were computed across all sampling schemes between methods. 
                      This correlation compares the ranking of genes from one method to their corresponding ranking in a different method. 
                      Thus the spearman rank correlation, gives us a quantitative measure of how similar are the rankings of each genes 
                      p-values are across methods. The rank correlation value of the comparison being displayed in the above scatter plot is 
                      highlighted by the orange box in this figure."),
                    hr()),


             column(width = 8,offset = 2,
                    h4(strong("Sampling Scheme Comparison:"))),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure10.png', align = "center", width = "85%")),
             column(width = 8,offset = 2,
                    br(),
                    p("In the sampling scheme comparison  tab, users can test two sampling schemes head-to-head across methods by selecting the desired comparison in the parameter selection section.
                      Comparison results are displayed in the form of a scatter plot of both sampling scheme's -log(p-value) results. Larger -log(p-value)'s denote more cyclic genes, where smaller -log(p-value)'s 
                      denote less cyclic genes. The gene expression of the gene of interest for the sampling scheme being analyzed - in this case 1 versus 2 hour sampling for clock - is highlighted in orange.
                      Click on a different point in the scatter plot or type a gene into the search bar to change the gene of interest. It is important to note that for comparison purposes results are 
                      reported as raw p-values, and have not been adjusted for multiple hypothesis testing."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure11.png', align = "center", width = "75%")),
             column(width = 8,offset = 2,
                    br(),
                    p("To compare sampling schemes’ output the spearman rank correlation, (ρ), of the resultant p-values were computed across all methods between sampling schemes. 
                      This correlation compares the ranking of genes from one sampling scheme to their corresponding ranking in a different sampling scheme. 
                      Thus the spearman rank correlation, gives us a quantitative measure of how similar are the rankings of each genes 
                      p-values are across schemes. The rank correlation value of the comparison being displayed in the above scatter plot is highlighted by the orange box in this figure."),
                    hr()),
             
             
             column(width = 8,offset = 2,
                    h4(strong("Custom Sampling Scheme Comparison:"))),
             column(width = 8,offset = 2,
                    p("Finally, TimeTrial allows users to develop their own custom sampling scheme for cycle detection. While sampling for 48 hour every 1 hour would be 
                      “ideal” for cycling detection, it is also expensive. We thus allows user to see how scheduling fewer samples will affect the results and explore 
                      whether enhanced sampling at specific times of day can improve detection. For instance, in a temperature perturbation study, one might assume that 
                      the transcriptional changes of interesting occurs in the first several hours post stimulus. One might therefore want dense sampling at the 
                      beginning of the time-series to catch interesting changes in dynamics of oscillation with sparser sampling as the time-course progresses to detect 
                      cycling genes at equilibrium. In this case, a user can test how this alternative sampling scheme compares to the ideal every 1 hour for 48 hours sampling scheme."),
                    br()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure12.png', align = "center", width = "85%")),
             column(width = 8,offset = 2,
                    br(),
                    p("The first step in the analysis requires the user to select the timepoints to be used in the custom sampling scheme by clicking on the corresponding timepoints in the top plot.
                       Selected points are highlighted in orange, and a plot of the expression of the selected gene on interest (in this case clock) is displayed below."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure13.png', align = "center", width = "50%")),
             column(width = 8,offset = 2,
                    br(),
                    p("Having selected a sampling scheme, the next step in the analysis requires the user to select the detection parameters for both the JTK_Cycle and RAIN methods. 
                       The custom analysis is performed using only JTK_Cycle and RAIN, since ARSER and BooteJTK cannot handle uneven sampling. Selection of the period parameter in 
                       both methods, sets a period search range for cycling genes. Selection of the Peak Border sets the range of asymmetry in cycling waveforms RAIN will search for.
                       See the RAIN vignette for further examples and assistance in selecting an appropriate parameter range."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure14.png', align = "center", width = "100%")),
             column(width = 8,offset = 2,
                    br(),
                    p("Much like the methods comparison  tab, users can determine how the RAIN and JTK_Cycle compare head-to-head with the custom sampling scheme.
                      Comparison results are displayed in the form of a scatter plot of both method's -log(p-value) results. Larger -log(p-value)'s denote more cyclic genes, where smaller -log(p-value)'s 
                      denote less cyclic genes. The gene expression of the gene of interest for the sampling scheme being analyzed - in this case for clock - is highlighted in orange.
                      Click on a different point in the scatter plot or type a gene into the search bar to change the gene of interest. Users are further presented with the spearman rank correlation, (ρ), of the 
                      resultant p-values as computed across both methods using the custom sampling scheme. The spearman rank correlation, gives a quantitative measure of how similar 
                      are the rankings of each genes p-values are across method. This measure can thus be used to help determine if one method may be better for custom analysis or if they essentially behave similarly.
                      It is important to note that for comparison purposes results are reported as raw p-values, and have not been adjusted for multiple hypothesis testing."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure15.png', align = "center", width = "90%")),
             column(width = 8,offset = 2,
                    br(),
                    p("Given the assumption that the 1 hour sampling scheme is more representative of the true underlying signal, the venn diagram plots allows users to get a sense of how the 
                       custom sampling scheme compares to the ideal 1 hour sampling scheme. Ideally, the custom sampling scheme will maximize genes detected in both the 1 hour and custom 
                       sampling Scheme (blue area), while minimizing genes that missed by the custom sampling scheme (grey area) and genes that are uniquely detected in the custom sampling (orange area).
                       The genes that are missed in the custom sampling scheme (grey area) are presumed to be false negatives, while the genes that are uniquely detected in the custom sampling scheme 
                       (orange area) are presumed to be false positives that are artifacts of the down sampling. Users can further adjust p-value thresholds and log fold change thresholds to see how this impacts the results."),
                    hr()),
             column(width = 8,offset = 2,align = 'center',
                    img(src='Figure16.png', align = "center", width = "90%")),
             column(width = 8,offset = 2,
                    br(),
                    p("The venn diagram analysis are based on the 1 hour sampling acting as ground truth; however, as ground truth cannot be determined in biological data without further validation studies, we have implemented 
                       a means to inspect the actual waveforms for consistency in cycling detection.
                       By clicking on the corresponding sets in the plot above, user can flip through the waveforms to inspect that genes have truly been classified as cycling and non-cycing appropriately, as well as search
                       the output for genes of interest.")),
             
             
             
             column(width = 10, offset = 1,
                    hr(),
                    p("Please cite TimeTrial <INSERT CITATION>"),
                    p("© 2020 TimeTrial, Northwestern University"))
            
             )
             )
             ,
    
    ## ----------------------------------------- Comparison Summary Tab Layout ------------------------------------------------------
    
    tabPanel("Comparison Summary Plots",
             h1(),
             hr(),
             h1(),
             hr(),
             h1(),
             sidebarPanel(width = 2,
                          h3("Parameter Selection"),
                          p("Method outputs have been FDR Corrected"),
                          radioButtons(inputId = "alphaVal", label = "Alpha Signficance Threshold",choices = c(0.01,0.05,0.1), selected = 0.05),
                          radioButtons(inputId = "fcVal", label = "Log Fold Change Threshold",choiceNames = c("NA",1,2,2.5),choiceValues =  c(0,1,2,2.5), selected = 0)
             ),
             mainPanel(
               
               absolutePanel(id = "cyclingGenes", class = "panel panel-default", fixed = T,
                             draggable = F, top = 123, left = 0, right = 0, bottom = "auto",
                             width = "auto", height = "auto",style = "background-color: rgba(255,255,255,1) ; z-index: 9999",
                             
                             fluidRow(
                               column(1, offset = 0,
                                      disabled(actionButton(inputId = "cyclingGenesView",label = "Close", align = "center",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                               column(4, offset = 3,   align = "center",   
                                      h4(strong(textOutput(outputId = "selectedUpsetPlot"))))
                               ),
                             
                             div(id = 'toggleCyclingGenesView', class="collapse out",
                                 plotOutput(outputId = "mainPlot_CyclingGenes",width = "100%", height = 400),
                                 hr(),
                                 fluidRow(
                                   column(width = 5,offset=1,
                                          plotOutput(outputId = "dataSet_summary", width = "150%", height = 100, clickOpts(id = "upsetDatasetSel_click_summary"))
                                          ),
                                   column(width = 2,offset=3,
                                          h2(),
                                          actionButton(inputId = "previousPage",label = "Previous", align = "center",
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   ),
                                   column(width = 1,
                                          h2(),
                                          actionButton(inputId = "nextPage",label = "Next", align = "center",
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") 
                                   )
                                   
                                   
                                 ),
                                 hr()
                                 )
                             ),
               
               
               h3("Arser"),
               plotOutput(outputId = "mainPlot_Arser",width = 6000, height = 500, clickOpts(id = "upset_click_Arser")),
               hr(),
               h3("BooteJTK"),
               plotOutput(outputId = "mainPlot_BooteJTK",width = 6000, height = 500, clickOpts(id = "upset_click_BooteJTK")),
               hr(),
               h3("JTK_Cycle"),
               plotOutput(outputId = "mainPlot_JTK_Cycle",width = 6000, height = 500, clickOpts(id = "upset_click_JTK_Cycle")),
               hr(),
               h3("RAIN"),
               plotOutput(outputId = "mainPlot_RAIN",width = 6000, height = 500, clickOpts(id = "upset_click_Rain"))
             )
    ),
    
    ## ----------------------------------------- Methods Comparison Tab Layout------------------------------------------------------
    
    tabPanel("Methods Comparison",
             tags$style(type="text/css", "body {padding-top: 120px;}"),
             sidebarPanel(width = 2,
                          h3("Parameter Selection"),
                          p("Method outputs have NOT been FDR Corrected for Comparison Purposes"),
                          radioButtons(inputId = "dataSet_method", label = "Data Set Selection",choiceNames = c("Hogenesch","Hughes","Zhang"), choiceValues = c(1,2,3), selected = 1),
                          selectizeInput(inputId = "schemeSel_method", label = "Scheme Selection", choices = c("1" = 1,"2A" = 2,"2B" = 3,"4A" = 4,"4B" = 5,"4C" = 6,"4D" = 7), selected = list(1), multiple = T, options = list(maxItems = 1, placeholder = 'Select Schemes')),
                          selectizeInput(inputId = "methodSel_method", label = "Methods Comparison Selection", choices = c("Arser" = 1,"BooteJTK" = 2,"JTK_Cycle" = 3,"RAIN" = 4), selected = list(1,2), multiple = T, options = list(maxItems = 2, placeholder = 'Select Methods'))
             ),
             mainPanel(
               hr(),
               fluidRow(
                 column(width = 7,align="left",
                        plotOutput(outputId = "mainPlot_method",width = 550, height = 550, clickOpts(id = "scatter_click_method"))),
                 column(width = 5,align="left",
                        plotOutput(outputId = "subPlot_2_4A_4B_method",width = "200%", height = 450))
               ),
               h2("Rank Correlation of all Comparisons"),
               hr(),
               plotOutput(outputId = "heatMap_method",width = "120%", height = 650),
               hr()
             )
    ),
    
    ## ----------------------------------------- Sampling Scheme Comparison Tab Layout------------------------------------------------------
    
    tabPanel("Sampling Scheme Comparison",
             tags$style(type="text/css", "body {padding-top: 120px;}"),
             sidebarPanel(width = 2,
                          h3("Parameter Selection"),
                          p("Method outputs have NOT been FDR Corrected for Comparison Purposes"),
                          radioButtons(inputId = "dataSet_schemes", label = "Data Set Selection",choiceNames = c("Hogenesch","Hughes","Zhang"), choiceValues = c(1,2,3), selected = 1),
                          radioButtons(inputId = "methodSel_schemes", label = "Method Selection",choiceNames = c("Arser","BooteJTK","JTK_Cycle","RAIN"), choiceValues = c(1,2,3,4), selected = 1),
                          selectizeInput(inputId = "schemeSel_schemes", label = "Scheme Selection", choices = c("1" = 1,"2A" = 2,"2B" = 3,"4A" = 4,"4B" = 5,"4C" = 6,"4D" = 7), selected = list(1,2), multiple = T, options = list(maxItems = 2, placeholder = 'Select Schemes'))
                          ),
             mainPanel(
               # wellPanel(h4("GENE NAME: "),
               #           h4(strong(textOutput(outputId = "geneName_scheme")))),
               fluidRow(
                 column(width = 7,align="center",
               plotOutput(outputId = "mainPlot_schemes",width = 550, height = 550, clickOpts(id = "scatter_click_schemes"))),
               column(width = 5,align="center",
                      plotOutput(outputId = "subPlot_2_4A_4B_schemes",width = "200%", height = 450)
               )),
               
               hr(),
               h3("Rank Correlation of all Comparisons"),
               plotOutput(outputId = "mainPlot_HeatMap_scheme",width = 650, height = 650),
               hr()
             )
             
    ),
    
    ## ----------------------------------------- Sampling Scheme Comparison Tab Layout------------------------------------------------------
    
    
    tabPanel("Custom Sampling Scheme Comparison",
               tags$style(type="text/css", "body {padding-top: 120px;}"),
               
               h3("Select ZT Times For Processing"),
               plotOutput(outputId = "selectionPlot_hogSample", width = "100%", height = 200, clickOpts(id = "scatter_sel_hogSample"), brush = brushOpts(id = "selectionPlot_hogSample_brush",resetOnNew = T)),
               div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", 
                  plotOutput(outputId = "subPlot_hogSample",width = "100%", height = 450)),
               
             hr(),
             h2("Comparisons of Sampling Scheme: JTK_Cycle versus Rain", align = "center"),
             hr(),
             wellPanel(
                   hr(),
                   h3("Select Parameter For Processing"),
                   hr(),
                   h6("ZT times Selected"),
                   verbatimTextOutput("brush_info"),
                   verbatimTextOutput("click_info"),
                   hr(),
                   h3("RAIN"),
                   h6("Select Period Range for RAIN to Search for"),
                   sliderInput("Rain_period_range", "Period Range:",
                               min = 1, max = 48,
                               value = c(24,24), step = 1),
                   h6("Select Peak Border Range For RAIN to Search through: See RAIN Vinneate for Examples"),
                   sliderInput("peak_range", "Peak Border Range:",
                               min = 0.1, max = 0.9,
                               value = c(.2,.8), step = 0.1),
                   hr(),
                   h3("JTK_Cycle"),
                   h6("Select Period Range for JTK_Cycle to Search for"),
                   sliderInput("JTK_period_range", "Period Range:",
                               min = 1, max = 48,
                               value = c(24,24), step = 1),
                   hr(),
                   h6("Data Processing May Take Up to 15 Minutes"),
                   actionButton(inputId = "runJtkRain", label = 'Submit')
                   ),
                 
                   h2("Scatter Plot Comparison of Sampling Scheme: JTK_Cycle versus Rain", align = "center"),
                   p("Method outputs have NOT been FDR Corrected for Comparison Purposes", align = "center"),
                   hr(),
                   fluidRow(
                     column(width = 12,align="center",
                            plotOutput(outputId = "mainPlot_hogSample",width = 550, height = 550, clickOpts(id = "scatter_click_hogSample")))
            ),
             
            hr(),
            h2("Venn Diagram Comparison of Sampling Scheme: JTK_Cycle versus Rain", align = "center"),
            p("Method outputs have been FDR Corrected", align = "center"),
            hr(),
            column(width = 6, align="center",
                    plotOutput(outputId = "jtkVenn_hogSample",width = "100%", height = 450)),
            column(width = 6, align="center",
                      plotOutput(outputId = "RAINVenn_hogSample",width = "100%", height = 450)),
            column(width = 12, align="left",
            wellPanel(
                      h3("Select Thresholding Values"),
                      radioButtons(inputId = "alpha_hogSample", label = "alpha Threshold", choices = c(0.01,0.05,0.1), selected = 0.05, inline = T),
                      radioButtons(inputId = "FC_hogSample", label = "Fold Change Threshold", choices = c(NA,1,2), selected = NA, inline = T)
              )),
            column(width = 12, align="center",
                      plotOutput(outputId = "jtk_RAIN_Sel_Waveforms_hogSample",width = "100%", height = 250, clickOpts(id = "click_jtkRain_Sel_hogSample"))),
            column(width = 12, align="center",
                      plotOutput(outputId = "waveforms_hogSample",width = "100%", height = 400)),
            column(width = 12,
                   h4("")),
            column(width = 6, align="center",
                   actionButton(inputId = "previous_hogSample",label = "previous",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(width = 6, align="center",
                   actionButton(inputId = "next_hogSample",label = "next",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(width = 12,
                   h4("")),
            hr()
    ),
  
  ## -----------------------------------------  PLOT LOADING SIGN ------------------------------------------------------
    
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   absolutePanel(left = 10,bottom = 0,
                                 width = 300, height = 60, fixed = T, style = "border: 0px; background-color: rgba(255,255,255,.5) ; z-index: 29999",
                                 strong(h2("Updating Plots..."))))
))

server <- function(input, output, session){
  
  ##----------------------------------------- Comparison Summary Overview Data Plots-----------------------------------
  
  
  #initialize Reactive Values
  genesToPlot <- reactiveVal(NULL)
  numberGenesToPlot <- reactiveVal()
  count <- reactiveVal(1)
  selectedUpsetData <- reactiveVal("Click Plot to View Genes in Subset")
  subSetList <- reactiveVal(rep(1,7))
  subSetList_selData <- reactiveVal(1)
  
  
  getpValGeneList <- function(df,pval,foldC = 0){
    colnames <- c("Hogenesch_1","Hogenesch_2A","Hogenesch_2B","Hogenesch_4A","Hogenesch_4B","Hogenesch_4C","Hogenesch_4D","Hughes_2","Hughes_4A","Hughes_4B","Zhang_2","Zhang_4A","Zhang_4B")
    
    names <- rownames(df)
    pVals <- apply(df < pval, 2, function(x) names[x])
    foldChanges <- lapply(data,function(x){getFoldChangeGeneList(x,foldC)})
    
    #get Intersection between the pVal and foldChange Gene List  
    reducedGeneList <- sapply(1:13, function(i){
        Reduce(intersect,list(pVals[[i]] , foldChanges[[i]]))
    })
    names(reducedGeneList) <- colnames
    return(reducedGeneList)
  }
  
  getFoldChangeGeneList <- function(df,fc){
    names <- rownames(df)
    output <- apply(df,1,function(x){
      min_max <- range(x)
      return(abs(min_max[2]-min_max[1]) > fc)
    })
    names[which(output)]
  }
  
  plotUpset <- function(df,showPlot = T, method = 1){
    
    methodNames <- c("Arser", "BooteJTK", "JTK_Cycle", "RAIN")
    setSize <- 400
    #identify the Cycling Genes in the Dataset
    Cycle <- c("Per1","Per2","Per3","Cry1","Cry2","Npas2","Clock","Arntl","Arntl2")
    Genes <- unique(unlist(df))
    collected <- as.numeric(Genes %in% Cycle)
    Cyclers <- c(0,2)[collected+1]
    
    
    #identify the Gene The User Searches for
    goi <- rownames(data[[1]])[geneID()]
    collected <- as.numeric(Genes %in% goi)
    GOI <- c(0,2)[collected+1]

    #Bind Data together to Use for Plotting
    if(sum(collected)>0){
      data <- cbind(fromList(df),Cyclers,GOI)
    } else{
      data <- cbind(fromList(df),Cyclers)
    }
    
    #find largest Common Interaction Between Gene That are plotted
    freq <- vennCounts(data[,1:13])
    freq <- freq[order(-freq[,14]),]
    maxSetSize <- max(which(freq[,14] > 0))
    maxIntersection <- freq[as.numeric(unlist(which.max(apply(freq[1:maxSetSize,1:13],1,sum)))),]
    names <- names(maxIntersection)[c(as.logical(unname(maxIntersection[1:13] == 1)),F)]
    
    #set Up the Query
    if(showPlot){
    if(sum(collected)>0){ #if GOI was identified plot, else skip it in query
      dataQuery <- list(
                        list(query = intersects,
                         params = list(names),
                         color = "blue",
                         active = T,
                         query.name = "Largest Common Interaction"),
                        list(query = elements,
                             params = list("Cyclers",2),
                             color = "orange",
                             active = F,
                             query.name = "Circadian Clock Genes"),
                        list(query = elements,
                             params = list("GOI",2),
                             color = "green",
                             active = F,
                             query.name = "Gene of Interest"))
      showNotification(paste0(goi," - ",  methodNames[method]," - Cycling"),duration = 10,type = "message")
      
    } else{
      dataQuery <- list(
                        list(query = intersects,
                             params = list(names),
                             color = "blue",
                             active = T,
                             query.name = "Largest Common Interaction"),
                        list(query = elements,
                             params = list("Cyclers",2),
                             color = "orange",
                             active = F,
                             query.name = "Circadian Clock Genes"))
      showNotification(paste0(goi," - ",  methodNames[method]," - NOT Cycling"),duration = 10,type = "error")
    }

    upset(data,nsets = 13,
          nintersects = setSize,
          order.by = "freq",
          mb.ratio = c(0.6, 0.4),
          sets = c("Hogenesch_1","Hogenesch_2A","Hogenesch_2B","Hogenesch_4A","Hogenesch_4B","Hogenesch_4C","Hogenesch_4D","Hughes_2","Hughes_4A","Hughes_4B","Zhang_2","Zhang_4A","Zhang_4B"),
          keep.order = T,
          number.angles = 0,
          text.scale = 1.1,
          point.size = 2.8,
          set_size.show = T,
          line.size = 1,
          set_size.scale_max = 8000,
          query.legend = "bottom",
          empty.intersections = "off",
          group.by = "degree",
          queries = dataQuery
    )
    }else{
        rownames(data) <- Genes
        return(data[,1:13])
    }
  }
  
  getGenesToPlotFromUpset <- function(click, method){
    labels <- c("Arser", "BooteJTK", "JTK_Cycle","RAIN")
    #Get the Genes For Plotting by Clicking on a specific Subset in the upset Plot
    if(10 < click$coords_css$y & click$coords_css$y < 453 & 1283 < click$coords_css$x & click$coords_css$x < 5987){
      selectedSet <- round((click$coords_css$x-1288)/11.79487)+1
      #create upset dataframe with the correct alpha value threshold
      output <- getpValGeneList(resultsAdj[[method]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
      upSets <- plotUpset(output,F)
      
      #get frequency in Each Venn Intersection and order by frequency
      freq <- vennCounts(upSets)
      freq <- freq[order(-freq[,14]),]
      
      #get list of selected genes 
      subSetList(as.vector(unlist(freq[selectedSet,1:13])))
      subSetList_selData(min(which(subSetList()==1)))
      sel <- unique(which(apply(upSets, 1, function(x) all(x==subSetList()))))
      genesToPlot <- rownames(upSets[sel,])
      selectedUpsetData(paste0("Method: ", labels[method], " | Genes Detected: ",length(genesToPlot)))
    } else if(286 < click$coords_css$y & click$coords_css$y < 453 & 7 < click$coords_css$x & click$coords_css$x < 1267){
      
      
      selectedSet <- 13-round((click$coords_css$y-286)/12.84-.5)
      output <- getpValGeneList(resultsAdj[[method]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
      subSetList(as.vector(selectedSet))
      subSetList_selData(as.vector(selectedSet))
      genesToPlot <- unlist(output[selectedSet])
      
      selectedUpsetData(paste0("Method: ",labels[method], " | Genes Detected: ",length(genesToPlot)))
    } else{
      genesToPlot <- NULL 
    }
    return(genesToPlot)
  }
  
  observeEvent(input$upset_click_Arser,{
    click <- input$upset_click_Arser
    method <- 1 #Arser is 1 in alpha order
    genesToPlot(getGenesToPlotFromUpset(click, method))
    initialPlotting()
    
  })
  
  observeEvent(input$upset_click_BooteJTK,{
    click <- input$upset_click_BooteJTK
    method <- 2 #BooteJTK is 2 in alpha order
    genesToPlot(getGenesToPlotFromUpset(click, method))
    initialPlotting()
  })
  
  observeEvent(input$upset_click_JTK_Cycle,{
    click <- input$upset_click_JTK_Cycle
    method <- 3 #JTK_Cycle is 3 in alpha order
    genesToPlot(getGenesToPlotFromUpset(click, method))
    initialPlotting()
  })
  
  observeEvent(input$upset_click_Rain,{
    click <- input$upset_click_Rain
    method <- 4 #Rain is 4 in alpha order
    genesToPlot(getGenesToPlotFromUpset(click, method))
    initialPlotting()
  })
  
  initialPlotting <- function(){
    count(1)
    if(length(genesToPlot()) != 0 ){#if there are genes to plot
    totalGenes <- 1:length(genesToPlot())
    nums <- split(totalGenes, ceiling(seq_along(totalGenes)/20))
    numberGenesToPlot(nums[count()])
    shinyjs::enable("cyclingGenesView")
    toggle("toggleCyclingGenesView", anim = TRUE)
    if(count() == length(nums)){
      shinyjs::disable("nextPage")
      shinyjs::enable("previousPage")
      } else if(count() == 1){
      shinyjs::disable("previousPage")
      shinyjs::enable("nextPage")
      }else{
      shinyjs::enable("nextPage")
      shinyjs::enable("previousPage")
      }
    
    if(length(nums) == 1){
      shinyjs::disable("previousPage")
      shinyjs::disable("nextPage")
    }
    }
  }
  
  output$mainPlot_CyclingGenes <- renderPlot({
    par(mfrow = c(4,5), mar = c(2,2,2,2))
    names <- genesToPlot()
    genes <- as.vector(unlist(numberGenesToPlot()))
    plotData <- data[[subSetList_selData()]][genesToPlot()[genes],]
    tp <- as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(plotData)))
    count <- 1
    Cycle <- c("Per1","Per2","Per3","Cry1","Cry2","Npas2","Clock","Arntl","Arntl2")
    GOI <- rownames(data[[1]])[geneID()]
    apply(plotData, 1,function(y){
      col <- 1
      star <- ""
      if(names[genes[count]] %in% Cycle){
        col <- "orange"
        star <- " | *Core Clock Gene"
      }
      if(names[genes[count]] %in% GOI){
        col <- "Green"
        star <- " | *Gene of Interest"
      }
      
      if(names[genes[count]] %in% GOI & names[genes[count]] %in% Cycle){
        col <- "Green"
        star <- " | *Gene of Interest | *Core Clock Gene"
      }
      plot(tp,y,"o",main = paste0(genes[count],": ",names[genes[count]],star),col = col, pch = 16, font = 2, font.lab = 2)
      box(lwd = 2)
      count <<- count +1
    })
  }
  )
  
  output$dataSet_summary <- renderPlot({
    colnames <- c("Hogenesch_1",
                  "Hogenesch_2A",
                  "Hogenesch_2B",
                  "Hogenesch_4A",
                  "Hogenesch_4B",
                  "Hogenesch_4C",
                  "Hogenesch_4D",
                  "Hughes_2",
                  "Hughes_4A",
                  "Hughes_4B",
                  "Zhang_2",
                  "Zhang_4A",
                  "Zhang_4B")
    par(mar = c(1,1,1,1))
    subSetsToPlot <- subSetList()
    
    if(length(subSetsToPlot) > 1){
    subSetsToPlot[which(subSetsToPlot == 0)] <- NA
    par(bg = '#F7F7F7')
    
    
    plot(1:13,rep(1,13), pch = 16, font = 2, font.lab = 2, cex = 2.5,axes=F, xlim = c(0,14), col ="grey" )
    box("outer",lwd = 6)
    points(1:13,subSetsToPlot, pch = 16, cex = 2.5, col = "#393939")
    text(1:13, rep(1,13),offset = 1,  colnames, cex=1, font = 2, pos=c(1,3,1,3,1,3,1,3,1,3,1,3,1),col=c("grey","#393939")[subSetList()+1])
    connection <- min(which(subSetList()==1)):max(which(subSetList()==1))
    lines(connection,rep(1,length(connection)), lwd = 3, col = "#393939")
    points(c(1:13)[subSetList_selData()],1,pch = 21, cex = 1.5, bg = "orange", col = "#393939", lwd = 2)
    }
    else{
      plot(1,1, pch = 16, font = 2, font.lab = 2, cex = 2.5,axes=F, col ="#393939" )
      box("outer",lwd = 6)
      text(1, 1, offset = 1,  colnames[subSetList()], cex=1, font = 2, pos= 1 ,col="#393939")
      points(1, 1, pch = 21, cex = 1.5, bg = "orange", col = "#393939", lwd = 2)
    }
  })
  
  
  observeEvent(input$upsetDatasetSel_click_summary,{
    click <- input$upsetDatasetSel_click_summary
    subSetList_selData(round(click$x))
  })
  
  output$selectedUpsetPlot <- renderText({
    selectedUpsetData()
  })
  
  observeEvent(input$cyclingGenesView, {
    toggle("toggleCyclingGenesView", anim = TRUE)
    selectedUpsetData("Click Plot to View Genes in Subset")
    shinyjs::disable("cyclingGenesView")
  })

  observeEvent(input$nextPage, {
    totalGenes <- 1:length(genesToPlot())
    nums <- split(totalGenes, ceiling(seq_along(totalGenes)/20))
    if(count() < length(nums)){
      count(count()+1)
    } 
    
    if(count() == length(nums)){
      shinyjs::disable("nextPage")
      shinyjs::enable("previousPage")
    } else if(count() == 1){
      shinyjs::disable("previousPage")
      shinyjs::enable("nextPage")
    }else{
      shinyjs::enable("nextPage")
      shinyjs::enable("previousPage")
    }
    
    numberGenesToPlot(nums[count()])
  })
  
  observeEvent(input$previousPage, {
    totalGenes <- 1:length(genesToPlot())
    nums <- split(totalGenes, ceiling(seq_along(totalGenes)/20))
    if(count()>1){
      count(count()-1)
    }
    
    if(count() == length(nums)){
      shinyjs::disable("nextPage")
      shinyjs::enable("previousPage")
      } else if(count() == 1){
      shinyjs::disable("previousPage")
      shinyjs::enable("nextPage")
      } else{
        shinyjs::enable("previousPage")
        shinyjs::enable("nextPage")
        }
    numberGenesToPlot(nums[count()])
  })
  
  
  output$mainPlot_Arser <- renderPlot({
    selectedUpsetData("Click Plot to View Genes in Subset")
    p <- getpValGeneList(resultsAdj[[1]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
    plotUpset(p, method = 1)
  }
  )
  
  output$mainPlot_BooteJTK <- renderPlot({
    selectedUpsetData("Click Plot to View Genes in Subset")
    p <- getpValGeneList(resultsAdj[[2]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
    plotUpset(p,method = 2)
  }
  )
  
  output$mainPlot_JTK_Cycle <- renderPlot({
    selectedUpsetData("Click Plot to View Genes in Subset")
    p <- getpValGeneList(resultsAdj[[3]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
    plotUpset(p,method = 3)
  }
  )
  
  output$mainPlot_RAIN <- renderPlot({
    selectedUpsetData("Click Plot to View Genes in Subset")
    p <- getpValGeneList(resultsAdj[[4]],as.numeric(input$alphaVal), as.numeric(input$fcVal))
    plotUpset(p,method = 4)
  }
  )
  
  
  ##----------------------------------------- Comparison Method Overview Data Plots -----------------------------------
  
  customPlot <- function(x,y, title = NULL){
    plot(x,y, main = title, xlab = "-log(pVal)", ylab = "-log(pVal)", xlim = c(0,50), ylim = c(0,70),cex = .5, pch = 16, col = alpha("black",0.5), font = 2, font.lab = 2)
    abline(fit <- lm(y ~ x), col='red', lwd = 2)
    legend("topleft", bty="n", cex = 2, text.font = 2,legend=paste("Rank Corr", format(cor(x,y,method = "spearman"), digits=2)))
    points(x[geneID()],y[geneID()],pch = 21, bg = "orange",col = "black", cex =1.5)
    box(lwd = 3)
  }
  
  #used to draw rectangle on heat map
  .highLightHeatMap <<- function(tfMat,border,sizeMat){
    if(all(!tfMat)){ #if all false
      return(NULL)
    }else{
      cAbove = expand.grid(1:sizeMat,1:sizeMat)[tfMat,]
      xl = cAbove[,1]-0.49
      yb = cAbove[,2]-0.49
      xr = cAbove[,1]+0.49
      yt = cAbove[,2]+0.49
      rect(xl,yb,xr,yt,border=border,lwd=5)
    }
  }
  
  plotDataSet <- function(ds){
    par(mfrow = c(3,3), mar = c(2,2,2,2), oma=c(7,7,1,1))
    customPlot(-log(results[[1]][,ds]), -log(results[[2]][,ds]))
    mtext("BooteJTK", side=2, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    plot.new()
    plot.new()
    customPlot(-log(results[[1]][,ds]), -log(results[[3]][,ds]))
    mtext("JTK_Cycle", side=2, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    customPlot(-log(results[[2]][,ds]), -log(results[[3]][,ds]))
    plot.new()
    customPlot(-log(results[[1]][,ds]), -log(results[[4]][,ds]))
    mtext("RAIN", side=2, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    mtext("Arser", side=1, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    customPlot(-log(results[[2]][,ds]), -log(results[[4]][,ds]))
    mtext("BooteJTK", side=1, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    customPlot(-log(results[[3]][,ds]), -log(results[[4]][,ds]))
    mtext("JTK_Cycle", side=1, line=7, cex=1.5, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
  }
  
  
  heatMapMethodComparison <- function(selectedColumns, scheme, method){
    
    #take the log of all results
    logResults <-lapply(results, function(x){-log(x[selectedColumns])})
    
    labels <- c("ARSER", "BooteJTK", "JTK_Cycle" , "RAIN")
    DataSetsMeta <- c("1", "2A", "2B", "4A","4B","4C","4D","2", "4A","4B","2","4A","4B")[selectedColumns]
    .labelNumber <<- 0
    
    #remane columns in each dataframe for plotting purposes
    sapply(seq_along(logResults), function(x){
      .labelNumber <<- .labelNumber+1
      colnames(logResults[[x]]) <<- paste0(labels[.labelNumber],"_",DataSetsMeta)
    })
    
    #bind result into single Dataframe
    logResults <- do.call(cbind,logResults)
    
    #arrange result by Sampling Scheme
    logResults <-lapply(as.list(1:length(selectedColumns)),function(x){
      subSamplings <- gsub(pattern = ".*_",replacement = "",x = colnames(logResults))
      sel <- which(subSamplings == subSamplings[x])
      output <- logResults[,sel]
      colnames(output) <- labels
      return(output)
    })
    
    .labelNumber <<- 0
    #plot heatmap
    hmcol <- colorRampPalette(c("white","blue"))(10)
    gl <-lapply(logResults, function(x){
      .labelNumber <<- .labelNumber + 1 
      data <- round(cor(x,method = "spearman"), digits = 2)
      
      #highlight selected box on heatmap
      .selBox <<- rep(F,as.numeric(dim(x)[2])^2)
      if(scheme == .labelNumber){
        selGrid <- expand.grid(dim(x)[2]:1,1:dim(x)[2])
        selGrid <- apply(selGrid[,1:2],1,function(z) {z[order(z)]})
        selGrid <- rbind(selGrid,(as.numeric(dim(x)[2])^2):1)
        sels <- which(method[1] == selGrid[1,] & method[2] == selGrid[2,])
        .selBox[selGrid[3,sels]] <<- T
      }
      
      heatmap.2(data,Rowv = NA,
                Colv = NA,
                cellnote = round(data,3),
                notecex=1.0,
                notecol="black",
                symkey=FALSE, 
                key = F,
                density.info='none',
                trace='none',
                scale="none",
                col = hmcol,
                breaks = seq(0.5, 1, length.out = 11),
                symbreaks = TRUE,
                cexRow=1,
                cexCol=1,
                margins = c(16, 20),
                add.expr = {.highLightHeatMap(tfMat = c(.selBox),border = "orange",sizeMat = dim(x)[2])})
      
      title(paste0("Sampling Scheme ", DataSetsMeta[.labelNumber]), line= -2)
      grid.echo()
      grid.grab()
    })
    
    
    grid.arrange(grobs=gl, ncol=4, nrow = 2,newpage = T,clip=T)
    
  }
  
  #get Info For Which Gene was Selected in the plot 
  observeEvent(input$scatter_click_method,{
    click <- isolate(input$scatter_click_method)
    
    ds <- as.numeric(input$dataSet_method)
    method <- as.numeric(input$methodSel_method)
    scheme <- as.numeric(input$schemeSel_method)
    method <- method[order(method)]
    
    dist <- sqrt((click$x + log(results[[method[1]]][,scheme]))^2 + (click$y + log(results[[method[2]]][,scheme]))^2)
    if(!is.null(min(dist,na.rm = T))) {
      output <- which.min(dist)
      geneID(output)
      
    }else{
      output <- NULL
    }
    if(!is.null(output)){
      gene <- rownames(results[[method[1]]])[output]
      updateSelectizeInput(session,"geneSel", selected = gene, options = list())
    } else{
      gene = "Please Select a Gene"
    }
  })
  
  #Set the Gene to be Displayed
  output$geneName_method <- renderText({
    rownames(results[[1]])[geneID()]
  })
  
  observeEvent(eventExpr = input$dataSet_method,handlerExpr = {
        ds <- as.numeric(input$dataSet_method)
        if(ds == 1){
          updateSelectInput(session,inputId = "schemeSel_method", label = "Scheme Selection", choices = c("1" = 1,"2A" = 2,"2B" = 3,"4A" = 4,"4B" = 5,"4C" = 6,"4D" = 7), selected = list(1))
        }
        if(ds == 2){
          updateSelectInput(session,inputId = "schemeSel_method", label = "Scheme Selection", choices = c("2" = 8,"4A" = 9,"4B" = 10), selected = list(8))
        }
        if(ds == 3){
          updateSelectInput(session,inputId = "schemeSel_method", label = "Scheme Selection", choices = c("2" = 11,"4A" = 12,"4B" = 13), selected = list(11))
        }
  })
  
  output$mainPlot_method <- renderPlot({
    
    validate(
      need(length(input$methodSel_method) > 1, 'Please choose 2 Methods for Comparison'),
      need(length(input$schemeSel_method) > 0, 'Please choose 1 Schemes for Comparison')
    )
    
    ds <- as.numeric(input$dataSet_method)
    
    method <- as.numeric(input$methodSel_method)
    scheme <- as.numeric(input$schemeSel_method)
    #place scheme in the correct order so smaller is always compared to larger
    method <- method[order(method)]
    
    labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
    methodsLabels <- c("Arser", "BooteJTK", "JTK_Cycle", "RAIN")
    
    par(mar = c(2,2,2,2), oma=c(7,7,1,1))
    customPlot(-log(results[[method[1]]][,scheme]), -log(results[[method[2]]][,scheme]), title = paste0("Sampling Scheme: ",labels[scheme], " | Gene: ", rownames(data[[1]])[geneID()]))
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    mtext(methodsLabels[method[2]], side=2, line=7, cex=1.5, font = 2)
    mtext(methodsLabels[method[1]], side=1, line=7, cex=1.5, font = 2)
  })
  
  output$subPlot_2_4A_4B_method <- renderPlot({
    ds <- as.numeric(input$dataSet_method)
    scheme <- as.numeric(input$schemeSel_method)
    par(mfrow = c(2,4))
    
    if(ds == 1){
      plotSets <- c(1,0,2,3,
                    4,5,6,7)
    }
    if(ds == 2){
      plotSets <- c(8,0,0,0,
                    9,10,0,0)
    }
    if(ds == 3){
      plotSets <- c(11,0,0,0,
                    12,13,0,0)
    }
    
    labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
    
    ylims <- range(as.vector(unlist(data[[(plotSets[1])]][geneID(),])))
    xlims <- range(as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(data[[(plotSets[1])]][geneID(),]))))
    for(i in plotSets){
      if(i == 0){
        plot.new()
      } else{
        plotData <- data[[(i)]][geneID(),]
        tp <- as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(plotData)))
        vals <- as.vector(unlist(plotData))
        if(i %in% scheme){
          plotCol <- "orange"
        } else{
          plotCol <- "black"
        }
        plot(tp,vals,"o", cex = 1, lwd = 2, pch = 16, col = plotCol, font = 2, font.lab = 2, 
             main = paste0("Sampling Scheme ",labels[i]),
             xlab = "ZT Time Point",
             ylab = "Expression",
             ylim = c(ylims[1],ylims[2]),
             xlim = c(xlims[1],xlims[2]))
          box(lwd = 3, col = plotCol)
      }
    }
  })
  
  
  
  output$heatMap_method <- renderPlot({
    
    validate(
      need(length(input$methodSel_method) > 1, 'Please choose 2 Methods for Comparison'),
      need(length(input$schemeSel_method) > 0, 'Please choose 1 Schemes for Comparison')
    )
    
    ds <- as.numeric(input$dataSet_method)
    scheme <- as.numeric(input$schemeSel_method)
    method <- as.numeric(input$methodSel_method)
    if(ds == 1){
      heatMapMethodComparison(1:7, scheme,method)
    } else if(ds == 2){
      heatMapMethodComparison(8:10, scheme,method)
    } else {
      heatMapMethodComparison(11:13, scheme,method)
    }
  }
  )
  
  
  ## ----------------------------------------- Sampling Scheme Comparison Data Plots ------------------------------------------------------
  
  customScatterPlot <- function(x,y, title, xlim = c(0,40), ylim = c(0,40)){
    # , 
    plot(x,y, main = title, xlab = "-log(pVal)", ylab = "-log(pVal)", xlim = xlim, ylim = ylim , cex = .5, pch = 16, col = alpha("black",0.5), font = 2, font.lab = 2)
    abline(fit <- lm(y ~ x), col='red')
    legend("topleft", bty="n", cex = 2, text.font = 2,legend=paste("Rank Corr:", format(cor(x,y,method = "spearman"), digits=2)))
    points(x[geneID()],y[geneID()],pch = 21, bg = "orange",col = "black", cex =1.5)
    box(lwd = 3)
  }
  
  schemeScatterPlotFormat <- function(dataS1,dataS2){
    
    labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
    
    par(mfrow = c(2,2), mar = c(2,2,2,2), oma=c(7,7,1,1))
    customScatterPlot(-log(results[[1]][,dataS1]), -log(results[[1]][,dataS2]), "ARSER")
    mtext(labels[dataS2], side=2, line=7, adj = -0.25, cex=1.5, font = 2)
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    customScatterPlot(-log(results[[2]][,dataS1]), -log(results[[2]][,dataS2]), "BooteJTK")
    customScatterPlot(-log(results[[3]][,dataS1]), -log(results[[3]][,dataS2]), "JTK_Cycle")
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    customScatterPlot(-log(results[[4]][,dataS1]), -log(results[[4]][,dataS2]), "RAIN")
    mtext(labels[dataS1], side=1, line=7, adj = -0.25, cex=1.5, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    
  }
  
  plotSchemeComparionsHeatMap <- function(selectColumns, scheme, method){
    logResults <-lapply(results, function(x){-log(x)})
    
    
    
    shortColumnNames<-sapply(seq_along(logResults), function(x){
      colnames(logResults[[x]]) <<- c("1", "2A", "2B", "4A","4B","4C","4D","2", "4A","4B","2","4A","4B")
    })
    
    labels <- c("ARSER", "BooteJTK", "JTK_Cycle" , "RAIN")
    .labelNumber <- 0
    hmcol <- colorRampPalette(c("white","blue"))(10)
    gl <-lapply(logResults, function(x){
      .labelNumber <<- .labelNumber + 1 
      
      #compute correlation
      data <- round(cor(x[,selectColumns],method = "spearman"), digits = 2)
      
      #highlight selected box on heatmap
      .selBox <<- rep(F,as.numeric(dim(x[,selectColumns])[2])^2)
      if(method == .labelNumber){
        selGrid <- expand.grid(dim(x[,selectColumns])[2]:1,1:dim(x[,selectColumns])[2])
        selGrid <- apply(selGrid[,1:2],1,function(z) {z[order(z)]})
        selGrid <- rbind(selGrid,(as.numeric(dim(x[,selectColumns])[2])^2):1)
        sels <- which(which(selectColumns == scheme[1]) == selGrid[1,] & which(selectColumns == scheme[2]) == selGrid[2,])
        .selBox[selGrid[3,sels]] <<- T
      }
      
      heatmap.2(data,Rowv = NA,
                Colv = NA,
                cellnote = round(data,3),
                notecex=1.0,
                notecol="black",
                symkey=FALSE,
                col = hmcol,
                breaks = seq(0.5, 1, length.out = 11),
                key = F,
                density.info='none',
                trace='none',
                scale="none",
                cexRow=1.5,
                cexCol=1.5,
                margins = c(6, 6),
                add.expr = {.highLightHeatMap(tfMat = c(.selBox),border = "orange",sizeMat = dim(x)[2])}
                )
      title(labels[.labelNumber], line= -2)
      grid.echo()
      grid.grab()
    })
    
    
    grid.arrange(grobs=gl, ncol=2, nrow = 2,newpage = T,clip=T)
    
  }
  
  observeEvent(eventExpr = input$dataSet_schemes,handlerExpr = {
    ds <- as.numeric(input$dataSet_schemes)
    if(ds == 1){
      updateSelectInput(session,inputId = "schemeSel_schemes", label = "Scheme Selection", choices = c("1" = 1,"2A" = 2,"2B" = 3,"4A" = 4,"4B" = 5,"4C" = 6,"4D" = 7), selected = list(1,2))
    }
    if(ds == 2){
      updateSelectInput(session,inputId = "schemeSel_schemes", label = "Scheme Selection", choices = c("2" = 8,"4A" = 9,"4B" = 10), selected = list(8,9))
    }
    if(ds == 3){
      updateSelectInput(session,inputId = "schemeSel_schemes", label = "Scheme Selection", choices = c("2" = 11,"4A" = 12,"4B" = 13), selected = list(11,12))
    }
  })
  
  output$mainPlot_schemes <- renderPlot({
    
    validate(
      need(length(input$methodSel_schemes) > 0, 'Please choose 1 Methods for Comparison'),
      need(length(input$schemeSel_schemes) > 1, 'Please choose 2 Schemes for Comparison')
    )
    
    ds <- as.numeric(input$dataSet_schemes)
    method <- as.numeric(input$methodSel_schemes)
    scheme <- as.numeric(input$schemeSel_schemes)
    scheme <- scheme[order(scheme)]
    
    labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
    methodsLabels <- c("Arser", "BooteJTK", "JTK_Cycle", "RAIN")
    
    par(mar = c(2,2,2,2), oma=c(7,7,1,1))
    customScatterPlot(-log(results[[method]][,scheme[1]]), -log(results[[method]][,scheme[2]]), paste0("Method: ", methodsLabels[method]," | Gene: ", rownames(data[[1]])[geneID()]))
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    mtext(labels[scheme[1]], side=1, line=7, cex=1.5, font = 2)
    mtext(labels[scheme[2]], side=2, line=7, cex=1.5, font = 2)
  })
  
  #create a variable to Store the Gene that is clicked
  geneID <- reactiveVal(2316)
  
 #get Info For Which Gene was Selected in the plot 
    observeEvent(input$scatter_click_schemes,{
    click <- isolate(input$scatter_click_schemes)
    
    ds <- as.numeric(input$dataSet_schemes)-1
        method <- as.numeric(input$methodSel_schemes)
        scheme <- as.numeric(input$schemeSel_schemes)
    
        initd1 <- min(scheme)
        initd2 <- max(scheme)
   
        dist <- sqrt((click$x + log(results[[method]][,initd1]))^2 + (click$y + log(results[[method]][,initd2]))^2)
    if(!is.null(min(dist,na.rm = T))) {
      output <- which.min(dist)
      geneID(output)
      
    }else{
      output <- NULL
    }
    if(!is.null(output)){
    gene <- rownames(results[[method]])[output]
    updateSelectizeInput(session,"geneSel", selected = gene, options = list())
    } else{
      gene = "Please Select a Gene"
    }
  })
  
    #Set the Gene to be Displayed
  output$geneName_scheme <- renderText({
    rownames(results[[1]])[geneID()]
  })
  
  
  output$mainPlot_HeatMap_scheme <- renderPlot({
    
    validate(
      need(length(input$methodSel_schemes) > 0, 'Please choose 1 Methods for Comparison'),
      need(length(input$schemeSel_schemes) > 1, 'Please choose 2 Schemes for Comparison')
    )
    
    ds <- as.numeric(input$dataSet_schemes)
    scheme <- as.numeric(input$schemeSel_schemes)
    method <- as.numeric(input$methodSel_schemes)
    
    if(ds == 1){
      plotSchemeComparionsHeatMap(1:7, scheme, method)
    } else if(ds == 2){
      plotSchemeComparionsHeatMap(8:10, scheme, method)
    } else{
      plotSchemeComparionsHeatMap(11:13, scheme, method)
    }
  
  })
  
  
  output$subPlot_2_4A_4B_schemes <- renderPlot({
    ds <- as.numeric(input$dataSet_schemes)
    scheme <- as.numeric(input$schemeSel_schemes)
    
    par(mfrow = c(2,4))
    
    if(ds == 1){
      plotSets <- c(1,0,2,3,
                    4,5,6,7)
    }
    if(ds == 2){
      plotSets <- c(8,0,0,0,
                    9,10,0,0)
    }
    if(ds == 3){
      plotSets <- c(11,0,0,0,
                    12,13,0,0)
    }
    
    labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
    
    ylims <- range(as.vector(unlist(data[[(plotSets[1])]][geneID(),])))
    xlims <- range(as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(data[[(plotSets[1])]][geneID(),]))))
    for(i in plotSets){
        if(i == 0){
            plot.new()
      } else{
            if(i %in% scheme){
              plotCol <- "orange"
            } else{
              plotCol <- "black"
            }
            plotData <- data[[(i)]][geneID(),]
            tp <- as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(plotData)))
            vals <- as.vector(unlist(plotData))
            plot(tp,vals,"o", cex = 1, lwd = 2, pch = 16, col = plotCol, font = 2, font.lab = 2, 
                 main = paste0("Sampling Scheme ",labels[i]),
                 xlab = "ZT Time Point",
                 ylab = "Expression")
            box(lwd = 3, col = plotCol)
            }
    }
  })
  
  ## ----------------------------------------- ALL HOGENESH SAMPLES Comparison Data Plots -------------------------------
  
  hogSamplesDF <- data.frame(1:48,rep(0,48))
  colnames(hogSamplesDF) <- c('xval','yval')
  
  #create reactive value to hold points to be used by RAIN and JTK_Cycle
  hogsampled <- reactiveValues(Points = rep(0,48))
  rainOutPut <- reactiveValues(df = NULL, df2 = NULL)
  JTK_CYCLEOutPut <- reactiveValues(df = NULL)
  
  #plot for selecting Data Points to Use in analysis
  output$selectionPlot_hogSample <- renderPlot({
    plot(x = hogSamplesDF$xval, y = hogSamplesDF$yval, pch = 16, cex = 2, font = 2, font.lab = 2, col = c("grey","orange")[hogsampled$Points+1], ylim = c(-0.1,0.1), ylab = "", xlab = "", axes = F, main = "Selected Sampling Scheme")
    text(x = hogSamplesDF$xval, y = hogSamplesDF$yval, offset = 1,labels = paste0('ZT_',18:65), cex=1, font = 2, pos=c(rep(c(1,3),24)), col=c("grey","orange")[hogsampled$Points+1])
    box(lwd = 3)
    })
  
  #update Points for Use in sampling when Plot is Brushed 
  output$brush_info <- renderPrint({
   infoBrush <- brushedPoints(hogSamplesDF, xvar = "xval", yvar = "yval", brush = input$selectionPlot_hogSample_brush)
   
   output <- isolate(hogsampled$Points)
   for(i in infoBrush$xval){
     x <- isolate(hogsampled$Points[i])
     if( x == 0 ){
       output[i] <- 1
     }else{
       output[i] <- 0
     }
   }
   
   hogsampled$Points <- output
   print(paste0("ZT_",c(18:65)[which(hogsampled$Points == 1)]))
   })
  
  #update Points for Use in sampling when Plot is Clicked
  output$click_info <- renderPrint({
  infoClick <- nearPoints(hogSamplesDF, xvar =  "xval", yvar = "yval", coordinfo = input$scatter_sel_hogSample)
  output <- isolate(hogsampled$Points)
  for(i in infoClick$xval){
    x <- isolate(hogsampled$Points[i])
    if( x == 0 ){
      output[i] <- 1
    }else{
      output[i] <- 0
    }
  }
  hogsampled$Points <- output
  })
  
  #on Submit
  observeEvent(input$runJtkRain,{
    
    validate(
      need(any(hogsampled$Points != 0), 'Please Select Time Points for Analysis')
    )
   
    subSamplingdata <- data[[1]][,which(hogsampled$Points==1)]
    
    # source(RAIN)
    cat("RAIN analysis started on",date(),"\n")
    if(isolate(input$JTK_period_range)[2] == isolate(input$JTK_period_range)[1]){
      #if they period input is the same value, just use one of the inputs
      periodsToUseInRain <- isolate(input$JTK_period_range)[1]
    } else{
      periodsToUseInRain <- isolate(input$JTK_period_range)
    }
    rainOutPut$df <- rain(t(subSamplingdata), deltat = 1, period = periodsToUseInRain, measure.sequence = isolate(hogsampled$Points), peak.border = isolate(input$peak_range), verbose = F, adjp.method = "rawp")
    rainOutPut$df2 <- rain(t(subSamplingdata), deltat = 1, period = periodsToUseInRain, measure.sequence = isolate(hogsampled$Points), peak.border = isolate(input$peak_range), verbose = F)
    cat("Finished RAIN analysis on",date(),"\n")
    
    # source(JTK_CYCLE)
    source("App_Data/JTK_CYCLEv3.1.R")
    
    subSamplingdata <- data[[1]][,]
    subSamplingdata[,which(hogsampled$Points==0)] <- rep(NA, dim(subSamplingdata)[1])
    Names <- rownames(subSamplingdata)
    jtkdist(timepoints = 48, reps=1)
    
    periods <- isolate(input$JTK_period_range)[1]:isolate(input$JTK_period_range)[2]
    jtk.init(periods,1);

    cat("JTK analysis started on",date(),"\n")
    flush.console()

    st <- system.time({
      res <- apply(subSamplingdata,1,function(z) {
        jtkx(z)
        c(JTK.ADJP,JTK.PERIOD,JTK.LAG,JTK.AMP)
      })
      res <- as.data.frame(t(res))
      bhq <- p.adjust(unlist(res[,1]),"BH")
      res <- cbind(bhq,res)
      colnames(res) <- c("BH.Q","ADJ.P","PER","LAG","AMP")
      JTK_CYCLEOutPut$df <- cbind(Names,res)
    })
    cat("Finished JTK analysis on",date(),"\n")
    
    
  }
  )
  
  # Main Scatter plot of Sampling comparisons
  output$mainPlot_hogSample <- renderPlot({
    !is.null(JTK_CYCLEOutPut$df)
    !is.null(isolate(rainOutPut$df))
    
    validate(
      need(!is.null(isolate(JTK_CYCLEOutPut$df)) & !is.null(isolate(rainOutPut$df)), 'Please Run a Sampling Scheme')
    )
    
    x <- -log(isolate(JTK_CYCLEOutPut$df$ADJ.P))
    y <- -log(isolate(rainOutPut$df$pVal))
    
    par(mar = c(2,2,2,2), oma=c(7,7,1,1))
    plot(x,y, main = paste0("JTK_Cycle Vs Rain | Gene: ", rownames(data[[1]])[geneID()]), xlab = "-log(pVal)", ylab = "-log(pVal)", cex = .5, pch = 16, col = alpha("black",0.5), font = 2, font.lab = 2)
    abline(fit <- lm(y ~ x), col='red')
    legend("topleft", bty="n", cex = 2, text.font = 2,legend=paste("Rank Corr:", format(cor(x,y,method = "spearman"), digits=2)))
    points(x[geneID()],y[geneID()],pch = 21, bg = "orange",col = "black", cex =1.5)
    box(lwd = 3)
    mtext("-log(pVal)", side=2, line=4, cex=1, font = 2)
    mtext("-log(pVal)", side=1, line=4, cex=1, font = 2)
    mtext(text = "JTK_Cycle", side=1, line=7, cex=1.5, font = 2)
    mtext(text = "RAIN", side=2, line=7, cex=1.5, font = 2)
  })
  
  observeEvent(input$scatter_click_hogSample,{
    click <- isolate(input$scatter_click_hogSample)
    x <- log(isolate(JTK_CYCLEOutPut$df$ADJ.P))
    y <- log(isolate(rainOutPut$df$pVal))
    
    dist <- sqrt((click$x + x)^2 + (click$y + y)^2)
    if(!is.null(min(dist,na.rm = T))) {
      output <- which.min(dist)
      geneID(output)
      
    }else{
      output <- NULL
    }
    if(!is.null(output)){
      gene <- rownames(results[[1]])[output]
      updateSelectizeInput(session,"geneSel", selected = gene, options = list())
    } else{
      gene = "Please Select a Gene"
    }
  })
  
  # Main Scatter plot of Sampling comparisons
  output$subPlot_hogSample <- renderPlot({
    validate(
      need(any(hogsampled$Points != 0), 'Please Select Time Points for Analysis')
    )
    
    plotData <- data[[1]][geneID(),which(hogsampled$Points == 1)]
    tp <- c(18:65)[which(hogsampled$Points == 1)]
    vals <- as.vector(unlist(plotData))
    plot(tp,vals,"o", cex = 1, lwd = 2, pch = 16, col = 'orange', font = 2, font.lab = 2, xlim = c(17,66),
         main = paste0(rownames(data[[1]])[geneID()]),
         xlab = "ZT Time Point",
         ylab = "Expression")
    box(lwd = 3)
  })
  
  vennPlotsSet <- function(p, method){
    countsVENN <- vennCounts(fromList(p))
    
    #Unique to 1 Counts 
    uniqueTo1h <- which(countsVENN[,1] == 1 & countsVENN[,2] == 0)
    `1Count` <- countsVENN[uniqueTo1h,3]
    
    #Unique to 1 and Sampling 
    uniqueTo2andSampling <- which(countsVENN[,1] == 1 & countsVENN[,2] == 1)
    `1SCount` <- countsVENN[uniqueTo2andSampling,3]
    
    #Unique only to Sampling Only
    `SCount` <- countsVENN[which(countsVENN[,1] == 0 & countsVENN[,2] == 1),3]
    
    # Plot Euler Gram
    eulerr_options(font = 2)
    par(mar = c(25,25,25,5))
    v <- euler(c(`1h` = `1Count`,`Xh` = `SCount`,"1h&Xh"= `1SCount`))
    plot(v,
         fills = list(fill = c("grey90","orange","#377eb8"), alpha = 1),
         labels = list(col = "Black", font = 4, fontsize = 40),
         quantities = list(TRUE,col = "Black", font = 2, fontsize = 30),
         lty = 1:2,
         lwd = 6,
         edge = F,
         main = list(label = method, font = 2, fontsize = 10))
    
  }
  #JTK_Cycle venn Diagram
  output$jtkVenn_hogSample <- renderPlot({
    Genes <- rownames(resultsAdj[[3]])
    if(input$FC_hogSample != ""){
      genesFCThresXh <- Genes[foldChangeCalc(which(isolate(hogsampled$Points)==1)) > as.numeric(input$FC_hogSample)]
      genesFCThres1h <- Genes[foldChangeCalc(rep(T,48)) > as.numeric(input$FC_hogSample)]
    }else{
      genesFCThresXh <- Genes
      genesFCThres1h <- Genes
    }
    #JTK Genes Detected
    list1 <- rownames(resultsAdj[[3]])[resultsAdj[[3]][,1] < as.numeric(input$alpha_hogSample)]
    list1 <- Reduce(intersect, list(list1,genesFCThres1h))
    list2 <- rownames(resultsAdj[[3]])[JTK_CYCLEOutPut$df$BH.Q < as.numeric(input$alpha_hogSample)]
    list2 <- Reduce(intersect, list(list2,genesFCThresXh))
    vennPlotsSet(list(list1,list2), method = "JTK_Cycle")

  })
  #RAIN venn Diagram
  output$RAINVenn_hogSample <- renderPlot({
    Genes <- rownames(resultsAdj[[3]])
    if(input$FC_hogSample != ""){
      genesFCThresXh <- Genes[foldChangeCalc(which(isolate(hogsampled$Points==1))) > as.numeric(input$FC_hogSample)]
      genesFCThres1h <- Genes[foldChangeCalc(rep(T,48)) > as.numeric(input$FC_hogSample)]
    }else{
      genesFCThresXh <- Genes
      genesFCThres1h <- Genes
    }
    #RAIN Genes Detected
    list3 <- rownames(resultsAdj[[4]])[resultsAdj[[4]][,1] < as.numeric(input$alpha_hogSample)]
    list3 <- Reduce(intersect, list(list3,genesFCThres1h))
    list4 <- rownames(resultsAdj[[4]])[rainOutPut$df2$pVal < as.numeric(input$alpha_hogSample)]
    list4 <- Reduce(intersect, list(list4,genesFCThresXh))
    vennPlotsSet(list(list3,list4), method = "RAIN")
  })
  

  output$jtk_RAIN_Sel_Waveforms_hogSample <- renderPlot({
    Genes <- rownames(resultsAdj[[3]])
    if(input$FC_hogSample != ""){
      genesFCThresXh <- Genes[foldChangeCalc(which(isolate(hogsampled$Points)==1)) > as.numeric(input$FC_hogSample)]
      genesFCThres1h <- Genes[foldChangeCalc(rep(T,48)) > as.numeric(input$FC_hogSample)]
    }else{
      genesFCThresXh <- Genes
      genesFCThres1h <- Genes
    }
    #JTK Genes Detected
    list1 <- rownames(resultsAdj[[3]])[resultsAdj[[3]][,1] < as.numeric(input$alpha_hogSample)]
    list1 <- Reduce(intersect, list(list1,genesFCThres1h))
    list2 <- rownames(resultsAdj[[3]])[JTK_CYCLEOutPut$df$BH.Q < as.numeric(input$alpha_hogSample)]
    list2 <- Reduce(intersect, list(list2,genesFCThresXh))
    #RAIN Genes Detected
    list3 <- rownames(resultsAdj[[4]])[resultsAdj[[4]][,1] < as.numeric(input$alpha_hogSample)]
    list3 <- Reduce(intersect, list(list3,genesFCThres1h))
    list4 <- rownames(resultsAdj[[4]])[rainOutPut$df2$pVal < as.numeric(input$alpha_hogSample)]
    list4 <- Reduce(intersect, list(list4,genesFCThresXh))
    
    JTKList <- fromList(list(rownames(resultsAdj[[3]]),list1,list2))
    RAINList <- fromList(list(rownames(resultsAdj[[4]]),list3,list4))
    
    
    goi_JTK <- JTKList[geneID(),2:3]
    goi_RAIN <- RAINList[geneID(),2:3]
    
    if(goi_JTK[,1] == 0 & goi_JTK[,2] == 0){
      xJTK <- 1
    } else if(goi_JTK[,1] == 1 & goi_JTK[,2] == 0) {
      xJTK <- 3
    } else if(goi_JTK[,1] == 0 & goi_JTK[,2] == 1){
      xJTK <- 2
    } else  {
      xJTK <- 4
    }
    
    if(goi_RAIN[,1] == 0 & goi_RAIN[,2] == 0){
      xRAIN <- 6
    } else if(goi_RAIN[,1] == 1 & goi_RAIN[,2] == 0) {
      xRAIN <- 8
    } else if(goi_RAIN[,1] == 0 & goi_RAIN[,2] == 1){
      xRAIN <- 7
    } else  {
      xRAIN <- 9
    }
    
    jTKFreqCount <- vennCounts(JTKList)[5:8,4]
    rainFreqCount <- vennCounts(RAINList)[5:8,4]

    par(font.lab = 2, font = 2)
    plot(c(1,2,3,4,6,7,8,9),c(jTKFreqCount,rainFreqCount), pch=16, col = rep(c("grey","orange", "black", "black"),2), cex = 2, ylim = c(0,15000), axes = F, xlab = "", ylab = "Counts")
    text(x = 2.5,y = 12000, "JTK_Cycle", font = 2)
    text(x = 7.5,y = 12000, "RAIN", font = 2)
    text(x = 1:4,y = jTKFreqCount+1500, labels = jTKFreqCount, font = 2)
    text(x = 6:9,y = rainFreqCount+1500, labels = rainFreqCount, font = 2)
    abline(v=5, lwd = 3)
    segments(x0= c(1,2,3,4,6,7,8,9),y0 = rep(-1000,8),x1 = c(1,2,3,4,6,7,8,9),y = c(jTKFreqCount,rainFreqCount), lwd = 3,col = rep(c("grey","orange", "black", "black"),2))
    points(c(1,2,3,4,6,7,8,9),c(jTKFreqCount,rainFreqCount), pch=16, col = rep(c("grey","orange", "black", "black"),2),cex = 2)
    points(x = xJTK, y = 10, pch = 24, cex = 2, bg = "green", col = 'black')
    points(x = xRAIN, y = 10, pch = 24, cex = 2, bg = "green", col = 'black')
    axis(side = 1,at = c(1,2,3,4,6,7,8,9), labels = c("Non-Cyclers","Xh","1h","1h-Xh Intersection","Non-Cyclers","Xh","1h","1h-Xh Intersection"), font = 2)
    axis(2)
    legend("center", box.lwd = 2, pch = 24, legend = c("Gene of Interest"),col = 'black', pt.cex = 2, pt.lwd = 2,pt.bg = c('green'))
    box(lwd = 3)
    
  })
  
  FCforDataset <- reactiveValues()
  
  foldChangeCalc <- function(TP){
    FCs <- apply(data[[1]][,TP],1, function(x){
      return(abs(diff(range(x))))
    })
    return(FCs)
  }
  
  JTK_RAIN_waveformToPlot <- reactiveValues(genes = NULL)
  observeEvent(input$click_jtkRain_Sel_hogSample,{
    
    click <- input$click_jtkRain_Sel_hogSample
    selection <- c(1,2,3,4,6,7,8,9)[which.min((c(1,2,3,4,6,7,8,9)-click$x)^2)]
  
    Genes <- rownames(resultsAdj[[3]])
    if(input$FC_hogSample != ""){
    genesFCThresXh <- Genes[foldChangeCalc(which(isolate(hogsampled$Points)==1)) > as.numeric(input$FC_hogSample)]
    genesFCThres1h <- Genes[foldChangeCalc(rep(T,48)) > as.numeric(input$FC_hogSample)]
    }else{
      genesFCThresXh <- Genes
      genesFCThres1h <- Genes
    }
    #JTK Genes Detected
    list1 <- rownames(resultsAdj[[3]])[resultsAdj[[3]][,1] < as.numeric(input$alpha_hogSample)]
    list1 <- Reduce(intersect, list(list1,genesFCThres1h))
    list2 <- rownames(resultsAdj[[3]])[JTK_CYCLEOutPut$df$BH.Q < as.numeric(input$alpha_hogSample)]
    list2 <- Reduce(intersect, list(list2,genesFCThresXh))
    #RAIN Genes Detected
    list3 <- rownames(resultsAdj[[4]])[resultsAdj[[4]][,1] < as.numeric(input$alpha_hogSample)]
    list3 <- Reduce(intersect, list(list3,genesFCThres1h))
    list4 <- rownames(resultsAdj[[4]])[rainOutPut$df2$pVal < as.numeric(input$alpha_hogSample)]
    list4 <- Reduce(intersect, list(list4,genesFCThresXh))
    
    JTKList <- fromList(list(rownames(resultsAdj[[3]]),list1,list2))
    RAINList <- fromList(list(rownames(resultsAdj[[4]]),list3,list4))
    
    rownames(JTKList) <- Genes
    rownames(RAINList) <- Genes
    
    #decide if JTK or Cycling Genes to Be Plotted
    if(selection < 5){
      selDF <- JTKList
    }else{
      selDF <- RAINList
    }
   
    if(selection %in% c(1,6)){
      use <- which(selDF[,2] == 0 & selDF[,3]== 0)
    }else if(selection %in% c(2,7)){
      use <-  which(selDF[,2] == 0 & selDF[,3]== 1)
    }else if(selection %in% c(3,8)){
      use <-  which(selDF[,2] == 1 & selDF[,3]== 0)
    }else{
      use <- which(selDF[,2] == 1 & selDF[,3]== 1)
    }

     selectedGenesForPlotting <- rownames(selDF[use,])
     JTK_RAIN_waveformToPlot$genes <- selectedGenesForPlotting
     
     #set Up next and Previous plotting buttons to be enable or disabled
     if(any(hogsampled$Points!= 0) & length(JTK_RAIN_waveformToPlot$genes) > 0){
       initialPlottingJTKRAIN()
     }
  })
  
  jtkRainPlotCount <- reactiveVal(value = 1)
  output$waveforms_hogSample <- renderPlot({
    
    validate(
      need(length(JTK_RAIN_waveformToPlot$genes) > 0, message = "Please Select Genes For Plotting By Clicking On The Bar Graph Above")
    )
        
    
        #split gene list into groups of 20
        nums <- split(JTK_RAIN_waveformToPlot$genes, ceiling(seq_along(JTK_RAIN_waveformToPlot$genes)/20))
        genesPlot <- nums[[jtkRainPlotCount()]]
        
        par(font.lab = 2, font.axis = 2, mfrow = c(4,5), mar = c(2,2,2,2))
        for(i in genesPlot){
          dataToPlot <- data[[1]][JTK_RAIN_waveformToPlot$genes,]
          selectedGene <- which(i == rownames(dataToPlot))
          
          if(i == rownames(data[[1]])[geneID()]){
            cols <- "green"
          } else {
            cols <- "black"
          }
          
          plot(c(18:65),dataToPlot[i,], pch = 16, "o", col = cols, cex = 1, ylab = "Expression", xlab = "ZT Time", main = paste0(selectedGene," : ", i))
          if(any(hogsampled$Points!= 0)){
            points(c(18:65)[which(isolate(hogsampled$Points)==1)],dataToPlot[i,which(isolate(hogsampled$Points)==1)], pch = 16, "o", cex = 2, col = "orange")
          }
          box(lwd = 3, col = cols)
        }
  })
  
  initialPlottingJTKRAIN <- function(){
    jtkRainPlotCount(1)
    nums <- split(JTK_RAIN_waveformToPlot$genes, ceiling(seq_along(JTK_RAIN_waveformToPlot$genes)/20))
    
    if(jtkRainPlotCount() == length(nums)){
      shinyjs::disable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    } else if(jtkRainPlotCount() == 1){
      shinyjs::disable("previous_hogSample")
      shinyjs::enable("next_hogSample")
    }else{
      shinyjs::enable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    }
    
    if(length(nums) == 1){
      shinyjs::disable("previous_hogSample")
      shinyjs::disable("next_hogSample")
    }
    
    }
  
  observeEvent(input$previous_hogSample,{
    nums <- split(JTK_RAIN_waveformToPlot$genes, ceiling(seq_along(JTK_RAIN_waveformToPlot$genes)/20))
    if(jtkRainPlotCount()>1){
      jtkRainPlotCount(jtkRainPlotCount()-1)
    }
    
    if(jtkRainPlotCount() == length(nums)){
      shinyjs::disable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    } else if(jtkRainPlotCount() == 1){
      shinyjs::disable("previous_hogSample")
      shinyjs::enable("next_hogSample")
    }else{
      shinyjs::enable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    }
  })
  
  observeEvent(input$next_hogSample,{
    nums <- split(JTK_RAIN_waveformToPlot$genes, ceiling(seq_along(JTK_RAIN_waveformToPlot$genes)/20))
    if(jtkRainPlotCount() < length(nums)){
      jtkRainPlotCount(jtkRainPlotCount()+1)
    }
    
    if(jtkRainPlotCount() == length(nums)){
      shinyjs::disable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    } else if(jtkRainPlotCount() == 1){
      shinyjs::disable("previous_hogSample")
      shinyjs::enable("next_hogSample")
    }else{
      shinyjs::enable("next_hogSample")
      shinyjs::enable("previous_hogSample")
    }
  })
  
  observeEvent(input$alpha_hogSample,{
    JTK_RAIN_waveformToPlot$genes <- NULL
  })
  
  observeEvent(input$FC_hogSample,{
    JTK_RAIN_waveformToPlot$genes <- NULL
  })
  
  ##---------------------------- ON Search Button Pressed -----------------------------------
  
  observeEvent(input$geneSel,{
    geneID(which(rownames(data[[1]]) == input$geneSel))
  })
  ##----------------------------------------- ON CLOSE -----------------------------------
 
  session$onSessionEnded(function() {
    stopApp()
  })
  options(warn=-1)
  
  
}

shinyApp(ui = ui, server = server)
