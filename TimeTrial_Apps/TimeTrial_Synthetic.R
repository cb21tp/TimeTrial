#========================================================#
# Author: Elan Ness-Cohn                                 #
# Email: elanness-cohn2017@u.northwestern.edu            #
# Title: TimeTrial_SyntheticData                         #
# Code Produced: March 7, 2019                           #
#========================================================#

#Load Required Packages and Install if missing
reqPkgs <- c('ggplot2', 'plyr','tidyverse', 'gplots','matrixStats', 'shiny', 'shinythemes',"pastecs", "pROC", "shinyjs","htmltools","vembedr")
for(pkg in reqPkgs){
  if(!require(pkg, character.only=TRUE)){
    install.packages(pkg)
  }
}
lapply(reqPkgs,library, character.only = TRUE)

#setworking directory and Load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("App_Data/timeTrialSyntheticDataTimeSeries.RData")
load("App_Data/timeTrialSyntheticDataResults.RData")
load("App_Data/RawData.Rdata")

#run app
ui <- tagList(

  useShinyjs(),
  navbarPage(
    title = "TimeTrial: Interactive Application for the Design and Analysis of Transcriptomic Times-Series Data in Circadian Biology Research",
    
    position = "fixed-top",
    theme = shinytheme("sandstone"),
    
    tabPanel("Overview",
      column(width = 8,offset = 1,
             h1(strong("Synthetic Data"))),
      
      fluidRow(
          column(width = 8,offset = 2,
              h2(strong("TimeTrial: Interactive Application for the Design and Analysis of Transcriptomic Times-Series Data in Circadian Biology Research")),
              h3("Elan Ness-Cohn, Marta Iwanaszko, William Kath, Ravi Allada, and Rosemary Braun"),
              hr(),
              h4(strong("Introduction:")),
              p("TimeTrial is an interactive application for the design and optimization of omic time-series experiments to aid researchers 
                in developing their experiment design. Consisting of two interactive applications using both synthetic and biological data, 
                TimeTrial allows researchers to explore the effects of experimental design on signal shape, examine cycling detection 
                reproducibility across biological datasets, and optimize experimental design for cycle detection. TimeTrial is currently in beta."),
              p(strong("Watch our introductory video for TimeTrial training material."))),
          column(width = 8,offset = 2, align = "center",
                 #Replace src file with the Final TimeTrial Video
              #tags$video(id="video1", type = "video/mp4",src = "3DEmbedding.mp4", controls = "controls"),
              embed_vimeo("388290542") %>%
                div(class = "vembedr") %>%
                div(align = "center"),
              # img(src='Slide1.png', align = "center", width = "75%"),
              hr()),
          
          
          column(width = 8,offset = 2,
              h4(strong("The Data Set:"))),
          column(width = 8,offset = 2, align = "center",
              img(src='Figure1.png', align = "center", width = "75%")),
          column(width = 8,offset = 2,
                 br(),
              p("To comprehensively evaluate the performance of cycling detection methods for different patterns of temporal gene expression, 
                we synthetically generated datasets to define a valid comparison metric for quantifying algorithm performance. "
                ,strong("[A]"), "Our synthetic datasets consists of 240 different experimental expression sets with varying number of replicates (1, 2, 3), 
                sampling intervals (2h, 4h, 6h, or 8h), sampling lengths (24h, 48h, 72h, 96h), and noise levels (0, 10%, 20%, 30%, or 40%) 
                as a function of amplitude. ", strong("[B]"), "Each of the 240 experimental expression sets consisted of 11000 waveforms, 1000 each for 
                the 11 base waveforms. 7 waveforms were considered", strong("Cyclic:"), "Sine, Peak, Sawtooth, Linear Trend, Damped, Amplified, Contractile;
                and 4 were considered", strong("Non-Cyclic:") ,"Flat, Linear, Sigmoid, and Exponential. For the 1000 waveforms in each waveform group,
                variation in amplitude, phase, and shape were added to model variation in gene expression. Details of the implementation and 
                parameters can be found in the method section and supplement of the accompanying paper."),
              hr()),
          
          
          column(width = 8,offset = 2,
              h4(strong("Summary Plot:")),
              h5("Initial Comparison of Sampling Schemes and Methods"),
              p("The 240 synthetic datasets were processed by all four methods using the suggested parameters defined by their documentation, respectively.
                For methods that did not have built in functions for handling replicates, ARSER and BooteJTK, two different procedures common in the field 
                were used in parallel and tested: concatenation and averaging. Methods were scored based on their ability to correctly classify cycling 
                versus non-cycling waveforms as measured by the receiver operating characteristics (ROC) curve and area under the curve (AUC) score. 
                An AUC = 0.5 denotes the classification is no better than chance and an AUC = 1 denotes perfect classification.")),
          column(width = 8,offset = 2, align = "center",
                 img(src='Figure2.png', align = "center", width = "75%")),
          column(width = 8,offset = 2,
                 br(),
                 p("The AUC summary plots above, allows TimeTrial users to quickly compare different sampling schemes and methods against each other. 
                   Taking experimental cost into considerations, users can select the appropriate number of replicates, time intervals, and sampling length, 
                   to assess methods overall ability to detect cycling genes. Users can further assess the robustness of each method under a given sampling scheme when noise is introduced 
                   by comparing the AUC scores standard deviation in the lower plot. Methods sampling scheme pairs that show lower standard deviation values imply the cycling 
                   detection results are more stable when noise is introduced; while larger standard deviation values indicate results are less stable when noise is introduced."),
                 hr()),
          
          
          column(width = 8,offset = 2,
              h4(strong("Direct Comparison:")),
              h5("In-depth Comparison of Sampling Schemes and Methods"),
              p("Having performed an initial check of sampling scheme and method pairings using the AUC Summary Plots, TimeTrial also allows users to 
                perform a head-to-head comparison of sampling schemes and methods to determine how they impact the detection of cycling genes on a per 
                waveform shape basis using the interphase under the Direct Comparison Tab.", strong("It is important to note that for comparison purposes results are 
                reported as raw p-values, and have not been adjusted for multiple hypothesis testing. As such usage of thresholds indicated by this analysis 
                are only valid if p-values from methods are left unadjusted.")),
              p("Users can select a sampling scheme, method, and noise level in scheme A; and quickly duplicate the parameters in scheme A 
                 to scheme B. Users can then change one parameter at a time in scheme B to assess the impact on cycle detection."),
              br()),
          
          column(width = 8,offset = 2, align = "center",
                 img(src='Figure3.png', align = "center", width = "85%")),
          column(width = 8,offset = 2,
                 br(),
                 p("By comparing the reported ROC curves and AUC scores (LEFT), -log(p-value) distributions (MIDDLE), and correct classification percentage (RIGHT), 
                   users can determine the optimal sampling scheme and method for cycle detection on a per signal shape bases (i.e. cosine, 
                   peak, saw-tooth, etc.). "),
                 hr()),
          column(width = 8,offset = 2, align = "center",
                 img(src='Figure4.png', align = "center", width = "85%")),
          column(width = 8,offset = 2,
                 br(),
                 p("The ROC curve can be used to look at the specificity (true positive rate) and sensitivity (true negative rate). 
                    Clicking along this curve will adjust the p-value threshold to maintain the specificity and sensitivity indicated.
                    Users can alternatively set the p-value threshold by clicking on the -log(p-value) distribution plots.
                    Setting the p-value threshold will update the percent correct classification plot, which reports the percent of each waveform correctly 
                    classified as cycling or not-cycling as specified in", em("the Data Set"),"section."),
                  p("Ultimately, these plots are useful to allow users to determine how robust a sampling scheme and method are at separating specific cycling signals 
                    from non-cyclers. In this manor, users can use the determined p-value thresholds and inspect output on a per signal basis to help determine appropriate 
                    threshold cutoffs for defining separation between signal shapes for downstream analysis."),
                 hr()),
          column(width = 6,offset = 2,
                 img(src='Figure5.png', align = "left", width = "100%")
          ),
          column(width = 2,
                 tags$li("Waveforms in the upper right quadrant are identified as cycling in both scheme A and scheme B."),
                 tags$li("Waveforms in the lower left quadrant are identified as non-cycling in both scheme A and scheme B."),
                 tags$li("Waveforms in the upper left quadrant are identified as cycling in scheme B, but non-cycling in scheme A."),
                 tags$li("Waveforms in the lower right quadrant are identified as cycling in scheme A, but non-cycling in scheme B.")),
          
          column(width = 8,offset = 2,
                 br(),
                 p("Finally users can look at the distribution of -log(p-values) of each waveform in both sampling conditions. By clicking on a point in the scatter plot (highlighted red dot), 
                   users can further investigate the shape of waveform under the sampling scheme, as well as its expression across replicates (Right).") 
                 ),
          
          column(width = 10, offset = 1,
              hr(),
              p("Please cite TimeTrial <INSERT CITATION>"),
              p("© 2020 TimeTrial, Northwestern University"))
          
      )
      
    ),
    
    tabPanel("Summary Plot",
             tags$style(type="text/css", "body {padding-top: 120px;}"),
             sidebarPanel(width = 2,
               h3("Parameter Selection"),
               checkboxGroupInput('method', "Methods: ",choices = c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"), selected = c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"), inline = F),
               checkboxGroupInput('rep', "Number of Replicates: ",choices = c(1,2,3), selected = c(1,2,3), inline = F),
               checkboxGroupInput('length', "Sampling Length (h): ",choices = c(24,48,72,96), selected = c(24,48,72,96), inline = F),
               checkboxGroupInput('interval', "Sampling Interval (h): ",choices = c(2,4,6,8), selected = c(2,4,6,8), inline = F),
               checkboxGroupInput('noiseLevel', "Noise Level (% of Amplitude): ",choices = c(0,.1,.2,.3,.4), selected = c(0,.1,.2,.3,.4), inline = F)
               ),
             mainPanel(
               plotOutput(outputId = "mainPlot",width = "120%", height = 700), 
               plotOutput(outputId = "SDPlot",width = "120%", height = 400) 
             )
             ),
             
    tabPanel("Direct Comparison",
            
             absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                           draggable = F, top = 60, left = 0, right = 0, bottom = "auto",
                           width = "auto", height = "auto",style = "background-color: rgba(255,255,255,1) ; z-index: 9999",
                           fluidRow(
                             
                             column(2, offset = 0,
                                    h4(strong("Selected Scheme"), style="white-space:pre-wrap;", align = "left")),
                             
                             column(4,
                                    h5(strong(tagAppendAttributes(verbatimTextOutput("schemeA"), style="white-space:pre-wrap;", align = "left")))),
                             
                             column(4,
                                    h5(strong(tagAppendAttributes(verbatimTextOutput("schemeB"), style="white-space:pre-wrap;", align = "left")))),
                             
                             column(2,offset = 0,
                                    h6(""),
                                    actionButton(inputId = "update",label = "Go/Reset", align = "center",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    h6(""))
                             
                             ),
                           
                           div(id = 'demo', class="collapse in",
                           
                           column(12,
                           verticalLayout(
                             column(12,
                               h3("Scheme A"),
                               style = "background-color: rgba(222,78,60,0.5)",
                               flowLayout(radioButtons('methodSchemeA', "Method: ",choices = c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"), selected = "Arser_Avg"),
                               radioButtons('repSchemeA', "Number of Replicates: ",choices = c(1,2,3), selected = 1),
                               radioButtons('lengthSchemeA', "Sampling Length (h): ",choices = c(24,48,72,96), selected = 24),
                               radioButtons('intervalSchemeA', "Sampling Interval (h): ",choices = c(2,4,6,8), selected = 2),
                               radioButtons('noiseLevelSchemeA', "Noise Level  (% of Amplitude): ",choices = c(0,.1,.2,.3,.4), selected = 0)
                               )
                               ),
                             
                             column(12,align="left",
                                    h3(""),
                                    actionButton(inputId = "duplicate", label = "Duplicate Sampling Scheme A",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    h3("")),
                             
                             column(12,
                               h3("Scheme B"),
                               style = "background-color: rgba(0,0,0,0.2)",
                               flowLayout(
                               radioButtons('methodSchemeB', "Method: ",choices = c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"), selected = "Arser_Avg"),
                               radioButtons('repSchemeB', "Number of Replicates: ",choices = c(1,2,3), selected = 1),
                               radioButtons('lengthSchemeB', "Sampling Length (h): ",choices = c(24,48,72,96), selected = 24),
                               radioButtons('intervalSchemeB', "Sampling Interval (h): ",choices = c(2,4,6,8), selected = 2),
                               radioButtons('noiseLevelSchemeB', "Noise Level  (% of Amplitude): ",choices = c(0,.1,.2,.3,.4), selected = 0)
                               )
                               )),
                                  h3("")
                           
                           )
                           
                           
                           
                           )
                          )#end Absolute Panel
                           
                           ,
             
             hr(),
             
             fluidRow(
               column(4,align="center",
                    plotOutput(outputId = "ROC",width = "100%", height = 450,click = clickOpts(id ="plot_click")),
                    hr(),
                    plotOutput(outputId = "ROCLegend",width = "100%", height = 50)
             ),
             
             column(4, align="center",
                   plotOutput(outputId = "Hist1",width = "100%", height = 225,click = clickOpts(id ="plot_click_histA")),
                   plotOutput(outputId = "Hist2",width = "100%", height = 225,click = clickOpts(id ="plot_click_histB")),
                   hr(),
                   plotOutput(outputId = "HistLegend",width = "100%", height = 100)
             ),
             
             column(4, align="center",
                    plotOutput(outputId = "waveformHist",width = "100%", height = 450),
                    hr(),
                    plotOutput(outputId = "waveformHistLegend",width = "100%", height = 50)
                    
             )
             ),#End Fluid Row
             
            hr(),
             
            splitLayout(plotOutput(outputId = "Scatter",width = 650, height = 650,click = clickOpts(id ="scatter_click")),
                      
                        column(12,align="center",
                              h6(''),#add in spacer
                              wellPanel(h4("Selected Wave: "), 
                                        h4(strong(textOutput(outputId = "wave")))
                                    ),
                              plotOutput(outputId = "waveSchemeA",width = "100%", height = 300),
                              plotOutput(outputId = "waveSchemeB",width = "100%", height = 300)
                                    
                      )
             ),
            
            hr()
            
               )
             )
  
    )

server <- function(input, output, session){
  
  ## --------------------------- Get Data For Summary Panel---------------------------
  
  #create Data frame with the selected inputs
  Reviewdf <- 
    reactive({data[
    (data$Method %in% input$method &
       data$Rep %in% input$rep &
       data$Length %in% input$length &
       data$Rate %in% input$interval &
       data$Noise %in% input$noiseLevel),]
    })
  
  
  ##---------------------------- Main Summary Plot -----------------------------------
  output$mainPlot <- renderPlot({
    colours <- c("#E1DAAE","#FF934F", "#848FA2","#CC2D35","#2D3142","#058ED9")
    
    
    #Validate Inputs
    validate(
      need(input$method, 'Please choose at least 1 Method'),
      need(input$rep, 'Please choose at least 1 Number of Replicates'),
      need(input$length, 'Please choose at least 1 Sampling Length'),
      need(input$interval, 'Please choose at least 1 Sampling Interval'),
      need(input$noiseLevel, 'Please choose at least 1 Level of Noise')
    )
    
    #generate empty Plot  
    par(mfrow = c(1,1))
    plot(Reviewdf()$numSmps, Reviewdf()$AUC,
         col = 'black', 
         cex = 0,
         xlim = c(0,150),
         ylim = c(0.5,1),
         ylab = "AUC Score", 
         xlab = "Number of Samples", 
         main = "AUC Scores Across All Samples",
         cex.lab = 1.25,
         cex.axis = 1.5,
         font.lab = 2,
         font.axis = 2,
         cex.main = 2)
    box(lwd = 3)
    par(font = 2)
    
    #Methods Lengend
    legend(70, .7, 
           title = "Method", 
           legend=c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"),
           pt.bg = alpha(colours,0.7),
           col = colours,
           border = "black", 
           bty = "n", 
           pch = 21,
           cex = 2,
           pt.cex=2, 
           box.lwd = 0)
    
    #Noise Lengend
    legend(120, .7,
           title = "Noise (%)",
           legend=c(0,10,20,30,40),
           col = 'black',
           border = "black",
           cex = 2,
           bty = "n", 
           pch = 16, 
           pt.cex= .2+.25*(1:5), 
           box.lwd = 0)
    
    #Run Methods to Add Trend Line and/or Shaded min/Max to Graph
     colToUse <- as.numeric(levels(as.factor(Reviewdf()$col)))
     xvec <- seq(min(Reviewdf()$numSmps)-1,max(Reviewdf()$numSmps)+1,length=201)
     count <- 1
     if(any(c(length(input$rep)>1,length(input$length)>1,length(input$interval)>1))){
     for(i in input$method){
       lm1 <- nls(AUC ~ 1 -as.numeric(Method==i)*a1*exp(-b1*numSmps), data = Reviewdf(), start = list(a1= 1, b1=.0001))
       lines(xvec,predict(lm1,data.frame(numSmps=xvec, Method = rep(i,length(xvec)))),col = colours[c(colToUse)[count]], lwd = 5)
       count <- count +1
     }
     }
     
    
    #Add AUC Plot Points
    points(Reviewdf()$numSmps,Reviewdf()$AUC, 
           col = colours[Reviewdf()$col], 
           bg = alpha(colours[Reviewdf()$col],alpha = 0.7), 
           pch = 21,
           cex = .2+.25*Reviewdf()$pch,
           lwd = .1+.2*Reviewdf()$pch
    )
  })
  
  
  ##----------------------------------------------Standard Deviation plots -----------------------------------
  
  output$SDPlot <- renderPlot({
    
    #Validate Inputs
    validate(
      need(input$method, 'Please choose at least 1 Method'),
      need(input$rep, 'Please choose at least 1 Number of Replicates'),
      need(input$length, 'Please choose at least 1 Sampling Length'),
      need(input$interval, 'Please choose at least 1 Sampling Interval'),
      need(length(input$noiseLevel) > 1, 'Please choose at least 2 Levels of Noise')
    )
    
    #colors for plotting
    colours <- c("#E1DAAE","#FF934F", "#848FA2","#CC2D35","#2D3142","#058ED9")
    
    #compute Summary Stats for Data
    getSummaryStats <- split(Reviewdf(),f = interaction(Reviewdf()$Rate,Reviewdf()$Length,Reviewdf()$Method,Reviewdf()$Rep),drop = T)
    
    getSummaryStats <- lapply(getSummaryStats, function(x) {
      vals <- x$AUC
      output <- c(x$Rate[1],x$Length[1],x$Method[1],x$Rep[1], ((x$Length[1]/x$Rate[1]+1)*x$Rep[1]), x$col[1])
      names(output) <- c("Rate","Length","Method","Rep", "NumSmp", "col")
      c(output,stat.desc(vals))
    })
    SummaryStats <- as.data.frame(t(do.call(cbind.data.frame, getSummaryStats)))
    rownames(SummaryStats) <- NULL 
    SummaryStats <- SummaryStats[order(-as.numeric(SummaryStats$mean)),]
    SummaryStats <- apply(SummaryStats,2,as.vector)
    SummaryStats <- as.data.frame(SummaryStats)
    
    plot(as.numeric(as.vector(SummaryStats$NumSmp)),as.numeric(as.vector(SummaryStats$std.dev)), 
         #pch = 20+as.numeric(as.factor(SummaryStats$Method)),
         cex = 0,
         main = "Standard Deviation Between Noise Levels For a Given Sampling Scheme",
         xlab = "Number of Samples",
         ylab = "Standard Deviation",
         cex.lab = 1.25,
         cex.axis = 1.5,
         font.lab = 2,
         font.axis = 2,
         cex.main = 2)
    
    #calculate Exponential Trends
    colToUse <- as.numeric(levels(as.factor(SummaryStats$col)))
    SummaryStats$NumSmp <- as.numeric(as.vector(SummaryStats$NumSmp))
    SummaryStats$std.dev <- as.numeric(as.vector(SummaryStats$std.dev))
    
    xvec <- seq(min(SummaryStats$NumSmp),max(SummaryStats$NumSmp),length=201)
    count <- 1
    if(any(c(length(input$rep)>1,length(input$length)>1,length(input$interval)>1))){
    for(i in input$method){
      lm1 <- nls(std.dev ~ -as.numeric(Method==i)*a1*exp(-b1*NumSmp), data = SummaryStats, start = list(a1= 1, b1=.0001))
      lines(xvec,predict(lm1,data.frame(NumSmp=xvec, Method = rep(i,length(xvec)))),col = colours[c(colToUse)[count]], lwd = 5)
      count <- count +1
    }
    }
    
    #plot Points
    points(as.numeric(as.vector(SummaryStats$NumSmp)),as.numeric(as.vector(SummaryStats$std.dev)), 
           col = colours[as.numeric(as.vector(SummaryStats$col))], 
           bg = alpha(colours[as.numeric(as.vector(SummaryStats$col))],alpha = 0.7), 
           pch = 21)
    
    #Add Legend
    box(lwd = 3)
    par(font = 2)
    legend("topright",
           inset=c(0.01,0),
           title = "Method", 
           xpd = TRUE,
           legend=c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN"),
           pt.bg = alpha(colours,0.7),
           col = colours,
           border = "black", 
           bty = "n", 
           pch = 21,
           cex = 1.5,
           pt.cex= 2, 
           box.lwd = 0)
    
    })
  
  
  ## --------------------------- Get Data For Direct Comparison Panel---------------------------
    
  
  observeEvent(input$update, {
    toggle("demo", anim = TRUE)
  })
  
    data_A <- reactive({
      methods <- c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN")
      getData_A <- match(input$methodSchemeA,methods)
      #dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
      return(results[[getData_A]])
    })
    
    data_B <- reactive(({
      methods <- c("Arser_Avg","Arser_Concat","BooteJTK_Avg","BooteJTK_Concat","JTK_Cycle","RAIN")
      getData_B <- match(input$methodSchemeB,methods)
      #dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
      return(results[[getData_B]])
    }))
    
    #get ROC data object
    roc_obj_A <- reactive(({
      category <- c(rep(1,7000),rep(0,4000))
      dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
      roc(category, as.numeric(as.vector(unlist(isolate(data_A())[,dataNameSchemeA]))))
    }))
    
    #get ROC data object
    roc_obj_B <- reactive(({
      category <- c(rep(1,7000),rep(0,4000))
      dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
      roc(category, as.numeric(as.vector(unlist(data_B()[,dataNameSchemeB]))))
    }))
    
    #set initial threshold A
    threshold_A <- reactiveVal(which.max(isolate(roc_obj_A())$sensitivities+isolate(roc_obj_A())$specificities-1))
    #set initial threshold B
    threshold_B <- reactiveVal(which.max(isolate(roc_obj_B())$sensitivities+isolate(roc_obj_B())$specificities-1))
    
    #on updated scheme change set to "Best" ROC value 
    observeEvent({input$update
    },
    threshold_A(which.max(isolate(roc_obj_A())$sensitivities+isolate(roc_obj_A())$specificities-1))
    )
    
    #on updated scheme change set to "Best" ROC value 
    observeEvent({input$update
    },
    threshold_B(which.max(isolate(roc_obj_B())$sensitivities+isolate(roc_obj_B())$specificities-1))
    )
    
    #Duplicate Scheme A to Scheme B
    observeEvent(input$duplicate,{
    updateSelectInput(session = session, inputId = "methodSchemeB", selected = isolate(input$methodSchemeA))
    updateSelectInput(session = session, inputId = "intervalSchemeB", selected = isolate(input$intervalSchemeA))
    updateSelectInput(session = session, inputId = "lengthSchemeB", selected = isolate(input$lengthSchemeA))
    updateSelectInput(session = session, inputId = "noiseLevelSchemeB", selected = isolate(input$noiseLevelSchemeA))
    updateSelectInput(session = session, inputId = "repSchemeB", selected = isolate(input$repSchemeA))
    })
    
    #update Threshold A
    observeEvent(input$plot_click_histA,
                 {
                   click <- input$plot_click_histA
                   dist <- sqrt((exp(-click$x) - roc_obj_A()$thresholds)^2)
                   threshold_A(which.min(dist))
                   
                 })
    
    #update Threshold B
    observeEvent(input$plot_click_histB,{
      click <- input$plot_click_histB
      dist <- sqrt((exp(-click$x) - roc_obj_B()$thresholds)^2)
        
      threshold_B(which.min(dist))
      
    })
    
    
    #update Threshold A
    observeEvent(input$plot_click,
                 {
                   click <- input$plot_click
                   dist <- sqrt((click$x - roc_obj_A()$specificities)^2 + (click$y - roc_obj_A()$sensitivities)^2)
                   
                   if(min(dist) < 0.01) {
                     threshold_A(which.min(dist))
                   } else {
                     NULL
                   }
                   
                 })
    
    #update Threshold B
    observeEvent(input$plot_click,{
      click <- input$plot_click
      dist <- sqrt((click$x - roc_obj_B()$specificities)^2 + (click$y - roc_obj_B()$sensitivities)^2)
      
      if(min(dist) < 0.01) {
        threshold_B(which.min(dist))
      } 
      else{
        NULL
      }
      
      
    })
    
    
    #set initial threshold A
    waveFormSelection <- reactiveVal(1)
    
    #Get Index of Clicked Point on Scatter Plot
    observeEvent(input$scatter_click,{
      click <- input$scatter_click
      
      dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
      dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
      
      dist <- sqrt((click$x + log(data_A()[,dataNameSchemeA]))^2 + (click$y + log(data_B()[,dataNameSchemeB]))^2)
      if(min(dist,na.rm = T) < 1) {
        waveFormSelection(which.min(dist))
      }else{
        NULL
      }
      
    })
    
    output$schemeA <- renderText(paste("Scheme A:\n",input$methodSchemeA, "\nRep: ",input$repSchemeA, " | Length: ", input$lengthSchemeA, " | Interval: " , input$intervalSchemeA, " | Noise Level: ",input$noiseLevelSchemeA, sep = "")
    )
  
    output$schemeB <- renderText(paste("Scheme B:\n",input$methodSchemeB, "\nRep: ",input$repSchemeB, " | Length: ", input$lengthSchemeB, " | Interval: " , input$intervalSchemeB, " | Noise Level: ",input$noiseLevelSchemeB, sep = "")
    )
    
  ##----------------------------------------------Comparison Scatter plots -----------------------------------
  
  output$Scatter <- renderPlot({
    input$update
    waveFormSelection()
    threshold_A()
    threshold_B()
    
    isolate({
    waveFormCols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#FFFF33","#a65628","#cccccc", "#969696", "#636363", "#252525")
    waveNames <- c("sin", 'peak', 'saw', "lTrend", "damp", "amped", "contract", 'flat', "linear", "sigmoid", "decay")
    dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
    dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
    
    wave <- gsub(pattern = "_.*",replacement = "",rownames(data_A()))
    data <- data.frame(-log(as.numeric(data_A()[,dataNameSchemeA])),-log(as.numeric(data_B()[,dataNameSchemeB])), wave)
    colnames(data) <- c('schemeA','schemeB','wave')
    data$wave  <- with(data, reorder(wave, 1:11000))
  
    selPoint <- data[waveFormSelection(),] 
    ggplot(data,aes(x= schemeA, y=schemeB, col = wave))+
      geom_point(shape=16, size = 3)+
      theme(legend.position = "none", text = element_text(size = 16, face = "bold")) +
      ggtitle("Scheme A vs Scheme B: Scatter Plot ") + 
      labs(x="-log(pVal)  Scheme A", y= "-log(pVal) Scheme B") +
      scale_colour_manual(values = waveFormCols) +
      geom_vline(xintercept= -log(as.numeric(roc_obj_A()$thresholds[threshold_A()])), color = "red", size = 2) +
      geom_hline(yintercept= -log(as.numeric(roc_obj_B()$thresholds[threshold_B()])), color = "black", size = 2) +
      annotate("point", x = selPoint[,1], y = selPoint[,2], colour = "black", size = 7) +
      annotate("point", x = selPoint[,1], y = selPoint[,2], colour = waveFormCols[match(selPoint[,3],waveNames)], size = 4)
    })
  })
  
  ##----------------------------------------------Comparison ROC plot -----------------------------------
  
  output$ROC <- renderPlot({
    input$update
    threshold_A()
    threshold_B()
    
    isolate({
    par(cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5, font.lab=2, font.axis = 2)
    plot.roc(roc_obj_A(), legacy.axes=T, col = "red", main = "Scheme A vs Scheme B: ROC Plots", asp = 1, lwd = 5,type = "l", pch = 16, cex = 1, xlab = "FPR", ylab = "TPR")
    lines.roc(roc_obj_B(),col = "black", lwd = 5,type = "l", pch = 16, cex = 1)
    points(x = roc_obj_A()$specificities[threshold_A()], y = roc_obj_A()$sensitivities[threshold_A()], col = "red", pch = 16, cex = 2.5)
    points(x = roc_obj_B()$specificities[threshold_B()], y = roc_obj_B()$sensitivities[threshold_B()], col = "black", pch = 16, cex = 2.5)
    text(x = 0.4,y = .09,labels = paste0("Alpha Threshold: ",format(as.numeric(roc_obj_A()$thresholds[threshold_A()]), scientific = T)), col = "red", cex = 1.5, font = 2)
    text(x = 0.4,y = .035,labels = paste0("Alpha Threshold: ",format(as.numeric(roc_obj_B()$thresholds[threshold_B()]),scientific = T)), cex = 1.5, font = 2)
    })
  })
    
    output$ROCLegend <- renderPlot({
      dat <- data.frame(y=c(1,1), Scheme = c("Scheme B", " Scheme A"))
      
      p <- ggplot(dat, aes(x = Scheme, y=y, color = Scheme, fill = Scheme)) + 
        geom_histogram(position="dodge", stat="identity") +
        scale_fill_manual("",values = c("Red","black") ) +
        scale_color_manual("",values = c("red",'black'))
      
      p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), legend.title = element_text(size = 16, face = "bold"),legend.text = element_text(size = 16, face = "bold"),
                axis.title.y=element_blank(),legend.position="bottom",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

    })
  
  ##----------------------------------------------Waveform Counts plot -----------------------------------
  
  output$waveformHist <- renderPlot({
    input$update
    threshold_A()
    threshold_B()
    
    isolate({
    waveFormCols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#FFFF33","#a65628","#cccccc", "#969696", "#636363", "#252525")
    dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
    dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
    
    
    #get counts of values greater than alpha value and less than alpha value
    output_obj_A <- matrix(as.vector(as.numeric(c(as.numeric(data_A()[1:7000,dataNameSchemeA]) <= as.numeric(roc_obj_A()$thresholds[threshold_A()]) , as.numeric(data_A()[7001:11000,dataNameSchemeA]) > as.numeric(roc_obj_A()$thresholds[threshold_A()]) ))),nrow = 11,ncol = 1000,byrow = T)
    output_obj_B <- matrix(as.vector(as.numeric(c(as.numeric(data_B()[1:7000,dataNameSchemeB]) <= as.numeric(roc_obj_B()$thresholds[threshold_B()])  , as.numeric(data_B()[7001:11000,dataNameSchemeB]) > as.numeric(roc_obj_B()$thresholds[threshold_B()]) ))),nrow = 11,ncol = 1000,byrow = T)
    
    #get count of correcly classified wave forms
    schemeA <- apply(as.data.frame(output_obj_A),1,sum)
    schemeA <- schemeA/1000*100 #convert to percentage
    schemeB <- apply(as.data.frame(output_obj_B),1,sum)
    schemeB <- schemeB/1000*100 #convert to percentage
    
    #get wave names
    wave <- gsub(pattern = "_.*",replacement = "",rownames(data_A()))[seq(1,11000,1000)]
    classification <- c(schemeB,schemeA)
    scheme <- c(rep("Scheme A", 11), rep("Scheme B", 11))
    data <- data.frame(classification, scheme, rep(wave,2))
    colnames(data) <- c('correctClass',"scheme", "wave")
    data$wave <- fct_relevel(wave, wave)
    
    ggplot(data, aes(y = correctClass, x = wave , color = interaction(wave,scheme), fill = interaction(wave,scheme)), group = scheme) +
      geom_histogram(position="dodge", stat="identity") +
      theme(legend.position = "none",text = element_text(size = 16, face = "bold"), axis.text.x=element_text(angle=45, hjust=1)) +
      ggtitle("Scheme A vs Scheme B: \nClassification Accuracy") +
      labs(x="", y= "Correct Classification (%)") +
      scale_y_continuous(breaks = c(0,25,50,75,100)) +
      geom_text(aes(x = wave,  y = correctClass + 7,label = correctClass),position = position_dodge(width = 1), stat="identity",size=5,angle = 0,fontface="bold") +
      #scale_fill_manual(values = c( rep("white",length(waveFormCols)),waveFormCols) )+
      scale_fill_manual(values = c(alpha(waveFormCols,0.6),waveFormCols))+
      scale_color_manual(values = c(waveFormCols, waveFormCols)) +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(data$wave)), labels=rev(c("Sin", 'Peak', 'Saw', "Linear Trend", "Damped", "Amped", "Contractile", 'Flat', "Linear", "Sigmoid", "Exponential")))
    })
  })
    
  output$waveformHistLegend <- renderPlot({
    dat <- data.frame(y=c(1,1), Scheme = c("Scheme B", " Scheme A"))
  
    p <- ggplot(dat, aes(x = Scheme, y=y, color = Scheme, fill = Scheme)) + 
      geom_histogram(position="dodge", stat="identity") +
      scale_fill_manual("",values = c("black","white") ) +
      scale_color_manual("",values = c('black',"black"))
    
    p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(), legend.title = element_text(size = 16, face = "bold"),legend.text = element_text(size = 16, face = "bold"),
              axis.title.y=element_blank(),legend.position="bottom",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
  })
  
  ##----------------------------------------------Comparison Hist plots -----------------------------------
  
  #Hist Scheme A
  output$Hist1 <- renderPlot({
    input$update
    threshold_A()

    
    isolate({
    waveFormCols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#FFFF33","#a65628","#cccccc", "#969696", "#636363", "#252525")
    dataNameSchemeA <- paste(input$methodSchemeA, input$intervalSchemeA, input$lengthSchemeA, input$noiseLevelSchemeA,input$repSchemeA, sep = "_")
    
    
    #create Dataframe for plotting
    wave <- gsub(pattern = "_.*",replacement = "",rownames(data_A()))
    data_A <- data.frame(-log(as.numeric(data_A()[,dataNameSchemeA])),wave)
    colnames(data_A) <- c('pval',"wave")
    data_A$wave  <- with(data_A, reorder(wave, 1:11000))
  
    ggplot(data_A, aes(x = pval,fill = wave)) +
      geom_histogram(boundary = 0, binwidth = 0.5) + 
      theme(legend.position = "none", text = element_text(size = 16, face = "bold")) + 
      ggtitle(paste0("Scheme A: -log(pVal) Histogram")) + 
      #labs(x="-log(pVal)", y = "Count") +
      labs(x="", y = "Count") +
      scale_fill_manual(values = waveFormCols) +
      geom_vline(xintercept= -log(as.numeric(roc_obj_A()$thresholds[threshold_A()])), color = "red", size = 2)
    })
    
  })
  
  #Hist Scheme B
  output$Hist2 <- renderPlot({
    input$update
    threshold_B()
    
    isolate({
    waveFormCols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#FFFF33","#a65628","#cccccc", "#969696", "#636363", "#252525")
    dataNameSchemeB <- paste(input$methodSchemeB, input$intervalSchemeB, input$lengthSchemeB, input$noiseLevelSchemeB,input$repSchemeB, sep = "_")
  
  #create Dataframe for plotting
  wave <- gsub(pattern = "_.*",replacement = "",rownames(data_B()))
  data_B <- data.frame(-log(as.numeric(data_B()[,dataNameSchemeB])),wave)
  colnames(data_B) <- c('pval',"wave")
  data_B$wave  <- with(data_B, reorder(wave, 1:11000))
  
  ggplot(data_B, aes(x = pval,fill = wave)) +
    geom_histogram(boundary = 0, binwidth = .5) + 
    #theme(legend.text=element_text(size=16)) + 
    theme(legend.position = 'none', text = element_text(size = 16, face = "bold")) + 
    ggtitle(paste0("Scheme B: -log(pVal) Histogram")) + 
    labs(x="-log(pVal)", y = "Count") +
    scale_fill_manual(values = waveFormCols, name = "Wave Forms", labels = c("Sin", 'Peak', 'Saw', "Linear Trend", "Damped", "Amped", "Contractile", 'Flat', "Linear", "Sigmoid", "Exponential")) +
    geom_vline(xintercept= -log(as.numeric(roc_obj_B()$thresholds[threshold_B()])), color = "black", size = 2) 
    })
  })
  
  output$HistLegend <- renderPlot({
    waveFormCols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#FFFF33","#a65628","#cccccc", "#969696", "#636363", "#252525")
    WaveForms <- c("Sin", 'Peak', 'Saw', "Linear Trend", "Damped", "Amped", "Contractile", 'Flat', "Linear", "Sigmoid", "Exponential")
    dat <- data.frame(y=c(rep(0,11)), WaveForms)
    dat$WaveForms  <- with(dat, reorder(WaveForms, 1:11))
    
    p <- ggplot(dat, aes(x = WaveForms, y=y, fill = WaveForms)) + 
      geom_histogram(position="dodge", stat="identity") +
      scale_fill_manual("",values =  waveFormCols )
    
    p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(), legend.title = element_text(size = 14, face = "bold"),legend.text = element_text(size = 14, face = "bold"),
              axis.title.y=element_blank(),
              legend.direction = "horizontal",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    
    
  })
  
  ##----------------------------------------- Wave Form Plots  -----------------------------------
  output$wave <-renderText({
    input$update
    waveFormSelection()
    
    isolate({
    #Make the Row names Cleaner for the reader by Using Full Name Instead of Rownames shorthand
    names <- c("Sin", 'Peak', 'Saw', "Linear Trend", "Damped", "Amped", "Contractile", 'Flat', "Linear", "Sigmoid", "Exponential")
    names <- as.vector(matrix(rep(names,1000),nrow = 1000, byrow = T))
    x <- rep(1:1000,11)
    names <- paste(names,x, sep = " ")
    names[waveFormSelection()]
    })
  }) 
  
  output$waveSchemeA <- renderPlot({
    input$update
    waveFormSelection()
    
    isolate({
    par(cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5, font.lab=2, font.axis = 2)
    interval <- as.numeric(input$intervalSchemeA)
    length <- as.numeric(input$lengthSchemeA)
    NL <- 3*as.numeric(input$noiseLevelSchemeA)*10
    rep <- as.numeric(input$repSchemeA)
    
    interval_ALT <- as.numeric(input$intervalSchemeB)
    length_ALT <- as.numeric(input$lengthSchemeB)
    NL_ALT <- 3*as.numeric(input$noiseLevelSchemeB)*10
    rep_ALT <- as.numeric(input$repSchemeB)
    
    min <- min(df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1])
    min_ALT <- min(df.list[[1+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[2+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[3+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1])
    
    max <- max(df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1])
    max_ALT <- max(df.list[[1+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[2+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[3+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1])
    
    par(oma = c(4, 1, 1, 1))
    plot(seq(0,length,interval),df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1], 
         ylim = c(min(min,min_ALT),max(max,max_ALT)),
         xlim = c(0,96),
         main = "Sampling Scheme A: Wave Form",
         xlab = "Time Points",
         ylab = "Expression",
         "o", 
         pch = 16, 
         axes = F)
   
    axis(2)
    axis(1, at = seq(0,96,12), labels = seq(0,96,12))
    box()
    if(rep == 2){
      lines(seq(0,length,interval),df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1],"o", pch = 17, col= "blue")
    }
    
    if(rep == 3){
      lines(seq(0,length,interval),df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1],"o", pch = 17, col= "blue")
      lines(seq(0,length,interval),df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1],"o", pch = 15, col = "red")
    }
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom", c("Rep 1", "Rep 2", "Rep 3"), xpd = TRUE, horiz = TRUE, inset = c(0,0), bty = "n", pch = c(16, 17, 15), col = c("black","blue","red"), cex = 1.5, text.font = 2)
    
    
  })
    })
  
  output$waveSchemeB <- renderPlot({
    input$update
    waveFormSelection()
    
    isolate({
    par(cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5, font.lab=2, font.axis = 2)
    interval <- as.numeric(input$intervalSchemeB)
    length <- as.numeric(input$lengthSchemeB)
    NL <- 3*as.numeric(input$noiseLevelSchemeB)*10
    rep <- as.numeric(input$repSchemeB)
    
    interval_ALT <- as.numeric(input$intervalSchemeA)
    length_ALT <- as.numeric(input$lengthSchemeA)
    NL_ALT <- 3*as.numeric(input$noiseLevelSchemeA)*10
    rep_ALT <- as.numeric(input$repSchemeA)
    
    min <- min(df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1])
    min_ALT <- min(df.list[[1+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[2+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[3+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1])
    
    max <- max(df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1])
    max_ALT <- max(df.list[[1+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[2+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1], df.list[[3+NL_ALT]][waveFormSelection(),seq(0,length_ALT,interval_ALT)/2+1])
    
    par(oma = c(4, 1, 1, 1))
    plot(seq(0,length,interval),df.list[[1+NL]][waveFormSelection(),seq(0,length,interval)/2+1],
         ylim = c(min(min,min_ALT),max(max,max_ALT)),
         xlim = c(0,96),
         main = "Sampling Scheme B: Wave Form",
         xlab = "Time Points",
         ylab = "Expression",
         pch = 16,
         "o",
         axes = F)
    axis(2)
    axis(1, at = seq(0,96,12), labels = seq(0,96,12))
    box()
    
    if(rep == 2){
        lines(seq(0,length,interval),df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], pch = 17,"o", col= "blue")
    }
    
    if(rep == 3){
      lines(seq(0,length,interval),df.list[[2+NL]][waveFormSelection(),seq(0,length,interval)/2+1], pch = 17,"o", col= "blue")
      lines(seq(0,length,interval),df.list[[3+NL]][waveFormSelection(),seq(0,length,interval)/2+1], pch = 15,"o", col = "red")
    }
    
   
    })
  })
  
    ##----------------------------------------- ON CLOSE -----------------------------------
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
}
shinyApp(ui = ui, server = server)