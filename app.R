##load required packages
library(shiny)
library(shinydashboard)
##library(shinydashboardPlus)
library(shinyWidgets)
library(shinybusy)
library(agricolae)
library(DT)
library(plotly)
library(plotrix)
library(openxlsx)
library(ggalt)
library(ggpubr)
##library(ggpmisc)
library(tidyverse)
library(reshape2)
##library(rlang)
library(relaimpo)
library(randomForest)
library(shinyFeedback)
library(ggcorrplot)
library(rstatix)
library(scales)
library(lubridate)
library(validate)
library(gmodels)
library(caTools)
library(rpart)
library(C50)
library(rpart.plot)
library(e1071)
##library(DMwR)
library(naivebayes)
library(d3r)
library(neuralnet)
library(MASS)
library(splines)
library(janitor)
##NEW
library(broom)
##library(RWeka)
library(partykit)
##library(ggbiplot)
###library(klaR)
##library(corrplot)

rbrewerpallist<-c(
  "greys",
  "magma","inferno","plasma","viridis","cividis",
  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", ##qual pal
  "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", ##div pal
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", 
  "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd" ##seq pal
)
typelist<-1:5
extralist<-c(0:11,100:111)

inline_t<-"display: inline-block;vertical-align:top; width: 140px;"
inline_smallt<-"display: inline-block;vertical-align:top; width: 90px;"

skins<-c("blue", "blue-light", "black", "black-light", 
         "purple", "purple-light", "green", "green-light", 
         "red", "red-light", "yellow", "yellow-light")

options(shiny.sanitize.errors = FALSE) 

##shinyUI(##fluidPage(
ui<- ##dashboardPagePlus(skin= "green",
                       dashboardPage(skin= "green",
                       header<-dashboardHeader(title="Exploratory Analysis and Visualization App",
                                                   titleWidth = 400),
                       sidebar<- dashboardSidebar(width = 300,
                                                  tags$head(tags$style('{color:red;}')),
                                                  tags$head(
                                                    tags$style(
                                                      HTML(".shiny-notification {position:fixed;top: calc(25%);left: calc(40%);font-size: 30px;}"
                                                      )
                                                    )
                                                  ),
                                                  conditionalPanel(condition = "input.condition==1",
                                                                   div(style="display: inline-block;vertical-align:top; width: 160px;",
                                                                       prettyRadioButtons('file_type',"File type",
                                                                                          choiceNames = list("csv",
                                                                                                             "excel"),
                                                                                          choiceValues = list("csv",
                                                                                                              "excel"),
                                                                                          shape = "square",
                                                                                          ##bigger=TRUE,
                                                                                          outline=TRUE,
                                                                                          inline=TRUE,
                                                                                          selected = "csv")),
                                                                   
                                                                   div(style="display: inline-block;vertical-align:top; width: 100px;",
                                                                       conditionalPanel(condition = "input.file_type=='excel'",
                                                                                        numericInput("sheet_num","Sheet",value=1,min=1, max=10, step=1))),
                                                                   fileInput('file1', 'Upload data file to get started...',
                                                                             accept=c('.xls',
                                                                                      '.xlsx',
                                                                                      'text/csv', 
                                                                                      'text/comma-separated-values,text/plain', 
                                                                                      '.csv')),
                                                                   prettyRadioButtons('subtrans',"Select one:",
                                                                                      choiceNames = list("Subset",
                                                                                                         "Transform"),
                                                                                      choiceValues = list("sub",
                                                                                                          "trans"),
                                                                                      shape = "square",
                                                                                      ##bigger=TRUE,
                                                                                      outline=TRUE,
                                                                                      inline=TRUE,
                                                                                      selected = "sub"),
                                                                   conditionalPanel(condition = "input.subtrans =='trans'",
                                                                                    div(style=inline_t,
                                                                                        selectInput('factor_col', 'To factor:', 
                                                                                                    choices = names(df), 
                                                                                                    selected = "", 
                                                                                                    multiple = TRUE)),
                                                                                    div(style=inline_t,
                                                                                        selectInput('numeric_col', 'To numeric:', 
                                                                                                    choices = names(df), 
                                                                                                    selected = "", 
                                                                                                    multiple = TRUE)),
                                                                                    div(style=inline_t,
                                                                                        selectInput('out_group', 'Outlier rmvl by factor group(s):', 
                                                                                                    choices = names(df), 
                                                                                                    selected = "", 
                                                                                                    multiple = TRUE)),
                                                                                    div(style=inline_t,
                                                                                        selectInput('out_var', 'Outlier rmvl by numeric var(s):', 
                                                                                                    choices = names(df), 
                                                                                                    selected = "", 
                                                                                                    multiple = TRUE)),
                                                                                    div(style=inline_t,
                                                                                        radioButtons('imp_dat', "Impute NA's?",
                                                                                                     choices=c(Yes="impu",
                                                                                                               No= "no_impu"),
                                                                                                     inline=TRUE,
                                                                                                     selected="no_impu")),
                                                                                    div(style=inline_t,
                                                                                        radioButtons('norm_scale', "Normalize data?",
                                                                                                     choices=c(Yes="nrmlz",
                                                                                                               No= "no_nrmlz"),
                                                                                                     inline=TRUE,
                                                                                                     selected="no_nrmlz")),
                                                                                    column(12,align = "left",offset = 0,
                                                                                           tags$h5(style="font-style:bold;","Note: Choosing to normalize data will automatically  
                                                                                           impute any missing values values"))),
                                                                   hr(),
                                                                   conditionalPanel(condition = "input.subtrans =='sub'",
                                                                                    
                                                                                    selectInput("rmv_col","Remove COLUMNS from dataframe",
                                                                                                choices = names(df), 
                                                                                                selected = "", 
                                                                                                multiple = TRUE),
                                                                                    radioButtons('subbingdat',"Select/remove ROWS from dataframe",
                                                                                                 choices = c(None="noselect",
                                                                                                             Select="select",
                                                                                                             Remove="remove"
                                                                                                 ),
                                                                                                 inline = "TRUE",
                                                                                                 selected="noselect"),
                                                                                    conditionalPanel(condition = "input.subbingdat !='noselect'",
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('subcolname', 'Subset data by col...',"",
                                                                                                                     ##choices = c(None = '.', names(df)),
                                                                                                                     selected = ".")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('w_names0','Select unique names', 
                                                                                                                     choices = names(df), 
                                                                                                                     selected = "", 
                                                                                                                     multiple = TRUE))),
                                                                                    conditionalPanel(condition = "input.subbingdat =='remove'",
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('subcol2', 'Subset data by col...',"",
                                                                                                                     ##choices = c(None = '.', names(df)),
                                                                                                                     selected = ".")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('w_names7','Select unique names', 
                                                                                                                     choices = names(df), 
                                                                                                                     selected = "", 
                                                                                                                     multiple = TRUE)),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('subcol3', 'Subset data by col...',"",
                                                                                                                     ##choices = c(None = '.', names(df)),
                                                                                                                     selected = ".")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('w_names8','Select unique names', 
                                                                                                                     choices = names(df), 
                                                                                                                     selected = "", 
                                                                                                                     multiple = TRUE))))),
                                                  add_busy_bar(timeout=750, color = "yellow", height = "10px", centered =TRUE),
                                                  add_busy_spinner(spin = "double-bounce", color = "#112446",
                                                                   timeout = 100, position = "bottom-right", onstart = TRUE, margins = c(10, 10),
                                                                   height = "50px", width = "50px"),
                                                  ##conditionalPanel(condition="input.condition==3 ||input.condition==9",
                                                  conditionalPanel(condition= "input.condition==9",
                                                                   prettyRadioButtons('features','Show/hide:',
                                                                                      choiceNames = list("Graphing features",
                                                                                                         ##"Subset/transform data",
                                                                                                         "More graph features(labels)",
                                                                                                         "Unnecessarily more features(labels)",
                                                                                                         "Too many features(size)"),
                                                                                      choiceValues =
                                                                                        list("graphing", 
                                                                                             ##"subset", 
                                                                                             "mgf", "unnec", "emgf"),
                                                                                      shape = "square",
                                                                                      ##bigger=TRUE,
                                                                                      outline=TRUE,
                                                                                      inline = FALSE,
                                                                                      selected="graphing"),
                                                                   
                                                                   prettyRadioButtons('plotting','Graph type:',
                                                                                      choiceNames = list("Bar",
                                                                                                         "Box",
                                                                                                         "Line",
                                                                                                         "Heat",
                                                                                                         "Scatter",
                                                                                                         "Corr matrix"),
                                                                                      choiceValues =
                                                                                        list("bar_plot", 
                                                                                             "box_plot",
                                                                                             "line_plot", 
                                                                                             "heat_m",
                                                                                             "scat_plot",
                                                                                             "corrmatrix"),
                                                                                      shape = "round",
                                                                                      outline=TRUE,
                                                                                      inline = TRUE,
                                                                                      selected="bar_plot"),
                                                                   conditionalPanel(condition="input.plotting=='line_plot'",
                                                                                    radioButtons('stagger',"Line graph view",
                                                                                                 choiceNames = list("Overlap",
                                                                                                                    "Seperate",
                                                                                                                    "Dates"),
                                                                                                 choiceValues = list("overlapline", "sepline","datetime"),
                                                                                                 inline = TRUE,
                                                                                                 selected="overlapline"),
                                                                                    div(style="display: inline-block;vertical-align:top; width: 160px;",
                                                                                        radioButtons('linetype2',"Line Type",
                                                                                                     choiceNames = list("loess",
                                                                                                                        "linear"),
                                                                                                     choiceValues = list("loess", "linear"),
                                                                                                     inline = TRUE,
                                                                                                     selected="linear")),
                                                                                    div(style="display: inline-block;vertical-align:top; width: 100px;",
                                                                                        conditionalPanel(condition = "input.linetype2=='loess'",
                                                                                                         numericInput("degrees","Degrees",value=3,min=1, step=1)))),
                                                                   conditionalPanel(condition = "input.features=='graphing'",
                                                                                    prettyRadioButtons('featsss',"Select plot features:",
                                                                                                       choiceNames = list("Plot textboxes",
                                                                                                                          "Label,size,color"),
                                                                                                       choiceValues = list("plotfeat",
                                                                                                                           "lsc"),
                                                                                                       shape = "square",
                                                                                                       ##bigger=TRUE,
                                                                                                       outline=TRUE,
                                                                                                       inline=TRUE,
                                                                                                       selected = "plotfeat"),
                                                                                    conditionalPanel(condition = "input.featsss=='plotfeat'",
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('ycol', 'Y-axis:',"",
                                                                                                                     selected = "")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('xcol', 'X-axis',"",
                                                                                                                     selected = "")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('zcol', 'Grouping:',"", 
                                                                                                                     selected = "")),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('facet_row', 'Facet Row:', 
                                                                                                                     choices = c(None = '.', names(df)), 
                                                                                                                     selected = ".", 
                                                                                                                     multiple = TRUE)),
                                                                                                     radioButtons('sigs',"Letters or symbols sig. dif.",
                                                                                                                  choiceNames = list("Tky HSD",
                                                                                                                                     "Ttest",
                                                                                                                                     "T sym only",
                                                                                                                                     "None"),
                                                                                                                  choiceValues =
                                                                                                                    list("sigletters", "sigpandsym", "sigsym", "sblank"),
                                                                                                                  inline = "TRUE",
                                                                                                                  selected="sigpandsym"),
                                                                                                     conditionalPanel(condition = "input.sigs=='sigletters'",
                                                                                                                      shinyWidgets::sliderTextInput(inputId = "choice_p", 
                                                                                                                                                    label = "Choose p-value", 
                                                                                                                                                    choices = c(0.01,0.05,0.1,0.2),
                                                                                                                                                    selected = 0.1)),
                                                                                                     conditionalPanel(condition = "input.sigs!='sigletters'",
                                                                                                                      textInput('stats',"Ttest reference group comparison:", value=".all.", 
                                                                                                                                placeholder = "Leave blank for no symbols"))),
                                                                                    conditionalPanel(condition = "input.plotting=='heat_m'",
                                                                                                     column(12,align = "left",offset = 0, 
                                                                                                            tags$h5(style="font-style:bold;",
                                                                                                                    "For heatmap, 'Grouping' becomes 'Z variable'"),
                                                                                                            tags$h5(style="font-style:bold;",
                                                                                                                    "For Scatter3D, 'Facet Row' becomes '4D color'")
                                                                                                     ),
                                                                                                     prettyRadioButtons('mapvals',"Data to view in heat map:",
                                                                                                                        choiceNames = list("Raw data",
                                                                                                                                           "Raw data 3D",
                                                                                                                                           "Interpolated data",
                                                                                                                                           "Interp data 3D",
                                                                                                                                           "Scatter 3D "),
                                                                                                                        choiceValues = list("raw values",
                                                                                                                                            "rd3d",
                                                                                                                                            "interpolated",
                                                                                                                                            "inter_3d",
                                                                                                                                            "scatter_3d"),
                                                                                                                        shape = "curve",
                                                                                                                        ##bigger=TRUE,
                                                                                                                        outline=TRUE,
                                                                                                                        inline=FALSE,
                                                                                                                        selected = "raw values"),
                                                                                                     selectInput('wcol', 'Heat map tile labels:',"", 
                                                                                                                 selected = "")),
                                                                                    
                                                                                    ##div(style=inline_t,
                                                                                    
                                                                                    conditionalPanel(condition = "input.featsss=='lsc'",
                                                                                                     div(style=inline_t,
                                                                                                         numericInput('sigheight',"Label height (default=0):", value=0)),
                                                                                                     div(style=inline_t,
                                                                                                         numericInput('sigsize',"Adjust signif. label size:", value="5", min=2, max=50)),
                                                                                                     div(style=inline_t,
                                                                                                         numericInput('adjsize',"Adjust plot text size:", value="12", min=1, max=30)),
                                                                                                     div(style=inline_t,
                                                                                                         selectInput('colorsss', "Change plot color:",
                                                                                                                     choices = c(None = '.',rbrewerpallist),
                                                                                                                     selected = "viridis")),
                                                                                                     numericInput('h_line',"Add horizontal line on graph:", value="0", min=0, max=100))),
                                                                   conditionalPanel(condition = "input.features=='mgf'",
                                                                                    textInput("goodtitle","New title:", value = ""), 
                                                                                    div(style=inline_t,
                                                                                        textInput("xname","Rename x-axis:", value = "")), 
                                                                                    div(style=inline_t,
                                                                                        textInput("yname","Rename y-axis:", value = "")),
                                                                                    radioButtons("addlabel", "Add barplot labels (only for non-facets and non-grouping plots)", 
                                                                                                 choices=c(None="nonelab",
                                                                                                           Percent= "pctlab",
                                                                                                           Means= "meanlab"),
                                                                                                 inline=TRUE,
                                                                                                 selected="nonelab"),
                                                                                    conditionalPanel(condition="input.addlabel!='nonelab'",
                                                                                                     ##checkboxInput("addlabel", "Add pct(%) labels (only for non-facets)", value=FALSE),
                                                                                                     div(style=inline_t,
                                                                                                         numericInput('labelheight',"Label height:", value="2", min=-1000, max=9999)),
                                                                                                     div(style=inline_t,
                                                                                                         numericInput('pctsize',"Label size:", value="4", min=-1000, max=9999)),
                                                                                                     radioButtons("pctorder","If pct(%) incorrect, select best answer below to recalculate:",
                                                                                                                  choices = c(control_starts_with_abcdef...="abc",
                                                                                                                              control_starts_with_...uvwxyz="xyz"),
                                                                                                                  selected="xyz"))),
                                                                   conditionalPanel(condition = "input.features=='unnec'",
                                                                                    radioButtons('errbar',"Error bar type:",
                                                                                                 choices = c(None="nobar",
                                                                                                             Sterr="serr",
                                                                                                             Stdev="sdev"),
                                                                                                 inline = "TRUE",
                                                                                                 selected="serr"),
                                                                                    hr(),
                                                                                    radioButtons('alpha',"X-axis order by name:",
                                                                                                 choices = c(DForder="order",
                                                                                                             Ascend="asc",
                                                                                                             Descend="desc",
                                                                                                             Yval_asc="asc_val",
                                                                                                             Yval_desc="desc_val"
                                                                                                 ),
                                                                                                 inline = "TRUE",
                                                                                                 selected="order"),
                                                                                    hr(),
                                                                                    radioButtons('sci','Y-scale label type:',
                                                                                                 choices= c(Default="default",
                                                                                                            Pct="percent",
                                                                                                            Scientific="scinota",
                                                                                                            Log="log"),
                                                                                                 inline= TRUE,
                                                                                                 selected= "default"),
                                                                                    hr(),
                                                                                    div(style=inline_smallt,
                                                                                        checkboxInput("linelab", "Lineplot labels/violin plot", value=TRUE)),
                                                                                    div(style=inline_smallt,
                                                                                        checkboxInput("gpoints", "Add line/boxplot points?", value=FALSE)),
                                                                                    div(style=inline_smallt,
                                                                                        checkboxInput("addleg", "Remove Legend?", value=FALSE)),
                                                                                    hr(),
                                                                                    div(style=inline_t,
                                                                                        checkboxInput("theming", "Change plot background?", value=FALSE)),
                                                                                    div(style=inline_t,
                                                                                        checkboxInput("axisor","Flip x and y axis?", value=FALSE))),
                                                                   conditionalPanel(condition = "input.features=='emgf'",
                                                                                    div(style=inline_t,
                                                                                        numericInput("ymin","Ymin",value=NULL)),
                                                                                    div(style=inline_t,
                                                                                        numericInput("ymax","Ymax", value=NULL)),
                                                                                    radioButtons('scaling',"Facet wrap scaling:",
                                                                                                 choices = c(Na="fixed",
                                                                                                             Free="free",
                                                                                                             Free_x="free_x",
                                                                                                             Free_y="free_y"),
                                                                                                 inline = TRUE,
                                                                                                 selected="free_y"),
                                                                                    sliderInput('wrap', 'Wrap x-axis label', 
                                                                                                min = 1, max = 25, value = 10), ##, width = "200px"),
                                                                                    sliderInput('adjangle', 'Adjust angle of x-axis text', 
                                                                                                min = 0, max = 90, value = 0), 
                                                                                    sliderInput('plotWidth', 'Width of plot (in pixels)', 
                                                                                                min = 100, max = 1600, value = 1000),
                                                                                    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                                                                                min = 100, max = 1200, value = 600))),
                                                  
                                                  conditionalPanel(condition="input.condition==5",
                                                                   column(12,align = "left",offset = 0, 
                                                                          tags$h4(style="font-style:bold;","The correlation plot is a visual display of a correlation 
                                                                   matrix of all the numeric data in our dataset. Can only be used for data in wide format."
                                                                          ), 
                                                                          tags$br(),
                                                                          tags$h5(style="font-style:bold;"," 1= a strong positive correlation."
                                                                          ), 
                                                                          tags$h5(style="font-style:bold;"," 0= no correlation."
                                                                          ), 
                                                                          tags$h5(style="font-style:bold;","-1= a strong negative correlation."
                                                                          ))), 
                                                  conditionalPanel(condition="input.condition==4",
                                                                   
                                                                   ##column(12,align = "left",offset = 0, 
                                                                   ##tags$h5(style="font-style:bold;","Summarize your data")),
                                                                   column(12,align = "left",offset = 0,
                                                                          tags$h5(style="font-style:bold;","Convert data from wide format to 
                                                                                  long format which can then be used with the line graph")),
                                                                   prettyRadioButtons('datatype','Summarize data or convert to long datafile?:',
                                                                                      choiceNames = list("Summarize",
                                                                                                         "Long Data File"),
                                                                                      choiceValues =
                                                                                        list("summ_dat", "long_dat"),
                                                                                      shape = "square",
                                                                                      outline=TRUE,
                                                                                      inline = FALSE,
                                                                                      selected="summ_dat"),
                                                                   selectInput('groupnames', 'Select group name(s) to summarize:', 
                                                                               choices = c(None = '.', names(df)), 
                                                                               selected = ".", 
                                                                               multiple = TRUE),
                                                                   selectInput('varnames', 'Select variable name(s) to summarize:', 
                                                                               choices = c(None = '.', names(df)), 
                                                                               selected = ".", 
                                                                               multiple = TRUE),
                                                                   radioButtons("pctnut","Include pct diff compared to control? (Note: does not work for multiple groups)", 
                                                                                choices= c(Yes= 'yes',
                                                                                           No= 'no'),
                                                                                inline=TRUE,
                                                                                selected= "no"),
                                                                   conditionalPanel(condition="input.pctnut=='yes'",
                                                                                    checkboxInput('convlong','Convert to long DF for line graphing?', 
                                                                                                  value= TRUE),
                                                                                    selectInput('morenames', 'Select variables',"", 
                                                                                                ##choices = c(None = '.', names(dat4)), 
                                                                                                "",##selected = ".", 
                                                                                                multiple = TRUE),
                                                                                    radioButtons("pctcalcor","Is control in data column first or last?",
                                                                                                 choices=c(First="controlfirst", Last="controllast"),
                                                                                                 inline= TRUE,
                                                                                                 selected= "controllast")),
                                                                   checkboxInput("meansonly", "Check for means only (no sd,se,cv)", value=TRUE),
                                                                   column(12,align = "center",offset = 0, 
                                                                          ##       actionButton("newsummdata", label= "View table"),
                                                                          downloadButton('summarized_datafile','Download Summary Data'))
                                                                   ##downloadButton('converted_datafile','Download Long Data'))),
                                                  ),
                                                  conditionalPanel(condition="input.condition == 2 || input.condition == 6",
                                                                   column(12,align = "left",offset = 0,
                                                                          tags$h5(style="font-style:bold;color:red;size:14;",
                                                                                  "This portion is still under development. All model features may not be fully functional")),
                                                                   prettyRadioButtons('model_type','Model Type:',
                                                                                      choiceNames = list("LM Regression",
                                                                                                         "k-NN",
                                                                                                         "Naive Bayes  ",
                                                                                                         "Decision Tree",
                                                                                                         "Ripp Algorith",
                                                                                                         "Class/Rg Tree",
                                                                                                         "Random Forest",
                                                                                                         "Artifical NN ",
                                                                                                         "Support VM   "
                                                                                      ),
                                                                                      choiceValues =
                                                                                        list("lmrgr",
                                                                                             "knn","naive_b",
                                                                                             "dt_c50","jrip","rparty",
                                                                                             "rand_for","neur_n",
                                                                                             "svm"),
                                                                                      shape = "square",
                                                                                      outline=TRUE,
                                                                                      inline = TRUE,
                                                                                      selected="rparty"),
                                                                   numericInput('seed_ran',"Set randomization number", value=9999),
                                                                   column(12,align = "left",offset = 0, 
                                                                          tags$h5(style="font-style:bold;","Select 1 outcome variable. 
                                                                                  Ex: lsa (leaf surface area)")),
                                                                   div(style=inline_t,
                                                                       selectInput('outcomevar', 'Select Outcome Variable:',
                                                                                   "",
                                                                                   ##choices = names(df), 
                                                                                   selected = "")),
                                                                   div(style=inline_t,
                                                                       selectInput('indepvar', 'Select Independent Variable(s):', 
                                                                                   choices = c(None = '.', names(df)), 
                                                                                   selected = ".", 
                                                                                   multiple = TRUE)),
                                                                   conditionalPanel(condition = "input.model_type=='rparty'",
                                                                                    div(style=inline_t,
                                                                                        selectInput('type_list', "Type:",
                                                                                                    choices = typelist,
                                                                                                    selected = "5")),
                                                                                    div(style=inline_t,
                                                                                        selectInput('extra_list', "Extra:",
                                                                                                    choices = extralist,
                                                                                                    selected = "108"))),
                                                                   conditionalPanel(condition = "input.model_type=='naive_b' || input.model_type=='knn'",
                                                                                    div(style=inline_t,
                                                                                        numericInput('rownum', "Graph row:",
                                                                                                     value = 2)),
                                                                                    div(style=inline_t,
                                                                                        numericInput('colnum', "Graph col:",
                                                                                                     value = 3))),
                                                                   conditionalPanel(condition = "input.model_type=='dt_c50'",
                                                                                    div(style=inline_t,
                                                                                        numericInput('sub_tree', "Subtrees:",
                                                                                                     value = 3)),
                                                                                    div(style=inline_t,
                                                                                        numericInput('trial_num', "Trials:",
                                                                                                     value = 10))),
                                                                   conditionalPanel(condition = "input.model_type=='neur_n'",
                                                                                    
                                                                                    numericInput('hidden_num', "Hidden:",
                                                                                                 value = 1)),
                                                                   conditionalPanel(condition = "input.model_type=='svm'",
                                                                                    div(style=inline_t,
                                                                                        selectInput('y_svm', 'Plot y variabl', 
                                                                                                    choices = c(None = '.', names(df)), 
                                                                                                    selected = "")),
                                                                                    div(style=inline_t,
                                                                                        selectInput('x_svm', 'Plot x variable', 
                                                                                                    choices = c(None = '.', names(df))))), 
                                                                   ## rf ui:
                                                                   conditionalPanel(condition = "input.model_type=='rand_for'",
                                                                                    radioButtons('rf_params',"Select random forest plots",
                                                                                                 choiceNames = list("MDS Plot",
                                                                                                                    "Accuracy",
                                                                                                                    "Importance"),
                                                                                                 choiceValues =list("mds_plot","oob_err", "importvar"),
                                                                                                 inline = FALSE,
                                                                                                 selected="mds_plot"),
                                                                                    selected = ""),
                                                                   
                                                                   conditionalPanel(condition= "input.model_type=='lmrgr'",
                                                                                    radioButtons('view_mods',"View model outputs.",
                                                                                                 choiceNames = list("ANOVA",
                                                                                                                    "lm sig. diff.",
                                                                                                                    "Relative importance",
                                                                                                                    "GLM tree"),
                                                                                                 choiceValues =
                                                                                                   list("viewaov","viewlm", "viewbrelimpo", "viewglm"),
                                                                                                 inline = TRUE,
                                                                                                 selected="viewbrelimpo"),
                                                                                    selectInput('facet_aov', 'Select facetting groups', 
                                                                                                choices = c(None = '.', names(df)), 
                                                                                                selected = "", 
                                                                                                multiple = TRUE)),
                                                                   column(12,align = "center",offset = 0,
                                                                          actionButton("activate", label= "Click to View Model:")))),
                       
                       
                       body<- dashboardBody(
                         tabsetPanel(type= "pills",
                                     tabPanel("Raw Data Table",dataTableOutput("contents"),value=1),
                                     tabPanel("Data structure", verbatimTextOutput('str'),value=1,
                                              tags$head(tags$style(HTML("#str {font-size: 20px;}")))),
                                     ##tabPanel("Stats", dataTableOutput("stat_cont"), value=2),
                                     tabPanel("Graph", value=9,
                                              conditionalPanel(condition= "input.plotting=='scat_plot' | 
                                                               input.plotting=='corrmatrix'",
                                                               plotOutput('graphs2')),
                                              conditionalPanel(condition= "input.plotting!='scat_plot' | 
                                                               input.plotting!='corrmatrix'",
                                                               plotlyOutput('graphs'))),
                                     tabPanel("Regression and Classification", 
                                              fluidRow(column(8, 
                                                              plotOutput('class_tree', height = "500px")),
                                                       ##fluidRow(
                                                       column(7, verbatimTextOutput('test_table')),
                                                       column(8, verbatimTextOutput('test_pred'))),value=6, 
                                              tags$head(tags$style(HTML("#class_tree {font-size: 20px;}")))),
                                     tabPanel("Summarize/ Convert Data File",tableOutput("contents2"),value=4),
                                     id="condition")
                       ))

server <- function(input, output, session) {
  
  ##shinyServer(# added "session" because updateSelectInput requires it
  options(warn=-1)
  # options(encoding="UTF-8")
  data0 <- reactive({ 
    req(input$file1) ##require that the input is available
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(input$file_type=="csv"){
      df <- read.csv(inFile$datapath, header = TRUE, 
                     sep = ",", 
                     na.strings= c("NA","na"))
    }
    else{
      df <- read.xlsx(inFile$datapath, 
                      sheet= input$sheet_num, 
                      sep = ",", 
                      detectDates = TRUE,
                      na.strings=c("NA","na"))
    }
    ##input$sep,
    ##quote = input$quote
    ##)
    df<-clean_names(df)
    df$trt <- if("trt" %in% colnames(df)) {
      as.factor(as.character(df$trt))
      ##factor(df$trt, levels=unique(df$trt))
    } else {
    }
    df$trts <- if("trts" %in% colnames(df)) {
      as.factor(as.character(df$trts))
    } else {
    }
    if("date" %in% colnames(df)){
      df$date<-as.Date(df$date,"%m/%d/%y")
    } else {
    }
    if("day" %in% colnames(df)){
      df$day<-factor(df$day, levels=unique(df$day))
    } else {
    }
    if("date.time" %in% colnames(df)){
      df$date.time <-mdy_hm((df$date.time))
    }
    else{
    }
    df$value<-if("value" %in% colnames(df)) {
      as.numeric(df$value)
    } else {
    }
    df$variable <- if("variable" %in% colnames(df)) {
      factor(df$variable, levels=unique(df$variable))
    } else {
    }
    ##Update selectinput names
    
    updateSelectInput(session, inputId = 'ycol', label = 'Y variable',
                      choices = c(names(df)), selected = "")
    updateSelectInput(session, inputId = 'xcol', label = 'X variable',
                      choices = c(names(df)), selected = "trt")
    updateSelectInput(session, inputId = 'zcol', label = 'Grouping',
                      choices = c(names(df)), selected = "trt")
    updateSelectInput(session, inputId = 'wcol', label = 'Heatmap tile labels',
                      choices = c(names(df)), selected = "trt")
    updateSelectInput(session, inputId = 'facet_row', label = 'Facet Row',
                      choices = names(df), selected = "")
    updateSelectInput(session, inputId = 'factor_col', label = 'Vars to fctr:',
                      choices = names(df), selected = "")
    updateSelectInput(session, inputId = 'numeric_col', label = 'Vars to nmrc:',
                      choices = names(df), selected = "")
    updateSelectInput(session, inputId = 'varnames', label = "Select variables:",
                      choices = c(None = '.', names(df)), selected = "fsb")
    updateSelectInput(session, inputId = 'indepvar', label = "Select independent variables:",
                      choices = names(df), selected = "")
    updateSelectInput(session, inputId = 'groupnames', label = 'Select group name(s) to summarize:',
                      choices = c(None = '.', names(df)), selected = "trt")
    updateSelectInput(session, inputId = 'rmv_col', label = 'Remove COLUMNs from dataframe',
                      choices = names(df), selected = "")
    updateSelectInput(session, inputId = 'subcolname', label = 'Subset dataframe by...',
                      choices = c(None = '.', names(df)), selected = ".")
    updateSelectInput(session, inputId = 'subcol2', label = 'Subset dataframe by...',
                      choices = c(None = '.', names(df)), selected = ".")
    updateSelectInput(session, inputId = 'subcol3', label = 'Subset dataframe by...',
                      choices = c(None = '.', names(df)), selected = ".")
    updateSelectInput(session, inputId = 'outcomevar', label = 'Select Dependent Variable:',
                      choices = names(df), selected = "fsb")
    updateSelectInput(session, inputId = 'x_svm', label = 'Plot x variable:',
                      choices = c(names(df)), selected = "")
    updateSelectInput(session, inputId = 'y_svm', label = 'Plot y variable:',
                      choices = c(names(df)), selected = "")
    
    return(df)
  })
  
  data<-reactive({
    dat<-data0()
    if(input$subcolname!="."){
      updateSelectInput(session, inputId = 'w_names0', label = 'Select unique names:',
                        choices = as.character(unique(dat[,input$subcolname])), selected = "")}
    else{}
    if(input$subcol2!="."){
      updateSelectInput(session, inputId = 'w_names7', label = 'Select unique names:',
                        choices = as.character(unique(dat[,input$subcol2])), selected = "")}
    else{}
    if(input$subcol3!="."){
      updateSelectInput(session, inputId = 'w_names8', label = 'Select unique names:',
                        choices = as.character(unique(dat[,input$subcol3])), selected = "")}
    else{}
    
    updateSelectInput(session, inputId = 'out_group', label = 'Outlier rmvl by factor group(s):',
                      choices = names(dat), selected = "")
    updateSelectInput(session, inputId = 'out_var', label = 'Outlier rmvl by numeric var(s):',
                      choices = names(dat), selected = "")
    if (is.null(input$factor_col)){
      dat}
    ##else if(input$factor_col %in% colnames(dat)){
    else {
      dat[,input$factor_col]<-lapply(dat[,input$factor_col],factor)
      dat}
    if (is.null(input$numeric_col)){
      return(dat)}
    else{
      datmut<-dat %>% 
        mutate_at(vars(!!!syms(input$numeric_col)), funs(as.numeric))
      ##datmut<-transform(dat,as.numeric(factor(vars(!!!syms(input$numeric_col)))))
      dat<-as.data.frame(datmut)
      dat
    }
    return(dat)
  })
  
  ##Remove outliers code from user al3xa:
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
  
  ##Normalization function
  normalize<-function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  data2<-reactive(
    ##input$subset_data,
    {##req(input$subcolname,input$w_names)
      ##w2_names<- unlist(strsplit(input$w_names,", "))
      if(input$subbingdat=="noselect"){
        dataa<-data()}
      else if(input$subbingdat=="select"){
        if(input$subcolname!="."){
          dataa<-data()%>% 
            ##filter(data()[,input$subcolname] %in% w2_names)}
            filter(data()[,input$subcolname] %in% input$w_names0)}
        else{dataa<-data()}
      }
      else{
        if(input$subcolname!="."){
          dataa<-data()%>% 
            ##filter(!data()[,input$subcolname] %in% w2_names)}
            filter(!data()[,input$subcolname] %in% input$w_names0)}
        else{dataa<-data()}
        if(input$subcol2!="."){
          dataa<-dataa%>% 
            filter(!dataa[,input$subcol2] %in% input$w_names7)}
        else{dataa}
        if(input$subcol3!="."){
          dataa<-dataa%>% 
            filter(!dataa[,input$subcol3] %in% input$w_names8)}
        else{dataa}
      }
      if(!is.null(input$rmv_col)){
        dataa<-select(dataa, -c(input$rmv_col))
        dataa
      }
      else{
        dataa
      }
      if(!is.null(input$out_group)){
        if(!is.null(input$out_var)){
          # Apply the outlier function to multiple columns. Preserve dataframe
          datout <- dataa %>%
            group_by(!!!syms(input$out_group)) %>%
            mutate_at(vars(!!!syms(input$out_var)), funs(remove_outliers))
          dataa<-as.data.frame(datout)
          dataa
        }
        else{dataa}
      }
      else{dataa}
      
      ##scaling
      if(input$imp_dat=='impu'){
        datimpute<-knnImputation(select_if(dataa, is.numeric))
        dat_not_num<-select_if(dataa,negate(is.numeric))
        dat_all<-cbind(dat_not_num,datimpute)
        dataa<-dat_all
        dataa
      }
      else{dataa
      }
      if(input$norm_scale=='nrmlz'){
        datimpute<-knnImputation(select_if(dataa, is.numeric))
        norm<-as.data.frame(lapply(datimpute,normalize))
        norm2<-select_if(dataa,negate(is.numeric))
        all_dat<-cbind(norm2,norm)
        dataa<-all_dat
        dataa
      }
      else{dataa
      }
      
      return(dataa)
    })
  
  cv <- function(x) ((100*sd(x,na.rm = TRUE))/mean(x,na.rm=TRUE))
  
  ##manip_dat<- eventReactive(
  manip_dat<- reactive(
    ##input$newsummdata,
    {dat<- data2()
    if(input$meansonly== FALSE)
    {dat2<-dat %>%
      group_by(!!!syms(input$groupnames)) %>%
      summarise_at(c(input$varnames),
                   funs(mean(.,na.rm=TRUE),
                        sd(.,na.rm=TRUE), 
                        std.error, cv))
    }
    else{
      dat2<-dat %>%
        group_by(!!!syms(input$groupnames)) %>%
        summarise_at(c(input$varnames),
                     funs(mean(.,na.rm=TRUE)))
      if(input$pctnut=="yes"){
        if(input$pctcalcor=="controllast"){
          dat3<-dat2 %>%
            mutate_at(c(input$varnames),function(x)
              ((x-last(x))/last(x)*100)) %>%
            rename_at(vars(-(!!!syms(input$groupnames))),function(x) paste0(x,"_pct"))
          ##colnames(dat3)<- paste(colnames(dat3), "pct", sep = "_")  
          dat3<-select_at(dat3, vars(-(!!!syms(input$groupnames))))
          dat4<-cbind.data.frame(dat2,dat3)
        }
        else{
          dat3<-dat2 %>%
            mutate_at(c(input$varnames),function(x)
              ((x-first(x))/first(x)*100)) %>%
            rename_at(vars(-(!!!syms(input$groupnames))),function(x) paste0(x,"_pct"))
          ##colnames(dat3)<- paste(colnames(dat3), "pct", sep = "_")  
          dat3<-select_at(dat3, vars(-(!!!syms(input$groupnames))))
          dat4<-cbind.data.frame(dat2,dat3)}
        updateSelectInput(session, inputId = 'morenames', label = "Select variable name(s) to summarize. 
                          First, click on 'View new summarytable' to upload variable names",
                          choices = names(dat4), selected = "fsb")
        if(input$convlong==TRUE){
          dat5<- dat4 %>%
            select(paste(input$groupnames),
                   ##grep(paste(input$findcols), names(dat)),
                   grep("_pct", names(dat4)),
                   paste(input$morenames))
          data6<- melt(dat5, id.vars = paste(input$groupnames))
        }
        else{dat4}}
      else{dat2}
    }})
  ##calc percent change vs control(first row)
  percent_calc<- reactive({
    dat2<-data2()%>%
      group_by(!!!syms(input$xcol)) %>%
      summarise_at(c(input$ycol),
                   funs(mean(.,na.rm=TRUE)))
    if(input$pctorder=="xyz"){
      dat3<-dat2 %>%
        mutate(pct_diff= 
                 ((!!!syms(input$ycol))-
                    last(!!!syms(input$ycol)))/
                 last(!!!syms(input$ycol))*100)
    }
    else{
      ##pct_diff by reverse oder depending on ordr of the y_col in the dataframe
      dat3<-dat2 %>%
        mutate(pct_diff= 
                 ((!!!syms(input$ycol))-
                    first(!!!syms(input$ycol)))/
                 first(!!!syms(input$ycol))*100)
    }
    return(dat3)
  })
  
  ##calculate placement for letters
  letdat<-reactive({
    superdat<- data2() %>% 
      group_by(!!!syms(input$xcol)) %>% 
      summarize(mxl=max(!!!syms(input$ycol),na.rm = TRUE)) %>% 
      mutate(mxl=max(mxl))
    return(superdat)
  })
  ##calculate tukey's significant difference. Aplha default= 0.05
  hsd<-reactive(
    {req(input$ycol,input$xcol)
      my_model<-aov(reformulate(input$xcol,input$ycol),data=data2())
      test_result<-HSD.test(my_model,input$xcol,alpha=input$choice_p, group=TRUE)
      test_1<-as.data.frame(test_result$means)
      test_1<-cbind(trt=rownames(test_1),test_1)
      test_2<-as.data.frame(test_result$groups)
      test_2<-cbind(trt=rownames(test_2),test_2)
      test_combo<-merge(test_1,test_2)
      return(test_combo)
    })
  
  ##calc long dataframe
  contents <- reactive({
    req(input$varnames,input$groupnames)
    dat<- data2()
    dat<- dat %>%
      select(paste(input$varnames),
             ##grep(paste(input$findcols), names(dat)),
             paste(input$groupnames))
    data<- melt(dat, id.vars = paste(input$groupnames))
  })
  
  ##set significance level parameters
  symnum.args2 <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.2, 1), 
                       symbols = c("****", "***", "**", "*",".","'","ns"))
  addtheme<- reactive({
    theme(plot.title = element_text(##size = 15,
      margin=ggplot2::margin(0,0,30,0), 
      hjust = 0.5,
      size = input$adjsize+4, 
      ##color = "black",
      face="bold"),
      plot.subtitle= element_text(
        size=input$adjsize,
        color="darkblue"
      ),
      axis.text.x = element_text(angle = input$adjangle, 
                                 hjust = 1, color = "black", 
                                 size = input$adjsize),
      axis.text.y = element_text(color = "black", size = input$adjsize),
      axis.title.y = element_text(color = "black", size = input$adjsize),
      axis.title.x = element_text(color = "black", size = input$adjsize),
      legend.title=element_blank(),
      legend.text=element_text(size = input$adjsize))
  })
  
  addtheme2<- reactive({
    theme(plot.title = element_text(##size = 15,
      margin=ggplot2::margin(0,0,30,0), 
      hjust = 0.5,
      size = input$adjsize, 
      ##color = "black",
      face="bold"),
      plot.subtitle= element_text(
        size=input$adjsize,
        color="darkblue"
      ),
      axis.text.x = element_text(angle = input$adjangle, 
                                 hjust = 1, color = "black", 
                                 size = input$adjsize-4),
      axis.text.y = element_text(color = "black", size = input$adjsize-4),
      axis.title.y = element_text(color = "black", size = input$adjsize-4),
      axis.title.x = element_text(color = "black", size = input$adjsize-4),
      legend.title=element_blank(),
      legend.text=element_text(size = input$adjsize-4))
  })
  
  summ_stat1<-reactive({
    stat_compare_means(
      aes(label =ifelse( p < 1.e-2,
                         sprintf("p = %1.0e", as.numeric(..p.format..)),
                         sprintf("p = %1.2f", as.numeric(..p.format..)))),
      method = if(input$xcol==input$zcol){
        "t.test"
      }
      else NULL,
      ref.group = if(input$xcol!=input$zcol)
        NULL
      else{
        paste(input$stats) 
      },
      symnum.args = symnum.args2, 
      label.y.npc = 0.85,
      label.y = if(input$sigheight==0)
        NULL
      else {input$sigheight},
      size=input$sigsize)
  })
  summ_stat2<-reactive({
    stat_compare_means(label = "p.signif", 
                       method = if(input$xcol==input$zcol){
                         "t.test"}
                       else NULL,
                       ref.group = if(input$xcol!=input$zcol)
                         NULL
                       else{
                         paste(input$stats) 
                       },
                       symnum.args = symnum.args2, 
                       hide.ns = FALSE,
                       label.y.npc = 0.75,
                       label.y = if(input$sigheight==0)
                         NULL
                       else {input$sigheight*.92},
                       size=input$sigsize)
  })
  ##summary stat p-values for facet groups 
  summ_stat1_facet<-reactive({
    stat_compare_means(
      aes(label =ifelse( p < 1.e-2,
                         sprintf("p = %1.0e", as.numeric(..p.format..)),
                         sprintf("p = %1.2f", as.numeric(..p.format..)))),
      method = if(input$xcol==input$zcol){
        "t.test"
      }
      else NULL,
      ref.group = if(input$xcol!=input$zcol)
        NULL
      else{
        paste(input$stats) 
      },
      symnum.args = symnum.args2, 
      label.y.npc = .85,
      label.y = if(input$sigheight==0)
        NULL
      else {input$sigheight},
      size=input$sigsize-2)
  })
  ##p-value symbols only, no p-values
  summ_stat2_facet<-reactive({
    stat_compare_means(label = "p.signif", 
                       method = if(input$xcol==input$zcol){
                         "t.test"
                       }
                       else NULL,
                       ref.group = if(input$xcol!=input$zcol)
                         NULL
                       else{
                         paste(input$stats) 
                       },
                       symnum.args = symnum.args2, 
                       hide.ns = TRUE,
                       label.y.npc = .75,
                       label.y = if(input$sigheight==0)
                         NULL
                       else {input$sigheight*.92},
                       size=input$sigsize-2)
  })
  
  corrdata<-reactive({
    dat<-na.omit(data2()) 
    dat<-dat%>%
      select_if(~is.numeric(.))
    dat2<- round(cor(dat, use="complete.obs"), 1)
    return(dat2)
  })
  
  ##Heat map and scatter 3D
  ##interpolated
  heat_dat<-reactive({
    req(input$xcol,input$ycol, input$zcol)
    formulax<-formula(data2()[,input$zcol]~
                        data2()[,input$xcol]*
                        data2()[,input$ycol])
    dat_loess <-loess(formulax)
    interpolated <- predict(dat_loess, 
                            data.frame(input$xcol==data2()[,input$xcol], 
                                       input$ycol==data2()[,input$ycol]))
  })
  
  ##heat dat
  heat_dat2<-reactive({
    interdat<-as.data.frame(heat_dat())
    heatdatnew<-cbind(data2(),interdat)
  })
  
  dat3<-reactive({
    dat2<-data2()%>%
      group_by(!!!syms(input$xcol), 
               !!!syms(input$zcol),
               !!!syms(input$facet_row)) %>%
      summarise_at(input$ycol,mean,na.rm=TRUE)
  })
  
  ##barplot
  output$graphs <- renderPlotly({
    id <- showNotification("Plotting data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    if(input$plotting=="bar_plot"){
      req(input$xcol,input$ycol,input$zcol)
      sigheight2<-input$sigheight
      ycolname<-input$ycol
      p<- ggplot(data=(data2()), aes_string(x= if(input$alpha=="desc"){
        fct_rev(data2()[,input$xcol])}
        else if (input$alpha=="asc"){
          data2()[,input$xcol]
        }
        else if(input$alpha=="asc_val"){
          reorder(data2()[,input$xcol], data2()[,input$ycol], 
                  FUN= function(x) mean(x, na.rm = TRUE))
        }
        else if(input$alpha=="desc_val"){
          reorder(data2()[,input$xcol], data2()[,input$ycol], 
                  FUN= function(x) -mean(x, na.rm = TRUE))
        }
        else{
          factor(data2()[,input$xcol], levels = unique(data2()[,input$xcol]))
        },
        y=data2()[,input$ycol],
        ##group=data2()[,input$zcol],
        fill =if(input$alpha=="desc"){
          fct_rev(data2()[,input$zcol])}
        else if (input$alpha=="asc"){
          data2()[,input$zcol]
        }
        else if(input$alpha=="asc_val"){
          reorder(data2()[,input$zcol], data2()[,input$ycol], 
                  FUN= function(x) mean(x, na.rm = TRUE))
        }
        else if(input$alpha=="desc_val"){
          reorder(data2()[,input$zcol], data2()[,input$ycol], 
                  FUN= function(x) -mean(x, na.rm = TRUE))
        }
        else{
          factor(data2()[,input$zcol], levels = unique(data2()[,input$zcol]))
        }))+
        stat_summary(fun.y = "mean", geom = "bar", position = position_dodge(width=0.8),
                     color="black",width=0.7,
                     na.rm = TRUE)+
        ggtitle(input$goodtitle, subtitle = input$goodtitle)+
        scale_x_discrete(labels = function(x) str_wrap(x, width = input$wrap))
      
      if(input$errbar=="sdev"){
        p<-p+stat_summary(fun.data = "mean_sd", geom="errorbar", position = position_dodge(width=0.8),width=.2, na.rm = TRUE)
      }
      else if(input$errbar=="serr"){
        p<-p+stat_summary(fun.data = "mean_se", geom="errorbar", position = position_dodge(width=0.8),width=.2, na.rm = TRUE)
      }
      else{p}
      
      if(input$axisor==TRUE){
        p<-p+coord_flip()
      }
      else{
        p
      }
      
      if(input$xname==""){
        p<-p+xlab(input$xcol)
      }
      else{
        p<-p+ xlab(input$xname) 
      }
      if(input$yname==""){
        p<-p+ylab(input$ycol)
      }
      else{
        p<-p+ylab(input$yname)
      }
      
      if(input$theming==FALSE){
        p<-p+theme_bw()
      }
      else{p}
      ##if(input$colorsss !=c(".","magma","inferno","plasma","viridis","cividis")){
      if(input$colorsss %in% rbrewerpallist[7:41]){
        p<-p+scale_fill_brewer(palette = input$colorsss)
      }
      ##else if(input$colorsss ==c("magma","inferno","plasma","viridis","cividis")){
      else if(input$colorsss %in% rbrewerpallist[2:6]){
        p<-p+scale_fill_viridis_d(option= input$colorsss)}
      else if(input$colorsss %in% rbrewerpallist[1]){
        p<-p+scale_fill_grey(start=0, end=1)}
      ##facets
      else{p}
      facets <- paste('.','~',input$facet_row)
      ##if(input$facet_row !="."){
      if(!is.null(input$facet_row)){
        if(input$sigs=="sigsym"){
          p <- p + 
            summ_stat2_facet()+
            scale_y_continuous(expand = c(0,0))+
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
          if(input$addlabel=="meanlab"){
            p<-p+stat_summary(aes(label=round(..y..,2)), fun.y= mean, geom= "text",
                              nudge_y = input$labelheight, 
                              vjust=1, size=input$pctsize)
          }
          else{p}
        }
        else{
          p <- p + 
            summ_stat1_facet()+
            summ_stat2_facet()+
            scale_y_continuous(expand = c(0,0))+
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
          if(input$addlabel=="meanlab"){
            p<-p+stat_summary(aes(label=round(..y..,2)), 
                              fun.y= mean, 
                              geom= "text",
                              nudge_y = input$labelheight, 
                              vjust=1, size=input$pctsize)
          }
          else{p}
        }}
      else {
        ##if(input$sigs=="sigletters"){
        if(input$sigs=="sigletters" & input$xcol==input$zcol){
          p<-p+addtheme()+geom_text(data = letdat(),
                                    aes(x=(!!!syms(input$xcol)),
                                        y= if(input$sigheight==0){
                                          mxl+input$labelheight} 
                                        else{mxl+input$labelheight+input$sigheight},
                                        fill=(!!!syms(input$xcol)),
                                        label=hsd()$groups),
                                    size=input$sigsize,vjust=0)
        }
        else if(input$sigs=="sigpandsym")
        {p<-p+addtheme()+summ_stat1()+summ_stat2()
        }
        else if(input$sigs=="sigsym")
        {p<-p+addtheme()+summ_stat2()
        }
        else {p<-p+addtheme()
        }
        if(input$addlabel == "pctlab"){
          if(input$xcol==input$zcol){
            p<- p+geom_text(data=percent_calc(), 
                            aes(x=as.character(unlist(percent_calc()[,input$xcol])), 
                                y=as.numeric(unlist(percent_calc()[,input$ycol])), 
                                group=as.character(unlist(percent_calc()[,input$xcol])), 
                                fill =as.character(unlist(percent_calc()[,input$xcol])),
                                label= ifelse(pct_diff != 0.00, 
                                              paste(round(pct_diff, digits = 1),"%"), "")),
                            ##position = position_dodge(width=0.8),
                            nudge_y = input$labelheight, 
                            vjust=1, size=input$pctsize)
          }
          else{p}
        }
        else if(input$addlabel == "meanlab"){
          if(input$xcol==input$zcol){
            p<-p+geom_text(data=percent_calc(), 
                           aes(x=as.character(unlist(percent_calc()[,input$xcol])), 
                               y=as.numeric(unlist(percent_calc()[,input$ycol])), 
                               group=as.character(unlist(percent_calc()[,input$xcol])), 
                               fill =as.character(unlist(percent_calc()[,input$xcol])),
                               label=paste(round(hsd()[,2],digits = 1))),
                           ##position = position_dodge(width=0.8),
                           nudge_y = input$labelheight, 
                           vjust=1, size=input$pctsize)
          }
          else{p}
        }}
      if(input$addleg==TRUE){
        p<-p+theme(legend.position = "none")
      }
      else
      {p}
      ##if(input$ymax!=0){
      if(!is.null(input$ymax)){
        if(input$sci=="scinota"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= scientific)
        }
        else if(input$sci=="percent"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= percent)
        }
        else if(input$sci=="log"){
          p<-p+scale_y_continuous(trans="log", 
                                  limits= c(input$ymin+1,input$ymax), 
                                  ##limits= c(0,1e8), 
                                  expand = c(0,0),
                                  labels=scientific) 
        }
        else{
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), 
                                  expand = c(0,0), 
                                  labels= comma)
        }}
      else{
        if(is.null(input$ymax)){
          if(input$sci=="scinota"){
            p<-p+scale_y_continuous(expand = c(0,0), labels=scientific)
          }
          else if(input$sci=="percent"){
            p<-p+scale_y_continuous(expand = c(0,0), labels= percent)
          }
          else if(input$sci=="log"){
            p<-p+scale_y_continuous(trans="log",expand = c(0,0), labels=scientific)
          }
          else{
            p<-p+scale_y_continuous(expand = c(0,0), 
                                    labels= comma)
          }}}
      if(input$h_line !="0"){
        p<-p+ geom_hline(yintercept= input$h_line, linetype=2, color="maroon")
      }
      else{p}
      ggplotly(p) %>% 
        layout(legend = list(x= 100,y=0.5),
               height = input$plotHeight, 
               width= input$plotWidth,
               autosize=TRUE,
               xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
    }
    else if(input$plotting=="line_plot"){
      req(input$xcol,input$ycol,input$zcol)
      if(input$linetype2=="loess"){
        
        p<-ggplot(dat3(), aes_string(x=input$xcol,
                                     y=input$ycol, 
                                     group=input$zcol,
                                     color=input$zcol))+
          ##geom_xspline(size= 0.5)
          stat_smooth(se=FALSE, 
                      method = "lm", 
                      formula = y ~ poly(x, degree=input$degrees))
      }
      else{
        p<- ggplot(na.omit(data2()), aes_string(x=input$xcol,
                                                y=input$ycol, 
                                                group=input$zcol,
                                                color=input$zcol))+
          ggtitle(input$goodtitle)
        p<-p+stat_summary(fun.y = "mean", geom = "line", 
                          position = if(input$stagger== "sepline"){
                            position_dodge(width=0.8)}
                          else {
                            "identity"
                          },
                          na.rm = TRUE)
        ##scale_x_discrete(labels = function(x) str_wrap(x, width = input$wrap))
        if(input$errbar=="sdev"){
          p<-p+stat_summary(fun.data = "mean_sd", geom="errorbar", color= "gray40", ##position = position_dodge(width=0.8),
                            position = if(input$stagger== "sepline"){
                              position_dodge(width=0.8)}
                            else {
                              "identity"
                            },
                            width=.05, 
                            na.rm = TRUE)
        }
        else if(input$errbar=="serr"){
          p<-p+stat_summary(fun.data = "mean_se", geom="errorbar", color= "gray40",##position = position_dodge(width=0.8),
                            position = if(input$stagger== "sepline"){
                              position_dodge(width=0.8)}
                            else {
                              "identity"
                            },
                            width=.05, 
                            na.rm = TRUE)
        }
        else{p}}
      
      if(input$axisor==TRUE){
        p<-p+coord_flip()
      }
      else{
        p
      }
      
      if(input$linelab==TRUE){
        p<-p+ ##annotate("text",label="label means",hjust=-0.2,vjust=1)+ ##-Inf,Inf,
          stat_summary(aes(label=paste(round(..y..,2))), 
                       fun.y=mean, geom="text", 
                       position = if(input$stagger== "sepline"){
                         position_dodge(width=0.8)}
                       else {
                         "identity"
                       },
                       ##color="black", 
                       size=input$sigsize-1,
                       vjust = -0.5, hjust=-0.5)
      }
      else{
        p<-p+stat_summary(fun.y = "mean", geom = "point", 
                          position = if(input$stagger== "sepline"){
                            position_dodge(width=0.8)}
                          else {
                            "identity"
                          },
                          na.rm= TRUE) ##+
        ##stat_summary(fun.data = "mean_se", geom = "errorbar", 
        ##position = position_dodge(width=0.8), width= 0.2, na.rm = TRUE)
      }
      
      if(input$xname==""){
        p<-p+xlab(input$xcol)
      }
      else{
        p<-p+ xlab(input$xname) 
      }
      if(input$yname==""){
        p<-p+ylab(input$ycol)
      }
      else{
        p<-p+ylab(input$yname)
      }
      
      if(input$gpoints==TRUE){
        p<-p+geom_jitter(na.rm=TRUE)
      }
      else
      {p}
      if(input$theming==FALSE){
        p<-p+theme_bw()
      }
      else{p}
      ##if(input$colorsss !=c(".","magma","inferno","plasma","viridis","cividis")){
      if(input$colorsss %in% rbrewerpallist[7:41]){
        p<-p+scale_fill_brewer(palette = input$colorsss)
      }
      ##else if(input$colorsss ==c("magma","inferno","plasma","viridis","cividis")){
      else if(input$colorsss %in% rbrewerpallist[2:6]){
        p<-p+scale_fill_viridis_d(option= input$colorsss)}
      else if(input$colorsss %in% rbrewerpallist[1]){
        p<-p+scale_fill_grey(start=0, end=1)}
      ##facets
      else{p}
      ##facets
      ##facets <- paste(input$facet_row, '~', input$facet_col)
      facets <- paste('.','~',input$facet_row)
      ##if (facets != '. ~ .') 
      if(!is.null(input$facet_row)){
        ##if(input$sigs=="sigletters"){
        if(input$sigs=="sigletters" & input$xcol==input$zcol){
          p <- p + 
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), ncol=1,scales = input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
        }
        else if(input$sigs=="sigpandsym"){
          p <- p + 
            summ_stat1_facet()+ 
            summ_stat2_facet()+
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), ncol=1,scales = input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
        }
        else {
          p <- p + 
            summ_stat2_facet()+
            ##scale_y_continuous(expand = c(0,0))+
            ##scale_x_datetime(breaks=date_breaks("1 day"))+
            ##, minor_breaks=date_breaks("1 hour"))+
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), ncol=1,scales = input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()    
        }
      } 
      else {
        ##if(input$sigs=="sigletters"){
        if(input$sigs=="sigletters" & input$xcol==input$zcol){
          p<-p+addtheme()
        }
        else if(input$sigs=="sigpandsym"){
          p<-p+addtheme()+summ_stat1()
        }
        else if(input$sigs=="sigsym"){
          p<-p+addtheme()+summ_stat2()
        }
        else {
          p<-p+addtheme()
        }
      }
      if (input$addleg == TRUE) {
        p <- p + theme(legend.position = "none")
      }
      else
      {p}
      if(!is.null(input$ymax)){
        if(input$sci=="scinota"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= scientific)
        }
        else if(input$sci=="percent"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= percent)
        }
        else if(input$sci=="log"){
          p<-p+scale_y_continuous(trans="log", 
                                  limits= c(input$ymin+1,input$ymax), 
                                  ##limits= c(0,1e8), 
                                  expand = c(0,0),
                                  labels=scientific) 
        }
        else{
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), 
                                  expand = c(0,0), 
                                  labels= comma)
        }}
      else{
        if(is.null(input$ymax)){
          if(input$sci=="scinota"){
            p<-p+scale_y_continuous(expand = c(0,0), labels=scientific)
          }
          else if(input$sci=="percent"){
            p<-p+scale_y_continuous(expand = c(0,0), labels= percent)
          }
          else if(input$sci=="log"){
            p<-p+scale_y_continuous(trans="log",expand = c(0,0), labels=scientific)
          }
          else{
            p<-p+scale_y_continuous(expand = c(0,0), 
                                    labels= comma)
          }}}
      if(input$h_line !="0"){
        p<-p+ geom_hline(yintercept= input$h_line, linetype=2, color="maroon")
      }
      else{p}
      ##p<-p+scale_x_datetime(breaks=date_breaks("1 hour")) ##,
      ##minor_breaks= date_breaks("1 hour"))
      if(input$stagger== "datetime"){
        p<-p+scale_x_date(breaks=date_breaks("1 day"))}
      else {
        p}##+
      ##, minor_breaks=date_breaks("1 hour")+
      ggplotly(p) %>% 
        layout(legend = list(x= 100,y=0.5),
               height = input$plotHeight, 
               width= input$plotWidth,
               autosize=TRUE,
               xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
      
    }
    else if(input$plotting=="box_plot"){
      req(input$xcol,input$ycol,input$zcol)
      p<- ggplot(data=(data2()), aes_string(x= 
                                              if(input$alpha=="desc"){
                                                fct_rev(data2()[,input$xcol])}
                                            else if (input$alpha=="asc"){
                                              data2()[,input$xcol]
                                            }
                                            else if(input$alpha=="asc_val"){
                                              reorder(data2()[,input$xcol], data2()[,input$ycol], 
                                                      FUN= function(x) mean(x, na.rm = TRUE))
                                            }
                                            else if(input$alpha=="desc_val"){
                                              reorder(data2()[,input$xcol], data2()[,input$ycol], 
                                                      FUN= function(x) -mean(x, na.rm = TRUE))
                                            }
                                            else{
                                              factor(data2()[,input$xcol], levels = unique(data2()[,input$xcol]))
                                            },
                                            y=data2()[,input$ycol],
                                            ##group=data2()[,input$zcol],
                                            fill =if(input$alpha=="desc"){
                                              fct_rev(data2()[,input$zcol])}
                                            else if (input$alpha=="asc"){
                                              data2()[,input$zcol]
                                            }
                                            else if(input$alpha=="asc_val"){
                                              reorder(data2()[,input$zcol], data2()[,input$ycol], 
                                                      FUN= function(x) mean(x, na.rm = TRUE))
                                            }
                                            else if(input$alpha=="desc_val"){
                                              reorder(data2()[,input$zcol], data2()[,input$ycol], 
                                                      FUN= function(x) -mean(x, na.rm = TRUE))
                                            }
                                            else{
                                              factor(data2()[,input$zcol], levels = unique(data2()[,input$zcol]))
                                            }))+
        ##geom_violin(aes(colour=(data2()[,input$zcol])),
        ##          alpha=0.3)
        geom_boxplot(position = "dodge")+
        ggtitle(input$goodtitle)+
        scale_x_discrete(labels = function(x) str_wrap(x, width = input$wrap))
      if (input$linelab=="TRUE"){
        p<-p+ geom_violin(position=position_dodge(0.70), alpha=0.3, 
                          trim=FALSE, adjust=0.5, color="gray")
      }
      if(input$axisor==TRUE){
        p<-p+coord_flip()
      }
      else{
        p
      }
      
      if(input$xname==""){
        p<-p+xlab(input$xcol)
      }
      else{
        p<-p+ xlab(input$xname) 
      }
      if(input$yname==""){
        p<-p+ylab(input$ycol)
      }
      else{
        p<-p+ylab(input$yname)
      }
      
      if(input$theming==FALSE){
        p<-p+theme_bw()
      }
      else{p}
      ##if(input$colorsss !=c(".","magma","inferno","plasma","viridis","cividis")){
      if(input$colorsss %in% rbrewerpallist[7:41]){
        p<-p+scale_fill_brewer(palette = input$colorsss)
      }
      ##else if(input$colorsss ==c("magma","inferno","plasma","viridis","cividis")){
      else if(input$colorsss %in% rbrewerpallist[2:6]){
        p<-p+scale_fill_viridis_d(option= input$colorsss)}
      else if(input$colorsss %in% rbrewerpallist[1]){
        p<-p+scale_fill_grey(start=0, end=1)}
      ##facets
      else{p}
      ##facets <- paste(input$facet_row, '~', input$facet_col)
      facets <- paste('.','~',input$facet_row)
      ##if (facets != '. ~ .')
      if(!is.null(input$facet_row)){
        if(input$sigs=="sigsym")
        {p <- p + 
          summ_stat2_facet()+
          scale_y_continuous(expand = c(0,0))+
          ##facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales = "free")+ 
          facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
          theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
          theme(strip.text.y = element_text(size=1, colour = "gray"))+
          addtheme2()
        }
        else{
          p <- p + 
            summ_stat1_facet()+
            summ_stat2_facet()+
            scale_y_continuous(expand = c(0,0))+
            ##facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales = "free")+ 
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
        }}
      else {
        ##if(input$sigs=="sigletters"){
        if(input$sigs=="sigletters" & input$xcol==input$zcol){
          p<-p+addtheme()+geom_text(data = letdat(),aes(x=(!!!syms(input$xcol)),
                                                        y= if(input$sigheight==0){
                                                          mxl+input$labelheight} 
                                                        else{mxl+input$labelheight+input$sigheight},
                                                        fill=(!!!syms(input$xcol)),
                                                        label=hsd()$groups),size=input$sigsize,vjust=0)
        }
        else if(input$sigs=="sigpandsym")
        { p<-p+addtheme()+summ_stat1()+summ_stat2()
        }
        else if(input$sigs=="sigsym"){
          p<-p+addtheme()+summ_stat2()
        }
        else {
          p<-p+addtheme()
        }}
      if(input$gpoints==TRUE){
        p<-p+geom_jitter()
      }
      else
      {p}
      if(input$addleg==TRUE){
        p<-p+theme(legend.position = "none")
      }
      else
      {p}
      if(!is.null(input$ymax)){
        if(input$sci=="scinota"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= scientific)
        }
        else if(input$sci=="percent"){
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), expand = c(0,0), labels= percent)
        }
        else if(input$sci=="log"){
          p<-p+scale_y_continuous(trans="log", 
                                  limits= c(input$ymin+1,input$ymax), 
                                  ##limits= c(0,1e8), 
                                  expand = c(0,0),
                                  labels=scientific) 
        }
        else{
          p<-p+scale_y_continuous(limits= c(input$ymin,input$ymax), 
                                  expand = c(0,0), 
                                  labels= comma)
        }}
      else{
        if(is.null(input$ymax)){
          if(input$sci=="scinota"){
            p<-p+scale_y_continuous(expand = c(0,0), labels=scientific)
          }
          else if(input$sci=="percent"){
            p<-p+scale_y_continuous(expand = c(0,0), labels= percent)
          }
          else if(input$sci=="log"){
            p<-p+scale_y_continuous(trans="log",expand = c(0,0), labels=scientific)
          }
          else{
            p<-p+scale_y_continuous(expand = c(0,0), 
                                    labels= comma)
          }}}
      if(input$h_line !="0"){
        p<-p+ geom_hline(yintercept= input$h_line, linetype=2, color="maroon")
      }
      else{p}
      ggplotly(p) %>% 
        layout(##boxmode="group",
          boxmode= if(input$plotting=="box_plot"){
            if(input$xcol== input$zcol){
              NULL}
            else{"group"}}
          else{NULL},
          legend = list(x= 100,y=0.5),
          height = input$plotHeight, 
          width= input$plotWidth,
          autosize= TRUE,
          xaxis = list(autorange = TRUE),
          yaxis = list(autorange = TRUE))
    }
    else if (input$plotting== "heat_m"){
      if (input$mapvals=="raw values"){
        p<-ggplot(data = data2(),aes(x=(data2()[,input$xcol]), 
                                     y=(data2()[,input$ycol]), 
                                     fill=data2()[,input$zcol]))+ 
          ##color=trt))+
          xlab(input$xcol)+
          ylab(input$ycol)+
          scale_fill_gradient(low="yellow",high = "red",name=input$zcol)+
          ##scale_fill_gradient2(low="white", 
          ##                   mid="yellow", 
          ##                 high="red", #colors in the scale
          ##               midpoint=18,    #same midpoint for plots (mean of the range)
          ##breaks=seq(0,1,0.25), #breaks in the scale bar
          ##             limits=c(12, 52),
          ##           name= paste(input$zcol))+
          geom_tile(aes(color="black"), size=1)+
          scale_y_reverse(expand = c(0,0))+
          scale_x_continuous(expand = c(0,0))
        if(!is.null(input$wcol)){
          p<-p+geom_text(aes(label=paste(data2()[,input$wcol])), angle=90)
        }
        else{
          p
        }
        facets <- paste('.','~',input$facet_row)
        ##if (facets != '. ~ .')
        ##if(input$facet_row !="."){
        if(!is.null(input$facet_row)){
          p <- p + 
            ##scale_y_reverse(expand = c(0,0))+ 
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
        }
        else{p}
        p<-p+ggtitle(input$goodtitle)
        
        if(input$axisor==TRUE){
          p<-p+coord_flip()
        }
        else{
          p
        }
        
        if(input$xname==""){
          p<-p+xlab(input$xcol)
        }
        else{
          p<-p+ xlab(input$xname) 
        }
        if(input$yname==""){
          p<-p+ylab(input$ycol)
        }
        else{
          p<-p+ylab(input$yname)
        }
      }
      else if(input$mapvals=="rd3d"){
        data3 <- data2() %>%
          select(x = input$xcol, y = input$ycol, z = input$zcol)%>%
          group_by(x,y)
        dat3d<-spread(data3, x, z)
        dat3d<-as.matrix(dat3d[-1])
        p<-plot_ly(z = dat3d)%>%
          add_surface() %>%
          layout(
            title = paste(input$xcol,"x",input$ycol,"x",input$zcol),
            scene = list(
              xaxis = list(autorange = "reversed",title = input$xcol),
              yaxis = list(title = input$ycol),
              zaxis = list(title = input$zcol)))
      }
      ##else if(input$mapvals=="interpolated"){
      else if(input$mapvals=="interpolated"){
        p<-ggplot(data2(),aes(x=data2()[,input$xcol], 
                              y=data2()[,input$ycol], 
                              fill=heat_dat()))+ 
          scale_fill_gradient(low="yellow",high = "red", name= heat_dat())+
          ##geom_text(aes(label=paste(round(heat_dat(), digits = 2))))+
          xlab(input$xcol)+
          ylab(input$ycol)+
          geom_tile()+
          ##geom_tile(aes(fill=heat_dat(), color="black"), size=1)+
          scale_y_reverse(expand = c(0,0))+
          scale_x_continuous(expand = c(0,0))
        facets <- paste('.','~',input$facet_row)
        ##if(input$facet_row !="."){
        if(!is.null(input$facet_row)){
          p <- p + 
            ##scale_y_reverse(expand = c(0,0))+
            facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
            theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
            theme(strip.text.y = element_text(size=1, colour = "gray"))+
            addtheme2()
        }
        else{p}
      }
      else if(input$mapvals=="inter_3d"){
        xone<- input$xcol
        yone<- input$ycol
        inter<- colnames(as.data.frame(heat_dat()))
        data3 <- heat_dat2() %>%
          select(x=xone, y=yone, z=inter) %>%
          group_by(x,y)
        dat3d<-spread(data3, x, z)
        dat3d<-as.matrix(dat3d[-1])
        p<-plot_ly(z = dat3d)%>%
          add_surface() %>%
          layout(
            title = paste(input$xcol,"x",input$ycol,"x",input$zcol),
            scene = list(
              xaxis = list(autorange = "reversed",title = input$xcol),
              yaxis = list(title = input$ycol),
              zaxis = list(title = input$zcol)))
      }
      else{
        ##3D scatter plots
        req(input$facet_row)
        p <- plot_ly(data2(), 
                     x = ~(data2()[,input$xcol]), 
                     y = ~(data2()[,input$ycol]), 
                     z = ~(data2()[,input$zcol]), 
                     color = ~(data2()[,input$facet_row]),
                     type= if(input$gpoints==TRUE){
                       type = 'scatter3d'}
                     else{NULL
                     },
                     mode = if(input$gpoints==TRUE){
                       "lines+markers"}
                     else{NULL
                     })
        p <- p %>% add_markers() %>%
          layout(
            title = paste(input$xcol,"x",input$ycol,"x",input$zcol),
            scene = list(
              xaxis = list(autorange = "reversed",title = input$xcol),
              yaxis = list(title = input$ycol),
              zaxis = list(title = input$zcol)))
      }
      ggplotly(p) %>% 
        layout(legend = list(x= 100,y=0.5),
               height = input$plotHeight, 
               width= input$plotWidth,
               autosize= TRUE,
               xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
    }
  })
  output$graphs2<-renderPlot({
    id <- showNotification("Plotting data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    if(input$plotting=="scat_plot"){
      req(input$xcol,input$ycol,input$zcol)
      lineform <- y~x
      ##lineform <- input$ycol~input$xcol
      p<-ggplot(data=data2(), aes_string(x=input$xcol,
                                         y= input$ycol, 
                                         ##group=input$zcol,
                                         color=input$zcol ##,
                                         #3size= if(!is.null(input$facet_row)){
                                         ##  if(input$facet_row!="."){
                                         #3    input$facet_row
                                         #3}
                                         ##else{NULL}
                                         ##}
                                         ##else{NULL}
      ))+
        geom_point(alpha=0.8)+ ggtitle(input$goodtitle)
      facets <- paste('.','~',input$facet_row)
      if(!is.null(input$facet_row)){
        p <- p + 
          ##scale_y_reverse(expand = c(0,0))+ 
          facet_wrap(facets, labeller= label_wrap_gen(width=50, multi_line = FALSE), scales=input$scaling)+ 
          theme(strip.text.x = element_text(size = input$adjsize-2, colour = "black"))+
          theme(strip.text.y = element_text(size=1, colour = "gray"))+
          addtheme2()
      }
      else{
        p<-p+addtheme()
      }
      
      if(input$linelab==TRUE){
        p<-p+geom_smooth(method = "lm", se=FALSE, 
                         formula = lineform)+
          stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep="~~~")), 
                       formula = lineform, 
                       parse = TRUE, size=input$pctsize+2)}
      else{p}
      
      if(input$gpoints==TRUE){
        p<-p+stat_ellipse(data=data2(), aes_string(
          fill= input$zcol, 
          group= input$zcol),
          type = "norm", geom = "polygon", alpha=0.1)
      }
      else
      {p}
      ##colors
      ##if(input$colorsss !=c(".","magma","inferno","plasma","viridis","cividis")){
      if(input$colorsss %in% rbrewerpallist[7:41]){
        p<-p+scale_fill_brewer(palette = input$colorsss)
      }
      ##else if(input$colorsss ==c("magma","inferno","plasma","viridis","cividis")){
      else if(input$colorsss %in% rbrewerpallist[2:6]){
        p<-p+scale_fill_viridis_d(option= input$colorsss)}
      else if(input$colorsss %in% rbrewerpallist[1]){
        p<-p+scale_fill_grey(start=0, end=1)}
      ##facets
      else{p}
      if(input$axisor==TRUE){
        p<-p+coord_flip()
      }
      else{
        p
      }
    }
    else if(input$plotting=="corrmatrix"){
      dat1<-na.omit(data2()) 
      dat2<-dat1%>%
        select_if(~is.numeric(.))
      p.mat2 <- cor_pmat(dat2)
      p.mat2<-as.data.frame(p.mat2)
      rownames(p.mat2) <- p.mat2[,1]
      p.mat2<-p.mat2[-1]
      p.mat2<-as.matrix.data.frame(p.mat2)
      p<-ggcorrplot(corr=corrdata(),
                    hc.order = TRUE,
                    type = "lower",
                    outline.col = "black", 
                    p.mat = p.mat2,
                    pch=7,
                    pch.col = "red",
                    pch.cex = input$sigsize+10,
                    color=c("blue","white","green3"), ##turquoise2  ##method = "circle",
                    lab=TRUE)+
        ggtitle(input$goodtitle)
      p
    }
    if(input$xname==""){
      p<-p+xlab(input$xcol)
    }
    else{
      p<-p+ xlab(input$xname) 
    }
    if(input$yname==""){
      p<-p+ylab(input$ycol)
    }
    else{
      p<-p+ylab(input$yname)
    }
    if(input$theming==FALSE){
      p<-p+theme_bw()
    }
    else{p}
    p<-p+addtheme()
    p
  }, width=exprToFunction(input$plotWidth), 
  height=exprToFunction(input$plotHeight))
  
  ##Training set
  dat_train <- reactive({
    set.seed(input$seed_ran)
    dat_samp<-sample(nrow(data2()))
    dat<-data2()[dat_samp,]
    sample<- sample.split(dat,SplitRatio = 0.75)
    dat_train<-subset(dat, sample ==TRUE)
    dat_train<-select_at(dat_train, vars(!!!syms(input$outcomevar),!!!syms(input$indepvar)))
    dat_train
  })
  
  ##Testing set
  dat_test <- reactive({
    set.seed(input$seed_ran)
    dat_samp<-sample(nrow(data2()))
    dat<-data2()[dat_samp,]
    sample<- sample.split(dat,SplitRatio = 0.75)
    dat_test<-subset(dat, sample==FALSE)
    dat_test<-select_at(dat_test, vars(!!!syms(input$outcomevar),!!!syms(input$indepvar)))
    dat_test
  })
  
  modform<-reactive({
    req(input$outcomevar, input$indepvar, input$seed_ran)
    pops<- input$outcomevar  
    mops2<- strsplit(input$indepvar,", ")
    mops3<-paste0(mops2, collapse = '+')
    mod <- as.formula(paste0(pops,"~",mops3))
  })
  
  ##rf function
  rf_model<-reactive({
    dat_model <- randomForest(formula=modform(), 
                              data = dat_train(), 
                              proximity=TRUE, 
                              importance = TRUE)
  })
  rf_oob<-reactive({
    listnames<-paste(c("OOB",levels(dat_train()[,input$outcomevar])))
    ooberr<-data.frame(TREES=rep(1:nrow(rf_model()$err.rate)),
                       TRTS= rep(listnames, each=nrow(rf_model()$err.rate)),
                       ERR=c(rf_model()$err.rate[,listnames]))
    ooberr
  })
  mds_dat<-reactive({
    dismat<-dist(rf_model()$proximity)
    mds_scale<-cmdscale(dismat, eig=TRUE, x.ret=TRUE)
    dsvars<-round(mds_scale$eig/sum(mds_scale$eig)*100,1)
    mdsvals<-mds_scale$points
    mdsdata<-data.frame(sample=rownames(mdsvals),
                        X= mdsvals[,1],
                        Y= mdsvals[,2], 
                        treatments=dat_train()[,input$outcomevar])    
    mdsdata
  })
  ##rf_importance<-reactive({
  ##  tryimp <- importance(model)%>% data.frame() %>% mutate(feature=row.names(.))
  ##})
  r_table_rf<-reactive({
    ##Predicted_output<-predict(svm_model(),dat_test())
    ##table_mat <- table(dat_test()[,input$outcomevar], Predicted_output)
  })
  # SVM get accuracy from comparing actual to predicted results ----
  r_acc_rf<-reactive({
    ##accuracy_Test <- sum(diag(r_table6()))/sum(r_table6())
    pred_rf<-predict(rf_model(), test=dat_test(), type="response")
    pred_rf
  })
  
  svm_model<-reactive({
    dat_model <- svm(formula=modform(), data = dat_train())
    ##type                 
    ##kernal=
    ##dat_model
  })
  # SVM generate a summary of the data ----
  r_table6<-reactive({
    Predicted_output<-predict(svm_model(),dat_test())
    table_mat <- table(dat_test()[,input$outcomevar], Predicted_output)
  })
  # SVM get accuracy from comparing actual to predicted results ----
  r_acc6<-reactive({
    accuracy_Test <- sum(diag(r_table6()))/sum(r_table6())
  })
  
  ##j_rip model
  jrip_model<-reactive({
    dat_model <- JRip(modform(), data = dat_train())
  })
  jrip_model_plot<-reactive({
    dat_model <- ctree(modform(), data = dat_train())
  })
  # jrip generate a summary of the data ----
  r_table5<-reactive({
    Predicted_output<-predict(jrip_model(),dat_test())
    ##table_mat <- table(knn_test()[,input$outcomevar], Predicted_output)
  })
  # jip get accuracy from comparing actual to predicted results ----
  r_acc5<-reactive({
    accuracy_Test <- sum(diag(r_table4()))/sum(r_table4())
  })
  
  ##Artificial neural_network model
  neur_model<-reactive({
    dat_model <- neuralnet(modform(), data = dat_train(), hidden = input$hidden_num)
  })
  # ANN generate a summary of the data ----
  r_table4<-reactive({
    ##Predicted_output<-predict(neur_model(),knn_test())
    ##table_mat <- table(knn_test()[,input$outcomevar], Predicted_output)
    results<-compute(neur_model(),dat_test()[-1])
    predicted<-results$net.result
  })
  # ANN get accuracy from comparing actual to predicted results
  r_acc4<-reactive({
    ##accuracy_Test <- sum(diag(r_table4()))/sum(r_table4())
    test<-cor(r_table4(), dat_test()[,1])
  })
  ##data frame for princlple components
  pre_pca<-reactive({
    dat<-select_at(data2(), vars(!!!syms(input$outcomevar),!!!syms(input$indepvar)))
    dat
  })
  ##dat_abr
  pca0<-reactive({
    dat<-pre_pca()
    knnOutput <- knnImputation(dat[,!names(dat) %in% input$outcomevar])
    testing2<-cbind(dat[input$outcomevar],knnOutput)
    testing2
  })
  pca<-reactive({
    ##pcadat<-prcomp(pca0()[,2:ncol(pca0())], center = TRUE, scale. = TRUE)
    pcadat<-prcomp(pca0()[,2:ncol(pca0())], center = TRUE, scale. = TRUE)
    pcadat
  })
  ##naive_bayes model
  nb_model<-reactive({
    dat_model <- naive_bayes(modform(), data = dat_train())
    ##dat_model
  })
  # Generate a summary of the data ----
  r_table3<-reactive({
    Predicted_output<-predict(nb_model(),dat_test())
    table_mat <- table(dat_test()[,input$outcomevar], Predicted_output)
  })
  # Get accuracy from comparing actual to predicted results ----
  r_acc3<-reactive({
    accuracy_Test <- sum(diag(r_table3()))/sum(r_table3())
  })
  ##C5.0 decision tree
  C50_model <- reactive({
    dat_model <- C5.0(modform(), data = dat_train(), trials = input$trial_num)
    dat_model
  })
  # Generate a summary of the data C5.0
  r_table2<-reactive({
    Predicted_output<-predict(C50_model(),dat_test())
    table_mat <- table(dat_test()[,input$outcomevar], Predicted_output)
  })
  # Get accuracy from comparing actual to predicted results ----
  r_acc2<-reactive({
    accuracy_Test <- sum(diag(r_table2()))/sum(r_table2())
  })
  ##regression tree model
  regt_model <- reactive({
    dat_model <- rpart(modform(), 
                       method="class",
                       data = dat_train())
  })
  # Generate a summary of the data ----
  r_table<-reactive({
    Predicted_output<-predict(regt_model(),dat_test(),
                              type="class")
    table_mat <- table(dat_test()[,input$outcomevar], Predicted_output)
    ##table_mat <- CrossTable(dat_test[,input$outcomevar], Predicted_output,
    ##                      prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
    ##                    dnn = c('Actual','Predicted'))
  })
  # Get accuracy from comparing actual to predicted results ----
  r_acc<-reactive({
    accuracy_Test <- sum(diag(r_table()))/sum(r_table())
  })
  aov_model<-reactive({
    req(input$outcomevar, input$indepvar)
    mops1<-paste0(input$outcomevar)
    ##moppin<-with(data=dater, interaction(trt,shelf))
    ##my_model<-aov(fsb~moppin,data=dater)
    my_model<-aov(reformulate(input$indepvar,input$outcomevar),data=data2())
    testhsd<-HSD.test(my_model,input$indepvar,group=TRUE, alpha = 0.1)
    ##testhsd<-HSD.test(my_model,"moppin",group=TRUE, alpha = 0.1)
    dat<-data.frame(testhsd$groups)
    trt<-rownames(testhsd$groups)
    testbind0<-cbind(trt, dat)
    ##testbind<-data.frame(testbind0)##, row.names = paste(1:length(testbind0$trt)))
    ##testbind2<-testbind %>% separate(trt, sep="[.]",remove=FALSE) ##col= c(input$indepvar),remove =FALSE)
    testbind3<-testbind0 %>% arrange(trt)
    ##Create another dataset with means
    testbind4<-data.frame(testhsd$means)
    testbind5<-merge(testbind3,testbind4)
    ##testbind6<-testbind5 %>% separate(trt, sep="[.]", c("a","b"),remove=FALSE)
    testbind6<-testbind5 %>% separate(trt, sep="[:]", c("trt_var1","var2","var3","var4","var5"),remove=FALSE)
    updateSelectInput(session, inputId = 'facet_aov', label = 'facet',
                      choices = names(testbind6), selected = "")
    return(testbind6)
  })
  lin_sum<-reactive({
    ##calculate tukey's significant difference. Aplha default= 0.05
    my_model<-aov(modform(),data=data2())
  })
  lin_sum_adj<-reactive({
    ##calculate tukey's significant difference. Aplha default= 0.05
    my_model<-lm(modform(),data=data2())
  })
  ## linear
  lin_hsd<-reactive({
    test_result<-HSD.test(lin_sum(),paste(input$indepvar),group=TRUE, alpha = 0.1)
    test_result
  })
  lin_hsd2<-reactive({
    test_result<-HSD.test(lin_sum(),paste(input$indepvar),group=TRUE, alpha = 0.05)
    test_result
  })
  glm_model<-reactive({
    cebio1lm<-glm(modform(),data=data2())
    cebio1lm
  })
  lm_imp<- reactive({
    booting<-boot.relimp(glm_model(),
                         type=c("lmg","last","betasq", "pratt", "genizi", "car"),
                         rank=TRUE, diff = TRUE, rela = TRUE)
    check<-booteval.relimp(booting)
    check
  })
  glm_tree<-reactive({
    cebio1lm<-glmtree(formula=modform(),data=data2())
    cebio1lm
    ##summary(cebio1lm)
  })
  output$class_tree <- renderPlot({
    id <- showNotification("Rendering plot...", 
                           duration = NULL, 
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    if(input$model_type=="rparty"){
      rpart.plot(regt_model(), 
                 type= if(is.null(input$type_list)){
                   2}
                 else {
                   as.numeric(input$type_list)
                 },
                 extra= if(is.null(input$extra_list)){
                   "auto"}
                 else {
                   as.numeric(input$extra_list)
                 }, digits = 3)}
    else if(input$model_type=="naive_b"){
      par(mfrow=c(input$rownum,input$colnum))
      plot(nb_model())}
    else if(input$model_type=="dt_c50"){
      plot(C50_model(), subtree=input$sub_tree)
    }
    else if(input$model_type=="jrip"){
      plot(jrip_model_plot())
    }
    else if(input$model_type=="knn"){
      rs<-input$rownum
      cs<-input$colnum
      p<-ggbiplot(pca(), ellipse=TRUE, choices=c(rs,cs), 
                  obs.scale = 1, 
                  var.scale = 1,
                  labels= pca0()[,1], 
                  group= pca0()[,1]) +
        theme_minimal()+
        ggtitle("Visualization using PCA")
      ##if(input$colorsss !=c(".","magma","inferno","plasma","viridis","cividis")){
      if(input$colorsss %in% rbrewerpallist[6:40]){
        p<-p+scale_color_brewer(palette = input$colorsss)
        p
      }
      ##else if(input$colorosss ==c("magma","inferno","plasma","viridis","cividis")){
      else if(input$colorsss %in% rbrewerpallist[1:5]){
        p<-p+scale_color_viridis_d(option= input$colorsss)
        p
      }}
    else if(input$model_type=="neur_n"){
      plot(neur_model(), rep="best",col.entry= "red")
    }
    ##facets
    else if(input$model_type=="lmrgr"){
      if(input$view_mods=="viewlm"){
        LM<-paste(input$outcomevar,'~',input$indepvar)
        lm_message<-cat("linear model (Outcome variable~Predictor Variable):", LM)
        ##par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
        ##plot(lin_sum(), main=lm_message)
        par(mfrow=c(1,2))
        aaa<-bar.group(lin_hsd()$group, ##col = brewer.pal(5,"Set2"), 
                       las=2, 
                       bar=TRUE, ylim=c(0, max(data2()[,input$outcomevar], na.rm = TRUE)))
        bbb<-bar.group(lin_hsd2()$group, ##col = brewer.pal(5,"Set2"), 
                       las=2,
                       bar=TRUE, ylim=c(0, max(data2()[,input$outcomevar], na.rm = TRUE)))
        aaa
        bbb
      }
      else if(input$view_mods=="viewaov"){
        plothsd<-ggplot(data=aov_model(), aes(x=trt, 
                                              y=aov_model()[,input$outcomevar],
                                              ##y=tfc,
                                              fill=trt))+
          geom_bar(stat = "identity")+
          geom_label(data=aov_model(), aes(x=trt,
                                           y= max(Max)*1.3,
                                           label=groups), fill= "white", vjust=2)+
          geom_errorbar(aes(ymin=aov_model()[,input$outcomevar]-std,
                            ymax=aov_model()[,input$outcomevar]+std), 
                        width=0.2, size=1)+
          labs(title="ANOVA Tukey HSD", 
               subtitle = "Unshared letters= significant difference at p<0.1")+
          ylab(paste("rootwt","model mean,estimates"))+
          xlab("Independent variable interactions")+
          theme(plot.title = element_text(##size = 15,
            margin=ggplot2::margin(0,0,30,0), 
            hjust = 0.5,
            size = 12,
            face="bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45,hjust = 1))+
          scale_y_continuous(expand = c(0,0))+
          scale_fill_viridis_d(option= "viridis")
        plothsd
        if(!is.null(input$facet_aov)){
          facetaov <- paste('.','~',input$facet_aov)
          plothsd2<-plothsd+facet_wrap(facetaov, scales = input$scaling)
          plothsd2
        }
        else{plothsd
        }
      }
      else if(input$view_mods=="viewbrelimpo"){
        plot(lm_imp(), sort=TRUE)
      }
      else{
        plot(glm_tree())
      }
    }
    else if(input$model_type=="svm"){
      req(input$x_svm,input$y_svm)
      xs<-input$x_svm
      ys<-input$y_svm
      mod <- as.formula(paste0(xs,"~",ys))
      plot(svm_model(),data=dat_train(), mod)
    }
    else{
      ##graph outputs in server
      if(input$rf_params=="mds_plot"){
        p1<-ggplot(data=mds_dat(), aes(x=X,y=Y, label=sample))+
          geom_text(aes(color=treatments))+theme_bw()+
          xlab(paste("MDS1 ",mds_dat()[,2],"%", sep = ""))+
          ylab(paste("MDS2 ",mds_dat()[,3],"%", sep = ""))+
          ggtitle("MDS plot of Random forest Probability")+
          stat_ellipse(aes(color=treatments), size=1, alpha=0.5)+
          ##addtheme()
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = "bottom")
        p1
      }
      else if(input$rf_params=="oob_err"){
        p2<-ggplot(data=rf_oob(), aes(x=TREES, y=ERR))+
          geom_line(aes(color=TRTS))+ggtitle("Number of trees vs error rate")+
          ##addtheme()
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = "bottom")
        p2
        
        ##plot_grid(p1,p2, labels="auto")
      }
      else if(input$rf_params=="importvar"){
        par(mfrow=c(1,2))
        varImpPlot(rf_model(), color="red", main="Mean Accuracy Decrease", cex=1.5, pch=19, type = 1)
        varImpPlot(rf_model(), color="blue", main="Mean Node Impurity Decrease", cex=1.5, pch=19, type = 2)
      }
    }
  })##,width=exprToFunction(input$plotWidth), 
  ##height=exprToFunction(input$plotHeight))
  
  output$test_pred <- renderPrint({
    if(input$model_type=="rparty"){
      print(r_table())
    }
    else if(input$model_type=="naive_b"){
      print(r_table3())
    }
    else if(input$model_type=="dt_c50"){
      print(r_table2())
    }
    else if(input$model_type=="jrip"){
      print(jrip_model())
    }
    else if(input$model_type=="knn"){
      cat("Prediction accuracy not available")
    }
    else if(input$model_type=="lmrgr"){
      if(input$view_mods=="viewlm"){
        print(summary(lin_sum()))
      }
      else if(input$view_mods=="viewaov"){
        print(aov_model())
      }
      else if(input$view_mods=="viewbrelimpo"){
        print(lm_imp())
      }
      else{
        print(summary(glm_model()))
      }
    }
    else if(input$model_type=="neur_n"){
      print(r_table4())
    }
    else if(input$model_type=="svm"){
      print(r_table6())
    }
    else{
      print(rf_model())
    }
  })
  output$test_table <- renderPrint({
    if(input$model_type=="rparty"){
      cat(paste('Accuracy of model in predicting test set data =', r_acc()))
    }
    else if(input$model_type=="naive_b"){
      cat(paste('Accuracy of model in predicting test set data =', r_acc3()))
    }
    else if(input$model_type=="dt_c50"){
      cat(paste('Accuracy of model in predicting test set data =', r_acc2()))
    }
    else if(input$model_type=="jrip"){
      cat(paste('Accuracy of model in predicting test set data =', r_table5()))
    }
    else if(input$model_type=="knn"){
      cat("Confusion matrix not available")
    }
    else if(input$model_type=="lmrgr"){
      cat(paste('Adjusted r-squared for linear model. 
                      Percentage of variation explained by the predictors affecting the outcome variable. 
                      The higher the adj. r^2, the better the model fit =', 
                round(summary(lin_sum_adj())$adj.r.squared, digits = 3)))
    }
    else if(input$model_type=="neur_n"){
      cat(paste('Accuracy of model in predicting test set data =', r_acc4()))
    }
    else if(input$model_type=="svm"){
      cat(paste('Accuracy of model in predicting test set data =', r_acc6()))
    }
    else{
      cat(paste("Predictions:", r_acc_rf()))
    }
  })
  
  ##summarize data
  output$summarized_datafile <- downloadHandler(
    filename <- function() { 
      paste("converted", Sys.Date(), ".csv", sep="_")
    },
    summ_dat <- function(file) {
      if(input$datatype== "summ_dat"){
        write.csv(manip_dat(), file, row.names = FALSE)}
      else {
        write.csv(contents(), file, row.names = FALSE)}
    })
  
  output$str <- renderPrint({
    str(data2())
  })
  output$contents <- renderDataTable({
    id <- showNotification("Rendering data table...", 
                           duration = NULL, 
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    datatable(data2(), options = list(scrollX = TRUE))
  })
  output$contents2 <-renderTable({
    id <- showNotification("Rendering data table...", 
                           duration = NULL, 
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    if(input$datatype== "summ_dat"){
      manip_dat()}
    else{
      contents()
    }
  })
}
shinyApp(ui, server)