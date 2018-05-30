#################################################################################################
#                                                                                               #
# Title: Text Analytics Group Assignment- 2                                                     #  
# Topic: Building a Shiny App                                                                   #
# Team: Pradeep Sai Ramisetty:11910044, Sunil Gorantla:11910018, Nagarjuna Resu: 11910071       #
#                                                                                               #
#################################################################################################

######################################### UI.R ##############################################


if (!require(shiny)) {install.packages("shiny")}
library(shiny)

shinyUI(
  fluidPage(
  titlePanel("Natural Language Processing using UDPipe R Function "),
  sidebarLayout( 
    sidebarPanel(
      fileInput("file", " Upload any text file"),
      radioButtons(inputId = "Language", label = "Language Model", choices = c("English")),
      checkboxGroupInput(inputId = "pos", label = "Select Parts of Speech", choices = c("ADJ", "ADP", "ADV", "CCONJ", "NOUN", "PRON", "PROPN", "VERB"),selected=c("ADJ","NOUN","PROPN")),
      downloadButton("annotate_download.csv", label = "Download Annotation in a CSV file"), br(),
      
      br(),sliderInput("freq", "Minimum Frequency of Words:",       min = 1,  max = 50,   value = 1),
      sliderInput("max",  "Maximum Number of Words:", min = 10, max = 1000, value = 300)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Application Overview",
                           h4(p((strong("Overview of this Application")))),
                           p("This Application is build on Shiny App and is intended to demonstrate the Natural Language Processing workflow using R's UDPIPE package " ),
                           br(),
                           h4(strong("How to Use this Application")),
                           p(" This Application is built to support only text files at the moment"),
                           p("1. Click on  'Browse' and upload any text file "),
                           p("2. This Application has 3 tabs"),
                           p(    "Annotation   : Tokenization, tagging and parsing of the given text data, Allows to download the annotated data as a csv file"),
                           p(    "Word Cloud   : It has 2 word clouds One for Verbs and other one for Nouns"),
                           p(    "Co-Occurance Plot  : To plot top 30 Co-occurances based on selected UPOS options")
                           ),
                  
                  tabPanel("Annotation_100", dataTableOutput('ann')),
                  tabPanel("Annotation", dataTableOutput('ann1')),
                  tabPanel("Word Clouds", plotOutput('word_cloud')),
                  tabPanel("Co-Occurance Plot", plotOutput("network"))
                  
                  )
                )
             )
         )
      )
    
