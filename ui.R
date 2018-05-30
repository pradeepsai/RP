#if (!require(shiny)) {install.packages("shiny")}
library(shiny)

#ui <- 
  
shinyUI(
  fluidPage(
  titlePanel("Natural Language Processing using UDPipe R Function "),
  sidebarLayout( 
    sidebarPanel(
      fileInput("file", " Upload any text file"),
      radioButtons(inputId = "Language", label = "Language Model", choices = c("English")),
      checkboxGroupInput(inputId = "pos", label = "Select Parts of Speech", choices = c("ADJ",   "ADP",   "ADV",   "AUX", "CCONJ",  "DET",  "INTJ",  "NOUN",   "NUM",  "PART",  "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB"),selected=c("ADJ","NOUN","PROPN")),
      downloadButton("annotate_download.csv", label = "Download Annotation in a CSV file")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Application Overview",
                           h4(p("Overview of this Application")),
                           p("This Application is build on Shiny App and is intended to demonstrate the Natural Language Processing workflow using R's UDPIPE package " ),
                           br(),
                           h4("How to Use thi Application"),
                           p(" --> This Application is built to support only text files at the moment"),
                           p("1. Click on  'Browse' and upload any text file "),
                           p("2. This Application has 3 tabs"),
                           p("   ~ Annotation   : Tokenisation, tagging and parsing of the given text data, Allows to download the annotated data as a csv file"),
                           p("   ~ Word Cloud   : It has 2 word clouds One for Verbs and other one for Nouns"),
                           p("   ~ Netword Plot : To plot top 30 Co-occurances based on selected UPOS options")
                           ),
                  tabPanel("Annotation", dataTableOutput('ann')),
                  tabPanel("Word Cloud", plotOutput('word_cloud')),
                  tabPanel("Network Plot", plotOutput("network"))
                  
                           )
                  )
             )
         )
      )
