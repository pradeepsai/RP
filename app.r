
#################################################################################################
#                                                                                               #
# Title: Text Analytics Group Assignment- 2                                                     #  
# Topic: Building a Shiny App                                                                   #
# Team: Pradeep Sai Ramisetty:11910044, Sunil Gorantla:11910018, Nagarjuna Resu: 11910071       #
#                                                                                               #
#################################################################################################

# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

if (!require(udpipe)){install.packages("udpipe")};library(udpipe)
if (!require(lattice)){install.packages("lattice")};library(lattice)
if (!require(wordcloud)){install.packages("wordcloud")};library(wordcloud)
if (!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
if (!require(tidytext)) {install.packages("tidytext")};library(tidytext)
if (!require(stringr)) {install.packages("stringr")};library(stringr)
if (!require(readr)) {install.packages("readr")};library(readr)
if (!require(textstem)) {install.packages("textstem")};library(textstem)
if (!require(shiny)) {install.packages("shiny")};library(shiny)


####### Web UI Functionality ###########

ui <- shinyUI(
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

######## Server Functionality ###########

server <- shinyServer(function(input, output) {
  ud_model_english <- udpipe_download_model(language = "english")
  english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")
  
  d1 <- reactive( {
    txt  <- read_file(input$file$datapath)
    #txt <- read_file("./1.txt")
    txt0 <- gsub("[^a-zA-Z\\s]", " ", txt) %>% str_replace_all("[\\s]+", " ") %>% tolower() 
    txt1 <- txt0 %>% data.frame(txt0, stringsAsFactors = FALSE) %>% unnest_tokens(word,txt0) %>% anti_join(stop_words)
    txt2 <- txt1$word %>%lemmatize_words()
    
    x <- udpipe_annotate(english_model, txt2) 
    x <- as.data.frame(x) 
    x <- subset(x, select = -c(feats, head_token_id, doc_id, deps, misc))
  })
  
# 1. Annotate 
  
  output$ann <- renderDataTable({ 
    head( d1(), 100)  
  })
  
  output$ann1 <- renderDataTable({ 
    d1()
  })
  
  output$annotate_download.csv <- downloadHandler(
    filename = function() { paste(input$d1, ".csv", sep = "") },
    content = function(file) { write.csv(d1(), file, row.names = FALSE)  }
  )
  
  #2. word cloud  
  wc1 <- reactive({
    nouns <-  d1() %>% subset(., upos %in% "NOUN")
    nouns_count <- txt_freq(nouns$lemma)
    wordcloud(words = nouns_count$key, scale = c(2, 0.5), freq = nouns_count$freq, min.freq = input$freq, max.words=input$max, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  })
  
  wc2 <- reactive({
    verbs <-  d1() %>% subset(., upos %in% "VERB")
    verbs_count <- txt_freq(verbs$lemma)
    wordcloud(words = verbs_count$key, scale = c(2, 0.5), freq = verbs_count$freq, min.freq = input$freq, max.words=input$max,  random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  })
  
  output$word_cloud = renderPlot({ par(mfrow=c(1,2), cex = 0.83)
    wc1()
    wc2()
  })
  
  #3. co occurance 
  
  d2 <- reactive({
    #txt <- read_file("./1.txt")
    txt  <- read_file(input$file$datapath)
    txt0 <- gsub("[^a-zA-Z\\s]", " ", txt) %>% str_replace_all("[\\s]+", " ") %>% tolower() 
    
    x <- udpipe_annotate(english_model, txt0) #, tagger = "default", parser = "none") ## Tokenization + finds sentences, POS tagging and lemmatization
    x <- as.data.frame(x) 
    x <- subset(x, select = -c(feats, head_token_id, doc_id, deps, misc))
  })
  
  pos <- reactive({ pos <- input$pos })
  
  output$network = renderPlot({ 
    
    pos_english  <- pos()
    network <- d2()
    net <- keywords_rake(network, term = "lemma", group = "upos", relevant = network$upos %in% pos_english , ngram_max = 4, n_min = 2, sep = " ")
    barchart(keyword ~ rake, data = head(subset(net, freq > 3), 30), col = "Orange", main = "Co-occurance Plot for the Parts of Speech checked ", xlab = "Occurances")
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)

