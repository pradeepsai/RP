if (!require(udpipe)){install.packages("udpipe")};library(udpipe)
if (!require(lattice)){install.packages("lattice")};library(lattice)
if (!require(wordcloud)){install.packages("wordcloud")};library(wordcloud)
if (!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
if (!require(tidytext)) {install.packages("tidytext")};library(tidytext)
if (!require(stringr)) {install.packages("stringr")};library(stringr)
if (!require(readr)) {install.packages("readr")};library(readr)


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
    
# 1. Annotation
    
    output$ann <- renderDataTable({ 
      head( d1(), 100)  
    })
    
    output$annotate_download.csv <- downloadHandler(
      filename = function() { paste(input$d1, ".csv", sep = "") },
      content = function(file) { write.csv(d1(), file, row.names = FALSE)  }
    )
   
#2. word cloud  
    wc1 <- reactive({
      nouns <-  d1() %>% subset(., upos %in% "NOUN")
      nouns_count <- txt_freq(nouns$lemma)
      wordcloud(words = nouns_count$key, freq = nouns_count$freq, min.freq = input$freq, max.words=input$max, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    })
  
    wc2 <- reactive({
      verbs <-  d1() %>% subset(., upos %in% "VERB")
      verbs_count <- txt_freq(verbs$lemma)
      wordcloud(words = verbs_count$key, freq = verbs_count$freq, min.freq = input$freq, max.words=input$max,  random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    })
    
    output$word_cloud = renderPlot({ par(mfrow=c(1,2), cex = 0.75)
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
      barchart(keyword ~ rake, data = head(net, 50), col = "Orange", main = "Co-occurance Plot for the Parts of Speech checked ", xlab = "Occurances")
    })
    
})
