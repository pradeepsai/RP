library(readr)
library(udpipe)
library(tidyverse)
library(tidytext)
library(udpipe)
library(NLP)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(shiny)
library(readr)
library(rsconnect)
library(gridExtra)
library(tm)
library(textstem)



setwd("D:/ISB CBA/FROM VIAO/Term1/Text Analytics/Assignments/Group Assignments/GA_2")
getwd()

data <- read_file("training.txt")
#data <- read_file(input$file$datapath)
data0 <- gsub("[^A-Za-z\\s]"," ",data)
data1 <- str_replace_all(data0, "[\\s]+", " ") %>% tolower()
data2 <- data.frame(data1,stringsAsFactors = FALSE) 
data2 <- data2 %>% unnest_tokens(word, data1) %>% anti_join(stop_words) 
head(data2, 10)
str(data2)
#d2 <- data1

english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")

x <- udpipe_annotate(english_model, x = data1)#, tagger = "default") ## Tokenization + finds sentences, POS tagging and lemmatization
x <- as.data.frame(x)
x <- subset(x, select = -c(feats, head_token_id, doc_id, deps, misc))
table(x$upos)
head(x, 10)
#########################################
  
  
  txt <- read_file("./1.txt")
  txt0 <- gsub("[^a-zA-Z\\s]", " ", txt) %>% str_replace_all("[\\s]+", " ") %>% tolower() #%>% data.frame(txt0, stringsAsFactors = FALSE)
  
  txt1 <- txt0 %>% data.frame(txt0, stringsAsFactors = FALSE) %>% unnest_tokens(word,txt0) %>% anti_join(stop_words)
  txt2 <- txt1$word %>%lemmatize_words()

  
  head(txt1)
str(txt2)
head(txt1$word)
head(data2$word)
