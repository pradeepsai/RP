############################### Functions for Text Analytics Assignment -1 ###########################

# 1. Creating Unigrams.
Unigram <- function(d1){
  d1_Unigram = d1 %>% mutate(Doc_Name = file)%>% unnest_tokens(word, bd.text) %>% anti_join(stop_words)%>%group_by(Doc_Name)%>%count(word, sort=TRUE)
  return(d1_Unigram) }

# 2. Creating BiGrams (n = 2)
Bigram <- function(d1) {
  d1_bigram <- d1 %>% mutate(Doc_Name = file)%>% unnest_tokens(bigram, bd.text, token = "ngrams", n = 2) %>% group_by(Doc_Name) %>% count(bigram, sort=TRUE)
  bigram_split <- d1_bigram %>% separate(bigram, c("word1", "word2"), sep = " ") 
  bigram_filter_sw <- bigram_split %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
  bigram_count <- bigram_filter_sw %>% count(word1, word2, sort = TRUE)
  bigrams_count_freq <- head(bigram_count, 20)
  bigrams_united <- bigrams_count_freq %>% unite(bigram_freq, word1, word2, sep = " ")
  return(bigrams_united) }


# 3. Creating Phrase words: Nouns, Verbs, etc...  
  
Noun <- function(x) {
  nouns = x %>% subset(., upos %in% "NOUN") 
  nouns_count = txt_freq(nouns$lemma)  
  #head(top_nouns, 10)	
  return(nouns_count) }

Verbs <- function(x) {
  verbs = x %>% subset(., upos %in% "VERB") 
  verbs_count = txt_freq(verbs$lemma)
  #head(top_verbs, 10)
  return(verbs_count) }

Adverbs <- function(x) {
  adverbs = x %>% subset(., upos %in% "ADV") 
  adverbs_count = txt_freq(adverbs$lemma)
  return(adverbs_count) }

Adjectives <- function(x) {
  adjectives = x %>% subset(., upos %in% "ADJ") 
  adjectives_count = txt_freq(adjectives$lemma) 
  return(adjectives_count)
}

# 4. Creating DTM object

DTM_Matrix <- function(lem_Token, docs.list) {
  dtm = matrix(0, nrow = 1, ncol = length(lem_Token))
  row.names(dtm) = seq(1)
  colnames(dtm) = lem_Token

  for (p in 1:length(lem_Token)){    # looping over tokens
    for (q in 1){    # loop over documents 
      dtm[q, p] = length(grep(lem_Token[p], docs.list[[q]]))
      #dtms[[a]] <- dtm[,]
      #write.csv(dtms[[a]], "dtm.2005.csv")
      return(dtm)
    }}}
  

