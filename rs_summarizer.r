
#need to install java
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

library(gutenbergr)
library(rlist)
library(tidytext)
library(tidyverse)
library(tidyr)
#library(erer)
#library(plyr)
library(tm)
library(dplyr)
library(stringr)
library(NLP)
library(openNLP) #only for win32
library(lexRankr)
library(entity)
library(tokenizers)
library(pdftools)

#=============================
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_gh("trinker/entity")
#=============================

library(tidygeocoder)
library(sf)
library(osmdata)
library(magrittr)
library(readtext)
library(spacyr)

#============================
#spacy_install() #may need to install git, accpet defaults; reboot
#============================

library(stringr)
library(raster)
library(igraph)
library(ggraph)
library(scales)
library(ngram)

#===========================
#Constants
#===========================
START <- "rw"
#START <- "lexr"
#START <- "comb"
#START <- "entity"
#START <- "ngrams"

stopWords <- stopwords("en")
class(stopWords)
'%nin%' <- Negate('%in%')

#top how many ngrams
CNT <- 5

#what ngrams?
N <-3 

#============================
#Relationship words
#============================
rwList = c(" about ", " at ", " for ", " of ", " round ", " to ", " across ", " because ", " from ", " off ", " still ",
" under ", " after ", " before ", " if ", " on ", " so ", " up ", " against ", " between ", " in ", " opposite ", " then ",
" when ", " among ", " but ", " near ", " or ", " though ", " where ", " and ", " by ", " not ", " out ", " through ",
" while ", " as ", " down ", " now ", " over ", " until ", " with ")

#============================
#Basic Action Words
#============================
actList = c(" come ", " get ", " give ", " go ", " have ", " keep ", " let ", " make ", " put ", " take ", " be ", " do ", " say ", " see ", " seem ", " send ", " take ") 

#==================================
#Use a standard document summarizer
#==================================
lex_summary<-function(text) { 
  top3sentences = lexRankr::lexRank(text,
     docId = rep(1, length(text)),
     # return 10 sentences
     n = 10,
     continuous = TRUE)

  top3sentences %>%
         dplyr::mutate(sentenceId =     as.numeric(stringr::str_remove_all(sentenceId, ".*_"))) %>%
         dplyr::arrange(sentenceId) %>%
         dplyr::pull(sentence)
  return(top3sentences$sentence)
}

#====================================
#the relationship word summarizer
#====================================
rw_summary<-function(newBk, inLst, complexity_limit, source, nm){
  
  #bkSen <- strsplit(newBk, "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE)

  bkSen <- newBk %>%
    paste(collapse = " ") %>%
    tokenize_sentences()

  n = length(bkSen[[1]])
  bkSen[[1]][n]

  #Count the number of relational words in each sentence
  lSen <- list()
  numWords <- length(inLst)
  for (i in 1:n){
    numRel = 0

    for (k in 1:numWords) {  
        x = strsplit(bkSen[[1]][i], split=inLst[k])
        spLen = length(x[[1]])
        if (spLen > 1) {
           numRel = numRel + 1
        }
    }
    lSen[[i]] <- c(i, numRel)
  }
  #combine elements in lSen and transpose the result 
  #put correct names in column headers
  df = data.frame(t(sapply(lSen,c)))
  names(df)[1] <- "Sentence"
  names(df)[2] <- "Count"

  #filter out rows with count above complexity_limit
  fdf <- df %>% filter(df$Count < complexity_limit) 
  
  dfs <- fdf[order(fdf[[2]], decreasing = TRUE),]

  #output message
  pref <- "Summary of "
  if(numWords == 42){
     pref<-"A summary that shows the concepts and their interrelationships in:"} else {
     pref<-"A summary to show relationships between concepts and acts dealing with them:"
  }
  book_nm <- nm
  if(source == "G"){
      book_nm <- book_map[names(book_map)==Gutenberg_ID][[1]]
      gmrHead = paste(pref, book_nm, CH_ID)
      print(gmrHead)
      #print(c(pref, book_nm, CH_ID))
      #print top 3 sentences
  }
  #NEED TO SORT THEM BY SENTENCE NUMBER - OUTPUT OF dfs[[1]][1,2,3]
  print(bkSen[[1]][dfs[[1]][1]])
  print(bkSen[[1]][dfs[[1]][2]])
  print(bkSen[[1]][dfs[[1]][3]])  
  print("---")
  print(" ")
  
  return(bkSen)
}
#=============================================
#Main Section
#Reads book from Gutenberg Project
#=============================================


#==============================================
#Process text file
#==============================================
inTxt <- readtext("Smoking_prevalnce.txt")
#inTxt <- readtext("adsb.txt")
#inTxt <- readtext("HB2694.txt")
str(inTxt$text)
complexity_limit = 10 
nm = "Smoking"

#get rid of new line char
inStr <- str_replace_all(inTxt$text, "[\\n]" , " ")
inSen <- rw_summary(inStr, rwList, complexity_limit, "P", nm)
gmr10 <- lex_summary(inStr)
