
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

#===========================
#Basic relationships between concepts and acts concerning them
#===========================
cmbList = c(" about ", " at ", " for ", " of ", " round ", " to ", " across ", " because ", " from ", " off ", " still ",
" under ", " after ", " before ", " if ", " on ", " so ", " up ", " against ", " between ", " in ", " opposite ", " then ",
" when ", " among ", " but ", " near ", " or ", " though ", " where ", " and ", " by ", " not ", " out ", " through ",
" while ", " as ", " down ", " now ", " over ", " until ", " with ", " come ", " get ", " give ", " go ", " have ", " keep ", " let ", " make ", " put ", " take ", " be ", " do ", " say ", " see ", " seem ", " send ", " take ") 

#===========================
#Books of the New Testament
#===========================
Num_Books<-c(8347:8373, 15962)
book_names<-c("mt","mr","lk","jn","act","rm","fcr","scr", "ga", "eph", "pp", "co", "fth", "sth","ftm","stm","ti","ph","hb","jam","fpt","spt", "fjn","sjn","tjn","jud","rev", "Taxes")  

#associate Gutenberg_ID with Book name
book_map<- book_names
names(book_map)<- Num_Books

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

#========================================
#ngrams
#========================================
ngrams<-function(gmr) {
  dhb <- tibble(txt = gmr)
  hb.trigrams<-dhb %>%
    unnest_tokens(ngram, txt, token = "ngrams", n = N)
  hb.trifreq<-hb.trigrams %>%
    count(ngram, sort = TRUE)
  #check first char of trigram for 1, ie chapter verse repitition
  hb.top20<-head(hb.trifreq, n=100)
  for (m in 100:1) {
    if (is.na(hb.top20$ngram[m])) {
        hb.top20<-hb.top20[-m,]
    } else {
       if(substr(hb.top20$ngram[m],1,1) == "1"){
          hb.top20<-hb.top20[-m,]
       }
    }
  }
  hb.top20<-head(hb.top20, n=CNT)
  return (hb.top20)
}

#=========================================
#Extract the named entities in a document
#Based on
#https://towardsdatascience.com/quick-guide-to-entity-recognition-and-geocoding-with-r-c0a915932895
#=========================================
extName<-function(text){
   #remove ID column
   text %<>% dplyr::select(-gutenberg_id) 

   #view the structure of the data
   str(text)

   #remove the front matter up to row 120
   text<- text[-c(1:120),]

   #removing underscores from the entire text
   text$text <- gsub("_", "", text$text)

   #create new df
   full_text <- text %>%
     dplyr::select(1) 
   #create a text string
   full_text <- apply(full_text, 2, paste0, collapse=" ") 

   #create sentence tokens
   full_txt_sentence <- spacyr::spacy_tokenize(full_text, 'sentence') 
   #create a df from the result
   doc_sentences <- as.data.frame(do.call(cbind,
                        full_txt_sentence))

   #retrieve locations
   #finding locations and adding a new column to the data frame
   doc_sentences$locations <- location_entity(doc_sentences[,1])

   #add row number 
   doc_sentences %<>% mutate(row = row_number())

    #creating a locations data frame with collapsed list: locations_df
   locations_df  <- doc_sentences %>% 
     {.[!sapply(.$locations, is.null), ]} %>%
     unnest(locations) %>%
     dplyr::select(c(locations, row))

   #create and clean distinct dataframe with counts: locs_dist
   locs_dist <- locations_df %>% group_by(locations) %>% count()

   print(locs_dist)

   #multi return setup
   ret_list <- list()
   ret_list[[1]] <- locs_dist

   #retrieve Persons
   #finding people and adding a new column to the data frame
   doc_sentences$persons <- person_entity(doc_sentences[,1])

   #add row number 
   doc_sentences %<>% mutate(prow = row_number())

    #creating a persons data frame with collapsed list: locations_df
   persons_df  <- doc_sentences %>% 
     {.[!sapply(.$persons, is.null), ]} %>%
     unnest(persons) %>%
     dplyr::select(c(persons, prow))

   #create and clean distinct dataframe with counts: locs_dist
   pers_dist <- persons_df %>% group_by(persons) %>% count()

   print(pers_dist)

   #multi return setup
   ret_list[[2]] <- pers_dist

   #retrieve Organizations
   #finding orgs and adding a new column to the data frame
   doc_sentences$orgs <- organization_entity(doc_sentences[,1])

   #add row number 
   doc_sentences %<>% mutate(orow = row_number())

   #creating a orgs data frame with collapsed list: locations_df
   orgs_df  <- doc_sentences %>% 
     {.[!sapply(.$orgs, is.null), ]} %>%
     unnest(orgs) %>%
     dplyr::select(c(orgs, orow))

   #create and clean distinct dataframe with counts: locs_dist
   orgs_dist <- orgs_df %>% group_by(orgs) %>% count()

   print(orgs_dist)

   #multi return setup
   ret_list[[3]] <- orgs_dist


   return(ret_list)

}

#====================================
#POS tagger
#====================================
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
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

#====================================
#get the frequency count of number of 
#relationship words in a sentence
#====================================
rw_dist<-function(newBk, CH_ID){
  bkSen <- strsplit(newBk, "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE)
  n = length(bkSen[[1]])
  bkSen[[1]][n]

  #Count the number of relational words in each sentence
  lSen <- list()
  for (i in 1:n){
    numRel = 0
    for (k in 1:40) { #MAGIC COOKIE
        x = strsplit(bkSen[[1]][i], split=rwList[k])
        spLen = length(x[[1]])
        if (spLen > 1) {
           numRel = numRel + 1
        }
    }
    #lSen[[i]] <- list(i, numRel)
    lSen[[i]] <- c(i, numRel)
  }
  df = data.frame(t(sapply(lSen,c)))
  names(df)[1] <- "Sentence"
  names(df)[2] <- "Count"

  dfs <- df[order(df[[2]], decreasing = TRUE),]

  #GET FREQ CNT 
  relCnt <- dfs$Count
  a <- table(relCnt)

  book_nm <- book_map[names(book_map)==Gutenberg_ID][[1]]

  print(c(book_nm, " chap ", CH_ID))
  print(a)
  print("---")
}

#=============================================
# Read Wellby
#Need to generalize for all pdf journal articles
#=============================================
process_well <- function(wellby) {
wellby<-wellby[-1] #get rid of publisher foreward
well_str <- wellby[1]
n <- length(wellby)
for (i in 2:n) {
   well_str <- paste(well_str, wellby[i], sep=" ")
}
well_str<-str_replace_all(well_str, "[\\n]" , " ")

#============================================
#need to remove publisher ornamentation
#since this is not the original text from the author
#but is the original texct plus publisher and journal
#ornamentation
#likewise, remove foot notes as they are not the author's
#core argumnet, but usyally references
#apa style don't need to do this
#============================================
etl <- "This content downloaded from"
well_str<-str_replace_all(well_str, etl , " ")

etl <- "132.174.253.126" 
well_str<-str_replace_all(well_str, etl , " ")

etl <- "://about.jstor.org/terms"
well_str<-str_replace_all(well_str, etl , " ")

etl <- "All use subject to https"    
well_str<-str_replace_all(well_str, etl , " ")

etl <- "on Sun, 17 Dec 2023"    
well_str<-str_replace_all(well_str, etl , " ")

etl <- "21:15:12"
well_str<-str_replace_all(well_str, etl , " ")

etl<-"00:00                      "
well_str<-str_replace_all(well_str, etl , " ")

etl <- "                                           \\+"
well_str<-str_replace_all(well_str, etl , " ")

etl <- "384        V.        WELBY:   "
well_str<-str_replace_all(well_str, etl , " ")

etl <- "                                TIME AS DERIVATIVE. 385 "
well_str<-str_replace_all(well_str, etl , " ")

etl <- "IThe World and the Individual, series i., pp. 343-344.             2 Art. on 'Time,' Dict. of Phil. and Psych., vol. ii., p. 699.             3 The Pilot, 28th December, 1901.              386         V.       WELBY: "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"                               TIME          AS     DERIVATIVE.           387   "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"   1 \"Time as Related to Causality and to Space,\" MIND, April, 1899, p. 218.    2MINDa, January, 1880, pp. 72-73.        388         V.        WELBY:  "
well_str<-str_replace_all(well_str, etl , " ")

etl <-"                  'The Metaphysics of ANature, pp. 178-179.                  2Ibid., pp. 295-296.                                  TIME         AS     DERIVATIVE.         389  "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"                               I Second edit., p. 180.           390          V.       WELBY:     "
well_str<-str_replace_all(well_str, etl , " ")

etl <- "2Principtes of Psychology, vol. i, p. 623. 3 Ibid., p. 623 (footnote).                               TIME         AS      DERIVATIVE.             391  "
well_str<-str_replace_all(well_str, etl , " ")

etl <- "TIME         AS      DERIVATIVE.             391  "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"                 I Philosophical Review, July, 1902, p. 372.     392         V.        WELBY:   "
well_str<-str_replace_all(well_str, etl , " ")

etl <-"             1 Essai sur les Donntes ImrnEdiates de la Conscience.              2 Matiere et Meraoire.                               TIME AS DERIVATIVE. 393  "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"    394          V.        WELBY:   "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"                                   TIME AS DERIVATIVE. 395  "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"           396         V.        WELBY:   "
well_str<-str_replace_all(well_str, etl , " ")

etl<-" I This is admirably shown in Mr. McGilvary's Article on Green, MIND Oct., 1901.   2 CC Is Position in Time and Space Absolute or Relative? \" B. Russell, MIND, JUly, 1901, p. 299.                                          26     398         V.        WELBY:  "
well_str<-str_replace_all(well_str, etl , " ")

etl<-" , "
well_str<-str_replace_all(well_str, etl , " ")

etl<-" 2 "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"    "
well_str<-str_replace_all(well_str, etl , " ")

etl<-"   "
well_str<-str_replace_all(well_str, etl , " ")

return(well_str)

}
#=============================================
#Main Section
#Reads book from Gutenberg Project
#=============================================

#=============================================
#Proces PDF files
#=============================================
#getwd())
#put pdfs in that directory
wellby <- pdf_text("timederivative.pdf")
well_str <- process_well(wellby)

newBk <- gsub("[^A-Za-z0-9 .,?!]", " ", well_str)  
#experiment with the exclusions
complexity_limit = 10 #20
nm = "Time as Derivative"
rw_sen <- rw_summary(newBk, rwList, complexity_limit, "P", nm)

gmr10 <- lex_summary(newBk)

#==============================================
#Process text file
#==============================================
inTxt <- readtext("Smoking_prevalnce.txt")
inTxt <- readtext("adsb.txt")
inTxt <- readtext("HB2694.txt")
str(inTxt$text)
complexity_limit = 10 
nm = "HB 2694"

#get rid of new line char
inStr <- str_replace_all(inTxt$text, "[\\n]" , " ")
inSen <- rw_summary(inStr, rwList, complexity_limit, "P", nm)

gmr10 <- lex_summary(inStr)

#===============================================
#Process Gutenberg Files
#===============================================
#i = 8309 # 1 Sam 
#i=8347 #for Matthew
#i = 8348 # for Mark
#i = 8352 for Romans

Gutenberg_ID = 15962 # for Taxes

#need book as df for some work and in original format for others
gmr1<-as.data.frame(list(gutenberg_download(Gutenberg_ID, mirror = "http://mirrors.xmission.com/gutenberg/")))
gmr2<-gutenberg_download(Gutenberg_ID, mirror = "http://mirrors.xmission.com/gutenberg/")
if (START=="entity"){
  #extract names
  extList <- extName(gmr2)
}

#get rid of stop words
gmr2$text<-lapply(gmr2$text, function(x) {
    t <- unlist(strsplit(x, " "))
    t[t %nin% stopWords]})

#Put back into chr vec
gmr3<-as.data.frame(gmr2)
gmr3$text<-paste( unlist(gmr3$text), collapse=' ')

if (START == "ngrams") {
  #get ngrams does not work well with extract names
  ng <- ngrams(gmr3$text)
  ng_perc<-get.phrasetable(ng)
  #show top three ngrams
  for (l in 1:3){
     print(ng_perc$ngrams[l])
  }
}

#do other analysis
chaps <- gmr1 %>% 
  dplyr::mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, "Chapter"))) 

#Gutenberg gives corpus as lines not sentences.
#each line may have 1 or 2 sentence parts
#next - combine lines into a new corpus
#then tokenize that corpus into sentences

#+++++++++++++++++++++++++++++++++++++++++++++++
#===============================================
#AT THIS POINT JUST SEND ONE CHAPTER AT A TIME TO 
#THE ALGO BELOW - USE CHAPS NOT GMR1
#===============================================
#+++++++++++++++++++++++++++++++++++++++++++++++

#get the last chapter number
lstCH <- tail(chaps, n=1)$chapter

if (lstCH == 0) {
   chBeg = 0
} else {
   chBeg = 1
}

for (CH_ID in chBeg: lstCH) {
  ch <- chaps[chaps$chapter == CH_ID,]

  #get rid of ch-verse identifiers
  #transformation in ETL
  #if Taxes then only take lines ...
  cht <- ch$text[1501:1636]
  vVec <- c()
  chVec <- c()
  
  #need to get length of chaps in book for i
  #for each ch, get the length in sentences for j
  y <- as.character(CH_ID)
  for (j in 1:32) { #32 is the number of 1:i phrases in Romans
       z <- as.character(j)
       verse <- paste(y, ":", z, "..", sep="") #why does tyhis workl
       vVec[j]=verse
       gmrCh <- paste("Romans Chapter ",y,sep="")
       chVec[j] = gmrCh
  }
  for (k in 1:32){
    cht <- str_replace_all( cht, vVec[k], "")
    cht <- str_replace_all( cht, chVec[k], "")
  }

  ch_sen <- cht %>%
    paste(collapse = " ") %>%
    tokenize_sentences()

  #convert vec to sentence
  linelen = length(ch_sen[[1]])
  newBk  = ch_sen[[1]][1]
  for (j in 2:linelen) {
    newBk = paste(newBk, ch_sen[[1]][j], sep = " ")
  }

  #Set to 200 if it should not be considered
  complexity_limit <- 200 # no limit
  #rw_summary(newBk, rwList, complexity_limit) 

  if ((START == "lexr") || (START == "comb")) { 
      gmr10 <- lex_summary(newBk)
      #convert vec to sentence
      linelen = length(gmr10)
      newBk  = gmr10[1]
      for (j in 2:linelen) {
        newBk = paste(newBk, gmr10[j], sep = " ")
      }
  }

  if ((START == "rw") || (START == "comb")) { 
     newBk <- gsub("[^A-Za-z0-9 .,?!]", " ", newBk)  #experiment with the exclusions
     rw_summary(newBk, rwList, complexity_limit, "G", "G-ID")
  }
  #lex_summary(newBk)
  #rw_dist(newBk, CH_ID)

}
