library(twitteR)
library(RCurl)
library(tm)

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  # Unexpectedly, stemCompletion completes an empty string to
  
  # a word in dictionary. Remove empty string to avoid above issue.
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}

consumer_key <- "pCZHoQUEtKXvntrTRNbzYLtBU"
consumer_secret <- "ZJALW6HuEUnjIIN0nSXaObN4rgjk77x6540FHADSzYIQvgo3Nk"
access_token <- "3758958376-n7tSSXcJat9TuGuMdhLefc8XKx0yALjQ4XnT3lO"
access_secret <- "0iBP6MXdWsZ0n0GfIqRlPeSDA6iIbSFogYVXDBH0L0j09"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

###Preberem twitter accounte###################

naslov<-read.csv(file = "twitter.csv", header = FALSE, sep=";", stringsAsFactors = FALSE)
naslov<-as.list(naslov[,2])

#Začem pobirati dol
for (i in 1:length(naslov))
{
 
  trenutni<-naslov[[i]]
  tweets <- userTimeline(trenutni, n = 3200)
  
  tweets.df <- twListToDF(tweets)
  
  if (nrow(tweets.df)==0) {
    next
  }
  
  dolzina<-nrow(tweets.df)
  
  ociscentext<-removeURL(tweets.df$text)
  
  ociscentext<-removeNumPunct(ociscentext)
  
  myCorpus <- Corpus(VectorSource(tweets.df$text))
  
  # convert to lower case
  
  # tm v0.6
  
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  # tm v0.5-10
  
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  
  myStopwords <- c(stopwords('english'))
  
  
  # remove stopwords from corpus
  
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  
  # remove extra whitespace
  
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  
  myCorpusCopy <- myCorpus
  
  # stem words
  
  myCorpus <- tm_map(myCorpus, stemDocument)
  
  myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
  
  myCorpus <- Corpus(VectorSource(myCorpus))
  
  podatki <- data.frame(trenutni,ociscentext,text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)
  
  print("Shranjevanje.....")
  print(i)
  
  filenm = paste("twitter_", trenutni,".Rda", sep = "")
  save(dfrating, file = filenm)
  
}

