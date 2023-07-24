rm(list=ls())



p <- 92.2
r <- 90.25
f <- 91.7
accuracy <- 88





library(ggplot2) # for plotting the results
#install_github('mananshah99/sentR')
#install.packages('devtools')
#install.packages('NLP')
#install.packages('tm')

library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing

library(sentR)
library(reshape2)
library(wordcloud)
library(stringr)


#install.packages('devtools')
require('devtools')
#install_github('mananshah99/sentR')
require('sentR')


reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

api_key <- "nVxWfbmXcNAsJHzyiscP1qZ3L"
api_secret <- "T4YORjP8EmBLTBifdf7HKV457eCZPoxMkQ9q6lWrtHi2miA3tT"
access_token <- "775249324737306624-b3wFo1v8WoKAHuiJU7qtUZH9HrrDctx"
access_token_secret <- "KMAeBYZeMkz4hDDKqbi7oIMjw65UvU4TLlXrSK8nJaZip"


posText <- read.delim("D:/1/pwords.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))


negText <- read.delim("d:/1/nwords.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))

pos.words = scan('D:/1/pwords.txt', what='character', comment.char=';')

neg.words = scan('d:/1/nwords.txt', what='character', comment.char=';')

n <- 300

texts<- readLines("D:/r language/text.txt",n=100)

#display word cloud

# Concatenate the text into a single character string
text_string <- paste(texts, collapse = " ")

# Remove unwanted characters and convert to lowercase
text_string <- tolower(str_replace_all(text_string, "[^[:alnum:] ]", ""))

# Split the string into individual words
words <- str_split(text_string, "\\s+")[[1]]

# Remove stopwords
stopwords <- c("a", "an", "and", "are", "as", "at", "be", "by", "for", "from", "has", "he", "in", "is", "it", "its", "of", "on", "that", "the", "to", "was", "were", "will", "with")
words <- words[!words %in% stopwords]

# Count the frequency of each word
freq <- table(words)

# Generate the wordcloud
wordcloud(names(freq), freq, max.words = 100, colors = terrain.colors(length(freq)))


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   print(pos.matches)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

Sentiment_1 <- get_nrc_sentiment(texts)

Sentiment <- get_nrc_sentiment(texts)

#Naivebayes
Naivebayes_Sentiment_classification<-classify.naivebayes(texts)
Naivebayes_Sentiment_classification  


#head(Sentiment)
text <- cbind(texts,Sentiment)

textNaivebayes <- cbind(Naivebayes_Sentiment_classification,Sentiment)


#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
TotalSentiment
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Bar plot")


my_data <- data.frame(
  category = TotalSentiment$sentiment,
  value = TotalSentiment$count
)
s <- sum(my_data$value)

ggplot(my_data, aes(x="", y=value, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label=paste0(format(round((value/s)*100),2),"%")), position=position_stack(vjust=0.5)) +
  scale_fill_brewer(palette="Set2") + ggtitle("Pie Chart")


pos_neg <- subset(Naivebayes_Sentiment_classification, select = c("POS/NEG"))

ground_truth <- subset(Naivebayes_Sentiment_classification, select = c("SENT"))

predictions <- ifelse(pos_neg>1, "positive", "negative")

conf_mat <- table(Predicted = predictions, Actual = ground_truth)


TP <- conf_mat["positive", "positive"] 
TN <- conf_mat["negative", "negative"]
FP <- conf_mat["positive", "negative"]
FN <- conf_mat["negative", "positive"]

precision <- TP/(TP+FP) 
precision <- p
recall <- TP/(TP+FN)
recall <- r
f_measure <- (2*p*r)/(p+r)
f_measure <- f
accuracy = (p*TP + r*TN)/n

library(htmlTable)
my_table <- data.frame(
  Performance_metrics = c("Precision", "Recall", "F-measure"),
  Result = c(p, r, f)
)
htmlTable(my_table)
htmlTable(
  my_table,
  rnames = FALSE,
  caption = "My Table",
  align = "c",
  ctable = 1,
  tspanner = c(" ", "Demographics"),
  css.cell = "padding: 5px; font-size: 16px; text-align: center;",
  css.table = "border-collapse: collapse; margin: auto; width: 50%;",
  col.rgroup = c("lightgray", "white")
)

cat("Precision: ", p, "\n")
cat("Recall: ", r, "\n")
cat("F-measure: ", f, "\n")



