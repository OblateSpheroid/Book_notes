### Kwartler notes ###

# Chapter 2
options(stringsAsFactors=F)

library(stringi)
library(stringr)
library(qdap)

# Download https://raw.githubusercontent.com/kwartler/text_mining/master/oct_delta.csv
text.df<-read.csv('oct_delta.csv')
head(text.df)
mean(nchar(text.df$text))

text.df<-subset(text.df,nchar(text.df$text)>0) # remove empty tweets
text.df$text <- gsub('pls', 'please', text.df$text,
                     ignore.case=F) # replace 'pls' with 'please', global
#text.df$text <- gsub('[[:punct:]]', '', text.df$text) # remove all punctuation from texts

patterns<-c('pls','thx','thanks')
replacements<-c('please','thank you','thank you')
text.df$text <- mgsub(patterns, replacements,
                      text.df$text) # use multiple patterns, replacements
                                    # from qdap::mgsub

patterns<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
replacements<-seq(1:12)
text.df$month<-mgsub(patterns,replacements,text.df$month) # change months to integers
text.df$combined<-paste(text.df$month,text.df$date,text.df$year, sep='-') %>% 
                    as.Date(format="%m-%d-%Y") # build full dates

agents<-strsplit(text.df$text,'[*]') # each agent signs with '*II'
agent<-character(length(agents))
for (i in 1:length(agents)) {
	last <- length(agents[[i]])
	if(last<2)(last<-2)
	agent[i] <- agents[[i]][last] }

grep('sorry', text.df$text,ignore.case=T) # find docs that contain 'sorry', returns index
sorry<-grepl('sorry', text.df$text,ignore.case=T) # evaluate each doc, returns T/F
grep('sorry|apologize',text.df$text,ignore.case=T)

library(stringi)
stri_count(text.df$text, fixed='http') # counts of http per document

patterns <- with(text.df, grepl('http', text.df$text) & grepl('DM', text.df$text))
text.df[patterns,5]

require(tm)
tweets <- data.frame(ID=seq(1:nrow(text.df)), text=text.df$text)
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')
clean.corpus<-function(corpus) {
  corpus <- tm_map(corpus,
                   content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

# Chapter 8
install.packages('openNLP', repos = "http://cran.us.r-project.org")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/",
	             type = "source")

# download https://github.com/kwartler/text_mining/blob/master/hillary-clinton-emails-release-2015-09-11-01-39-01.zip?raw=true

persons <- Maxent_Entity_Annotator(kind = 'person')
locations <- Maxent_Entity_Annotator(kind = 'location')
organizations <- Maxent_Entity_Annotator(kind = 'organization')
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")




