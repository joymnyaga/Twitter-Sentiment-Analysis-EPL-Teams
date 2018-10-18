#Call libraries
library(NLP)
library(ROAuth)
library(twitteR)
library(syuzhet)
library(tm)
library(SnowballC)
library(stringi)
library(topicmodels)

#Access Twitter
consumer_key = "vwNbVuZtgNULNitX3ojlxcQsK" 
consumer_secret = "5H6jJyayJOvFwPg5uSsQ4lrffrs8sFcurRqqYknQ53WDiTpv83"
access_key = "4829791752-DXJ5O4L3rd2eZFtOTcUYzcsFRWKCSAA8MeMepVl"
access_secret = "ouGK1xzaGnsuc8SatHTFXdP4iWheUMcNdDOSuTEmH4DYE"
setup_twitter_oauth(consumer_key, consumer_secret, access_key, access_secret)

#Extract tweets
tweets_mu <- searchTwitter("#manutd", n=1000,lang = "en")
tweets_a <- searchTwitter("#arsenal", n=1000,lang = "en")
tweets_ch <- searchTwitter("#chelsea", n=1000,lang = "en")
tweets_mc <- searchTwitter("#mancity", n=1000,lang = "en")

#Convert to dataframe
mufc_tweets <- twListToDF(tweets_mu)
arsenal_tweets <- twListToDF(tweets_a)
chelsea_tweets <- twListToDF(tweets_ch)
mancity_tweets <- twListToDF(tweets_mc)

#Preprocess data
manutd_text<- mufc_tweets$text
arsenal_text<- arsenal_tweets$text
chelsea_text<- chelsea_tweets$text
mancity_text<- mancity_tweets$text

#Convert all text to lower case
manutd_text<- tolower(manutd_text)
arsenal_text<- tolower(arsenal_text)
chelsea_text<- tolower(chelsea_text)
mancity_text<- tolower(mancity_text)

#Replace blank space ("rt")
manutd_text <- gsub("rt", "", manutd_text)
arsenal_text <- gsub("rt", "", arsenal_text)
chelsea_text <- gsub("rt", "", chelsea_text)
mancity_text <- gsub("rt", "", mancity_text)

# Replace @UserName
manutd_text <- gsub("@\\w+", "", manutd_text)
arsenal_text <- gsub("@\\w+", "", arsenal_text)
chelsea_text <- gsub("@\\w+", "", chelsea_text)
mancity_text <- gsub("@\\w+", "", mancity_text)

# Remove punctuation
manutd_text <- gsub("[[:punct:]]", "", manutd_text)
arsenal_text <- gsub("[[:punct:]]", "", arsenal_text)
chelsea_text <- gsub("[[:punct:]]", "", chelsea_text)
mancity_text <- gsub("[[:punct:]]", "", mancity_text)

# Remove links
manutd_text <- gsub("http\\w+", "", manutd_text)
arsenal_text <- gsub("http\\w+", "", arsenal_text)
chelsea_text <- gsub("http\\w+", "", chelsea_text)
mancity_text <- gsub("http\\w+", "", mancity_text)

# Remove tabs
manutd_text <- gsub("[ |\t]{2,}", "", manutd_text)
arsenal_text <- gsub("[ |\t]{2,}", "", arsenal_text)
chelsea_text <- gsub("[ |\t]{2,}", "", chelsea_text)
mancity_text <- gsub("[ |\t]{2,}", "", mancity_text)

# Remove blank spaces at the beginning
manutd_text <- gsub("^ ", "", manutd_text)
arsenal_text <- gsub("^ ", "", arsenal_text)
chelsea_text <- gsub("^ ", "", chelsea_text)
mancity_text <- gsub("^ ", "", mancity_text)

# Remove blank spaces at the end
manutd_text <- gsub(" $", "", manutd_text)
arsenal_text <- gsub(" $", "", arsenal_text)
chelsea_text <- gsub(" $", "", chelsea_text)
mancity_text <- gsub(" $", "", mancity_text)

#Create corpus
manutd_corpus=Corpus(VectorSource(manutd_text))
arsenal_corpus=Corpus(VectorSource(arsenal_text))
chelsea_corpus=Corpus(VectorSource(chelsea_text))
mancity_corpus=Corpus(VectorSource(mancity_text))

#Clean up by removing stop words
manutd_corpus <- tm_map(manutd_corpus, removeWords,stopwords(kind = 'en'))
arsenal_corpus <- tm_map(arsenal_corpus, removeWords,stopwords(kind = 'en'))
chelsea_corpus <- tm_map(chelsea_corpus, removeWords,stopwords(kind = 'en'))
mancity_corpus <- tm_map(mancity_corpus, removeWords,stopwords(kind = 'en'))C

#Getting emotions using in-built function
mysentiment_manutd<-get_nrc_sentiment((manutd_text))
mysentiment_arsenal<-get_nrc_sentiment((arsenal_text))
mysentiment_chelsea<-get_nrc_sentiment((chelsea_text))
mysentiment_mancity<-get_nrc_sentiment((mancity_text))

#calculationg total score for each sentiment
Sentimentscores_manutd<-data.frame(colSums(mysentiment_manutd[,]))
Sentimentscores_arsenal<-data.frame(colSums(mysentiment_arsenal[,]))
Sentimentscores_chelsea<-data.frame(colSums(mysentiment_chelsea[,]))
Sentimentscores_mancity<-data.frame(colSums(mysentiment_mancity[,]))

names(Sentimentscores_manutd)<-"Score"
Sentimentscores_manutd<-cbind("sentiment"=rownames(Sentimentscores_manutd),Sentimentscores_manutd)
rownames(Sentimentscores_manutd)<-NULL

names(Sentimentscores_arsenal)<-"Score"
Sentimentscores_arsenal<-cbind("sentiment"=rownames(Sentimentscores_arsenal),Sentimentscores_arsenal)
rownames(Sentimentscores_arsenal)<-NULL

names(Sentimentscores_chelsea)<-"Score"
Sentimentscores_chelsea<-cbind("sentiment"=rownames(Sentimentscores_chelsea),Sentimentscores_chelsea)
rownames(Sentimentscores_chelsea)<-NULL

names(Sentimentscores_mancity)<-"Score"
Sentimentscores_mancity<-cbind("sentiment"=rownames(Sentimentscores_mancity),Sentimentscores_mancity)
rownames(Sentimentscores_mancity)<-NULL


#Plotting the sentiments with scores
library(ggplot2)
ggplot(data=Sentimentscores_manutd,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Manchester United")

ggplot(data=Sentimentscores_arsenal,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Arsenal")

ggplot(data=Sentimentscores_chelsea,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Chelsea")

ggplot(data=Sentimentscores_mancity,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Manchester City")
