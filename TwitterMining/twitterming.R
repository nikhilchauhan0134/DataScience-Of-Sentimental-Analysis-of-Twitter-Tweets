install.packages("twitteR")
library(twitteR)
api_key<-'5YhrBlFSbFC8NA3dAASd8Hjco'
api_secret_key<-'zavthyj6nvysGwHnkyLlvjQbW7wxzETQwdPd7PFvle5u4x14ld'
access_token<-'1192424346628677633-kUtHFXDW0e0EhWTROTqKIuxBCpa4Hp'
access_token_secret<-'1D9xI0BTa4E6qWtqhdExHOi5jrdxSSiA7irBYkXpFhRUZ'
setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)


#Details option in Twitter

modi<-getUser('narendramodi')
modi$getDescription()
modi$followersCount
modi$friendsCount
modi$getFriends(n=10)

modi_and_trump<-lookupUsers(c('narendramodi','realDonaldTrump'))
modi_and_trump$realDonaldTrump$name
modi_and_trump$narendramodi$screenName
modi_and_trump$narendramodi$lastStatus
modi_and_trump$realDonaldTrump$friendsCount


#getting tweets usertimeline tweets and sometimes tweets
modi_tweets<-userTimeline('narendramodi')
modi_tweets[1:10] 
# getting Trends data

trend_location<-availableTrendLocations()
trend_location
trends<-getTrends(2295414)
head(trends)

tweets<-searchTwitter("Trump",n=500)

#count the number of tweets
n.tweets<-length(tweets)
n.tweets
#Convert tweets into datafram
tweets.df<-twListToDF(tweets)
View(tweets.df)
#Now lets clean the data of tweets
library(tm)
mycorpus<-Corpus(VectorSource(tweets.df$text))
mycorpus<-tm_map(mycorpus,removeWords,stopwords())
#Removing any customerb stop words like Red Hat in Case
#Adding red hatv or redhatbinto stop words
myStopWords<-c(stopwords('english'),"red","hat","redhat")
remove_url<-function(x)gsub("^[:alpha:][:space:]]*","",x)
mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,content_transformer(tolower))
mycorpus<-tm_map(mycorpus,stripWhitespace)
#mycorpus<-tm_map(mycorpus,stemDocument)
dtm<-DocumentTermMatrix(mycorpus)
library(wordcloud)
wordcloud(mycorpus,min.freq = 5,random.order = FALSE)
#Sentimental anlaysis of twitter data
library(sentimentr)
tweets.df$text<-gsub("[^0-9A-Za-z///' ]","",tweets.df$text)
tweets.df$text<-gsub("http\\w+","",tweets.df$text)
#tweets.df$text<-gsub("rt","",tweets.df)
tweets.df$text<-gsub("@\\w+","",tweets.df$text)
tweets.df$text<-tolower(tweets.df$text)
emo_trump_tweets<-sentiment(tweets.df$text)
View(emo_trump_tweets)
tweets.df$sentiment<-emo_trump_tweets$sentiment
View(tweets.df)
positive_tweets<-head(tweets.df[order(emo_trump_tweets$sentiment,decreasing = T),c(1,17)],25)
positive_tweets
positive_tweets<-head(unique(tweets.df[order(emo_trump_tweets$sentiment,decreasing = T),c(1,17)]),25)
positive_tweets
write.table(positive_tweets$text,file = "~/RLanguage/Titanic/TwitterMining/Twitterminig/positive.txt",sep="\n")
negative_tweets<-head(unique(tweets.df[order(emo_trump_tweets$sentiment),c(1,17)]),25)
negative_tweets
write.table(negative_tweets$text,file ="~/RLanguage/Titanic/TwitterMining/Twitterminig/negative.txt",sep = "\n")
pos_neg_tweets<-c(positive_tweets$text,negative_tweets$text)
pos_neg_tweets
tweets_corpus<-Corpus(DirSource(directory = "~/RLanguage/Titanic/TwitterMining/Twitterminig"))
summary(tweets_corpus)
library(tm)
clean_tweets_corpus<-tm_map(tweets_corpus,tolower)
clean_tweets_corpus<-tm_map(clean_tweets_corpus,removePunctuation)
clean_tweets_corpus<-tm_map(clean_tweets_corpus,removeWords,stopwords())
clean_tweets_corpus<-tm_map(clean_tweets_corpus,stripWhitespace)
str(clean_tweets_corpus)
ctc_tdm<-TermDocumentMatrix(clean_tweets_corpus)
ctc_matrix<-as.matrix(ctc_tdm)
head(ctc_matrix)
colnames(ctc_matrix)<-c("Negative Tweets","Positive Tweets")
comparison.cloud(ctc_matrix,max.words = 500,random.order = F)


