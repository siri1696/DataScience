
library("twitteR")

library("ROAuth")

cred <- OAuthFactory$new(consumerKey='zh5iKDe3s4q2QERnp9ubr7RmR', # Consumer Key (API Key)
                         consumerSecret='SAGa0Pv703f1HHLJH1lLl7byGsXgMx4VxgGRnVb4J1Apq5mR1A', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("zh5iKDe3s4q2QERnp9ubr7RmR", # Consumer Key (API Key)
                    "SAGa0Pv703f1HHLJH1lLl7byGsXgMx4VxgGRnVb4J1Apq5mR1A", #Consumer Secret (API Secret)
                    "1273539865687101440-E8YTkeQBk23O7roZxvZI2azs9eUZfF",  # Access Token
                    "1AjaLdIOGIp4CfbLjBxgc7jFQTsbqtSmKpjA0QI3Fp5VZ")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('@narendramodi', n = 600,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

tweets_data=read.csv(file.choose())
tweets_data=as.data.frame(tweets_data[,1],drop = FALSE)

library(tm)
x=as.character(tweets_data$`tweets_data[, 1]`)


#corpus
x=Corpus(VectorSource(x))
inspect(x[2])
inspect(x[262])

#data cleansing

x1=tm_map(x,tolower)
inspect(x1[2])
inspect(x1[600])

x1=tm_map(x1,removePunctuation)
inspect(x1[2])
inspect(x1[600])

x1=tm_map(x1,removeNumbers)
inspect(x1[2])
inspect(x1[600])

x1=tm_map(x1,removeWords,stopwords('english'))
inspect(x1[2])
inspect(x1[600])

library(SnowballC)
x2=tm_map(x1,stemDocument)
inspect(x2[2])

library(textstem)
x3=tm_map(x2,lemmatize_strings)

inspect(x3[2])
inspect(x3[262])


###stripping white spaces
x4=tm_map(x3,stripWhitespace)
inspect(x4[262])

####3tdm
tdm=TermDocumentMatrix(x4)
tdm

m=as.matrix(tdm)
v= sort(rowSums(m),decreasing = TRUE)
d=data.frame(word=names(v),freq=v)
head(d,10)

dtm=DocumentTermMatrix(x4)
dtm


library(wordcloud2)
wordcloud2(data = d,size = 1.6,shape = 'star')

