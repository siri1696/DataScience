############# IMDB reviews Extraction ################
aurl <- "https://www.imdb.com/title/tt4154796/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}

#writing the extracted data in a .csv file
write.table(IMDB_reviews,file="imdb_reviews.txt")
length(IMDB_reviews)


#reading the data back
imdb_data=read.csv(file.choose())


library(tm)
x=as.character(imdb_data$x)


# converting to corpus
x=Corpus(VectorSource(x))
inspect(x[2])
inspect(x[300])

#data cleansing as removing stop words ,punctuations converting to lower case
#so as to clean the data

x1=tm_map(x,tolower)
inspect(x1[2])
inspect(x1[300])

x1=tm_map(x1,removePunctuation)
inspect(x1[2])
inspect(x1[300])

x1=tm_map(x1,removeNumbers)
inspect(x1[2])
inspect(x1[300])

x1=tm_map(x1,removeWords,stopwords('english'))
inspect(x1[2])
inspect(x1[300])

library(SnowballC)
x2=tm_map(x1,stemDocument)
inspect(x2[2])

library(textstem)
x3=tm_map(x2,lemmatize_strings)

inspect(x3[2])
inspect(x3[300])


###stripping white spaces
x4=tm_map(x3,stripWhitespace)
inspect(x4[300])

####3tdm
tdm=TermDocumentMatrix(x4)
tdm

m=as.matrix(tdm)
v= sort(rowSums(m),decreasing = TRUE)
d=data.frame(word=names(v),freq=v)
head(d,10)


#buiulding the wordclouds
library(wordcloud2)
#diff ways of projecting the word clouds
wordcloud2(data = d,size = 1.6,shape = 'circle')
wordcloud2(data = d,size = 1.6,shape = 'star')

letterCloud(data = d, word = "MOVIE", color='random-light' , backgroundColor="black")
