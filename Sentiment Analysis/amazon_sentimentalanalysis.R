apple11_reviews=read.table(file.choose())


library(tm)
x=as.character(apple11_reviews$V1)


#corpus
x=Corpus(VectorSource(x))
inspect(x[2])
inspect(x[300])

#data cleansing

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

library(wordcloud2)
wordcloud2(data = d,size = 1.6,shape = 'star')
