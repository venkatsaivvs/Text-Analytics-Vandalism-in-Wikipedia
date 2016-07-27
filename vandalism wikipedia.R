wiki<- read.csv("wiki.csv",stringsAsFactors = FALSE)
wiki$Vandal<-as.factor(wiki$Vandal)
str(wiki)
table(wiki$Vandal)

corpus<-Corpus(VectorSource(wiki$Added))
corpus<-tm_map(corpus,removeWords,stopwords("english"))

corpus<-tm_map(corpus,stemDocument)

corpus = tm_map(corpus, PlainTextDocument)
dtm<-DocumentTermMatrix(corpus)
dtm<-removeSparseTerms(dtm,0.997)
sparseadded<-dtm
wordsadded<-as.data.frame(as.matrix(sparseadded))
colnames(wordsadded)<-paste("A",colnames(wordsadded))


corpus<-Corpus(VectorSource(wiki$Removed))
corpus<-tm_map(corpus,removeWords,stopwords("english"))

corpus<-tm_map(corpus,stemDocument)

corpus = tm_map(corpus, PlainTextDocument)
dtm2<-DocumentTermMatrix(corpus)
dtm2<-removeSparseTerms(dtm,0.997)
sparseremoved<-dtm
wordsremoved<-as.data.frame(as.matrix(sparseremoved))
colnames(wordsremoved)<-paste("A",colnames(wordsremoved))

str(wordsremoved)

wikiWords = cbind(wordsadded, wordsremoved)
wikiWords$vandal<-wiki$Vandal

set.seed(123)
spl<-sample.split(wikiWords$vandal,SplitRatio = 0.7)
train<-subset(wikiWords,spl==TRUE)
test<-subset(wikiWords,spl==FALSE)

table(test$vandal)
1443+1270
1443/2713

618/(618+545)

wikicart<-rpart(vandal ~ .,data = train,method="class")
pred<-predict(wikicart,newdata = test,type = "class")
table(test$vandal,pred)
630/nrow(test)

prp(wikicart)

wikiWords2 = wikiWords

Make a new column in wikiWords2 that is 1 if "http" was in Added:
  
  wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)


wikiTrain2 = subset(wikiWords2, spl==TRUE)

str(wikiTrain2$HTTP)

wikiTest2 = subset(wikiWords2, spl==FALSE)
wikicart2<-rpart(vandal ~ .,data = wikiTrain2,method="class")
pred2<-predict(wikicart2,newdata = wikiTest2,type = "class")
table(wikiTest2$vandal,pred2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtm))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtm2))

mean(wikiWords2$NumWordsAdded)

mean(wikiWords2$NumWordsAdded)


spl<-sample.split(wikiWords2$vandal,SplitRatio = 0.7)
train<-subset(wikiWords2,spl==TRUE)
test<-subset(wikiWords2,spl==FALSE)


wikicart<-rpart(vandal ~ .,data = train,method="class")
pred<-predict(wikicart,newdata = test,type = "class")
table(test$vandal,pred)

(345+333)/nrow(test)

(604+70)/nrow(wikiTest2)
