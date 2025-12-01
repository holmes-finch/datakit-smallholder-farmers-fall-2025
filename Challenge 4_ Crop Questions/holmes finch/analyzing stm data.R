
attributes(all.files.stm)
belonging.out<-prepDocuments(all.files.stm$documents, all.files.stm$vocab, lower.thresh=15)

# inspect speech_corpus
summary(all.files.stm)
# check date range
date <- docvars(all.files.stm, "date")
date
# check title
title <- docvars(all.files.stm, "title")
title
# check size of texts
ntoken(all.files.stm)
is.corpus(all.files.stm)


# do some simple pre-processing (tokenization and removing punctuation)
# then, convert the corpus to a quanteda document-feature matrix (dfm)
# and trim the dfm to remove low frequency terms
# (see https://tutorials.quanteda.io/ for more info)
toks <- tokens(all.files.stm, remove_punct = TRUE)
mattering.dfm <- dfm(toks) %>%
  dfm_trim(min_termfreq = 5) 
print(mattering.dfm)

# check the most frequent words
topfeatures(mattering.dfm, 20)

mattering.stm<-stm(mattering.dfm, K=6)
mattering.select<-selectModel(mattering.dfm, K=6)
plotModels(mattering.select, pch=c(1,2,3,4))
mattering.searchK<-searchK(mattering.dfm)

# as we have only 5 topics in this learning example, we can easily inspect them
# labelTopics function helps us to assign intuitive labels to the topics based on the most frequent words in each topic
labelTopics(mattering.stm)
# we can also plot the topics to get a visual representation of how the words are distributed across the topics
# and how prevalent the topics are in the corpus

plot(mattering.stm)
# findThoughts function helps us to find the most relevant speech for each topic
# and, based on this, we can assign a more valid label to the topic
findThoughts(mattering.stm,texts = mattering.dfm, n = 2, topics = c(2))

topicQuality(model=mattering.stm, documents=mattering.dfm)
