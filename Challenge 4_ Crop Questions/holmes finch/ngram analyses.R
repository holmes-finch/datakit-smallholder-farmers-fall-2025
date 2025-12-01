library(ngram)
african_questions.str<-concatenate(lapply(african_questions.docs1, "[",1))
head(african_questions.str)

#BIGRAM#
african_questions.bigram<-ngram(african_questions.str, n=2)
print(african_questions.bigram, output="full")
african_questions.bigram.freq<-get.phrasetable(african_questions.bigram)
african_questions.bigram.freq[1:20,1:2]


#TRIGRAM#
african_questions.trigram<-ngram(african_questions.str, n=3)
#print(african_questions.trigram, output="full")
african_questions.trigram.freq<-get.phrasetable(african_questions.trigram)
african_questions.trigram.freq[1:20,1:2]

#4GRAM#
african_questions.4gram<-ngram(african_questions.str, n=4)
#print(african_questions.4gram, output="full")
african_questions.4gram.freq<-get.phrasetable(african_questions.4gram)
african_questions.4gram.freq[1:10,1:2]

