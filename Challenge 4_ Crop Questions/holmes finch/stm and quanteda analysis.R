library(stm)
library(quanteda)
library(ggplot2)
library(tidyr)
library(tidytable)
library(stminsights)
library(ggraph)

processed <- textProcessor(data$documents, metadata = data)
african_questions_processed<-textProcessor(african_questions)

african_questions_out <- prepDocuments(african_questions_processed $documents, african_questions_processed $vocab, african_questions_processed $meta)
african_questions_docs <- african_questions_out$documents
african_questions_vocab <- african_questions_out$vocab
african_questions_meta <-african_questions_out$meta

plotRemoved(african_questions_processed $documents, lower.thresh = seq(1, 200, by = 100))
african_questions_out2 <- prepDocuments(african_questions_processed$documents, african_questions_processed$vocab, african_questions_processed$meta, lower.thresh = 15)

african_questions.stm20 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 20, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

african_questions.stm5 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 5, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

african_questions.stm10 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 10, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

#EXPLORE VARIOUS NUMBERS OF TOPICS#

#LEE AND MIMMO 2014 METHOD#
african_questions.stm0 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 0, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

african_questions_topics <- searchK(african_questions_out$documents, african_questions_out$vocab, K = c(3,4,5,6,7,8,9,10)) 

african_questions_topicsB <- searchK(african_questions_out$documents, african_questions_out$vocab, K = c(3,4,5,6,7,8,9,10,
						11,12,13,14,15,16,17,18,19,20)) 

african_questions_topicsB.results<-data.frame(african_questions_topicsB$results)

plot(african_questions_topicsB)
						
plot(african_questions_topicsB.results$K, african_questions_topicsB.results$residual, type = "l", lty = 1, xlab="Topic number", ylab="Residual", main="Residuals by topic number")
axis(1, at = seq(3, 20, by = 1))

plot(african_questions_topicsB.results$K, african_questions_topicsB.results$exclus, type = "l", lty = 1, xlab="Topic number", ylab="Exclusivity", main="Exclusivity by topic number")
axis(1, at = seq(3, 20, by = 1))

plot(african_questions_topicsB.results$K, african_questions_topicsB.results$semcoh, type = "l", lty = 1, xlab="Topic number", ylab="Coherence", main="Coherence by topic number")
axis(1, at = seq(3, 20, by = 1))

plot(african_questions_topicsB.results$K, african_questions_topicsB.results$lbound, type = "l", lty = 1, xlab="Topic number", ylab="LBound", main="LBound by topic number")
axis(1, at = seq(3, 20, by = 1))

plot(african_questions_topicsB.results$K, african_questions_topicsB.results$heldout, type = "l", lty = 1, xlab="Topic number", ylab="Holdout", main="Holdout by topic number")
axis(1, at = seq(3, 20, by = 1))


#FIT MOST LIKELY MODELS#
african_questions_selectmodel5 <- selectModel(african_questions_docs, african_questions_vocab, K=5, runs=5)

#FIT CANDIDATE MODELS 5, 6, AND 7#
african_questions.stm5 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 5, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

african_questions.stm6 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 6, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")

african_questions.stm7 <- stm(documents = african_questions_out$documents, vocab = african_questions_out$vocab,
K = 7, max.em.its = 150, data = african_questions_out$meta, init.type = "Spectral")


#EXPLORE THE MODELS#
#5 TOPICS#
labelTopics(african_questions.stm5)
plot.STM(african_questions.stm5, type="labels")
plot.STM(african_questions.stm5, type="summary", n=10)
plot.STM(african_questions.stm5, type="hist")
plot.STM(african_questions.stm5, type="perspectives", topics=c(1,2))

findThoughts(african_questions.stm5, text= african_questions, topics=c(1,2), n=3)

cloud(african_questions.stm5, topic = 1, scale = c(2,.25))
cloud(african_questions.stm5, topic = 2, scale = c(2,.25))
cloud(african_questions.stm5, topic = 3, scale = c(2,.25))
cloud(african_questions.stm5, topic = 4, scale = c(2,.25))
cloud(african_questions.stm5, topic = 5, scale = c(2,.25))

#6 TOPICS#
labelTopics(african_questions.stm6)
plot.STM(african_questions.stm6, type="labels")
plot.STM(african_questions.stm6, type="summary", n=10)
plot.STM(african_questions.stm6, type="hist")
plot.STM(african_questions.stm6, type="perspectives", topics=c(1,2))

#7 TOPICS#
labelTopics(african_questions.stm7)
plot.STM(african_questions.stm7, type="labels")
plot.STM(african_questions.stm7, type="summary", n=10)
plot.STM(african_questions.stm7, type="hist")
plot.STM(african_questions.stm7, type="perspectives", topics=c(1,2))

cloud(african_questions.stm7, topic = 1, scale = c(2,.25))
cloud(african_questions.stm7, topic = 2, scale = c(2,.25))
cloud(african_questions.stm7, topic = 3, scale = c(2,.25))
cloud(african_questions.stm7, topic = 4, scale = c(2,.25))
cloud(african_questions.stm7, topic = 5, scale = c(2,.25))
cloud(african_questions.stm7, topic = 6, scale = c(2,.25))
cloud(african_questions.stm7, topic = 7, scale = c(2,.25))

african_questions.stm7.corr <- topicCorr(african_questions.stm7, cutoff=0.1)
plot(african_questions.stm7.corr)

#STM INSIGHTS#
african_questions_network7<-get_network(african_questions.stm7)

ggraph(african_questions_network7, layout = 'auto') +
geom_edge_link(
aes(edge_width = weight),
label_colour = '#fc8d62',
edge_colour = '#377eb8') +
geom_node_point(size = 4, colour = 'black') +
geom_node_label(
aes(label = name, size = props),
colour = 'black', repel = TRUE, alpha = 0.85) +
scale_size(range = c(2, 10), labels = scales::percent) +
labs(size = 'Topic Proportion', edge_width = 'Topic Correlation') +
scale_edge_width(range = c(1, 3)) +
theme_graph()


