library("rJava")
library("Rwordseg")
library("tm")
library("tmcn")
library("slam")
library("XML")
library("SnowballC")

GetTermVector <- function(content){
  d.corpus <- Corpus(VectorSource(content))
  d.corpus <- tm_map(d.corpus, removePunctuation)
  d.corpus <- tm_map(d.corpus, stripWhitespace)
  d.corpus <- tm_map(d.corpus, removeNumbers)
  stop.word <- readLines('stopword.tw')
  d.corpus <- tm_map(d.corpus, removeWords, stop.word)
  d.corpus <- tm_map(d.corpus, content_transformer(segmentCN))
  extract_tdm <- TermDocumentMatrix(d.corpus,
                                    control = list(wordLengths = c(1, 2)))
  return(apply(extract_tdm, 1, sum))
}

ShortVectoLong <- function(long_term, short_vec){
  loc <- which(long_term %in% names(short_vec))
  loc_word <- long_term[loc]
  if(length(loc)==0) return(rep(1,length=length(long_term)))
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == names(short_vec))] ) 
  }
  
  return(long_vec)
}


### Read All Require Data
setwd("~/Desktop/Criminal-Analysis-in-Qing/parser")

xmlfile_2 <- xmlTreeParse("d_0002.xml")
xmltop_2 <- xmlRoot(xmlfile_2)
main_text_2 <- xmlSApply(xmltop_2[[1]], function(x) xmlSApply(x, xmlValue))
main_text_2 <- data.frame(t(main_text_2),row.names=NULL)
main_text_2 <- data.frame(lapply(main_text_2, as.character), stringsAsFactors=FALSE)

xmlfile_1 <- xmlTreeParse("d_0001.xml")
xmltop_1 <- xmlRoot(xmlfile_1)
main_text_1 <- xmlSApply(xmltop_1[[1]], function(x) xmlSApply(x, xmlValue))
main_text_1 <- data.frame(t(main_text_1),row.names=NULL)
main_text_1 <- data.frame(lapply(main_text_1, as.character), stringsAsFactors=FALSE)

main_text <- rbind(main_text_1, main_text_2)

catego <- c(rep("civil", 836), rep("criminal", 418))
main_text <- cbind(catego, main_text)


### generate term freq. in each topic and All term set 
all_term <- c()
term_vec <- list()
for(i in 1:nrow(main_text)){
  term_vec[[i]] <- GetTermVector(main_text[i,"doc_content"])
  all_term <- union(all_term, names(term_vec[[i]]))
}


### generate term document matrix
tdm <- matrix(NA, nrow = length(all_term), ncol = length(term_vec))
for(i in 1:length(term_vec)){
  long_vec <- ShortVectoLong(all_term, term_vec[[i]])
  tdm[,i] <- long_vec
}
rownames(tdm) <- all_term


### generate term catego matrix
all_catego <- unique(main_text[,"catego"])
tcm <- matrix(0, nrow = length(all_term), ncol = length(all_catego))
for(i in 1:ncol(tdm)){
  index <- which(all_catego == main_text[i,"catego"])
  tcm[,index] <- tcm[,index] + tdm[,i]
}
colnames(tcm) <- all_catego
rownames(tcm) <- all_term


### naive bayes
all_wordcount_in_a_topic <- apply(tcm,2,sum)
V_len <- length(all_term)
word_in_a_class_prob <- log2(t(apply(tcm,1,function(x) (1 + x) / (V_len + all_wordcount_in_a_topic))))

topic_doc_count <- c(836, 418)
D_len <- sum(topic_doc_count)
C_len <- length(all_catego)
class_prior_prob <- log2( (1 + topic_doc_count) / (C_len + D_len) )


### Naive Algorithm
ans_list <- vector(mode="character",length=ncol(tdm))
all_ranking_list <- c()

for(i in 1:ncol(tdm)){
  query_term_long <- tdm[,i]
  
  ### Using a Classifier
  log_for_product <- apply(word_in_a_class_prob, 2, function(x) sum(query_term_long * x)) + class_prior_prob
  
  cat(log_for_product,"\n")
  
  ans_list[i] <- names(which(log_for_product == max(log_for_product)))
  
  cat( paste(i, ans_list[i], collapse = " "), "\n" )
  
  all_ranking_list <- c(all_ranking_list, ans_list[i])
}

given_ans <- as.vector(main_text[,"catego"])
sum(given_ans == all_ranking_list) / nrow(main_text)
which(given_ans != all_ranking_list )[1:81]




##### Other try!

# ### EM algorithm
# ans_list <- vector(mode="character",length=ncol(tdm))
# all_ranking_list <- data.frame() 
# 
# for(i in 1:ncol(tdm)){
#   final_tau <- EM_AlgorithmCpp(word_freq = tdm[,i],
#                                tau = rep(1/length(all_catego), length(all_catego)),
#                                tdm = tcm
#                               )
#   
#   ans_list[i] <- all_catego[ which(final_tau == max(final_tau)) ]
#   
#   cat( paste(i, ans_list[i], collapse = " "), "\n")
#   
#   ans.out <-cbind(i, ans_list[i])
#   all_ranking_list <- rbind(all_ranking_list,ans.out)
# }

## TF / IDF
# tdm_reweight <- matrix(nrow=nrow(tdm),ncol=ncol(tdm))
# gw.idf <- gw_idf(tdm[,])
# colsum <- colSums(tdm[,])
# for(i in 1:ncol(tdm)){
#   tdm_reweight[,i] <- (tdm[,i]/colsum[i]) * gw.idf
# }

# PART B : SVD
# S <- svd(tdm_reweight)
# main.compon <- 50
# S$u <- S$u[,1:main.compon]
# S$d <- S$d[1:main.compon] 
# S$v <- S$v[,1:main.compon]
# tdm_reweight <- t(S$v)
# 
# tdm_reweight <- apply(tdm_reweight, 2, function(x) normalize.vector(x))
# query <- solve(diag(S$d)) %*% t(S$u) %*% as.matrix(sample.query)

### Ranking : inner product
# ground_answer <- substring(main_text[,"compilation_name" ],5,20)
# my_answer <- vector(mode = "character", length = length(ground_answer))
# 
# for(i in 1:ncol(tdm)){
#   query <- tdm_reweight[,i]
#   
#   innerproduct <- function(x,y) x %*% y
#   cos_list <- apply(tdm_reweight, 2, innerproduct,y = as.numeric(query))
#   rank <- order(cos_list, decreasing = TRUE)
# 
#   result_catego <- substring(main_text[rank[2],"compilation_name" ],5,20)
#   my_answer[i] <- names(sort(summary(as.factor(result_catego))))[1]
# }
# 
# sum(ground_answer==my_answer)/length(ground_answer)

# http://www.statmethods.net/advstats/cluster.html

# ### Training : svm
# library("e1071")
# dtm <- cbind(substring(main_text[,"compilation_name" ],5,20), t(tdm_reweight))
# colnames(dtm) <- paste("f", 1:ncol(dtm), sep = "")
# svm_model <- svm(f1~. , dtm, scale = TRUE)
# print(svm_model)
# svm.results <- predict(svm_model, dtm[,-1])

### Training : rpart
# library("rpart")
# dtm <- as.data.frame(cbind(substring(main_text[,"compilation_name" ],5,20), t(tdm_reweight)))
# colnames(dtm) <- paste("f", 1:ncol(dtm), sep = "")
# rpart_model <- rpart(f1~. , data=dtm)
# print(rpart_model)
# rpart.results <- predict(rpart_model, dtm[,-1])

### show result
# svm_pred <- svm.results
# test <- svm.results
# temp <- cbind(test[,1],svm_pred)
# result_table_net <- table(svm_pred,test[,1])
# result_table_net




