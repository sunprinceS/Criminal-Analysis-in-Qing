# install.packages("tm")
# install.packages("slam")
# install.packages("rJava")
# install.packages("~/Downloads/tmcn_0.1-4.tar.gz", repos=NULL, type="source") 
# install.packages("~/Downloads/Rwordseg_0.2-1.tar.gz", repos=NULL, type="source")

library("tm")
library("rJava")
library("Rwordseg")
library("tmcn")
library("slam")
library("XML")
library("irlba")
library("futile.matrix")
library("ngram")

### Read All Require Data
xmlfile <- xmlTreeParse("test/query-test.xml")
long_vec <- read.table('out/wordvec.txt')
vocab.all <- readLines('test/vocab.all')
vocab.all <- vocab.all[-1]
stop.word <- readLines('test/stopword.tw')
m <- read.table('out/rev.txt', sep =",", header = FALSE)
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3]) 
#if use svd : s <- irlba(m.rev, nv = 100) 

xmltop = xmlRoot(xmlfile) # Read XML
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
# summary(plantcat_df)


### Generate All Term
long.q <- vector(mode="character",length=nrow(long_vec))
for(i in 1:nrow(long_vec)){
  if(long_vec[i,2]==-1){ long.q[i] <- paste(vocab.all[long_vec[i,1]]) }
  else{ long.q[i] <- paste( vocab.all[long_vec[i,1]], vocab.all[long_vec[i,2]], sep = "") }
} # length(long.q)


### Query extraction
insertWords(c('白色', '恐怖', '受难', '行政', '国防', '立法', '匪谍',
              '罚锾',
              '科省', '柯省', '科索', '柯索', '难民', '马其', '其顿', '土耳', '耳其', '阿尔', '巴尼',
              '中美',
              '两税', '业升', '抵减', '经济', '财政', '经建', '行政',
              '护盘', '公积', '资本', '证期', '董监', '央行', '增值', '印花', '证交',
              '麦可', '乔丹', '乔登', '代言', '耐吉', '史腾', '博德',
              '晓燕', '白案', '掳人', '勒赎', '合议', '进兴', '志辉', '素真',
              '儿扶',
              '迪士', '士尼',
              '策进', '招策', '教育',
              '培训', '国小', '教学', '检核',
              '美浓',
              '栖兰', '桧木', '枯立', '倒木', '退辅', '农委', '森保', '林务',
              '黑面', '琵鹭', '曾文', '圣婴', '猩红', '登革', '革热', '肠病'))










all_ranking_list <- data.frame() 
for(NUM_OF_QUERY in 1:nrow(plantcat_df)){
  
  ### Clean Query
  str <- as.character(plantcat_df[NUM_OF_QUERY,c(2)]) 
  str2 <- as.character(plantcat_df[NUM_OF_QUERY,c(3)]) 
  str3 <- as.character(plantcat_df[NUM_OF_QUERY,c(5)]) 
  insertWords(segmentCN(str))
  insertWords(segmentCN(str2))
  insertWords(segmentCN(str3))
  
  d.corpus <- Corpus(VectorSource(plantcat_df[NUM_OF_QUERY,c(5)]))
  d.corpus <- tm_map(d.corpus, content_transformer(segmentCN))
  d.corpus <- tm_map(d.corpus, removePunctuation)
  d.corpus <- tm_map(d.corpus, removeNumbers)
  d.corpus <- tm_map(d.corpus, removeWords, stop.word)
  tdm <- TermDocumentMatrix(d.corpus,control = list(wordLengths = c(1, 2)))
  temp <- as.matrix(inspect(tdm))
  
  s_clean_query <- cbind(rownames(temp),rowSums(temp))
  rownames(s_clean_query) <- rownames(1:nrow(s_clean_query))
  s_query_fin <- s_clean_query[which(s_clean_query[,1] %in% long.q),]
  
  ### Find Query Index in All_term (long.q)
  loc <- which(long.q %in% c(s_query_fin[,1]))
  loc.word <- long.q[loc]
  num.long.q <- vector(mode="integer",length=length(long.q))
  for(i in 1:length(loc)){
    num.long.q[loc[i]] <- as.numeric( s_query_fin[which(loc.word[i] == s_query_fin[,1]),2] ) 
  }
  
  
  ### Ranking : BM25
  query.temp <- as.numeric(s_query_fin[,2])
  k3 <- 2
  query.rev <- ((k3+1)*query.temp) / (k3+query.temp)
  doc.vec <- m.rev[which(num.long.q!=0),]
  
  innerproduct <- function(x,y) x %*% y # / sqrt(x%*%x * y%*%y)

  cos_list <- apply(doc.vec[,],2,innerproduct,y = as.numeric(query.rev))
  rank <- order(cos_list, decreasing = TRUE)
  print(c("bottleneck end", NUM_OF_QUERY))
  
  ### Rocchio feedback (performance not good)
  # top_hundred <- apply(m.rev[,rank[1:100]],1,sum)
  # new_para_loc <- which(top_hundred!=0)
  # new_para <- long.q[new_para_loc]
  # new_para_val <- apply(m.rev[new_para_loc,rank[1:100]], 1, mean)
  # 
  # ori.query <- vector(mode="integer",length=length(new_para_loc))
  # for(i in 1:length(s_query_fin[,1])){
  #   search_loc <- which(new_para ==s_query_fin[i,1])
  #   if(length(search_loc)!=0){
  #     ori.query[search_loc] <- new_para_val[search_loc]
  #   }
  # }
  # 
  # new.query <- ori.query + 0.8*new_para_val
  # 
  # query.rev <- new.query
  # doc.vec <- m.rev[new_para_loc,]
  
  
  ### Output Ranking Ans
  file.list <- read.table("test/file-list", header = FALSE)
  rank.answer <- c(tolower(substr(file.list[rank[1:100],1],17,31)))
  topic.num <- substr( plantcat_df[NUM_OF_QUERY,1] , 15,17)
  ans.out <- cbind(rep(topic.num,length(rank.answer)),rank.answer)
  
  all_ranking_list <- rbind(all_ranking_list,ans.out)

}

write.table(all_ranking_list, file="out/ranking_list.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)


### Compare to answer
# ans.train <- read.table("test/ans-train")
# ans.train.1 <- ans.train[which(ans.train$V1==NUM_OF_ANS),2]
# sum(rank.answer %in% ans.train.1)

