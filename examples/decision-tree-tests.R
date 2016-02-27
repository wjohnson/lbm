
gini_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

info_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(-sum(base_prob*log(base_prob,2)))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Col <- crossprob[crossprob[,1]>0,1]
  Yes_Col <- crossprob[crossprob[,2]>0,2]
  No_Node_Info <- -sum(No_Col*log(No_Col,2))
  Yes_Node_Info <- -sum(Yes_Col*log(Yes_Col,2))
  return(sum(base_prob * c(No_Node_Info,Yes_Node_Info)))
}

Gini Split: Average_Token_Length < 4.7
#Information Gain: num_imgs >= 5.5

# ## Calculating Gini Split
# gini_process(NewsPop$channel)
# 
# gini_process(NewsPop$channel,
#              NewsPop$average_token_length < 4.713221)
# 
# gini_process(NewsPop$channel,
#              NewsPop$num_imgs >= 5.5)
# 
# gini_process(NewsPop$channel,
#              NewsPop$num_keywords < 6.5)
# 
# ## Calculating Information Gain
# info_process(NewsPop$channel) - info_process(NewsPop$channel,
#                                              NewsPop$num_imgs >= 5.5)
# 
# info_process(NewsPop$channel) - info_process(NewsPop$channel,
#                                              NewsPop$average_token_length < 4.713221)