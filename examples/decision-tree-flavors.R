library(rpart)
library(rpart.plot)
source("decision-tree-tests.R")
#https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip",
              "NewPop.zip",method="curl")
unzip("NewPop.zip",
      c("OnlineNewsPopularity/OnlineNewsPopularity.csv"),
      junkpaths = T)

compress_class<-function(vector){
  if(any(vector==1)){
    return (min(which(vector==1)))
  }
  else{
    return(0)
  }
}

find_top_n <- function(x, n){
  sort_order <- order(x,decreasing=T)
  return(sort_order[1:n])
}

NewsPop<-read.csv("OnlineNewsPopularity.csv",
                  header=T)

### Quick look at the data
table(sapply(NewsPop,class))

## Process the Data

weekday_name<-gsub('weekday_is_','',names(NewsPop)[32:38])

weekday_int<-(apply(as.matrix(NewsPop[,32:38]),1,compress_class))
weekday_factor<-factor(weekday_int,
                       levels = as.character(seq(1,7)),
                       labels = weekday_name)
NewsPop$weekday <- weekday_factor

drop_names <- names(NewsPop)[32:38]

summary(NewsPop$n_tokens_content)

NewsPop$article_length <- factor(as.numeric(cut(NewsPop$n_tokens_content,
          c(0,250,500,716,max(NewsPop$n_tokens_content)))),
       levels = seq(1,4), labels = c('short','avg','long',
                                     'very long'))

drop_names <- c(drop_names, 'n_tokens_content')

table(NewsPop$num_videos>0)

NewsPop$has_vid <-factor(as.numeric(NewsPop$num_videos>0),
                         levels = c(0,1), labels = c('Vid','No Vid'))

drop_names <- c(drop_names, 'num_videos')

### Polarity Ranges
NewsPop$pos_polar_range <- NewsPop$max_positive_polarity - NewsPop$min_positive_polarity
NewsPop$neg_polar_range <- NewsPop$max_negative_polarity - NewsPop$min_negative_polarity

drop_names <- c(drop_names,'max_positive_polarity','min_positive_polarity',
                'avg_positive_polarity','max_negative_polarity',
                'min_negative_polarity','avg_negative_polarity')

## Create Class Variable

channel_names<-gsub('data_channel_is_','',names(NewsPop)[14:19])
channel_names<-c('unknown',channel_names)

channel_int<-(apply(as.matrix(NewsPop[,14:19]),1,compress_class))
channel_factor<-factor(channel_int,
                       levels = as.character(seq(0,6)),
                       labels = channel_names)
NewsPop$channel <- channel_factor

drop_names <- c(drop_names,names(NewsPop)[14:19])

NewsPop$shares <- log(NewsPop$shares+1)
NewsPop$is_weekend <- as.factor(NewsPop$is_weekend)
NewsPop$self_reference_avg_sharess <-log(NewsPop$self_reference_avg_sharess+1)

### Drop unncessary columns
drop_names <- c(drop_names, c('url','timedelta'))
NewsPop <- NewsPop[, !(names(NewsPop) %in% drop_names)]

# Analyzing the Data
head(NewsPop)
summary(NewsPop)

outliers <- unlist(lapply(NewsPop,find_top_n,10))
length(outliers)
length(unique(outliers))
outliers_unq <-unique(outliers)

### Store original graphing params for later
opar <- par()

par(mfrow=c(4,7),
    oma = c(5,4,0,0) + 0.1,
    mar = c(2,0,1,1) + 0.1,
    xpd = NA)

# Plot each variable as a scatter plot
cols <- dim(NewsPop)[2]-1

for (i in 1:cols){
  if (class(NewsPop[,i])!="factor"){
    selected_col = NewsPop[-outliers_unq,i]
    channel_col = NewsPop$channel[-outliers_unq]
    ymax = max(selected_col)
    ymin = min(selected_col)
    for (j in 1:length(levels(NewsPop$channel))){
      curr = levels(NewsPop$channel)[j]
      if (j == 1){
        smoothScatter(selected_col[channel_col==curr],
                      main="",xlab="",ylab=names(NewsPop)[i],
                      ylim = c(ymin,ymax),
                      colramp = colorRampPalette(c('white',j)))
      }else{
        smoothScatter(selected_col[channel_col==curr],
                      main="",xlab="",ylab="",
                      ylim = c(ymin,ymax),
                      colramp = colorRampPalette(c('white',j)),
                      yaxt='n',ann=FALSE)
      }
    }
  }
}

#Reset graphic parameters
par(opar)

# Split into training, testing, validation


# Building a Decision Tree
fmla <- channel~n_tokens_title+n_unique_tokens+n_non_stop_unique_tokens+
  num_hrefs+num_self_hrefs+num_imgs+average_token_length+
  num_keywords+self_reference_avg_sharess

gini_model<-rpart(fmla,data = NewsPop, 
                  parms = list(split = "gini"),
                  control = list(minsplit=100))

rpart.plot(gini_model,main="Gini Split Criteria")

info_model<-rpart(fmla,data = NewsPop, 
                   parms = list(split = "information"),
                  control = list(minsplit=100))

rpart.plot(info_model,main="InfoGain Split Criteria")

# Evaluate Decision Trees
