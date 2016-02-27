#Data Used
#https://archive.ics.uci.edu/ml/datasets/Wholesale+customers
data <-read.csv("Wholesale customers data.csv",header=T)
summary(data)

#Top N Function
top.n.custs <- function (data,cols,n=5) { #Requires some data frame and the top N to remove
  idx.to.remove <-integer(0) #Initialize a vector to hold customers being removed
  for (c in cols){ # For every column in the data we passed to this function
    col.order <-order(data[,c],decreasing=T) #Sort column "c" in descending order (bigger on top)
    #Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.
    idx <-head(col.order, n) #Take the first n of the sorted column C to
    idx.to.remove <-union(idx.to.remove,idx) #Combine and de-duplicate the row ids that need to be removed
  }
  return(idx.to.remove) #Return the indexes of customers to be removed
}

top.custs <-top.n.custs(data,cols=3:8,n=5)
length(top.custs) #How Many Customers to be Removed?
data[top.custs,] #Examine the customers
data.rm.top <-data[-c(top.custs),] #Remove the Customers

set.seed(76964057) #Set the seed for reproducibility

#Execute the K Means algorithm
k <-kmeans(data.rm.top[,-c(1,2)], centers=5) #Create 5 clusters, Remove columns 1 and 2
k$centers #Display cluster centers
table(k$cluster) #Give a count of data points in each cluster

#Experimenting with different K Values
rng<-2:20 #K from 2 to 20
tries<-100 #Run the K Means algorithm 100 times
avg.totw.ss<-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss<-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp<-kmeans(data.rm.top,centers=v) #Run kmeans
    v.totw.ss[i]<-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1]<-mean(v.totw.ss) #Average the 100 total withinss
}

plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")