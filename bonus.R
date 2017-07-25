library("ggplot2")


PATH_TO_DATA = "C:/Users/Patrick/Desktop/Studium/02 Master/Modeling and Analyzing Consumer Behaviour with R/Excercise/Bonus2/"


#Read data
udata <- read.csv(paste(PATH_TO_DATA,"Bonuspunkte 2 - Datensatz udata.csv", sep=""), header=TRUE, sep=";", stringsAsFactors = FALSE)
uitem <- read.csv(paste(PATH_TO_DATA,"Bonuspunkte 2 - Datensatz uitem.csv", sep=""), header=TRUE, sep=";", stringsAsFactors = FALSE)

#Check for missing observations
if (sum(is.na(udata))==0){
  print("udata has no missing observations") 
} else {
  print("udata has no missing values -> further data preparation required") 
}

if (sum(is.na(uitem))==0){
  print("uitem has no missing observations") 
} else {
  print("uitem has no missing values -> further data preparation required") 
}

#TODO: Doppelte Einträge entfernen

#Plot function to visualize movies (have to be manually selected)

#doesn't work
#ggplot(uitem[3:21], aes(x=colnames(uitem)[3:21], y=apply(uitem[3:21], 2, sum))) + geom_bar(stat="identity")


genre.dist$number = data.table(apply(uitem[3:21], 2, sum))
genre.dist$type = as.factor(colnames(uitem)[3:21])
ggplot(genre.dist, aes(x=type, y=number, label="Genres")) + geom_bar(stat="identity")



#########################################################################################################################
#Task 1
#k-means
clusterFilms <- function(uitem){
  
  #Scaling
  uitem_k <- scale(uitem[3:21])
  
  #Find a good number of clusters
  ssplot <- function(data, maxCluster = 10){
    SSw <- vector()
    for (i in 2:maxCluster){
      SSw[i] <- sum(kmeans(data, centers = i)$withinss)
    }
    plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
  }
  ssplot(uitem_k)
  
  #TODO Festlegung der Anzahl der Cluster mit ellbow kriterium  
  fit = kmeans(uitem_k, 6)
  
  #Append cluster assignment -> is not needed at the moment
  #uitem <- data.frame(uitem, fit$cluster)
  #uitem$fit.cluster <- as.factor(uitem$fit.cluster)

  return(fit)
}

#########################################################################################################################
#Task 2

getUserInfo <- function(userDF, userID){
  #get itemid and rating for given userID and create a new column called "cluster" only contains zeros
  activeUser = data.frame(userDF[userDF$userid == userID,]$itemid, userDF[userDF$userid == userID,]$rating, numeric(length(userDF[udata$userid == userID,]$rating)))
  
  #rename columns
  colnames(activeUser) <- c("itemid", "rating", "cluster")
  
  #sort the data in ascending order according to itemid
  activeUser = activeUser[order(activeUser[,1]),]

  return(activeUser)  
}


#########################################################################################################################
#Task 3

setUserFilmCluster <- function(movieCluster, activeUser){

  #assign the corresponding cluster to each itemid in activeUser
  for (i in 1:length(activeUser$itemid)){
    activeUser$cluster[i] = movieCluster$cluster[activeUser$itemid[i]]
  }  
  return(activeUser)
}

#########################################################################################################################
#Task 4

#TODO: Why is movieCluster not used here?
getMeanClusterRating <- function(movieCluster, activeUser){
  #Calculate the means for the ratings of all clusters
  like = aggregate(activeUser$cluster, FUN=mean, by =list(activeUser$rating)) 
  
  #Identify the clusters with a mean > 3
  like = like[like$x>3,]
  
  #Put the clusters in an int vector
  #TODO: Clarify if it makes sense to return intVec instead of like
  intVec = as.vector(like[1])
  
  return(like)
}

#########################################################################################################################
#Task 5

getGoodFilms <- function(like, movieCluster, uitem){
  if (length(like$x) > 0){
  
    #Get best cluster
    bestCluster = which.max(like$x)
    
    #Append cluster assignment -> is not needed at the moment
    uitem <- data.frame(uitem, movieCluster$cluster)
    uitem$fit.cluster <- as.factor(uitem$movieCluster.cluster)
    
    #find all movies of best cluster and create int vector
    recommend = uitem[uitem$movieCluster.cluster==bestCluster,]
    recommend = recommend$movid
    
  } else { 
    
    #Choose randomly 100 movies
    recommend = sample(uitem$movid, 100, replace = FALSE, prob = NULL)
  }
  return(recommend)
}

#########################################################################################################################
#Task 6

getRecommendedFilms <- function(uitem, userDF, userid){
  
  #Get movie clusters from k-means
  movieCluster = clusterFilms(uitem)
  
  #Get all ratings and movies for a user
  activeUser = getUserInfo(udata, userid)
  
  #Get the clusters for all seen movies
  activeUser = setUserFilmCluster(movieCluster, activeUser)
  
  #Get the clusters of films the user ranks higher than 3
  like = getMeanClusterRating(movieCluster, activeUser)
  
  #Get recommended movies
  recommend = getGoodFilms(like, movieCluster, uitem)
  
  
  #Identify movies the user hasn't seen yet that could be recommended
  recommend = setdiff(recommend, intersect(recommend, activeUser$itemid))
  
 
  #Add column for movtitle and rename the columns
  recommend = data.frame(recommend, numeric(length(recommend)))
  colnames(recommend) <- c("movid", "movtitle")
  
  #Add to each movid the corresponding movtitle
  for (i in 1:length(recommend$movid)){
    recommend$movtitle[i] = uitem$movtitle[recommend$movid[i]]
  }
  
  return(recommend)
}


#########################################################################################################################
#Task 7

suggestFilms <- function(titleFilmDF, userDF, userid, noFilms){

  recommend = getRecommendedFilms(uitem, userDF, userid)
  
  #Get sample out of all recommendations
  final_recommend = sample(recommend$movtitle, min(noFilms, length(recommend$movtitle)), replace=FALSE, prob=NULL)
  
  #Print recommendations
  print("Diese Filme könnten Ihnen auch gefallen:")
  
  for (i in final_recommend){
    print(i)
  }
}

#########################################################################################################################
#Main

titleFilmDF = uitem
userDF = udata
userid = 200
noFilms = 15


suggestFilms(titleFilmDF, userDF, userid, noFilms)
