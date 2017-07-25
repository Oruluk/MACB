# Modeling and Analyzing Consumer Behaviour with R - Bonus Exercise 2 (summer term 2017)
# Institute of Information Systems and Marketing, Karlsruhe Institute of Technology
# https://im.iism.kit.edu/1744_1892.php 
# 
# This program provides the solution for the bonus exercise 2 of the named course. 
# The result is a content based recommender system for movies. It receives as input a file with 
# the movies and one with ratings of a single user. Furthermore it receives as parameter
# the ID of the user and the number of movies, the system should recommend. The output is
# then a set of recommended movies of the specified size for the given user. 
# 
# Patrick Deininger (1717571)
# Alexander Haas (1668040)
# Elena Woessner (1972072)

library("ggplot2")
#library(data.table)


#########################################################################################################################
# Preprocessing

# Please specify path to folder containing the data 
PATH_TO_DATA = "~/Excercise/Bonus Challenge 2/"


# Read data
udata <- read.csv(paste(PATH_TO_DATA,"Bonuspunkte 2 - Datensatz udata.csv", sep=""), header=TRUE, sep=";", stringsAsFactors = FALSE)
uitem <- read.csv(paste(PATH_TO_DATA,"Bonuspunkte 2 - Datensatz uitem.csv", sep=""), header=TRUE, sep=";", stringsAsFactors = FALSE)

# Check for missing observations
if (sum(is.na(udata))==0){
  print("udata has no missing observations") 
} else {
  print("udata has missing values -> further data preparation required") 
}
if (sum(is.na(uitem))==0){
  print("uitem has no missing observations") 
} else {
  print("uitem has missing values -> further data preparation required") 
}

# Remove duplicate movies
uitem_wd <- uitem[!duplicated(uitem$movtitle), ]


#########################################################################################################################
# Task 1

sslastimpr <- function(data, maxCluster = 20) {
  # variable initialization
  SSw <- vector()
  data <- data[-1:-2]
  lastNoticeableImprovement <- 0
  # k-means for various (i) clusternumbers
  for (i in 2:maxCluster) {
    # calculate Sum of squared errors
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
    # check if adding a new cluster reduced heterogenity given the previously defined parameters
    if ((i>2) && SSw[i]<=SSw[i-1]*(1-improvementRequirement)){
      lastNoticeableImprovement <- i
    }
  }
  # return the highest number of clustes, which still fit the required improvement paramenters
  return(lastNoticeableImprovement)
}


# k-means
clusterFilms <- function(uitem){
  
  # Scaling
  # uitem_k <- scale(uitem[3:21])
  uitem_k = uitem[-1:-2]
  
  # Find a suitable number of clusters
  ssplot <- function(data, maxCluster = 20, min_improve = 0.2){
    SSw <- vector()
    lastImprovementCluster = 0
    for (i in 2:maxCluster){
      SSw[i] <- sum(kmeans(data, centers = i)$withinss)
      if ((i>2) && (1-SSw[i]/SSw[i-1])>=min_improve){
        lastImprovementCluster = i
        min_improve=1000
      }
    }
    plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
    
    return(lastImprovementCluster)
  }
  
  number_of_clusters = ssplot(uitem_k)
  
  # Print number of CLusters
  print(paste("Number of Clusters:", number_of_clusters))
  
  # Execute k-means for the identified number of clusters
  fit = kmeans(uitem_k, number_of_clusters)
  
  # Append cluster assignment -> is not needed at the moment
  # uitem <- data.frame(uitem, fit$cluster)
  # uitem$fit.cluster <- as.factor(uitem$fit.cluster)

  return(fit)
}

#########################################################################################################################
# Task 2

getUserInfo <- function(userDF, userID){
  # get itemid and rating for given userID and create a new column called "cluster" only contains zeros
  activeUser = data.frame(userDF[userDF$userid == userID,]$itemid, userDF[userDF$userid == userID,]$rating, numeric(length(userDF[udata$userid == userID,]$rating)))
  
  # rename columns
  colnames(activeUser) <- c("itemid", "rating", "cluster")
  
  # sort the data in ascending order according to itemid
  activeUser = activeUser[order(activeUser[,1]),]
  
  return(activeUser)  
}


#########################################################################################################################
# Task 3

setUserFilmCluster <- function(movieCluster, activeUser){

  # assign the corresponding cluster to each itemid in activeUser
  for (i in 1:length(activeUser$itemid)){
    activeUser$cluster[i] = movieCluster$cluster[activeUser$itemid[i]]
  }  
  
  return(activeUser)
}

#########################################################################################################################
# Task 4

getMeanClusterRating <- function(movieCluster, activeUser){
  # Calculate the means for the ratings of all clusters
  like = aggregate(activeUser$cluster, FUN=mean, by =list(activeUser$rating)) 
  
  # Identify the clusters with a mean > 3
  like = like[like$x>3,]
 
  # Put the clusters in an int vector
  intVec = as.vector(like[1])
 
  return(like)
}

#########################################################################################################################
# Task 5

getGoodFilms <- function(like, movieCluster, uitem){
  if (length(like$x) > 0){
  
    # Get best cluster
    bestCluster = which.max(like$x)
    
    # Append cluster assignment -> is not needed at the moment
    uitem <- data.frame(uitem, movieCluster$cluster)
    uitem$fit.cluster <- as.factor(uitem$movieCluster.cluster)
    
    # Find all movies of best cluster and create int vector
    recommend = uitem[uitem$movieCluster.cluster==bestCluster,]
    recommend = recommend$movid
    
  } else { 
    
    # Choose randomly 100 movies
    recommend = sample(uitem$movid, 100, replace = FALSE, prob = NULL)
  }

  return(recommend)
}

#########################################################################################################################
# Task 6

getRecommendedFilms <- function(uitem, userDF, userid){
  
  # Get movie clusters from k-means
  movieCluster = clusterFilms(uitem)
  
  # Get all ratings and movies for a user
  activeUser = getUserInfo(udata, userid)
  
  # Get the clusters for all seen movies
  activeUser = setUserFilmCluster(movieCluster, activeUser)
  
  # Get the clusters of films the user ranks higher than 3
  like = getMeanClusterRating(movieCluster, activeUser)
  
  # Get recommended movies
  recommend = getGoodFilms(like, movieCluster, uitem)
  
  # Identify movies the user hasn't seen yet that could be recommended
  recommend = setdiff(recommend, intersect(recommend, activeUser$itemid))
  
  # Add column for movtitle and rename the columns
  recommend = data.frame(recommend, numeric(length(recommend)))
  colnames(recommend) <- c("movid", "movtitle")
  
  # Add to each movid the corresponding movtitle
  for (i in 1:length(recommend$movid)){
    recommend$movtitle[i] = uitem$movtitle[recommend$movid[i]]
  }
  
  return(recommend)
}


#########################################################################################################################
# Task 7

suggestFilms <- function(titleFilmDF, userDF, userid, noFilms){

  recommend = getRecommendedFilms(uitem, userDF, userid)
  
  # Get sample out of all recommendations
  # Recommend maximum the number of films in recommend 
  final_recommend = sample(recommend$movtitle, min(noFilms, length(recommend$movtitle)), replace=FALSE, prob=NULL)
  
  # Print recommendations
  print("Diese Filme k?nnten Ihnen auch gefallen:")

  for (i in final_recommend){
    print(i)
  }
}

#########################################################################################################################
# Main

titleFilmDF = uitem_wd
userDF = udata
userid = 159
noFilms = 15

suggestFilms(titleFilmDF, userDF, userid, noFilms)





# Plot function to visualize movies (have to be manually selected)
# genre.dist$number = data.table(apply(uitem[3:21], 2, sum))
# genre.dist$type = as.factor(colnames(uitem)[3:21])
# ggplot(genre.dist, aes(x=type, y=number, label="Genres")) + geom_bar(stat="identity")
