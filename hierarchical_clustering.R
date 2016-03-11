movies <- read.table("movielens.txt", header = F, sep = "|", quote = "\"")
str(movies)
#Adding column names
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", 
                      "Unknown", "Action", "Adventure", "Animation", "Childrens",
                      "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                      "FilmNoir", "Horror", "Musical", "Mystery", "Romance", 
                      "SciFi", "Thriller", "War", "Western")
str(movies)
#We won't be using the ID, releasedate, videorelease date or IMDB variables.
#So letz remove them
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
#There are few duplicate entries in our dataset. To remove them we use the 
#unique function
movies <- unique(movies)
str(movies)
#Calculating the distances using dist. We are not using Title for clustering
#We are using euclidean distance
distances <- dist(movies[2:20], method = "euclidean")
#Letz cluster our movies using hclust function, ward.D for hierarchical clustering
#ward.D method takes care of the distance between clusters using centroid 
#distance and also the variances in each cluster
clusterMovies <- hclust(distances, method = "ward.D")
#Plotting the dentogram
plot(clusterMovies)
#There is this black color at the bottom as there are so many data points
#From the dentogram, we can pic 3 clusters. But for our recommendation system
#we need more clusters. Down there, we can see 10 clusters. It looks good

#we can label each of the data points according to what cluster it belongs
#to using the cutree function.
clusterGroups <- cutree(clusterMovies, k=10)
#Use the tapply function to compute the % of movies in each genre and cluster
#We can see that in cluster 2, 78% of movies are of action genre
tapply(movies$Action, clusterGroups, mean)
#Checking for Romance
tapply(movies$Romance, clusterGroups, mean)
#Checking the cluster which has the movie Men in Black
subset(movies, Title == "Men in Black (1997)")
#It shows that Men in Black is the 257th row in our data. 
#So which cluster did the 257th movie belong to
clusterGroups[257]
#Letz find the movies in cluster 2
cluster2 <- subset(movies, clusterGroups == 2)
#Observing the titles of first 10 observations in this cluster
cluster2$Title[1:10]
