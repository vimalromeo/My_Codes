flower <- read.csv("flower.csv", header = FALSE)
#R treats the rows as observations and columsn as variables
str(flower)
#Let us convert the dataframe to a matrix
flowerMatrix <- as.matrix(flower)
#Now we have a matrix with 50 rows and 50 colums. This is a very small picture
str(flowerMatrix)
#For clustering, we need toconvert the matrix into a vector of intensities 
#ranging from 0 to 1.And the clustering algorithm divides the intensity spectrum,
#the interval zero to one, into these joint clusters or intervals.
flowerVector <- as.vector(flowerMatrix)
#Now we have 2500 numerical values(50*50)
str(flowerVector)
#We cannot convert the dataframe directly to vector 
#flowerVector2 <- as.vector(flower). This does not work. So matrix is necessary

#Hierarchical Clustering
#First step is to calculate the distance btwn every 2 intensity values
distance <- dist(flowerVector, method = "euclidean")
clusterIntensity <- hclust(distance, method = "ward.D")
#Ward’s method is a minimum variance method, which tries to find compact and 
#spherical clusters.We can think about it as trying to minimize the variance

#Dentogram
plot(clusterIntensity)
#To visualize the clusters we can plot rectangles around the clusters on the tree
#we have given the no of clusters as input. Coloring the border with red
rect.hclust(clusterIntensity, k=3, border = "red")
#Letz split the data into these 3 clusters using cutree
flowerClusters <- cutree(clusterIntensity, k = 3)
flowerClusters
#1,2 and 3 shows the different clusters

#Finding the mean intensity value of each cluster using tapply
tapply(flowerVector, flowerClusters, mean)
#First cluster has a value closest to 0. So it corresponds to the darkest shape
#Third cluster has a value closest to 1. So it is the fairest shape

#Visualizing the segmented real image
#The image was in matrix form. So we need to convert the vector to matrix
#We can do this by setting the dimension variable using the dimension function
#50 * 50, so we set it as c(50,50)
dim(flowerClusters) <- c(50,50)
#axis = FALSE to turn of the axis
image(flowerClusters, axes = FALSE)
#Original image in greyscale
image(flowerMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))
