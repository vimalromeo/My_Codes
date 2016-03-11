library(flexclust)
#Header = FALSE as it contains the set of intensity values
healthy <- read.csv("healthy.csv", header = F)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
#It is a 566*646 matrix and is large enough

#Visualizing the image
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))
#
healthyVector <- as.vector(healthyMatrix)
str(healthyVector)
n <- 365636
n*(n-1)/2
#It is a huge number and takes a lot of memory to calculate the distance
#We cannot use hierarchical clustering here
#We use K-means clustering here
#clusters are set based on the expert knowledge. Letz set it as 5 for example
k = 5
#as k means randomly assigns clusters setting seed to be the same as the course
set.seed(1)
#we use the kmeans function in R. centers to set k and iter.max to set the
#maximum iterations as kmeans is an iterative process and takes long to converge
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
#cluster codes the details about clusters in 1-5 as we have 5 clusters
str(KMC)
#Extracting the information about clusters
healthyClusters <- KMC$cluster
#In hierarchial clustering we need to use tapply to get the mean intensity values
#Here it is stored in the centers. Eg mean intensity value of cluter 2
KMC$centers[2]
#The sizes of clusters are stored in size

#To get the real segmented image
#Converting to matrix using dim
dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
#Let us use the rainbow color scheme. rainbow take the no of colors we want.
#Here we chose the no of colors as k
image(healthyClusters, axes = FALSE, col = rainbow(k))

#Tumour data set
tumor <- read.csv("tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)
#We are not going to run the kmeans on this. We will use the kmeans clustering
#result which we got from healthy brain on the tumor vector
#Here, we use the healthy vector as training set and tumor vector as test set

#The flexclust package contains the object class KCCA, which stands for 
#K-Centroids Cluster Analysis. We need to convert the information from the 
#clustering algorithm to an object of the class KCCA.
#This conversion is needed before we can use the predict function on the test 
#set tumorVector.
#The first input is the original KMC variable and the second input is the data
#that we clustered
KMC.kcca <- as.kcca(KMC, healthyVector)
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
#the tumorClusters is a vector that assigns a value 1 through 5 to each of the 
#intensity values in the tumorVector, as predicted by the k-means algorithm.

#Visualizing the image
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))
