# Given similarity matrix
similarity_matrix <- matrix(c(1.00, 0.25, 0.32, 0.43, 0.23,
                              NA, 1.00, 0.58, 0.40, 0.88,
                              NA, NA, 1.00, 0.36, 0.61,
                              NA, NA, NA, 1.00, 0.65,
                              NA, NA, NA, NA, 1.00), nrow = 5, byrow = TRUE)

# Function to convert upper triangular matrix to a full symmetric matrix
toSymmetric <- function(mat) {
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  return(mat)
}

# Convert the similarity matrix to symmetric matrix
similarity_matrix <- toSymmetric(similarity_matrix)
single_cluster <- hclust(as.dist(1 - similarity_matrix), method = "single")
plot(single_cluster, main = "Single Linkage Hierarchical Clustering Dendrogram",
     xlab = "Points", ylab = "Distance")

# Perform hierarchical clustering and create dendogram
complete_cluster <- hclust(as.dist(1 - similarity_matrix), method = "complete")
plot(complete_cluster, main = "Complete Linkage Hierarchical Clustering Dendrogram",
     xlab = "Points", ylab = "Distance")

# Assuming similarity_matrix is already defined as in your previous code

library(dbscan)

# Convert similarity matrix to a distance matrix (if necessary)
# In case the similarity matrix is already a distance matrix, you can skip this step
distance_matrix <- as.dist(1 - similarity_matrix)

# Set parameters for DBSCAN
epsilon <- 0.4  # Epsilon value
minPts <- 2     # Minimum number of points

# Perform DBSCAN clustering
db <- dbscan(distance_matrix, eps = epsilon, minPts = minPts)

# Print the clusters and noise points
cat("Clusters:", max(db$cluster), "\n")
cat("Noise Points:", sum(db$cluster == 0), "\n")
