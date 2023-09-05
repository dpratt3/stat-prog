# No 11: Find the outliers... 
points = list(c(1, 2, 0), 
              c(3, 1, 4), 
              c(2, 1, 5), 
              c(0, 1, 6), 
              c(2, 4, 3), 
              c(4, 4, 2),
              c(5, 2, 1), 
              c(7, 7, 7), 
              c(0, 0, 0) , 
              c(3, 3, 3))  

distance = function(p1, p2){
    return (sqrt(sum( (p1 - p2)^2 )))
}

# (i) the threshold distance is 4 and threshold fraction p for non-neighbor samples is 3
distanceMatrix = matrix(nrow = length(points), ncol = length(points))

# populate matrix with distances
for(i in 1:length(points)){
    for(j in 1:length(points)){
        distanceMatrix[i, j] = distance(points[[i]], points[[j]])
    }
}

# populate list with counts of interest according to parameters p and d
p = 3; d = 4
distList1 = list()
for(i in 1:length(points)){
    distList1[[i]] = (sum(distanceMatrix[i, ] > d)) >= p
}

distList1 = do.call(rbind, distList1)
distList1 = as.data.frame(distList1)
rownames(distList1) = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10")
colnames(distList1) = "p"
print(distList1)

# (ii) the threshold distance is 6 and threshold fraction p for non-neighbor samples is 2.
d = 6; p = 2
distList2 = list()
for(i in 1:length(points)){
    distList2[[i]] = (sum(distanceMatrix[i, ] > d)) >= p
}

distList2 = do.call(rbind, distList2)
distList2 = as.data.frame(distList2)
rownames(distList2) = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10")
colnames(distList2) = "p"
print(distList2)

# (b) Describe the procedure and interpret the results of outlier detection based on mean
# values and variances for each dimension separately.

pointMatrix = do.call(rbind, points)
pointMeans = apply(pointMatrix, 2, mean)
print(pointMeans)

pointSDs = apply(pointMatrix, 2, sd)
print(pointSDs)

upperBounds = pointMeans + 2 * pointSDs
lowerBounds = pointMeans - 2 * pointSDs

print(upperBounds)
print(lowerBounds)

isOutlier = function(point, upperBounds, lowerBounds){
    for(i in 1:length(point)){
        if(point[i] > upperBounds[i] || point[i] < lowerBounds[i]){
            return(TRUE)
        }
    }
    return(FALSE)
}

meanVarOutliers = lapply(points, isOutlier, upperBounds, lowerBounds)
meanVarOutliers = do.call(rbind, meanVarOutliers)
meanVarOutliers = as.data.frame(meanVarOutliers)
colnames(meanVarOutliers) = "p"
rownames(meanVarOutliers) = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10")
print(meanVarOutliers)
