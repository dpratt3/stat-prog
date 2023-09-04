# No. 4: Find average distance between six-dimensional points
values <- c(0, 0.5, 1)
points <- list()
index <- 1

# generate all 6D combos in a verbose way
for (i in 1:length(values)) {
  for (j in 1:length(values)) {
    for (k in 1:length(values)) {
      for (l in 1:length(values)) {
        for (m in 1:length(values)) {
          for (n in 1:length(values)) {
            points[[index]] <- c(values[i], values[j], values[k], values[l], values[m], values[n])
            print(index)
            index <- index + 1
          }
        }
      }
    }
  }
}

distances = vector()
distance = function(pt1, pt2){
    return(sqrt(sum((pt1 - pt2)^2)))
}

# picture a matrix 
sideLength = length(values)^6
totalPoints = 0.5 * (sideLength^2 - sideLength) # by symmetry

idx = 1
while(idx <= totalPoints){
    for(i in 1:length(values)^6){
        for(j in (i + 1): (length(values)^6) - 1){
            # no zero distances or double counting
            distances[idx] = distance(points[[i]], points[[j]])
            print(idx)
            idx = idx + 1
        }
    }
}

hist(distances, col = "goldenrod")
print(min(distances))
print(max(distances))
print(totalPoints)

# Validation: the following two lines should be equal
print(sum(distances) / totalPoints) # answer
print(mean(distances))

