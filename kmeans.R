library(dplyr)

# load csv into a data frame, omit gameID column
lol <- select(read.csv("high_diamond_ranked_10min.csv"), -gameId)

# select number of clusters using total within sum square error
wss <- 0
wss_plot <- function () {
    for (i in 1:10) {
          km.out <- kmeans(lol, centers = i, nstart = 20, iter.max = 50)
            wss[i] <- km.out$tot.withinss
    }
    plot(1:10, wss, type = "b", 
         xlab = "Number of Clusters", 
         ylab = "Within groups sum of squares")
}
# uncomment the following to produce wss plot and find k
# wss_plot()
     
# assign number of clusters from wss plot
k <- 3

# Build model with k clusters: km.out
km.out <- kmeans(lol, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(lol[, c("blueExperienceDiff", "blueGoldDiff")],
     col = km.out$cluster,
     main = paste("k-means clustering of League of Legends with", k, "clusters"),
     xlab = "blueExperienceDiff", ylab = "blueExperienceDiff")
