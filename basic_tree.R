library(dplyr)
library(rpart)
library(rpart.plot)


# load csv into a data frame, omit gameID column
lol <- select(read.csv("high_diamond_ranked_10min.csv"), -gameId)

# creating training set 
sample <- sample(nrow(lol), nrow(lol) * 0.75)
lol_train <- lol[sample, ]

# creating testing set
lol_test <- lol[-sample, ]

# grow a tree with all factors
lol_tree <- rpart(blueWins ~ ., data = lol_train, method = "class", control = rpart.control(cp = 0))

# visualizing the tree
rpart.plot(lol_tree)

# plotcp(lol_tree)

# # finding accruacy of post-prune tree
# post_prune <- prune(lol_tree, cp = 0.0023)
# rpart.plot(post_prune)

# lol_test$predict <- predict(post_prune, lol_test, type = "class")
# mean(lol_test$blueWins == lol_test$predict)


# # predict based on the model
# lol_test$pred <- predict(lol_tree, lol_test, type = "class")
# table(lol_test$pred, lol_test$blueWins)

# mean(lol_test$pred == lol_test$blueWins)
