library(dplyr)
library(naivebayes)
library(pROC)

# load csv into a data frame, omit gameID column
lol <- select(read.csv("high_diamond_ranked_10min.csv"), -gameId)

# creating training and testing set 
sample <- sample(nrow(lol), nrow(lol) * 0.5)
lol_train <- lol[sample, ]
lol_test <- lol[-sample, ]

# factor 0 and 1 into defeat and victory
outcome <- c(lol_train$blueWins)
cat("here is:", outcome[0:5],"\n")
lol_train$blueOutcome <- factor(outcome)
levels(lol_train$blueOutcome) <- c("defeat", "victory")

# predict a naive bayes model
lol_nb <- naive_bayes(blueOutcome ~ blueWardsPlaced + blueGoldDiff, data = lol_train)
lol_prob <- predict(lol_nb, lol_test, type = "prob")

# finding AUC
ROC <- roc(lol_test$blueWins, lol_prob)
plot(ROC, col = "red")
auc(ROC)
