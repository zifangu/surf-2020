library(dplyr)
library(pROC)

# load csv into a data frame, omit gameID column
lol <- select(read.csv("high_diamond_ranked_10min.csv"), -gameId)

# check the structure of lol
# str(lol)
# creating training set 
sample <- sample(nrow(lol), nrow(lol) * 0.75)
lol_train <- lol[sample, ]

# creating testing set
lol_test <- lol[-sample, ]

# wards and gold diff as factors
first_model <- glm(blueWins ~ blueWardsPlaced + blueGoldDiff, data = lol_train, family = "binomial")

# summary(first_model)

lol_prob <- predict(first_model, lol_test, type = "response")

ROC <- roc(lol_test$blueWins, lol_prob)
plot(ROC, col = "red")
auc(ROC)

# null model
# null_model <- glm(blueWins ~ ., data = lol, family = "binomial")

# lol_prob <- predict(null_model, type = "response")

# ROC <- roc(lol$blueWins, lol_prob)
# plot(ROC, col = "red")
# auc(ROC)



