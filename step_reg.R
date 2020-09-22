library(dplyr)
library(pROC)

# load csv into a data frame, omit gameID column
lol <- select(read.csv("high_diamond_ranked_10min.csv"), -gameId)

# check the structure of lol
# str(lol)

# # forward null model
# null_model <- glm(blueWins ~ 1, data = lol, family = "binomial")

# # forward full model
# full_model <- glm(blueWins ~ ., data = lol, family = "binomial")

# backward null model
null_model <- glm(blueWins ~ ., data = lol, family = "binomial")

# backward full model
full_model <- glm(blueWins ~ 1, data = lol, family = "binomial")


# step model 
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "backward", trace = 0)

# estimate stepwise blueWins probility
step_prob <- predict(step_model,type = "response")

# finding ROC areas
ROC <- roc(lol$blueWins, step_prob)
plot(ROC, color = "green")
auc(ROC)
