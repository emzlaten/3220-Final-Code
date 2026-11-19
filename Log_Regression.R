library(tidyverse)
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
set <- train_data |> select(Environmental_Concern_Level, Will_Buy_EV, Annual_Income_USD, Daily_Commute_km, Age)

set$Will_Buy_EV <- ifelse(train_data$Will_Buy_EV == "Yes", 1, 0)
set$Will_Buy_EV <- as.factor(set$Will_Buy_EV)

model <- glm(set$Will_Buy_EV ~ set$Environmental_Concern_Level + set$Annual_Income_USD, data = train_data, family = "binomial")
set$pred <- predict(model, newdata = test_data, type = "response")

ctab <- table(pred = set$pred > .5, Will_Buy_EV = set$Will_Buy_EV) #confusion matrix
exp(coef(model))
accuracy <- ctab[1,1]/ nrow(set)
pre <- ctab[2, 2]/ sum(ctab[2, ]) #precision
re <- ctab[2, 2]/ sum(ctab[, 2]) #recall
enrich <- pre / mean(train_data$Will_Buy_EV) #enrichment (something screwy here)
summary(model)
ctab
PRSquared <- 1 - model$deviance / model$null.deviance