suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pROC))
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
set <- train_data |> select(Environmental_Concern_Level, Will_Buy_EV, Annual_Income_USD, Daily_Commute_km, Age, Range_Anxiety_Level, Home_Charging_Possible, Subsidy_Available)

set$Subsidy_Available <- ifelse(train_data$Subsidy_Available == "Yes", 1, 0)
set$Subsidy_Available <- as.factor(set$Subsidy_Available)
set$Home_Charging_Possible <- ifelse(train_data$Home_Charging_Possible == "Yes", 1, 0)
set$Home_Charging_Possible <- as.factor(set$Home_Charging_Possible)
set$Range_Anxiety_Level <- ifelse(train_data$Range_Anxiety_Level == "Low", 0, ifelse(train_data$Range_Anxiety_Level == "Medium", 1, 2))
set$Range_Anxiety_Level <- as.factor(set$Range_Anxiety_Level)
set$Will_Buy_EV <- ifelse(train_data$Will_Buy_EV == "Yes", 1, 0)
set$Will_Buy_EV <- as.factor(set$Will_Buy_EV)

start_time <- Sys.time()
model <- glm(set$Will_Buy_EV ~ set$Environmental_Concern_Level + set$Subsidy_Available + set$Range_Anxiety_Level + set$Annual_Income_USD + set$Daily_Commute_km, data = train_data, family = "binomial")

set$pred <- predict(model, newdata = train_data, type = "response")
end_time <- Sys.time()
paste0(end_time - start_time)

ctab <- table(pred = set$pred > .175, Will_Buy_EV = set$Will_Buy_EV) #confusion matrix
exp(coef(model))
accuracy <- ctab[1,1]/ nrow(set)
pre <- ctab[2, 2]/ sum(ctab[2, ]) #precision
re <- ctab[2, 2]/ sum(ctab[, 2]) #recall
enrich <- pre / mean(as.numeric(as.character(set$Will_Buy_EV))) #enrichment (something screwy here)
summary(model)
ctab
PRSquared <- 1 - model$deviance / model$null.deviance
roc_obj <- roc(train_data$Will_Buy_EV, set$pred)
auc(roc_obj)
