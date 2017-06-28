library(dplyr)
library(magrittr)
library(caret)
library(foreach)
library(pROC)

source("svmtree.R")

performance <- function(prediction, reference, positive) {
  cm <- confusionMatrix(prediction, reference, positive = positive)
  c(cm$overall["Accuracy"], cm$byClass[c("Balanced Accuracy", "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value")], AUC = auc(reference, as.numeric(prediction)))
}

# cats ------------------------------------------------------------------------------------
set.seed(1234567890)
perf.tree <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(cats$Sex)
  pr.tree <- unlist(foreach(fold = folds) %do% {
    test <- cats[fold, ]
    training <- cats[-fold, ]
    model <- svmtree(Sex ~ ., training)
    predict(model, select(test, -Sex))
  })
  performance(pr.tree, cats$Sex, "M")  
}

set.seed(1234567890)
perf.tree.nw <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(cats$Sex)
  pr.tree <- unlist(foreach(fold = folds) %do% {
    test <- cats[fold, ]
    training <- cats[-fold, ]
    model <- svmtree(Sex ~ ., training, class.weights = NULL)
    predict(model, select(test, -Sex))
  })
  performance(pr.tree, cats$Sex, "M")  
}

set.seed(1234567890)
perf.svm <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(cats$Sex)
  pr.svm <- unlist(foreach(fold = folds) %do% {
    test <- cats[fold, ]
    training <- cats[-fold, ]
    model <- svm(Sex ~ ., training)
    predict(model, select(test, -Sex))
  })  
  performance(pr.svm, cats$Sex, "M")  
}

rbind("Mean" = colMeans(perf.tree, na.rm = TRUE), "SD" = apply(perf.tree, 2, sd, na.rm = TRUE))
rbind("Mean" = colMeans(perf.tree.nw, na.rm = TRUE), "SD" = apply(perf.tree, 2, sd, na.rm = TRUE))
rbind("Mean" = colMeans(perf.svm, na.rm = TRUE), "SD" = apply(perf.svm, 2, sd, na.rm = TRUE))

# Embarc data -----------------------------------------------------------------------------

embarc <- read.csv("Brain & Handedness(imputed).csv", colClasses = c("CON_MDD" = "factor"))

set.seed(1234567890)
perf.tree <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(embarc$CON_MDD)
  pr.tree <- unlist(foreach(fold = folds) %do% {
    test <- embarc[fold, ]
    training <- embarc[-fold, ]
    model <- svmtree(CON_MDD ~ ., training, kernel = "linear")
    predict(model, select(test, -CON_MDD))
  })
  performance(pr.tree, embarc$CON_MDD, "1")  
}

set.seed(1234567890)
perf.svm <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(embarc$CON_MDD)
  pr.svm <- unlist(foreach(fold = folds) %do% {
    test <- embarc[fold, ]
    training <- embarc[-fold, ]
    model <- svm(CON_MDD ~ ., training, kernel = "linear")
    predict(model, select(test, -CON_MDD))
  })  
  performance(pr.svm, embarc$CON_MDD, "1")  
}

rbind("Mean" = colMeans(perf.tree, na.rm = TRUE), "SD" = apply(perf.tree, 2, sd, na.rm = TRUE))
rbind("Mean" = colMeans(perf.svm, na.rm = TRUE), "SD" = apply(perf.svm, 2, sd, na.rm = TRUE))


# Schizo data ---------------------------------------------
colClasses <- c('factor', 'factor', rep('numeric', 30), 'factor')
schizo <- read.csv("schizo.csv", colClasses = colClasses) %>%
  mutate(schizo = factor((diagall != 0))) %>%
  select(-diagall, -gaf_pas, -gaf_cur)

set.seed(1234567890)
perf.tree <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(schizo$schizo)
  pr.tree <- unlist(foreach(fold = folds) %do% {
    test <- schizo[fold, ]
    training <- schizo[-fold, ]
    model <- svmtree(schizo ~ ., training, class.weights = NROW(schizo$schizo) / table(schizo$schizo))
    predict(model, select(test, -schizo))
  })
  performance(pr.tree, schizo$schizo, "TRUE")  
}

set.seed(1234567890)
perf.svm <- foreach(1:20, .combine = "rbind") %do% {
  folds <- createFolds(schizo$schizo)
  pr.svm <- unlist(foreach(fold = folds) %do% {
    test <- schizo[fold, ]
    training <- schizo[-fold, ]
    model <- svm(schizo ~ ., training, class.weights = NROW(schizo$schizo) / table(schizo$schizo))
    predict(model, select(test, -schizo))
  })  
  performance(pr.svm, schizo$schizo, "TRUE")  
}

rbind("Mean" = colMeans(perf.tree, na.rm = TRUE), "SD" = apply(perf.tree, 2, sd, na.rm = TRUE))
rbind("Mean" = colMeans(perf.svm, na.rm = TRUE), "SD" = apply(perf.svm, 2, sd, na.rm = TRUE))
