require(RWeka)
require(rpart)


baseline.dt = function(train, test, label, nFolds) {
  if(nFolds==1) cat("--------------- baseline - decision tree -----------------\n")
  fml = as.formula(paste0(label, '~.'))
  model = rpart(fml, method="class", data=train)
  pred = predict(model, test, type = 'class')

  pred <- prediction(as.numeric(pred), as.numeric(test[,label]))
  perf <- performance(pred,"auc")
  accuracy = perf@y.values[[1]]
  if(nFolds==1) cat('baseline - decision tree - AUC:', accuracy, '\n\n\n')
  accuracy
}


baseline.log = function(train, test, label, nFolds) {
  if(nFolds==1) cat("--------------- baseline - logistic -----------------\n")
  for(i in 1:ncol(train)) {
    train[,i] = as.factor(train[,i])
  }
  for(i in 1:ncol(test)) {
    test[,i] = as.factor(test[,i])
  }
  train = check.levels(train)
  test = check.levels(test)
  features = intersect(colnames(train), colnames(test))
  train = train[,features]
  test = test[,features]
  
  fml = as.formula(paste0(label, '~.'))
  model <- glm(fml, data = train, family = "binomial")
  pred = predict(model, test, type = 'response')
  # model = Logistic(fml, data=train)
  # pred = predict(model, test, type = 'class')
  # e = evaluate_Weka_classifier(model, newdata = test, class=TRUE)
  # print(e)
  
  pred <- prediction(as.numeric(pred), as.numeric(test[,label]))
  perf <- performance(pred,"auc")
  accuracy = perf@y.values[[1]]
  if(nFolds==1) cat('baseline - logistic regression - AUC:', accuracy, '\n\n\n')
  accuracy
}


baseline.lmt = function(train, test, label, nFolds) {
  if(nFolds==1) cat("--------------- baseline - LMT -----------------\n")
  for(i in 1:ncol(train))
    train[,i] = as.factor(train[,i])
  for(i in 1:ncol(test))
    test[,i] = as.factor(test[,i])
  fml = as.formula(paste0(label, '~.'))
  model = LMT(fml, data=train)
  pred = predict(model, test, type = 'class')
  e = evaluate_Weka_classifier(model, newdata = test, class=TRUE)
  accuracy = mean(e$detailsClass[,'areaUnderROC'])
  
  # pred <- prediction(as.numeric(pred), as.numeric(test[,label]))
  # perf <- performance(pred,"auc")
  # accuracy = perf@y.values[[1]]
  if(nFolds==1) cat('baseline - logistic model tree - AUC:', accuracy, '\n\n\n')
  accuracy
}


baseline.j48 = function(train, test, label) {
  cat("--------------- baseline - J48 -----------------\n")
  for(i in 1:ncol(train))
    train[,i] = as.factor(train[,i])
  for(i in 1:ncol(test))
    test[,i] = as.factor(test[,i])
  fml = as.formula(paste0(label, '~.'))
  model = J48(fml, data=train)
  pred = predict(model, test, type = 'class')
  e = evaluate_Weka_classifier(model, newdata = test, class=TRUE)
  accuracy = mean(e$detailsClass[,'areaUnderROC'])
  
  # pred <- prediction(as.numeric(pred), as.numeric(test[,label]))
  # perf <- performance(pred,"auc")
  # accuracy = perf@y.values[[1]]
  cat('baseline - J48 - AUC:', accuracy, '\n\n\n')
}