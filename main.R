require(bnlearn)
# require(stringr)
require(ROCR)
require(glmnet)
# require(MASS)
# require(FSelectorRcpp)

source("baseline.R")
source('modelTree.R')
source('logistic.R')



path = '../../data/binary_data/'
files = list.files(path,
                   pattern = glob2rx("*.csv*"))
# files = c('adult_dataset-binary-remove.csv')
print(files)
for(fileName in files) {
  cat('\n\n\n\n~~~~~~~~~~~~~~~~~', fileName, '\n')
  
  data = read.csv(paste0(path, fileName))
  label = colnames(data)[ncol(data)]
  alpha = 0.05
  method = 2 # 1: baseline, 2: logistic, 3: model tree, 4: all
  nFolds = 1 # nFolds>1: cross validation, nFolds=1: no cross validation
  dbug = FALSE
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  if(nFolds > 1)
    holdout = split(sample(1:nrow(data)), 1:nFolds)
  
  acc.dt = c()
  acc.log = c()
  acc.lmt = c()
  acc.log.pc = c()
  acc.log.pc.pattern = c()
  acc.mt = c()
  for(i in 1:nFolds) {
    cat(i, 'of', nFolds, '\n')
    if(nFolds > 1) {
      train = data[-holdout[[i]],]
      test = data[holdout[[i]],]
    }
    else {
      tr.index = sample(1:nrow(data),  as.integer(nrow(data)*0.8))
      train = data[tr.index,]
      test = data[-tr.index,]
    }
    
    train.factor = as.factor.datafrme(train)
    test.factor = as.factor.datafrme(test)
    train.factor = check.levels(train.factor)
    test.factor = check.levels(test.factor)
    features = intersect(colnames(train.factor), colnames(test.factor))
    if(! label %in% features)
      next
    
    if(method == 1 || method == 4) {
      acc.dt = c(acc.dt, baseline.dt(train, test, label, nFolds))
      acc.log = c(acc.log, baseline.log(train, test, label, nFolds))
      acc.lmt = c(acc.lmt, baseline.lmt(train, test, label, nFolds))
    }

    if(method == 2 || method == 4) {
      acc = logistic(train, test, label, alpha, nFolds)
      acc.log.pc = c(acc.log.pc, acc[1])
      acc.log.pc.pattern = c(acc.log.pc.pattern, acc[2])
    }
    
    if(method == 3 || method == 4) {
      acc.mt = c(acc.mt, model.tree(train, test, label, alpha, nFolds))
    }
  }
  
  if(method == 1 || method == 4) {
    cat('baseline - decision tree, AUC: ', specify_decimal(mean(acc.dt),4), '\n')
    cat('baseline - logistic regression, AUC: ', specify_decimal(mean(acc.log),4), '\n')
    cat('baseline - logistic model tree, AUC: ', specify_decimal(mean(acc.lmt),4), '\n')
  }
  if(method == 2 || method == 4) {
    cat('logistic regression - PC, AUC: ', specify_decimal(mean(acc.log.pc),4), '\n')
    cat('logistic regression - PC - Pattern, AUC: ', specify_decimal(mean(acc.log.pc.pattern),4), '\n')
  }
  if(method == 3 || method == 4) {
    # print(acc.mt)
    cat('model tree, AUC: ', specify_decimal(mean(acc.mt),4), '\n')
  }
}