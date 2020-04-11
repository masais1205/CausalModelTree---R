require(arules)
require(dplyr)
source('functions.R')


### split variables based on PCs
# splitting.cand = function(data, vars, label) {
#   split.cand = get.split.cand(data, vars, label)
#     
#   pc.list = list()
#   pc.intersect.list = c()
#   pc.avg.count.list = c()
#   pc.diff.list = c()
#   for(s.var in split.cand) {
#     pcs = split.learn.pc(data, s.var, vars, label)
#     
#     pc.intersect.list = c(pc.intersect.list, length(intersect(pcs[[1]], pcs[[2]])))
#     pc.avg.count.list = c(pc.avg.count.list, mean(length(pcs[[1]]), length(pcs[[2]])))
#     pc.diff.list = c(pc.diff.list, calc.jaccard(pcs[[1]], pcs[[2]]))
#     pc.list = append(pc.list, list(pcs))
#   }
#   
#   ### Jaccard to get splitting var
#   # pc.diff.list[is.na(pc.diff.list)] = 1
#   # index = which(pc.diff.list == min(pc.diff.list))
#   # if(length(index) > 1)
#   #   index = index[1]
#   # cat('pc.intersect.list', pc.intersect.list, '\n')
#   # cat('pc.avg.count.list', pc.avg.count.list, '\n')
#   ### min intersection and max average count to get splitting var
#   index = which(pc.intersect.list == min(pc.intersect.list))
#   if(length(index) > 1)
#     index = which(pc.avg.count.list[index] == max(pc.avg.count.list[index]))
#   if(length(index) > 1)
#     index = index[1]
#   ### information gain to get splitting var
#   # weights = information_gain(as.formula(paste0(label,'~',split.cand)), data)
#   # index = which(weights[,'importance'] == max(weights[,'importance']))
#   # if(length(index) > 1)
#   #   index = index[1]
#   # print(index)
#   
#   list(s.var = split.cand[index], 
#        pc1 = pc.list[[index]][[1]], 
#        pc2 = pc.list[[index]][[2]])
# }


logistic = function(train, test, label, alpha=0.05, nFolds) {
  acc = c()

  train = as.factor.datafrme(train)
  test = as.factor.datafrme(test)
  train = check.levels(train)
  test = check.levels(test)
  features = intersect(colnames(train), colnames(test))
  if(! label %in% features)
    stop('Error. Label requires at least two levels of factors.')
  train = train[,features]
  test = test[,features]
  
  col.names = colnames(train)
  var.names = col.names[! col.names %in% label]
  
  if(nFolds==1) cat('--------------- logistic - single variable -----------------\n')
  pc = learn.nbr(train, label, method = "si.hiton.pc", alpha = 0.05)

  fml = as.formula(paste0(label, '~', paste(pc, collapse= "+")))

  model = glm(fml, family=binomial(link='logit'), data=train)
  if(nFolds==1) print(model)

  accuracy = calc.accuracy(model, test, label)
  if(nFolds==1) cat('Logistic - single, AUC:', accuracy, '\n\n\n')
  acc = c(acc, accuracy)
  
  
  
  if(nFolds==1) cat('--------------- logistic - pattern variable -----------------\n')
  var.wait.names = var.names[which(! var.names %in% pc)]
  # print(c(var.wait.names, label))

  train.pattern = train[,c(var.wait.names, label)]

  rules = apriori(train.pattern,
                   parameter = list(support = 0.05,
                                    confidence = 0.5,
                                    minlen = 3,
                                    maxlen = 3),
                   appearance = list(rhs = c(paste0(label, "=1"),
                                             paste0(label, "=0"))), 
                  control=list(verbose = FALSE))
  # print(rules)
  # inspect(sort(rules, by = "confidence"))

  for(i in 1:ncol(train)) {
    train[,i] = as.numeric(as.character(train[,i]))
  }
  rules.df = as(sort(rules, by = "confidence"), "data.frame")
  rules.rules = strsplit(as.character(rules.df[,'rules']), ' => ')

  pattern.list.tmp = c()
  pattern.var.list = c()
  leakage.ass.list = c()
  for(i in 1:length(rules.rules)) {
    lhs = rules.rules[[i]][1]
    lhs %<>%
         gsub("\\{", "", .) %>%
         gsub("\\}", "", .)

    items = unlist(strsplit(lhs, ','))
    vars = unlist(lapply(items, sub, pattern='\\=.*', replacement=''))
    vals = unlist(lapply(items, sub, pattern='.*\\=', replacement=''))

    pattern.list.tmp = c(pattern.list.tmp, gsub(',', 'AND', lhs))
    pattern.var.list = c(pattern.var.list, paste0(sort(vars),collapse = 'AND'))

    train.tmp = mutate(train, lhs = ifelse(
                            train[,vars[1]]==vals[1] & train[,vars[2]]==vals[2],
                            1,
                            0))
    leakage.ass = calc.leakage.ass(train.tmp, X='lhs', Y=label, S=pc, alpha)
    leakage.ass.list = c(leakage.ass.list, leakage.ass)
  }

  index = which(leakage.ass.list == 0)
  pattern.list.tmp = pattern.list.tmp[-index]
  pattern.var.list = pattern.var.list[-index]
  leakage.ass.list = leakage.ass.list[-index]
  # print(pattern.var.list)
  # print(leakage.ass.list)

  pattern.list = c()
  leakage.list = c()

  while (length(leakage.ass.list) > 0) {
    index = which(max(abs(leakage.ass.list)) == abs(leakage.ass.list))

    index.remove = c()
    while(length(index) > 0) {
      if(length(index) > 1)
        idx = index[1]
      else
        idx = index
      pattern.list = c(pattern.list, pattern.list.tmp[idx])
      leakage.list = c(leakage.list, leakage.ass.list[idx])

      vars.remove = unlist(strsplit(pattern.var.list[idx], 'AND'))

      L = lapply(strsplit(pattern.var.list, "AND"), Filter, f = function(x) any(x %in% vars.remove))
      L = replace(L, !lengths(L), NA)

      jdx = which(!is.na(L))
      index = index[!index %in% c(idx,jdx)]
      index.remove = c(index.remove, c(idx,jdx))
    }

    pattern.list.tmp = pattern.list.tmp[-index.remove]
    pattern.var.list = pattern.var.list[-index.remove]
    leakage.ass.list = leakage.ass.list[-index.remove]
  }

  pattern.list = gsub(",", "AND", pattern.list)
  pattern.list = gsub("=", "EQU", pattern.list)

  # print(pattern.list)
  # print(leakage.list)

  for(pat in pattern.list) {
    items = unlist(strsplit(pat, 'AND'))

    vars = unlist(lapply(items, sub, pattern='\\EQU.*', replacement=''))
    vals = unlist(lapply(items, sub, pattern='.*\\EQU', replacement=''))

    train[,pat] =ifelse(
      train[,vars[1]]==vals[1] & train[,vars[2]]==vals[2], 1, 0)
    test[,pat] =ifelse(
      test[,vars[1]]==vals[1] & test[,vars[2]]==vals[2], 1, 0)
  }

  for(i in 1:ncol(train)) {
    train[,i] = as.factor(train[,i])
    test[,i] = as.factor(test[,i])
  }

  fml = as.formula(paste0(label, '~', paste(c(pc,pattern.list), collapse= "+")))
  # print(fml)

  model = glm(fml, family=binomial(link='logit'), data=train)
  if(nFolds==1) print(model)

  accuracy = calc.accuracy(model, test, label)
  if(nFolds==1) cat('Logistic - pattern, AUC:', accuracy, '\n\n\n')
  acc = c(acc, accuracy)
}




# # adult_dataset-binary-remove
# # Job-binary
# # CollegeDistanceData-binary
# # census-UCI_ALL_my_2
# fileName = "C:/Users/mss/Dropbox/Causality/data/adult_dataset-binary-remove.csv"
# data = read.csv(fileName)
# label = colnames(data)[ncol(data)]
# alpha = 0.05
# method = 5 # 1: single, 2: pattern, 3: model tree, 4: single+pattern, 5: all
# smp_size <- floor(0.6 * nrow(data))
# ## set the seed to make your partition reproducible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# train <- data[train_ind, ]
# test <- data[-train_ind, ]
# 
# main(train, test, label, alpha, method)
# baseline.dt(train, test, label)
# baseline.log(train, test, label)




# fileName = "../../data/CollegeDistanceData-binary.csv"
# data = read.csv(fileName)
# label = colnames(data)[ncol(data)]
# alpha = 0.05
# noise.ratio = 0.1
# 
# smp_size <- floor(0.6 * nrow(data))
# ## set the seed to make your partition reproducible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# train <- data[train_ind, ]
# test <- data[-train_ind, ]
# if(noise.ratio>0)
#   test <- add.noise(test, noise.ratio)
# 
# for(i in 1:ncol(train)) {
#   train[,i] = as.factor(train[,i])
#   test[,i] = as.factor(test[,i])
# }
# print(dim(train))
# model = glm(paste0(label, '~.'), family=binomial(link='logit'), data=train)
# print(model)
#
# accuracy = calc.accuracy(model, test, label)
# cat('Logistic - pattern, AUC:', accuracy, '\n')