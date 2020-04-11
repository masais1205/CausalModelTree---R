as.factor.datafrme = function(data) {
  for(i in 1:ncol(data)) {
    data[,i] = as.factor(data[,i])
  }
  data
}


specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


remove.cands = function(vec, ele) {
  ele.split = strsplit(ele, '_')[[1]][1]
  index = which(unlist(lapply(vec, function(x) strsplit(x,'_')[[1]][1])) != ele.split)
  vec[index]
}


calc.jaccard = function(vec1, vec2) {
  length(intersect(vec1,vec2)) / length(union(vec1,vec2))
}

check.levels = function(data) {
  index = c()
  for(i in 1:ncol(data)) {
    if(nlevels(as.factor(data[,i])) < 2)
      index = c(index, i)
  }
  if(length(index) > 0)
    data[,-index]
  else
    data
}


has_NA = function(data) {
  apply(is.na(data), 1, any) #= 1 if any column in that row is NA
}


get.split.cand = function(data, vars, label) {
  split.cand = c()
  for(v in vars) {
    tbl = table(data[,v], data[,label])
    p.value = chisq.test(tbl)$p.value
    if(is.na(p.value))
      next
    if(p.value <= 0.05)
      split.cand = c(split.cand, v)
  }
  
  split.cand
}


### split variables based on PCs
splitting.cand = function(data, split.cand, vars, label) {
  pc.list = list()
  pc.intersect.list = c()
  pc.avg.count.list = c()
  pc.diff.list = c()
  for(s.var in split.cand) {
    pcs = split.learn.pc(data, s.var, vars, label)
    if(is.na(pcs)){
      pc.intersect.list = c(pc.intersect.list, 0)
      pc.avg.count.list = c(pc.avg.count.list, 0)
      pc.diff.list = c(pc.diff.list, 1)
      pc.list = append(pc.list, list(pcs))
      next
    }
      
    pc.intersect.list = c(pc.intersect.list, length(intersect(pcs$pc1, pcs$pc2)))
    pc.avg.count.list = c(pc.avg.count.list, mean(length(pcs$pc1), length(pcs$pc2)))
    pc.diff.list = c(pc.diff.list, calc.jaccard(pcs$pc1, pcs$pc2))
    pc.list = append(pc.list, list(pcs))
  }

  ### Jaccard to get splitting var
  if(all(is.na(pc.diff.list)))
    return(NA)
  pc.diff.list[is.na(pc.diff.list)] = 1
  index = which(pc.diff.list == min(pc.diff.list))
  pc.intersect.list[-index] = 100
  if(length(index) > 1)
    index = which(pc.intersect.list[index] == min(pc.intersect.list[index]))
    if(length(index) > 1)
      index = index[1]
  if(pc.diff.list[index] == 1)
    return(NA)
  # cat('pc.intersect.list', pc.intersect.list, '\n')
  # cat('pc.avg.count.list', pc.avg.count.list, '\n')
  
  ### min intersection and max average count to get splitting var
  # index = which(pc.intersect.list == min(pc.intersect.list))
  # if(length(index) > 1)
  #   index = which(pc.avg.count.list[index] == max(pc.avg.count.list[index]))
  # if(length(index) > 1)
  #   index = index[1]
  
  ### information gain to get splitting var
  # weights = information_gain(as.formula(paste0(label,'~',split.cand)), data)
  # index = which(weights[,'importance'] == max(weights[,'importance']))
  # if(length(index) > 1)
  #   index = index[1]
  # print(index)

  list(s.var = split.cand[index],
       pc1 = pc.list[[index]][[1]],
       pc2 = pc.list[[index]][[2]])
}


learn.pc = function(data, vars, label) {
  data = as.factor.datafrme(data)
  if(nlevels(data[,ncol(data)])==1)
    return(NA)
  data = check.levels(data)
  pc = learn.nbr(data, label, method = "si.hiton.pc", alpha = 0.05)
  
  pc
}


split.learn.pc = function(data, s.var, vars, label) {
  vals = sort(unique(data[,s.var]))
  vars = vars[! vars %in% s.var]
  if(length(vals) != 2)
    return(NA)
  
  data = as.factor.datafrme(data)
  sub.data = data[which(data[,s.var]==vals[1]),c(vars,label)]
  sub.data = check.levels(sub.data)
  if(! label %in% colnames(sub.data))
    return(NA)
  pc1 = learn.nbr(sub.data, label, method = "si.hiton.pc", alpha = 0.05)
  
  sub.data = data[which(data[,s.var]==vals[2]),c(vars,label)]
  sub.data = check.levels(sub.data)
  if(! label %in% colnames(sub.data))
    return(NA)
  pc2 = learn.nbr(sub.data, label, method = "si.hiton.pc", alpha = 0.05)
  
  # if(length(pc1)==0 | length(pc2)==0)
  #   return(NA)
  
  list(pc1=pc1, pc2=pc2)
}


calc.residuals.lasso = function(train, test, label, pred) {
  if(nrow(train) == 0)
    return(0)
  if(nrow(test) == 0)
    return(0)
  
  if(nlevels(as.factor(test[,label]))==1 ||
     nlevels(as.factor(pred))==1) {
    return(sum(test[,label]==pred) / length(pred))
  }
  pred = prediction(as.numeric(pred), as.numeric(test[,label]))
  perf = performance(pred,"auc")
  
  auc = perf@y.values[[1]]
  auc
}


calc.residuals = function(fml, train, test, label) {
  if(nrow(train) == 0)
    return(0)
  if(nrow(test) == 0)
    return(0)
  
  model = glm(fml, family=binomial(link='logit'), data=train)
  pred = predict(model, newdata = test, type = "response")
  
  if(nlevels(as.factor(test[,label]))==1 ||
     nlevels(as.factor(pred))==1) {
    return(sum(test[,label]==pred) / length(pred))
  }
  pred = prediction(as.numeric(pred), as.numeric(test[,label]))
  perf = performance(pred,"auc")
  
  auc = perf@y.values[[1]]
  auc
}



# splitting.cand.lasso = function(train, holdout, vars, label) {
#   cand = c()
#   for (i in 1:length(holdout)) {
#     tr.dat = train[-holdout[[i]],]
#     val.dat = train[holdout[[i]],]
#     
#     X = as.matrix(tr.dat[,vars])
#     Y = as.matrix(tr.dat[,label])
#     cv_output <- cv.glmnet(X[!has_NA(X),], Y[!has_NA(Y),],
#                            alpha=1, family='binomial', type.measure='mse')
#     lambda_1se <- cv_output$lambda.1se
#     pred_test <- predict(cv_output, as.matrix(val.dat[,vars]), type="class", s=lambda_1se)
#     auc = calc.residuals.lasso(tr.dat, val.dat, label, pred_test)
#     
#     auc.list = c()
#     for (var in vars) {
#       vals = sort(unique(train[,var]))
#       auc.tmp = c()
#       size.tmp = c()
#       for (j in 1:length(vals)) {
#         val = vals[j]
#         tr.dat.tmp = tr.dat[which(tr.dat[,var]==val),]
#         val.dat.tmp = val.dat[which(val.dat[,var]==val),]
#         
#         X = as.matrix(tr.dat.tmp[,vars])
#         Y = as.matrix(tr.dat.tmp[,label])
#         cv_output <- cv.glmnet(X[!has_NA(X),], Y[!has_NA(Y),],
#                                alpha=1, family='binomial', type.measure='mse')
#         lambda_1se <- cv_output$lambda.1se
#         pred_test <- predict(cv_output, as.matrix(val.dat.tmp[,vars]), type="class", s=lambda_1se)
#         
#         auc.tmp = c(auc.tmp,
#                     calc.residuals.lasso(tr.dat.tmp, val.dat.tmp, label, pred_test))
#         size.tmp = c(size.tmp, nrow(val.dat.tmp))
#       }
#       auc.list = c(auc.list,
#                    weighted.mean(auc.tmp, size.tmp))
#     }
#     
#     if(all(is.nan(auc.list))) {
#       cand = c(cand, 'NULL')
#       next
#     }
#     if (max(auc.list) <= auc) {
#       cand = c(cand, 'NULL')
#       next
#     }
#     index = which(auc.list == max(auc.list))
#     if (length(index) > 1)
#       index = index[1]
#     cand = c(cand, vars[index])
#   }
#   cand = cand[cand != 'NULL']
#   
#   names(sort(table(cand),decreasing=TRUE))[1]
#   
# }

# splitting.cand = function(train, holdout, vars, label) {
#   cand = c()
#   for (i in 1:length(holdout)) {
#     tr.dat = train[-holdout[[i]],]
#     val.dat = train[holdout[[i]],]
#     
#     fml = as.formula(paste0(label, '~', paste(vars, collapse='+')))
#     auc = calc.residuals(fml, tr.dat, val.dat, label)
#     
#     auc.list = c()
#     for (var in vars) {
#       vals = sort(unique(train[,var]))
#       auc.tmp = c()
#       size.tmp = c()
#       for (j in 1:length(vals)) {
#         val = vals[j]
#         fml = as.formula(paste0(label, '~', paste(vars, collapse='+')))
#         tr.dat.tmp = tr.dat[which(tr.dat[,var]==val),]
#         val.dat.tmp = val.dat[which(val.dat[,var]==val),]
#         auc.tmp = c(auc.tmp,
#                     calc.residuals(fml, tr.dat.tmp, val.dat.tmp, label))
#         size.tmp = c(size.tmp, nrow(val.dat.tmp))
#       }
#       auc.list = c(auc.list,
#                    weighted.mean(auc.tmp, size.tmp))
#     }
#     
#     if(all(is.nan(auc.list))) {
#       cand = c(cand, 'NULL')
#       next
#     }
#     if (max(auc.list) <= auc) {
#       cand = c(cand, 'NULL')
#       next
#     }
#     index = which(auc.list == max(auc.list))
#     if (length(index) > 1)
#       index = index[1]
#     cand = c(cand, vars[index])
#   }
#   cand = cand[cand != 'NULL']
#   
#   names(sort(table(cand),decreasing=TRUE))[1]
# }

calc.leakage.ass = function(data, X, Y, S, alpha) {
  data = as.data.frame(data)
  data = mutate(data, 
                XY = ifelse(data[,X]==1 & data[,Y]==1, 1, 0),
                nXY = ifelse(data[,X]==0 & data[,Y]==1, 1, 0))
  data$frequency = rep(1, dim(data)[1])
  agg = aggregate(cbind(data[,c(X,Y,'XY','nXY','frequency')]), by=data[,S], FUN=sum)
  
  m = ifelse(agg[,X]!=0, agg[,'XY']/agg[,X], 0) -
    ifelse(agg[,X]!=agg[,'frequency'], agg[,'nXY']/(agg[,'frequency']-agg[,X]), 0)
  w = agg[,'frequency']
  ass = weighted.mean(m, w)
  
  if (abs(ass) >= alpha)
    ass
  else
    0
}


calc.accuracy = function(model, test, label) {
  pred <- predict(model, newdata = test, type = "response")
  
  pred <- prediction(pred, test[,label])
  perf <- performance(pred,"auc")
  # print(perf@y.values)
  
  # pred <- ifelse(pred > 0.5,1,0)
  # pred.table = table(pred=pred, label=test[,label])
  # if(nrow(pred.table)==1)
  #   pred.table = rbind(pred.table, c(0,0))
  # # print(pred.table)
  # m = c(
  #   pred.table[1,1] / (pred.table[1,1]+pred.table[2,1]),
  #   pred.table[2,2] / (pred.table[1,2]+pred.table[2,2]))
  # # w = c(pred.table[1,1]+pred.table[2,1], pred.table[1,2]+pred.table[2,2])
  # accuracy = mean(m)
  
  accuracy = perf@y.values[[1]]
  accuracy
}