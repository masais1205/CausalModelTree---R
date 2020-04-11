# # require(pcalg)
# require(bnlearn)
# require(arules)
# require(dplyr)
# require(stringr)
# require(ROCR)
# require(MASS)
# require(FSelectorRcpp)
# source("baseline.R")
# 
# as.numeric.factor = function(x) {as.numeric(levels(x))[x]}
# 
# as.factor.datafrme = function(data) {
#   for(i in 1:ncol(data)) {
#     data[,i] = as.factor(data[,i])
#   }
#   data
# }
# 
# check.levels = function(data) {
#   index = c()
#   for(i in 1:ncol(data)) {
#     if(nlevels(as.factor(data[,i])) < 2)
#       index = c(index, i)
#   }
#   if(length(index) > 0)
#     data[,-index]
#   else
#     data
# }
# 
# 
# calc.jaccard = function(vec1, vec2) {
#   length(intersect(vec1,vec2)) / length(union(vec1,vec2))
# }
# 
# 
# calc.leakage.ass = function(data, X, Y, S, alpha) {
#   data = as.data.frame(data)
#   data = mutate(data, 
#                 XY = ifelse(data[,X]==1 & data[,Y]==1, 1, 0),
#                 nXY = ifelse(data[,X]==0 & data[,Y]==1, 1, 0))
#   data$frequency = rep(1, dim(data)[1])
#   agg = aggregate(cbind(data[,c(X,Y,'XY','nXY','frequency')]), by=data[,S], FUN=sum)
#   
#   m = ifelse(agg[,X]!=0, agg[,'XY']/agg[,X], 0) -
#     ifelse(agg[,X]!=agg[,'frequency'], agg[,'nXY']/(agg[,'frequency']-agg[,X]), 0)
#   w = agg[,'frequency']
#   ass = weighted.mean(m, w)
#   
#   if (abs(ass) >= alpha)
#     ass
#   else
#     0
# }
# 
# 
# add.noise = function(data, noise.ratio) {
#   for(i in 1:ncol(data)) {
#     tmp = data[,i]
#     index = sample(1:nrow(data), as.integer(nrow(data)*noise.ratio))
#     tmp[index] = as.numeric(! tmp[index])
#     data[,i] = tmp
#   }
#   data
# }
# 
# 
# get.split.cand = function(data, vars, label) {
#   split.cand = c()
#   for(v in vars) {
#     tbl = table(data[,v], data[,label])
#     p.value = chisq.test(tbl)$p.value
#     if(p.value <= 0.05)
#       split.cand = c(split.cand, v)
#   }
#   
#   split.cand
# }
# 
# 
# split.learn.pc = function(data, s.var, vars, label) {
#   vals = sort(unique(data[,s.var]))
#   vars = vars[! vars %in% s.var]
#   if(length(vals) != 2)
#     NULL
#   
#   data = as.factor.datafrme(data)
#   sub.data = data[which(data[,s.var]==vals[1]),c(vars,label)]
#   sub.data = check.levels(sub.data)
#   pc1 = learn.nbr(sub.data, label, method = "si.hiton.pc", alpha = 0.05)
#   sub.data = data[which(data[,s.var]==vals[2]),c(vars,label)]
#   sub.data = check.levels(sub.data)
#   pc2 = learn.nbr(sub.data, label, method = "si.hiton.pc", alpha = 0.05)
#   
#   list(pc1, pc2)
# }
# 
# 
# splitting.cand = function(data, vars, label, topN) {
#   weights = information_gain(as.formula(paste0(label,'~',paste(vars,collapse = '+'))), data)
#   weights = weights[order(weights[,'importance'],decreasing = T),]
#   
#   if(nrow(weights) > topN)
#     split.vars = weights[1:topN,'attributes']
#   else
#     split.vars = weights[,'attributes']
#   
#   split.vars
# }
# 
# 
# calc.accuracy = function(model, test, label) {
#   log_predict <- predict(model, newdata = test, type = "response")
#   
#   pred <- prediction(log_predict, test[,label])
#   perf <- performance(pred,"auc")
#   
#   accuracy = perf@y.values[[1]]
#   accuracy
# }
# 
# 
# main = function(train, test, label, alpha=0.05, method=3, topN=3) {
#   col.names = colnames(train)
#   var.names = col.names[! col.names %in% label]
#   
#   tree = data.frame(index=integer(0),
#                     node=integer(0),
#                     var=character(0),
#                     model=character(0), 
#                     size=integer(0),
#                     accuracy=integer(0), stringsAsFactors=FALSE)
#   a = b = c = 1
#   if(method == 3 | method == 5) {
#     split.cand = splitting.cand(train, var.names, label, topN)
#     
#     #### TODO, save to tree
#     # tree[nrow(tree)+1,] = list(1, var, '', nrow(train), NA)
#     # tree.height = nrow(tree)
#     
#     for (var in split.cand) {
#       cand.pc = split.learn.pc(data, var, var.names, label)
#       pc1 = cand.pc$pc1
#       pc2 = cand.pc$pc2
#     
#       vars = var.names[! var.names %in% var]
#       vals = sort(unique(train[,var]))
#       for (i in 1:length(vals)) {
#         val = vals[i]
#         if(i==1)
#           pc = pc1
#         else
#           pc = pc2
#         sub.data = train[which(train[,var]==val),]
#         
#         s.split.cand = splitting.cand(sub.data, vars, label, topN)
#         for (s.var in s.split.cand) {
#           s.vals = sort(unique(sub.data[,s.var]))
#           s.cand.pc = split.learn.pc(data, s.var, vars, label)
#           s.pc1 = s.cand.pc$pc1
#           s.pc2 = s.cand.pc$pc2
#           
#           if(length(s.pc1)==0 | length(s.pc2)==0) {
#             # tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, '<leaf>',
#             #                            paste0(label, '~', paste(pc, collapse= "+")),
#             #                            nrow(sub.data), NA)
#             next
#           }
#           
#           tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, s.var, '',
#                                      nrow(sub.data), NA)
#           tree[nrow(tree)+1,] = list(tree[nrow(tree),'node']*10+s.vals[1], '<leaf>', 
#                                      paste0(label, '~', paste(s.pc1, collapse= "+")),
#                                      nrow(sub.data[which(sub.data[,s.var]==vals[1]),]), NA)
#           tree[nrow(tree)+1,] = list(tree[nrow(tree)-1,'node']*10+s.vals[2], '<leaf>', 
#                                      paste0(label, '~', paste(s.pc2, collapse= "+")),
#                                      nrow(sub.data[which(sub.data[,s.var]==vals[2]),]), NA)
#         }
#       }
#     }
#     ###################TODO
#     # tree[nrow(tree)+1,] = list(1, var, '', nrow(train), NA)
#     # tree.height = nrow(tree)
#     
#     accuracy.list = c()
#     count.list = c()
#     for(i in 1:nrow(tree)) {
#       if(tree[i,'var'] != '<leaf>')
#         next
#       
#       cur.node = tree[i,'node']
#       fml = as.formula(tree[i,'model'])
#       pre.vars = c()
#       pre.vals = c()
#       while (cur.node != 1) {
#         pre.vals = c(pre.vals, cur.node %% 10)
#         cur.node = cur.node %/% 10
#         pre.vars = c(pre.vars, tree[which(tree[,'node']==cur.node),'var'])
#       }
#       
#       if(length(pre.vars) == 1)
#         sub.data = subset(train, train[,pre.vars]==pre.vals)
#       else
#         sub.data = subset(train, 
#                           train[,pre.vars[1]]==pre.vals[1] & train[,pre.vars[2]]==pre.vals[2])
#       if(nrow(sub.data) == 0) {
#         count.list = c(count.list, 0)
#         accuracy.list = c(accuracy.list, 0)
#         next
#       }
#       count.list = c(count.list, nrow(sub.data))
#       model = glm(fml, family=binomial(link='logit'), data=sub.data)
#       # print(model)
#       acc = calc.accuracy(model, test, label)
#       # tree[i,'size'] = nrow(sub.data)
#       tree[i,'accuracy'] = acc
#       accuracy.list = c(accuracy.list, acc)
#     }
#     print(tree)
#     accuracy = weighted.mean(accuracy.list, count.list)
#     cat('Model tree - accuracy:', accuracy, '\n')
#   }
# }
# 
# 
# 
# 
# # # adult_dataset-binary-remove
# # # Job-binary
# # # CollegeDistanceData-binary
# # # census-UCI_ALL_my_2
# # fileName = "C:/Users/admin-mas/Dropbox/Causality/data/adult_dataset-binary-remove.csv"
# # data = read.csv(fileName)
# # label = colnames(data)[ncol(data)]
# # alpha = 0.05
# # method = 3 # 1: single, 2: pattern, 3: model tree, 4: single+pattern, 5: all
# # 
# # smp_size <- floor(0.6 * nrow(data))
# # ## set the seed to make your partition reproducible
# # set.seed(123)
# # train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# # train <- data[train_ind, ]
# # test <- data[-train_ind, ]
# # 
# # main(train, test, label, alpha, method)