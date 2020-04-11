# source('functions.R')
# 
# 
# model.tree = function(train, test, label, alpha=0.05, nFolds=5) {
#   cat('--------------- model tree -----------------\n')
#   col.names = colnames(train)
#   vars = col.names[! col.names %in% label]
#   
#   tree = data.frame(node=integer(0),
#                     var=character(0),
#                     model=character(0), 
#                     size=integer(0),
#                     accuracy=integer(0), stringsAsFactors=FALSE)
#   
#   holdout = split(sample(1:nrow(train)), 1:nFolds)
#   cand = splitting.cand.lasso(train, holdout, vars, label)
#   
#   if (cand == 'NULL')
#     stop('Error. No splitting improves AUC.')
#   tree[nrow(tree)+1,] = list(1, cand, '', nrow(test), NA)
#   tree.height = nrow(tree)
#   
#   vars = vars[! vars %in% cand]
#   vals = sort(unique(train[,cand]))
#   for (j in 1:length(vals)) {
#     val = vals[j]
#     tr.dat = train[which(train[,cand]==val),]
#     te.dat = test[which(test[,cand]==val),]
#     holdout = split(sample(1:nrow(tr.dat)), 1:nFolds)
#     s.cand = splitting.cand.lasso(tr.dat, holdout, vars, label)
#     
#     pc = learn.pc(tr.dat, vars, label)
#     if(is.na(pc))
#       pc = vars
#     if(is.null(s.cand) || s.cand == 'NULL') {
#       tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, '<leaf>',
#                                  paste0(label, '~', paste(pc, collapse= "+")),
#                                  nrow(te.dat), NA)
#       next
#     }
#     
#     tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, s.cand, '',
#                                nrow(te.dat), NA)
#     s.vals = sort(unique(tr.dat[,s.cand]))
#     s.cand.pc = split.learn.pc(tr.dat, s.cand, vars, label)
#     if(is.na(s.cand.pc)) {
#       tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
#                                paste0(label, '~', paste(pc, collapse= "+")),
#                                nrow(te.dat), NA)
#       next
#     }
#     # s.pc1 = NA
#     # s.pc2 = NA
#     if(is.na(s.pc1) || is.na(s.pc2)) {
#       tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
#                                  paste0(label, '~', paste(pc, collapse= "+")),
#                                  nrow(te.dat), NA)
#     }
#     else if(all(s.pc1==s.pc2)) {
#       tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
#                                paste0(label, '~', paste(pc, collapse= "+")),
#                                nrow(te.dat), NA)
#     }
#     else if(length(s.pc1)==1 || length(s.pc2)==1) {
#       tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
#                                paste0(label, '~', paste(pc, collapse= "+")),
#                                nrow(te.dat), NA)
#     }
#     else {
#       tree[nrow(tree)+1,] = list(tree[nrow(tree),'node']*10+s.vals[1], '<leaf>', 
#                                  paste0(label, '~', paste(s.pc1, collapse= "+")),
#                                  nrow(te.dat[which(te.dat[,s.cand]==s.vals[1]),]), NA)
#       tree[nrow(tree)+1,] = list(tree[nrow(tree)-1,'node']*10+s.vals[2], '<leaf>', 
#                                  paste0(label, '~', paste(s.pc2, collapse= "+")),
#                                  nrow(te.dat[which(te.dat[,s.cand]==s.vals[2]),]), NA)
#     }
#   }
#   
#   accuracy.list = c()
#   count.list = c()
#   for(i in 1:nrow(tree)) {
#     if(tree[i,'var'] != '<leaf>')
#       next
#     cur.node = tree[i,'node']
#     
#     fml = as.formula(tree[i,'model'])
#     pre.vars = c()
#     pre.vals = c()
#     while (cur.node != 1) {
#       pre.vals = c(pre.vals, cur.node %% 10)
#       cur.node = cur.node %/% 10
#       pre.vars = c(pre.vars, tree[which(tree[,'node']==cur.node),'var'])
#     }
#     
#     if(length(pre.vars) == 1) {
#       tr.dat = subset(train, train[,pre.vars]==pre.vals)
#       te.dat = subset(test, test[,pre.vars]==pre.vals)
#     }
#     else {
#       tr.dat = subset(train, 
#                         train[,pre.vars[1]]==pre.vals[1] & train[,pre.vars[2]]==pre.vals[2])
#       te.dat = subset(test, 
#                     test[,pre.vars[1]]==pre.vals[1] & test[,pre.vars[2]]==pre.vals[2])
#     }
#     if(nrow(tr.dat) == 0) {
#       count.list = c(count.list, 0)
#       accuracy.list = c(accuracy.list, 0)
#       next
#     }
#     model = glm(fml, family=binomial(link='logit'), data=tr.dat)
#     # print(model)
#     acc = calc.accuracy(model, test, label)
#     # tree[i,'size'] = nrow(tr.dat)
#     tree[i,'accuracy'] = acc
#     count.list = c(count.list, nrow(te.dat))
#     accuracy.list = c(accuracy.list, acc)
#   }
#   print(tree)
#   accuracy = weighted.mean(accuracy.list, count.list)
#   cat('Model tree - AUC:', accuracy, '\n')
# }
# 
# 
# 
# 
# # # adult_dataset-binary-remove
# # # Job-binary
# # # CollegeDistanceData-binary
# # # census-UCI_ALL_my_2
# # fileName = "C:/Users/mss/Dropbox/Causality/data/adult_dataset-binary-remove.csv"
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