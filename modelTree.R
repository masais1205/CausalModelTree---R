source('functions.R')


model.tree = function(train, test, label, alpha=0.05, nFolds) {
  if(nFolds==1) cat('--------------- model tree -----------------\n')
  col.names = colnames(train)
  vars = col.names[! col.names %in% label]
  
  tree = data.frame(node=integer(0),
                    var=character(0),
                    model=character(0), 
                    size=integer(0),
                    accuracy=integer(0), stringsAsFactors=FALSE)
  
  cands = get.split.cand(train, vars, label)
  cand.pc = splitting.cand(train, cands, vars, label)
  
  if (is.na(cand.pc)) {
    # stop('Error. No PC found.')
    fml = as.formula(paste0(label, '~', paste(cands, collapse= "+")))
    model = glm(fml, family=binomial(link='logit'), data=train)
    acc = calc.accuracy(model, test, label)
    return(acc)
  }
  cand = cand.pc$s.var
  pc1 = cand.pc$pc1
  pc2 = cand.pc$pc2
  tree[nrow(tree)+1,] = list(1, cand, '', nrow(test), NA)
  tree.height = nrow(tree)
  
  vars = remove.cands(vars, cand)
  vals = sort(unique(train[,cand]))
  for (j in 1:length(vals)) {
    if(j==1) pc=pc1 else pc=pc2
    val = vals[j]
    tr.dat = train[which(train[,cand]==val),]
    te.dat = test[which(test[,cand]==val),]
    
    cands = get.split.cand(tr.dat, vars, label)
    cand.pc = splitting.cand(tr.dat, cands, vars, label)
    
    if(length(pc) == 0)
      pc = cands
      if(length(pc) == 0)
        pc = vars
    if(is.na(cand.pc)) {
      tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, '<leaf>',
                                 paste0(label, '~', paste(pc, collapse= "+")),
                                 nrow(te.dat), NA)
      next
    }
    
    s.cand = cand.pc$s.var
    s.pc1 = cand.pc$pc1
    s.pc2 = cand.pc$pc2
    s.pc1 = character(0)
    s.pc2 = character(0)
    tree[nrow(tree)+1,] = list(tree[tree.height,'node']*10+val, s.cand, '',
                               nrow(te.dat), NA)
    s.vals = sort(unique(tr.dat[,s.cand]))
    s.cand.pc = split.learn.pc(tr.dat, s.cand, vars, label)
    if(is.na(s.cand.pc)) {
      tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
                               paste0(label, '~', paste(pc, collapse= "+")),
                               nrow(te.dat), NA)
      next
    }
    
    if(length(s.pc1)==0 || length(s.pc2)==0) {
      tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
                               paste0(label, '~', paste(pc, collapse= "+")),
                               nrow(te.dat), NA)
    }
    else if(all(s.pc1==s.pc2)) {
      tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
                               paste0(label, '~', paste(pc, collapse= "+")),
                               nrow(te.dat), NA)
    }
    # else if(length(s.pc1)==1 || length(s.pc2)==1) {
    #   tree[nrow(tree),] = list(tree[tree.height,'node']*10+val, '<leaf>',
    #                            paste0(label, '~', paste(pc, collapse= "+")),
    #                            nrow(te.dat), NA)
    # }
    else {
      tree[nrow(tree)+1,] = list(tree[nrow(tree),'node']*10+s.vals[1], '<leaf>',
                                 paste0(label, '~', paste(s.pc1, collapse= "+")),
                                 nrow(te.dat[which(te.dat[,s.cand]==s.vals[1]),]), NA)
      tree[nrow(tree)+1,] = list(tree[nrow(tree)-1,'node']*10+s.vals[2], '<leaf>',
                                 paste0(label, '~', paste(s.pc2, collapse= "+")),
                                 nrow(te.dat[which(te.dat[,s.cand]==s.vals[2]),]), NA)
    }
  }
 
  accuracy.list = c()
  count.list = c()
  for(i in 1:nrow(tree)) {
    if(tree[i,'var'] != '<leaf>')
      next
    cur.node = tree[i,'node']

    fml = as.formula(tree[i,'model'])
    pre.vars = c()
    pre.vals = c()
    while (cur.node != 1) {
      pre.vals = c(pre.vals, cur.node %% 10)
      cur.node = cur.node %/% 10
      pre.vars = c(pre.vars, tree[which(tree[,'node']==cur.node),'var'])
    }

    if(length(pre.vars) == 1) {
      tr.dat = subset(train, train[,pre.vars]==pre.vals)
      te.dat = subset(test, test[,pre.vars]==pre.vals)
    }
    else {
      tr.dat = subset(train,
                        train[,pre.vars[1]]==pre.vals[1] & train[,pre.vars[2]]==pre.vals[2])
      te.dat = subset(test,
                    test[,pre.vars[1]]==pre.vals[1] & test[,pre.vars[2]]==pre.vals[2])
    }
    if(nrow(tr.dat) == 0) {
      count.list = c(count.list, 0)
      accuracy.list = c(accuracy.list, 0)
      next
    }
    model = glm(fml, family=binomial(link='logit'), data=tr.dat)
    # print(model)
    acc = calc.accuracy(model, te.dat, label)
    # tree[i,'size'] = nrow(tr.dat)
    tree[i,'accuracy'] = acc
    count.list = c(count.list, nrow(te.dat))
    accuracy.list = c(accuracy.list, acc)
  }
  if(nFolds==1) print(tree)
  accuracy = weighted.mean(accuracy.list, count.list)
  if(nFolds==1) cat('Model tree - AUC:', accuracy, '\n')
  accuracy
}