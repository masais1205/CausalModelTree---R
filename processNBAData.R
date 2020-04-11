f_median = function(vec) {
  m = median(vec)
  aBigger = vec > m
  aSmaller = !aBigger
  vec[aBigger] = 1
  vec[aSmaller] = 0
  return(vec)
}


cate2bin = function(vec, name) {
  dt = data.frame()
  val = unique(vec)
  for(v in val) {
    vec_new = ifelse(vec==v, 1, 0)
    if(length(dt)==0) {
      dt = vec_new
    }
    else {
      dt = cbind(dt, vec_new)
      colnames(dt)[ncol(dt)] = paste(name, v, sep='_')
    }
  }
  colnames(dt)[1] = paste(name, val[1], sep='_')
  dt
}


path = 'C:/Users/admin-mas/Documents/Causality/data/New/'
raw = read.csv(paste0(path,'nba_logreg.csv'), 
                stringsAsFactors = FALSE)
raw = raw[,-1]
raw[is.na(raw[,'X3P.']),'X3P.'] = 0
data = raw
remove_col = c()
for(i in 1:ncol(raw)) {
  if(typeof(raw[,i])=='integer' | typeof(raw[,i])=='double') {
    if(length(unique(raw[,i])) == 2)
      data[,i] = raw[,i]
    else if(length(unique(raw[,i])) > 2)
      data[,i] = f_median(raw[,i])
  }
  else if(typeof(raw[,i])=='character') {
    remove_col = c(remove_col, i)
    dt = cate2bin(raw[,i], colnames(raw)[i])
    data = cbind(data,dt)
  }
  else if(typeof(raw[,i])=='logical') {
    data[,i] = ifelse(raw[,i], 1, 0)
  }
}


write.csv(data, paste0(path,'nba_binary.csv'), row.names = FALSE, quote = FALSE)
