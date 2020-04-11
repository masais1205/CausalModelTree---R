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


path = 'C:/Users/admin-mas/Dropbox/Causality/data/UCI/'
raw = read.csv(paste0(path,'online_shoppers_intention.csv'), 
                stringsAsFactors = FALSE)
data = raw
remove_col = c()
for(i in 1:ncol(raw)) {
  if(typeof(raw[,i])=='integer' | typeof(raw[,i])=='double')
    data[,i] = f_median(raw[,i])
  else if(typeof(raw[,i])=='character') {
    remove_col = c(remove_col, i)
    dt = cate2bin(raw[,i], colnames(raw)[i])
    data = cbind(data,dt)
  }
  else if(typeof(raw[,i])=='logical') {
    data[,i] = ifelse(raw[,i], 1, 0)
  }
}
Revenue = data[,'Revenue']
data = data[,-c(remove_col,which('Revenue'==colnames(raw)))]
data = cbind(data, Revenue)

write.csv(data, paste0(path,'online_shoppers_intention_binary.csv'), row.names = FALSE, quote = FALSE)