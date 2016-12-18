#' Calculate Summary Statistics for Categorical Variables
#'
#' @param d, var
#' @keywords table
#' @export
#' 
calc_cat<-function(d, var){
  v<-d[,var]
  
  counts<-table(v)
  perc<-round(table(v)/sum(table(v)),2)*100
  miss_count<-sum(!complete.cases(v))

  res<-data.frame(lev=c(var,paste0(var,names(counts)),' - Missing'),
                  nums=c('',paste0(counts,', (',perc,'%)'),paste0(miss_count)))
  colnames(res)<-c('','')
  return(res)
}