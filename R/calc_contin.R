#' Calculate Summary Statistics for Continuous Variables
#'
#' @param d, var
#' @keywords table
#' @export

calc_contin<-function(d, var){
  v<-d[,var]
  
  m_sd<-paste0(round(mean(v,na.rm=TRUE),2),', ', round(sd(v,na.rm=TRUE),2))
  med<-round(median(v,na.rm=TRUE),2)
  iqr<-paste0(as.character(round(quantile(v,.25, na.rm=TRUE),2)),"-", as.character(round(quantile(v,.75, na.rm=TRUE),2)))
  med_iqr<-paste0(med,', ', iqr)
  miss_count<-sum(!complete.cases(v))

  res<-data.frame(lev=c(var,c(' - Mean, SD',' - Median, IQR'),' - Missing'),
                  nums=c('',c(m_sd, med_iqr, paste0(miss_count))))
  colnames(res)<-c('','')
  
  return(res)
}
