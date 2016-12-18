#' Create a table of Summary Statistics
#' 
#' Takes as input a data.frame and names of its columns which you wish to summarize. It will produce a data.frame with summary statistics. For continuous variables, the function returns mean, standard deviation, median, interquartile range. For catagorical variables, counts and percentages are returned. Count of missing values (coded as NAs) will be returned.
#' @param d data.frame containing variables to be summarized 
#' @param contin_vars character vector containing names of continuous variables to be summarized. Code missing values as NA.
#' @param cat_vars character vector containing names of catagorical variables to be summarized. These variables must be coded as factors. Code missing values as NA.
#' @param map a named character vector containing the formatted rownames of the data.frame which this function produceces. The names of this vector should be the raw rownames of the table. It's best to run this function once with map=NULL. See what the raw rownames are, then construct the map vector to associate a clean rownames for each unformatted rowname.
#' @param title a character vector of length 1. It will be the column name of the data.frame which this function will produce. The column name will contain the sample size count.
#' @keywords SummaryTable
#' @export

sum_stats<-function(d, contin_vars, cat_vars, map=NULL, title){
  
  cat_res<-lapply(cat_vars,calc_cat, d=d)
  contin_res<-lapply(contin_vars,calc_contin, d=d)
  
  all_res<-do.call('rbind', c(cat_res, contin_res))
  all_res[,1]<-as.character(all_res[,1])
  if(!is.null(map)){
    all_res[all_res[,1] %in% names(map), 1]<-as.character(map[all_res[all_res[,1] %in% names(map),1]])
  }
  colnames(all_res)<-c('',paste0(title,' (N = ', nrow(d),')'))
  
  return(all_res)
}



