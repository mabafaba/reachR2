

stratify.count.sample<-function(data.strata,sf.strata){
  # count samples per stratum
  samplecounts<-table(data.strata)
  # check which ones can be found in sampling frame
  strataexists<-(names((samplecounts)) %in% names(sf.strata))
  data.strata.not.in.sampleframe<-samplecounts[!strataexists]
  # throw error if data strata not found in sampling frame
  if(length(data.strata.not.in.sampleframe)!=0){stop(paste(
    "data has strata names that don't exist in sampling frame. records in this stratum will be ignored in all weighted functions."
  ))}
  # return sample counts
  return(samplecounts[strataexists])
}


stratify.weights<-function(pop_strata,sample_strata){


  # remove sample_strata names with no data (can happen when only a subset of the data is used)
  sample_strata %>% hasdata -> sample_strata
  # only use populations that appear in current sample:
  pop_strata<-pop_strata[names(sample_strata)]

  # insure that all names of sample strata are found in names of population strata (huuu so satisfying when code reads almost exactly like its explanation :) )
  insure(that.all=names(sample_strata)%in%names(pop_strata),
         err = "all data strata must exist in sampling frame")
  sample_global<-sum(sample_strata)
  pop_global<-sum(pop_strata)
  weights = (pop_strata/pop_global) / (sample_strata/sample_global)
  return(weights)

}

#' Chi-Square test on all variables
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
test_hypothesis_chisquare<-function(df.counts,write.to.file=NULL){
  # check input
  if(!all(lapply(df.counts,is.table) %>% unlist)){stop("chi square test only works on frequency tables (class 'table').
                                                     maybe you need to use 'aggregate_count()' or 'aggregate_count_weighted()?")}

  # suppressing warnings for chisq calculation.
  # chisq test assumptions are checked independently, so we can add them to the output  ('results valid' column) rather than throwing a warning
  suppressWarnings(chsq<-l(df.counts,chisq.test))

  # format results as data frame
  pvals  <-l(chsq,function(x){x$p.value}) %>% unlist
  xsqstat<-l(chsq,function(x){x$statistic}) %>% unlist
  valid<-l(chsq,chisq.assumptions.met) %>% unlist
  results<-data.frame(
    "variable name"=names(df.counts),
    "p-value" = pvals,
    "X-squared" =xsqstat,
    "results valid"=valid
  )
  if(!is.null(write.to.file)){write.csv(results,write.to.file)}
  return(results)
}


chisq.assumptions.met<-function(chsq.results){
  # none of the expected counts < 1
  # not more than 20% of the expected counts < 5
  if(any(chsq.results$expected<1) ||
     (length(which(chsq.results$expected<5))/length(chsq.results$expected)>0.2)){
      return(FALSE)}else{
        return(TRUE)
      }
}

