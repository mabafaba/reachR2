read.csv.auto.sep<-function(file,stringsAsFactors=F,...){
  df<-fread(file,stringsAsFactors=stringsAsFactors,...) %>% as.data.frame
  colnames(df)<-reachR:::to_alphanumeric_lowercase(colnames(df))
  return(df)
}

to_alphanumeric_lowercase<-function(x){tolower(gsub("[^a-zA-Z0-9]","\\.",x))}


#' load_data
#'
#' @param file name of a csv file containing the data
#' @param uuid.column name of the data colum containing the UUID's
#' @return Data frame with the data
#' @seealso \code{\link{load.questionnaire} (not implemented), \link{load_samplingframe}}
#' @export
#' @examples
#' load_data("mydata.csv",uuid.column="UUID")
load_data <- function(file,uuid.column="_uuid") {
  uuid.column<-make.names(uuid.column)
  insure.is.single.value(uuid.column)
  data <- read.csv.auto.sep(file, stringsAsFactors = F)
  names(data)<-to_alphanumeric_lowercase(names(data))
  return(data)
}




#' Loads the sampling frame from a csv file, performing inital tests on the data.
#'
#' @param sampling.frame.file name of a csv file containing the sampling frame
#' @param sampling.frame.population.column sampling frame name of column holding population counts
#' @param sampling.frame.stratum.column sampling frame name of column holding stratum names. Stratum names must match exactly values in:
#' @param data.stratum.column data column name that holds the record's strata names
#' @param return.stratum.populations  by default this function returns `NULL`, but must be called to make automatic weighting in ..._weighted() functions possible. you can however retreive the stratum population counts by setting this to `TRUE`.
#' @seealso \code{\link{load_data} (not implemented), \link{aggregate_count_weighted}}
#' @return - makes function `weights_of()` usable, and with it .._weighted() functions with automatic weighting
#' `NULL` if `return.stratum.populations` is `FALSE` (default)
#' A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
#' @export
#' @examples
#' load_data("mydata.csv",uuid.column="UUID")
load_samplingframe <- function(sampling.frame.file,
                                sampling.frame.population.column="population",
                                sampling.frame.stratum.column="stratum",
                                data.stratum.column="overview.camp_name",
                                return.stratum.populations=FALSE){

  # check input
  insure.is.single.value(sampling.frame.file)
  insure.is.single.value(sampling.frame.population.column)
  insure.is.single.value(sampling.frame.stratum.column)
  insure.is.single.value(data.stratum.column)

  # load file:

  sf_raw<-read.csv.auto.sep(sampling.frame.file,stringsAsFactors = F, header = T)

      # create unique strata names from sampling frame
  unique_strata <- sf_raw[, sampling.frame.stratum.column]
  # make sure strata are unique
  #this warning needs to be integrated in load_sampling frame
  if(any((unique_strata %>% hasdata %>% table)>1)){stop("duplicate stratum names in the sampling frame")}

  # standardise internal sampling frame format
  # - data.stratum.column: the name of the data column holding strata names (is function argument)
  # - population.counts: named vector with counts as values and strata names as names
  # use: population.counts[stratum_name_string]
  population.counts<-sf_raw[[sampling.frame.population.column]]
  names(population.counts)<-as.character(unique_strata)

  # error if any stratum has zero population
  if(any(population.counts==0)){stop("strata in sampling frame can not have population 0, remove. (how do you even sample from that?)")}
  # make sure all strata have data:
  population.counts <- population.counts[(
    !is.na(population.counts) &
      !is.na(population.counts) &
      population.counts > 0)]

  # closure function that calculates weights on the fly
  # uses immutable data provided to load_samplingframe()
  weights_of<- function(df) {
    # insure stratum column exists in df:
    insure.string.is.column.header(df,data.stratum.column)
    # insure only one data.stratum.column is given:
    insure.is.single.value(data.stratum.column)
    # make sure df is handled as characters, not factors. otherwise we match factor id's instead of names
    df[[data.stratum.column]]<-as.character(df[[data.stratum.column]])
    df <- df[!is.na(data.stratum.column),]
    df <- df[!(df[[data.stratum.column]] %in% c("NA", "N/A", "#N/A")),]

    # count number of records in each stratum
    sample.counts<-stratify.count.sample(data.strata = df[[data.stratum.column]],sf.strata = population.counts)

    # make sure all record's strata can be found in the sampling frame:
    if("weights" %in% names(df)){stop("'weights' is not allowed as a column name (will be calculated from the sampling frame)")}
    if(!all(names(sample.counts) %in% names(population.counts))){stop("all strata names in column '",
                                                                      data.stratum.column,"' must also appear in the loaded sampling frame.")}
    # population counts taken from weights_of() enclosing environment, created in load_samplingframe()
    weights <- stratify.weights(pop_strata = population.counts,
                                sample_strata = sample.counts)

    # final test that mean of weights == 1
    insure(that.all=mean(weights[df[[data.stratum.column]]]) %almost.equals% 1,
           err="Weighting calculation failed internally, this is our fault. Sorry! Contact the Reach Initiatives data unit to get this fixed!")
    return(weights[df[[data.stratum.column]]])


  }
  assignInNamespace("weights_of_internal", weights_of, ns="reachR")
  message(
    "Sampling frame loaded. you can now use .._weighted() functions with automatic weighting"
  )
  if(return.stratum.populations){return(population.counts)}
}

weights_of <- function(df) {
  weights_of_internal(df)
}

weights_of_internal <- function(df) {
  stop("Before weights_of() can be used, load_samplingframe() must be run.")
}


'%almost.equals%'<-function(x,y){
  abs(x-y)<0.0000000001
}


#' Loads the cleaning log from a csv file
#'
#' @param cleaning_log_file name of a csv file containing the cleaning log
#' @param cleaning.log.uuid.column name of the cleaning log column holding the UUIDs
#' @param cleaning.log.new.value.column name of the cleaning log column holding the new value
#' @param cleaning.log.variable.column name of the cleaning log column holding the variable name, referencing the column header in the data
#' @seealso This function enables \code{\link{data_validation_cleaning_log_comparison}. Similar data loading functions see \link{load_data},\link{load_questionnaire} (not implemented)}
#' @return - makes function \code{\link{data_validation_cleaning_log_comparison}} usable
#' `NULL` if `return.stratum.populations` is `FALSE` (default)
#' A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
#' @export
#' @examples
load_cleaninglog<-function(cleaning.log.file,cleaning.log.uuid.column,cleaning.log.new.value.column,cleaning.log.variable.column){

  cl <- read.csv.auto.sep(cleaning.log.file, stringsAsFactors = F)
  cleaning.log.new.value.column<-to_alphanumeric_lowercase(cleaning.log.new.value.column)
  cleaning.log.uuid.column<-to_alphanumeric_lowercase(cleaning.log.uuid.column)
  cleaning.log.variable.column<-to_alphanumeric_lowercase(cleaning.log.variable.column)


  names(cl)<-to_alphanumeric_lowercase(names(cl))
  insure.string.is.column.header(cl,cleaning.log.new.value.column)
  insure.string.is.column.header(cl,cleaning.log.variable.column)
  insure.string.is.column.header(cl,cleaning.log.uuid.column)

  cl[[cleaning.log.variable.column]]<-to_alphanumeric_lowercase(cl[[cleaning.log.variable.column]])

  reachR:::insure(!("data_index" %in% names(cl)),err = "data_index must not be a column header in the cleaning log.")
  reachR:::insure.no.duplicates(names(cl))
  # must check if arguments used in closure all supplied:
  columns<-c(cleaning.log.uuid.column,cleaning.log.new.value.column,cleaning.log.variable.column)
  # lapply(columns,reachR:::insure.string.is.column.header,df = cl)
  reachR:::insure(that.all = (columns %in% names(cl)),err = paste("non-existing cleaning log columns selected.\n",
                                                                  "available cleaning log column names are:\n",
                                                                        paste(names(cl),collapse = ", ")))
  data_validation_cleaning_log_comparison<-function(data,data.uuid.column){
    cleaning.log.new.value.column<-to_alphanumeric_lowercase(cleaning.log.new.value.column)

    insure.string.is.column.header(data,data.uuid.column)
    cl$data_index <- match(x = cl[, cleaning.log.uuid.column],
                           data[, data.uuid.column])
    cl <- apply(cl, 1, function(x) {
      value<-data[x["data_index"], x[cleaning.log.variable.column]]
      if(is.null(value)){value<-NA}
    }) %>% lapply(function(x) {
      if (is.na(x)) {
        return("variable name not found in data")
      }
      else {
        x
      }
    }) %>% unlist %>% (function(x) {
      x[is.na(x)] <- "UUID not found in data"
      x
    }) %>% cbind(data_value = ., cl)

    cl$has_issue <- !(cl[, "data_value"] %>% as.character) == (cl[, cleaning.log.new.value.column] %>% as.character)
    issues<-data.frame(
      index=cl$data_index,
      value=cl[,"data_value"],
      variable=cl[[cleaning.log.variable.column]],
      has_issue=cl$has_issue,
      issue_type="found no match in data for cleaning log new value"

    )
    issues %>% head
    return(issues)

  }
  # change in package namespace; this is a closure inside an R package which is fancy but a bit complicated but makes me happy.
  # if this makes no sense to you google "closure functional programming" and "R environments and namespaces". Don't google "closure inside R package" if you're in 2018, its doesn't seem to be a thing yet
  assignInNamespace("data_validation_cleaning_log_comparison_internal", data_validation_cleaning_log_comparison, ns="reachR")

}

#' Compare cleaning log and cleaned data
#'
#' @param data data frame with the cleaned data
#' @param data.uuid.column name of the data column holding the UUIDs
#' @seealso  \code{\link{load_cleaninglog}} must be run before this function can be used.
#' @return - Data frame containing uuids, variables and values of d
#' ata entries that do not match the cleaning log
#' `NULL` if `return.stratum.populations` is `FALSE` (default)
#' A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
#' @export
#' @examples
data_validation_cleaning_log_comparison<-function(data,data.uuid.column){
return(reachR:::data_validation_cleaning_log_comparison_internal(data,data.uuid.column))
  }

# needed so closure works fine; we can't change the user accessable namespace, so we change this internal function, called by the visible one:
data_validation_cleaning_log_comparison_internal<-function(data){
  stop("Before data_validation_cleaning_log_comparison() can be used, load_cleaninglog() must be run.")
}



