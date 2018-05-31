
data.cleaning.check.log<-function(name,sucess=NA,comment=NULL,kable=NULL,fun=NULL){
if(!exists("data.cleaning.check.log.strings")){
  data.cleaning.check.log.string<- ""
}
sink("checklog.txt")
print("\n")
print(name)
print("\n")
print(name)
sink()
}

# Data format
# [ ]	Data is exported with at least xml values for headers
# [ ]	Column headers are unique (see previous)
    data.cleaning.check.unique.headers<-function(data){
      headers_table <- data %>% names %>% table
      headers_unique<- headers_table  %>% (function(x){all(x==1)})
      if(headers_unique){ data.cleaning.check.log("Column headers are unique",success=TRUE)
        return(TRUE)
        }else{
        data.cleaning.check.log("Column headers are unique", success=FALSE,matrix=table.to.data.frame(headers_table))
        return(FALSE)
      }
    }

# [ ]	Column headers are exactly identical to names listed in the questionnaire (see previous)
# [ ]	Multiple choice questions have one column with the question name as a header, one column for each response option and one column for "other" responses
# [ ]	All columns for a multiple choice question are exactly next to each other, beginning with the one that correspond to no individual answer
# [ ]	All column headers for the multiple choice responses start with the exact question name followed by the response name
# [ ]	The values for multiple choice responses are exclusively "TRUE", "FALSE" or blank.
# [ ]	Column headers are exactly identical to names listed in the data analysis framework
# [ ]	Each column only contains a single datatype (numerical, text, TRUE/FALSE, 0/1). E.g. numerical columns should not contain text in any of the fields
    data.cleaning.check.unique.datatype<-function(data){
      data.cleaning.chec.log("* NOT CHECKED ")
    }

# [ ]	Missing data fields are left blank or replaced by NA. If for data collection other code were introduced (ie.999), replace by blank or NA

    data_validation_all_missing_blank<-function(data){
      illegal.missing.values<-c("no data", "", " ", "  ", "   ", ".", "N/A", "na","NA","n/a","missing data","(blank)")
      illegal.missing.found <-data %>% lapply(function(x){
        found<-illegal.missing %in% x
        names(found)<-illegal.missing.values
      })
      found.none<- illegal.missing.found %>% ul %>% (function(x){!any(x)})
      if(found.none){
        data.cleaning.check.log("Missing data fields are left blank or replaced by NA. If for data collection other code were introduced (ie.999), replace by blank or NA",
        sucess=T)} else if(!found.none){
        data.cleaning.check.log("Missing data fields are left blank or replaced by NA. If for data collection other code were introduced (ie.999), replace by blank or NA",
        sucess=F, comment=paste("found one of:",paste(illegal.missing.values,sep = ", ")))}

    }

# Data Cleaning
# [ ]	Non-respondents must be recorded. Whenever an interview can not be conducted, it should still appear as a row in the data, with only the meta data columns filled (stratum, call status, consent..)
# [ ]	Backup of the unedited raw data preserved
# [ ]	All records have an unique ID or UUID's
# [ ]	No duplicate UUIDs. Duplicate entries are checked and removed.
# [ ]	Unique names for categorical variables. For example a place name should always be spelled exactly the same including spaces and caps

#' Lists duplicates in a dataframe column
#' Returns all numerical values more than three standard deviations from the mean
#' @param data dataframe
#' @param duplicate.column.name name of the column to be searched for duplicate entries
#' @return returns values and counts (as a matrix) of duplicate entries in duplicate.column.name
#' @export
#' @examples

data_validation_find_duplicates<-function(data,duplicate.column.name){
       duplicate <- data[[duplicate.column.name]] %>% duplicated %>% which
       # if none found, return empty matrix in standard format
       if(length(duplicate)==0){
         return(matrix(0,nrow = 0,ncol = 5,dimnames = list(NULL,c("index","value","variable","has_issue","issue_type")))
         )}


       # change to standard format:
       duplicate <- cbind(index=duplicate,value=data[[duplicate.column.name]][duplicate])
       colnames(duplicate) <- c("index","value")
       duplicate<-cbind(duplicate,variable=duplicate.column.name,has_issue=T,issue_type=paste("duplicate in", duplicate.column.name))
       as.data.frame(duplicate)
  }

# [ ]	Spelling of same categories is consistent, including caps and blank spaces.
# [ ]	Data has the same unit in each column
# [ ]	Enumerator interview speed is reasonnable
# [ ]	Enumerator interview location is consistent with samplign target
# [ ]	None of the enumerator with interview consistenly follwing the shortest questionnaire path
# [ ]	Outliers identified, investigated and corrected


data_validation_outliers_normal<-function(data,maximum_standard_deviations=3){
  outliers_normal<-data %>% lapply(outliers.numerical,maximum_standard_deviations=maximum_standard_deviations)
  return(outliers_normal)}

data_validation_outliers_log_normal<-function(data,maximum_standard_deviations=3){
  outliers_log_normal<- data %>% lapply(log.outliers.numerical,maximum_standard_deviations=maximum_standard_deviations)
  return(outliers_log_normal)}

#' Lists outliers in a dataframe
#' Returns all numerical values more than three standard deviations from the mean (assumes normal distribution). If the same test on log transformation of the data produces fewer outliers, it returns those instead.
#' @param data dataframe
#' @param maximum_standard_deviations set the distance from the mean threshold for outliers in number of standard deviations. defaults to 3
#' @return data frame with the outlier values, as well as the row index and the name of the data column in which they appeared
#' @export
#' @examples data %>% data_validation_outliers %>% write.csv("list_of_outliers.csv")
data_validation_outliers <- function(data){
  outliers_normal <- data %>% data_validation_outliers_normal
  outliers_log_normal <- data %>% data_validation_outliers_log_normal

  # for each variable
  outliers <- lapply(names(data), function(x) {
    # stop if none were found in both tests
    if ((nrow(outliers_log_normal[[x]])==0) & (nrow(outliers_normal[[x]])==0)) {
      return(reachR:::empty_issues_table())
    }
    # if fewer log_normal are found, return those:
    else if (nrow(outliers_log_normal[[x]]) < nrow(outliers_normal[[x]])) {
      data.frame(outliers_log_normal[[x]],
                 variable = rep(x,nrow(outliers_log_normal[[x]])),
                 issue_type = rep("log normal distribution outlier",nrow(outliers_log_normal[[x]])))
    }
    # if fewer normal are found, return those:
    else {
      data.frame(outliers_normal[[x]],
                 variable = rep(x,nrow(outliers_normal[[x]])),
                 issue_type = rep("normal distribution outlier",nrow(outliers_normal[[x]])))
    }
    # put all variable's outliers into a single dataframe:
  }) %>% do.call(rbind, .)
  # if for some reas  on there's no outliers, return empty table

  if(nrow(outliers) == 0){return(empty_issues_table())}
  # they're all an issue:
  outliers <- data.frame(outliers, has_issue=T)
  # make sure variable is a character not a factor:
  outliers$variable<-as.character(outliers$variable)
  return(outliers)
  }





#by = NULL *by

#testing function bits
data_validation_inliers <- function(data, variable_1, variable_2){
  model <- lm(variable_1 ~ variable_2)
    if(summary.lm(model)[[adj.r.squared]]> 0.8){
      inliers <- data %>% lapply(cook.outliers(model))
    }
  return(inliers)
}


#calculating inliers function
inliers.numerical <- function(variable_1, variable_2, maximum_standard_deviations=3, ...){
  model <- lm(variable_1 ~ variable_2*...)
  x <- residuals(model)
  residuals_data_only <- hasdata(x)
  inliers_indices_in_data_only<-which(abs(residuals_data_only-mean(residuals_data_only))>maximum_standard_deviations*sd(residuals_data_only))
  inliers_indices_in_original_vector<-residuals_data_only_indices[inliers_indices_in_data_only]
  return(
    cbind(
      index=inliers_indices_in_original_vector,
      value=x[inliers_indices_in_original_vector]))
}

#adding inliers to the list of inliers
data_validation_inliers <- function(masterlist){
    inliers <- masterlist %>% lapply(inliers.numerical,maximum_standard_deviations=maximum_standard_deviations)
    residuals(model) %>% do.call(rbind,.)
    if(length(inliers)==0){
      return(matrix(0,nrow = 0,ncol = 5,dimnames = list(NULL,c("index","value","variable","has_issue","issue_type")))
      )}
  return(inliers)
}

testfun<-function(a,b,...){


  return(paste(...))

}


# [ ]	Inliers identified, investigated and corrected
# [ ]	Inconsistencies between questions found, investigated and corrected
# [ ]	"Other" responses that are similar to existing options are removed and added to choices.
# [ ]	"Other" responses that occur frequently are added to questionaire an the data transfered accordingly
# [ ]	All cleaning log entries are changed in the data
#
# Cleaning log format
# [ ]	Exactly one row for each individual data entry
# [ ]	Headers are exactly: "uuid", "question.name", "old.value","new.value", "comment", "feedback"
# [ ]	"question.name" values are exactly identical to column names in the data sheet
# [ ]	"new.value" column contains exactly and exclusively the new values as they appear in the data
# [ ]	Any other columns can be added to the cleaning log template
#
#
#


#' Subset columns that end in "other" or "autre"
#'
#' @param data data
#' @return data frame subset
#' `NULL` if `return.stratum.populations` is `FALSE` (default)
#' A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
#' @export
#' @examples
select_other_columns<-function(data){
  othernames <- grep("other$|Other$|autre$|Autre$",names(data),value=T)
  data[othernames]
}


empty_issues_table<-function(){
  data.frame("index"=numeric(),
             "value"=numeric(),
             "variable"=character(),
             "has_issue"=logical(),
             "issue_type"=character()
             )
}



#' frequencies of all answers in "other" columns
#'
#' In standard format of all data_validation...() functions
#' @param data data
#' @return data frame with one row per unique "other" response per variable.
#' @export
#' @examples
data_validation_all_others<-function(data){
  data %>% select_other_columns %>%  aggregate_count -> frequency_tables
  if(length(frequency_tables)==0){return(empty_issues_table())}
  frequency_tables %>% melt %>% setcolnames(c("value","count","variable"))  ->others
  others<-others[others$value!="" & others$value!=FALSE & others$value!=TRUE,]
  if(nrow(others)==0){return(empty_issues_table())}

  others[,"value"]<-paste(others[,"value"],"\\\\",others[,"count"],"instance(s)")
  others<-data.frame(index=NA,others[,c("value","variable")],has_issue=NA,issue_type="'other' response. may need recoding.")}


# validate_cleaning_frequency_tables_all<-function(data){
#   freq_tables<- data %l% table %l% table.to.data.frame
#   other.tables<-function(data,write.to.file="./output/other_tables.txt"){
#     othernames <- grep("other$|Other$|autre$|Autre$",names(data),value=T)
#     data[,othernames] %l% table %l% reachR:::table.to.data.frame -> tables
#     lapply(names(tables),function(x){
#       cat(x)
#       tables[[x]] %>% kable %l% cat})
#     sink()
#   }
#
#
# }


    # detecting outliers:
outliers.numerical<-function(x,maximum_standard_deviations=3){
  # IN:
  # x: numerical vector
  # maximum_standard_deviations: integer
  # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.
    x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
    x_data_only<-hasdata(x)
    x_data_only_indices<-hasdata(x,return.index = T)
    outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>maximum_standard_deviations*sd(x_data_only)& length(unique(x_data_only))>10)
    outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
       return(
         cbind(
           index=outliers_indices_in_original_vector,
           value=x[outliers_indices_in_original_vector]))
}

log.outliers.numerical<-function(x,maximum_standard_deviations=3){
  # IN:
  # x: numerical vector
  # maximum_standard_deviations: integer
  # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.
  x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
  x_not_logged<-x
  x <- log(x)
  x_data_only<-hasdata(x)
  x_data_only_indices<-hasdata(x,return.index = T)
  outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>maximum_standard_deviations*sd(x_data_only) & length(unique(x_data_only))>10)
  outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
  return(
    cbind(
      index=outliers_indices_in_original_vector,
      value=x_not_logged[outliers_indices_in_original_vector]))
}


other.tables<-function(data,write.to.file="./output/other_tables.txt"){
  othernames <- grep("other$|Other$|autre$|Autre$",names(data),value=T)
  sink(write.to.file,split = T)
  data[othernames] %l% tb %l% table.to.data.frame %l% kable %l% print
  sink()
}


outlier.hist<-function(x){
  hist(x,col='black',ylab=NA,,xlab = NA,main = NA)
  abline(v=c(mean(x)+(3*sd(x)),mean(test)-(3*sd(x))),col='red',lwd=2,bty='n')
  abline(v=summary(x),col='grey')
}




