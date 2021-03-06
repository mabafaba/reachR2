
#' load_questionnaire
#'
#' @param questions.file file name of a csv file containing the kobo form's question sheet
#' @param choices.file file name of a csv file containing the kobo form's choices sheet
#' @return list of questions and choices, sorted to match. to data columns.
#' @seealso \code{\link{load_data()}, \link{load_samplingframe}}
#' @export
#' @examples
#'
load_questionnaire<-function(data,
                             questions.file,
                             choices.file,
                             choices.label.column.to.use="label..English"){

    # generic function to replace values in a vector based on a lookup table
    replace_with_lookup_table<-function(x,y){
      x2 <- y[match(x, y[,1]),2]
      dim(x2) <- dim(x)
      x2
    }

      # load files
      questions <- read.csv.auto.sep(questions.file,stringsAsFactors = F, header = T)
      choices <- read.csv.auto.sep(choices.file,stringsAsFactors = F, header = T)
      data <- read.csv.auto.sep(data, stringsAsFactors = F, header = T)


      # harmonise data column references
      names(questions) <- reachR:::to_alphanumeric_lowercase(names(questions))
      names(choices) <- reachR:::to_alphanumeric_lowercase(names(choices))
      names(data) <- reachR:::to_alphanumeric_lowercase(names(data))
      choices.label.column.to.use <- reachR:::to_alphanumeric_lowercase(choices.label.column.to.use)

      # sanitise
      names(questions)

      reachR:::insure.string.is.column.header(questions, "type")
      reachR:::insure.string.is.column.header(questions, "name")
      reachR:::insure.string.is.column.header(choices, choices.label.column.to.use)
      reachR:::insure.string.is.column.header(choices, "list.name")

      questions$name <- reachR:::to_alphanumeric_lowercase(questions$name)
      begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), questions$type, ignore.case = T)
      end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), questions$type, ignore.case = T)
      number_of_questions <- (length(questions$name) - length(begin_gr) - length(end_gr))

      # get data column names
      data_colnames<-names(data); data_colnames

      # this changes the questionnaire questions and choices to fit the data columns,
      # with empty entries for data columns that don't appear in the questionnaire.
      if((sum(!is.na(match(data_colnames, questions$name)))/number_of_questions) < 0.3) {
        stop("The question names and data column names don't seem to match. please make sure the two columns are harmonized")
      }

      questions <- questions[match(data_colnames, questions$name),]

        choices_per_data_column<-questions$type %>% as.character %>% strsplit(" ") %>% lapply(unlist)%>% lapply(function(x){

        x %>% lapply(function(y){
        grep(y,choices$list.name,value=F)
      }
      ) %>% unlist
    }) %>% lapply(hasdata) %>% lapply(function(x){
      choices[x,]
    })
    names(choices_per_data_column)<- data_colnames


    # make functions that need questionnaire

    assignInNamespace(x = "question_get_choice_labels_internal", ns="reachR", value = function(responses,variable.name){

      labels<-replace_with_lookup_table(
        responses,
        # MAKE LABEL COLUMN A PARAMETER!!!
        cbind(as.character(choices_per_data_column[[variable.name]]$name),as.character(choices_per_data_column[[variable.name]]$label..datamerge))
      )

      # fix those that were not found to go back to original NA
      labels[is.na(labels)]<-responses[is.na(labels)]
      labels

    })

    assignInNamespace(x = "question_is_numeric_internal", ns="reachR", value =function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(qid)==0){return(FALSE)}
      if(length(c(grep("integer",questions$type[qid]),grep("decimal", questions$typep[qid])))>0){return(TRUE)}
      return(FALSE)
    })


    assignInNamespace(x = "question_is_select_one_internal", ns="reachR", value =function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_one",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    })

    assignInNamespace(x = "question_is_select_multiple_internal", ns="reachR", value = function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_multiple",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    })

    assignInNamespace(x = "question_is_categorical_internal", ns="reachR", value = function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      return(question_is_select_one(question.name) | question_is_select_multiple(question.name))
    })

    message("you can now use \n 'get_choice_labels():'change answers to their labels \n question_is_categorical() \n question_is_categorical()\n question_is_select_one() \n question_is_select_multiple()")
    return(list(questions=questions,choices=choices,choices_per_variable=choices_per_data_column))


}


    question_get_choice_labels_internal<-function(responses,variable.name){
      stop("you must successfully run load_questionnaire() first")

    }

    question_is_numeric_internal<-function(question.name){
      stop("you must successfully run load_questionnaire() first")
    }



    question_is_select_one_internal<-function(question.name){
      stop("you must successfully run load_questionnaire() first")

    }
    question_is_select_multiple_internal<-function(question.name){
      stop("you must successfully run load_questionnaire() first")

    }


    question_is_categorical_internal<-function(question.name){
      stop("you must successfully run load_questionnaire() first")
    }


#' question_get_choice_labels
#'
#' @param responses a vector of responses
#' @param variable.name the column name of the corresponding variable
#' @return vector of labels corresponding to the responses vector. If no corresponding label is found, return the original value
#' @seealso
#' @export
#' @examples
#'
question_get_choice_labels<-function(responses,variable.name){
  question_get_choice_labels_internal(responses,variable.name)
}

#' question_is_numeric
#'
#' @param question.name the name of a data column header
#' @return TRUE if the question is of type integer or decimal
#' @seealso
#' @export
#' @examples
#'
question_is_numeric<-function(question.name){
  question_is_numeric_internal(question.name)
  }


#' question_is_select_one
#'
#' @param question.name the name of a data column header
#' @return TRUE if the question is of type select_one
#' @seealso
#' @export
#' @examples
#'
question_is_select_one<-function(question.name){
  question_is_select_one_internal(question.name)
}

#' question_is_select_multiple
#'
#' @param question.name the name of a data column header
#' @return TRUE if the question is of type select_multiple
#' @seealso
#' @export
#' @examples
#'
question_is_select_multiple<-function(question.name){
  question_is_select_multiple_internal(question.name)
}


#' question_is_categorical
#'
#' @param question.name the name of a data column header
#' @return TRUE if the question is of type select_one or select_multiple
#' @seealso
#' @export
#' @examples
#'
question_is_categorical<-function(question.name){
  question_is_categorical_internal(question.name)
}

#' variable_type
#'
#' @param variables the vector or value for which the type should be determined
#' @return a vector or value with variable types
#' @seealso
#' @export
#' @examples
#'
variable_type <- function(variables){
    variable_types <- as.vector(sapply(variables, function(x){
      if(question_is_categorical(x)){return("categorical")}
      if(question_is_numeric(x)){return("numeric")}
      return("This variable is neither numeric nor categorical")})
    )
    return(variable_types)
    }

