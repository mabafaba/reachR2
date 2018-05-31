write.csv.untidy <- function(untidy.list,filename) {
  # cbind values and names(if any)
  untidy.list<-lapply(untidy.list,as.matrix)
  untidy.list<-colnames.add.parentname(untidy.list)
  lapply(seq_along(untidy.list),function(var){
    # if.. is.vector(untidy.list[[var]]) &
    if( !is.null(names(untidy.list[[var]]))){
      m<-cbind(names(untidy.list[[var]]),untidy.list[[var]])
      colnames(m)<-c(paste(names(untidy.list)[var],"name"),paste(names(untidy.list)[var],"value"))[1:ncol(m)]
      return(m)
    }else{
      return(
        untidy.list[[var]]

      )
    }
  })  %>% l(untidy.flatten.matrix.names) %>%
    # fill with NA to have same length
    do.call(untidy.cbind,.) %>%
    write.csv(file = filename,na = "")
}

untidy.cbind<-function(...){
  args<-list(...)
  print(names(args))
  # make sure all args are matrices
  args<-args %>% l(as.matrix)
  # get maximum rows
  max.nrow<-lapply(args,nrow) %>% unlist %>% max
  # set all matrices to have the same row length
  m<-args %>% l(matrix.nrow.set,nrow=max.nrow) %>%
    # cbind to wide matrix
    do.call(cbind,.)
  return(m)
}

untidy.flatten.matrix.names<-function(m){
  m<-cbind(rownames(m),m)
  m<-rbind(colnames(m),m)
  unname(m)
}

matrix.nrow.set<-function(m,nrow){
  return(apply(m,2,function(x){length(x)<-nrow;x}))
}

wtd.table.fraction<-function(...,ignore.missing.data=T){
  args<-list(...)
  if(ignore.missing.data){args$na.rm<-T}
  if(!ignore.missing.data){args$na.rm<-F}

  wt<-do.call(wtd.table,args)
  if(is.matrix.table(wt)){
  wt<-apply(wt,2,function(x){
    x/sum(x)
    }
  )}else{
    wt<-wt/sum(wt)
    }
  return(wt)
}


list.of.df.names.add.parentname<-function(alist){
  x<-lapply(seq_along(x),function(index){
    df<-as.data.frame(x[[index]])
    names(df)<-paste(names(x)[index],names(df))
    return(df)
  })
}

rownames.as.variable<-function(m,newvariable.name="rownames"){
  if(!is.null(rownames(m))){
    m<-as.data.frame(m)
    m$rownames<-rownames(m)
  }
  return(m)
}



table.to.data.frame<-function(x){
  # if not a table:
  if(!is.table(x)){return(NA)}
  # if empty table:
  if(nrow(x)==0){return(NA)}
  # rows and cols:
  if(!is.na(ncol(x))){
    return(as.data.frame.matrix(x,row.names=rownames(x)))
  }
  # no cols:
  return(as.data.frame(x))
}

list.of.tables.write.csv<-function(x,write.to.file){
  x %>%   l(table.to.data.frame) %>%   l(rownames.as.variable) %>% untidy.cbind.list %>%
    write.csv(paste(runif(1),"atest.csv"),na="")
}

untidy.cbind.list<-function(x){
  x<-lapply(seq_along(x),function(index){
    df<-as.data.frame(x[[index]])
    names(df)<-paste(names(x)[index],names(df))
    return(df)
  })
x<-do.call(untidy.cbind,x)
return(x)
}

is.matrix.table<-function(x){
# determins if a table (output from table() or wtd.table() usually) has multiple rows.
    !is.na(ncol(x))
}

colnames.add.parentname<-function(x){
    # for all list items..
    xnewcolnms<-lapply(seq_along(x),function(i){

    # if the list item does not have columns, make it so:
    if(length(dim(x[[i]])!=2)){
      x[[i]]<-cbind(x[[i]])
    }
    # check again that the item is 2 dimensional..
    if((length(dim(x[[i]])))==2 ){
            # concat the list item parent name and the column names of the item..
            colnames(x[[i]])<-paste(rep(names(x)[i],ncol(x[[i]])),colnames(x[[i]]))
            return(x[[i]])

    }

      return(x)
  })
    # return the whole list
  return(xnewcolnms)
}


#' has data
#' removes NA, empty strings and non-finite values from a vector
#' @param x vector
#'
#' @param return.index if true, returns indices of the vector that have valid data. Defaults to FALSE.
#' @param
#' @param
#' @return returns the values of the input vector that contain valid data
#' @seealso \code{\link{}}
#' @export
#' @examples
#' example1 code
#' example1 code
hasdata<-function(x,return.index=F){
  # in: vector of any class
  # out: the in vector without NULL,NA, or ""
  index<-which(!is.null(x) & !is.na(x) & x !="" & !is.infinite(x))
  value<-x[which(!is.null(x) & !is.na(x) & x !="" & !is.infinite(x))]
  if(return.index){
    return(index)
  }
  return(value)
  }

# subset data.frame by partial column names
select.fuzzy<-function(data,searchterms){
  # IN:
  # data: data.frame
  # searchterms: vector of strings
  # OUT:
  # subset of data.frame, only columns that contain one of the searchterms in their name.
  # NULL if no column matched (issues warning)
  # grep searchterms:
  whichcols<-lapply(searchterms,function(pttrn){
    grep(pttrn,names(data),perl = T)
  }) %>% unlist %>% unique
  # corresponding column names:
  whichnames<-names(data)[whichcols]

  # error handling:
  if(length(whichnames)==0){
    warning("fuzzy select says: no column names contain:"
            ,paste(searchterms,collapse = ",")
            ," ---> returned NULL")
    return(NULL)
  }
  # select column names corresponding to search:
  return(
    dplyr::select(data,whichnames)
  )

}#

# like select.fuzzy but selects all columns who's values contain the search term (only accepts a single search term)
select.fuzzy.value<-function(data,searchterm){

  hastermindata<-lapply(data,function(x){
    length(grep(searchterm,x))>0
  })
  return(select(data,(names(data)[unlist(hastermindata)])))
}


#' Median or Mode
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#' mixed_data <- c(1,2,3,"4","5","a","b")
#' medianORmode(mixed_data,"no median consensus",0.5)
#' medianORmode(mixed_data,"no median consensus",0.9)
medianORmode<-function(x,when.tie=NA,mean.when.fraction.numeric.greater.than=1){
  if(is.numeric.fuzzy(as.character(x),minfrac = mean.when.fraction.numeric.greater.than)){
    return(suppressWarnings(x %>% as.character %>% as.numeric %>% hasdata %>% median))
  }else{
    # warning("tried to take median, but more than half of the available data in this vector was not numeric. Took the Mode instead")
    return(
      x %>% hasdata %>% Mode(when.tie=when.tie)
      )
    }
}

#' Mean or Mode
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#' mixed_data <- c(1,2,3,"4","5","a","b")
#' medianORmode(mixed_data,"no median consensus",0.5)
#' medianORmode(mixed_data,"no median consensus",0.9)
mean_R <- function (x, when.tie = NA) {
  if (is.numeric.fuzzy(as.character(x))) {
    return(suppressWarnings(x %>% as.character %>% as.numeric %>%
                              hasdata %>% mean))}
  else {
    # warning("tried to take median, but more than half of the available data in this vector was not numeric. Took the Mode instead")
    return(x %>% hasdata %>% Mode(when.tie = when.tie))
  }
}



Mode<-function(x,when.tie=NA) {
  ux <- unique(x)
  wm<-which.max(tabulate(match(x, ux)))
  # return when.tie when tie
  if(((table(x) == max(table(x))) %>% which %>% length)>1){return(when.tie)}
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Try to make numeric
is.numeric.fuzzy<-function(x,minfrac=1){
  # are at least minfrac of the values that have data still values after trying to convert them to numbers?
  suppressWarnings(isnum<-(x %>% as.numeric %>% hasdata %>% length)/ (x %>% hasdata %>% length) >=minfrac)
  # this relies on NA's created by as.numeric, which issues a warning we don't want to see
  if(is.na(isnum)){isnum<-FALSE}
  if(is.null(isnum)){isnum<-FALSE}
  return(isnum)
}

percent=function(x){
  x/sum(x)
}

normalise<-function(x){
  (x-min(x))/max(x-min(x))
}

# META
number.of.records.with.data<-function(df,write.to.file=NULL){
  numrec<-data  %>% l(hasdata) %>% l(ln) %>% ul %>% .[. >0 & . < 100] %>% sort
  if(!is.null(write.to.file)){csvw(numrec,write.to.file)}
}

replace.values.in.vector<-function(v,was,becomes){

  # don't do anything if there's nothing to do:
  if(is.null(v)){return(v)}
  if(is.null(was)){return(v)}

    # check input
  insure.same.length(was,becomes)
  insure.no.duplicates(was)
  # let's not mix up factor levels/indices etc.. change to string:
  was.factor <- is.factor(v)
  if(was.factor){v<-as.character(v)}

  # replace all except where was is NA
  replacements<-becomes[match(v, was)]
  v[!is.na(replacements)]<-replacements[!is.na(replacements)]
  # replace all where "was" is NA
  v[is.na(v)]<-becomes[is.na(was)]

  # replace all where becomes is NA
  # which 'v' matches the 'was' that should become NA? make them NA
  v[which(v==was[is.na(becomes)])]<-NA

    # of it was a factor, preserve data type:
  if(was.factor){v<-as.factor(v)}
  return(v)
}


# like dplyr "select" but matches anything that contains any of the searchterms
select.fuzzy<-function(data,searchterms){
  # grep searchterms:
  whichcols<-lapply(searchterms,function(pttrn){
    grep(pttrn,names(data),perl = T)
  }) %>% unlist %>% unique
  # corresponding column names:
  whichnames<-names(data)[whichcols]

  # error handling:
  if(length(whichnames)==0){
    warning("fuzzy select says: no column names contain:"
            ,paste(searchterms,collapse = ",")
            ," ---> NULL returned")
    return(NULL)
  }
  # select column names corresponding to search:
  return(
    dplyr::select(data,whichnames)
  )

}


setcolnames<-function(x,colnames){
  colnames(x)<-colnames
  return(x)
}


factors2strings<-function(x){
  # remember whether x was a df
  wasdf<-is.data.frame(x)

  # if it's a list, lapply this function recursively until hit something that's not a list
  if(wasdf | is.list(x)){
    # if x was a df
    unfactored<-  lapply(x,factors2strings)
    if((df %>% lapply(length) %>% unlist) %>% table %>% length==1){unfactored<-as.data.frame(unfactored,stringsAsFactors=F)}
    return(unfactored %>% as.data.frame)
  }
  # convert to character if is.factor
  # do nothing if not a factor
  if(is.factor(x)){return(as.character(x))}else{return(x)}

}


csv.colnames<-function(file){
  read.csv.auto.sep(file) %>% names
}

is.in.csv.colnames<-function(find.header, in.csv){
  find.header %in% csv.colnames(in.csv)
}

