




replace.values<-function(df,was,becomes,in.columns=NULL){
  if(is.null(was)){return(df)}
  insure.same.length(was,becomes)
  insure.no.duplicates(was)

  # if no in.columns provided, apply to all columns:
    if(is.null(in.columns)){in.columns<-names(df)}
                       else{insure.string.is.column.header(df,in.columns)}
  # replace values:
  df<-as.data.frame(df,stringsAsFactors=FALSE)
  df[,in.columns] %>% lapply(replace.values.in.vector,was,becomes)->df[,in.columns]
  # lapply(in.columns,function(col){
     # replace.values.in.vector(df[[col]],was,becomes)}) -> df

     # df[in.columns] %>% lapply(replace.values.in.vector,was=was,becomes=becomes) -> df[in.columns]
    return(df)
    }





