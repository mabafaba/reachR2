
email.1<-function(file="mail",to=""){
  suppressWarnings(sink())
  sink(paste0(file,".htm"),split = T)
  cat("<HTML><Body>")
  cat(paste("Dear",to,"<br><br>"))
  cat("Thanks a lot for this!\n")
  cat("Could you please check on the outputs of the standard data cleaning checks below?\n")
  cat("<ul>")


}


email.0<-function(file="test"){
  cat("</ul>")
  cat("<br><br>Thanks a lot and all the best!")
  cat("<br><br>")
  cat("Martin")
  cat("</Body></HTML>")
  sink()
}


email.block<-function(title,content=NULL,text=NULL){
  cat("<li>")
  cat(paste0("<h4>",title,":</h4>"))
  if(!is.null(text)){cat(paste("\n",text))}
  if(is.matrix(content)|is.data.frame(content)){
    print(kable(content,format="html"))

  }
  cat("</li>")
}




# email on, off and blocks combined:


email.text<-function(duplicate_uuids,tbs,outliers,to=""){

  email.1(file = "email_auto", to = to)

  if(is.null(duplicate_uuids)){}
  else{
  if(nrow(duplicate_uuids>0)){
    email.block("duplicate_uuids",
                 duplicate_uuids)
  }
  }
  if(nrow(tbs>0)){
    email.block("'Other' recoding:"
                ,tbs
                ,"'other' responses that may need recoding into categories, and could maybe be added to the questionnaire choices:")
    }
  if(nrow(duplicate_uuids)){
    email.block("Outliers",
                outliers,
                "these may or may not be correct data. Depending on the variable, it should be cross checked if they are realistic, and if possible checked back with field teams. (some variables such as geographic coordinates or ID's can of course be ignored")
    }


  email.0()


}


# basic analysis plus email:


email<-function(data,to=""){

  # duplicate uuids
  ####################
  uuid_col<-"X_uuid"
  if(!is.null(data[[uuid_col]])){
    duplicate_uuids<- data[[uuid_col]] %>% tb %>% sort %>% .[which(.!=1)] %>% nm %>% as.mx
    colnames(duplicate_uuids)<-"duplicate uuids"
  }else{
    duplicate_uuids<-matrix(0,0,0)
  }

  # outliers
  ####################
    lapply(data,function(x){
    x<-as.numeric(as.character(x))
    outliers<-which(abs((x-mean(x)))>3*sd(x))
    return(cbind("value"=x[outliers],"data row"=outliers))

  }) %>% .[sapply(.,length)>0] ->outliers

  lapply(seq_along(outliers),function(x){cbind("variable"=names(outliers)[x],outliers[[x]])}) %l%
    (function(x){x[order(as.numeric(x[,"value"]),decreasing = T),]}) %>%
    do.call(rbind,.) ->
    outliers



  # 'other' tables
  ####################
  if((data %>% sf("other") %>% nm %>% ln)==0){tbs<-matrix(0,0,0)} else{
  data %>%
    sf("other") %l%
    tb %l%
    table.to.data.frame ->
    tbs

  tbs[tbs %>% is.na %>% not] %>% lapply(function(x){
    colnames(x)<-c("answer","count")
    x$answer<-as.character(x$answer)
    if(x[1,"answer"]=="") { return(x[-1,])}
    x
  }) ->
    tbs

  lapply(seq_along(tbs),function(x){cbind("variable"=names(tbs)[x],tbs[[x]])}) %l%
    (function(x){x[order(as.numeric(x[,"count"]),decreasing = T),]}) %>%
    do.call(rbind,.) ->
    tbs

  # compose email
  ####################
  email.text(duplicate_uuids,tbs,outliers,to=to)
}
}












