testdata<-function(messy=T,predictable=F,n){
  if(predictable){messy=F;warning("messy turned off when predictable=T")}
  testdata<-data.frame(
    "governorate"=sample(LETTERS[1:3],1000,T),
    "district"=sample(letters,1000,T),
    "nums"=c(runif(999),10000),
    "factors"= as.factor(sample(c("aaa","bbb","ccc"),1000,T)),
    "ints"<-sample(1:20,1000,T),
    "unique"<-paste("a",1:1000),
    stringsAsFactors = F
  )


  if(predictable){
    testdata<-data.frame(
      "governorate"=rep(LETTERS[1:2],10),
      "district"=(letters[1:20]),
      "nums"=c(1:20)+0.5,
      "factors"= as.factor(c("aaa","bbb")),
      "ints"<-c(1:20),
      "unique"<-paste("a",1:20),
      stringsAsFactors = F
    )
  }



  if(messy){
    testdata<-lapply(testdata,function(x){
      x[sample(c(1:length(x)),length(x)/100)]<-NA
      x[sample(c(1:length(x)),length(x)/100)]<-Inf
      x[sample(c(1:length(x)),length(x)/100)]<- -Inf
      x[sample(c(1:length(x)),length(x)/100)]<-0.0001
      x[sample(c(1:length(x)),length(x)/100)]<- -1
      if(!is.numeric(x)){  x[sample(c(1:length(x)),length(x)/100)]<-"a"}
      if(!is.numeric(x)){x[sample(c(1:length(x)),length(x)/100)]<- "NA"}
      if(!is.numeric(x)){x[sample(c(1:length(x)),length(x)/100)]<- "not sure"}
      if(!is.numeric(x)){x[sample(c(1:length(x)),length(x)/100)]<- ""}
      return(x)
    }) %>% as.data.frame

  }

  if(!is.null(n)){
    if(n>nrow(testdata)){n<-nrow(testdata)}
    return(testdata[1:n,])
  }

return(testdata)

}


all.tests<-function(){



# aggregate_mode

testdata(predictable=T,n=5) %>% aggregate_mode(aggregate.by = "governorate")


data.frame(rbind(c("A",NA,NA,"aaa",NA,"a",NA),
                c("B",NA,"bbb",NA,"a",NA)))

}
