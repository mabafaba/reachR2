l<-lapply
ln <- length
s<-dplyr::select
sf<-select.fuzzy
sfv<-select.fuzzy.value

tb<-table
w<-which
ul<-unlist
n<-length
csvr<-read.csv
csvw<-write.csv
p<-paste
p0<-paste0
nm<-names
cnm<-colnames
rnm<-rownames
not<- function(x){return(!x)}
eq<-function(x,y){return(x==y)}

rm.all<-function(){rm(list=ls())}
as.mx<-as.matrix
uq<-unique
swd<-function(x=NULL){
  if(is.null(x)){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    print(getwd())
  }else{
    setwd(x)
  }
}

'%infix%'<- function(x,y){
  print('infix')
  return(x+y)
}


#filter vector

'%remove%' <-function(x,y){}

fv<-function(x,y){
  
}



'%l%'<-function(x,fun,...){
  lapply(x,fun)
}






