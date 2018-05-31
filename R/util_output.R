write.csv.tables<-function(list.of.tables,write.to.file){
  list.of.tables %>% l(melt.with.names) %>% do.call(rbind,.) %>% write.csv(write.to.file1)
}


output.csv<-function(df,file){
  write.csv.untidy(df,file)
}



melt.with.names<-function(x){
  if(is.matrix(x)){
    x %>% rownames.as.variable %>% melt(.,id.vars="rownames") %>% return
  }
  x
}


output.csv
