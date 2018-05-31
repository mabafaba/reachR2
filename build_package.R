
build<-function(){

rm(list=ls())

detach("package:reachR")
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
getwd()

reachr_files<-paste0("./R/", list.files("./R/"))
sapply(reachr_files,source)

require("roxygen2")
require("devtools")
roxygenize(clean=T)
}


build()
