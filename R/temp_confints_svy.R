# rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# require("ggplot2")
# require("survey")
# require("questionr")
# require("reachR")
# require("dplyr")
#
# setwd("D:/projects/IRQ pdm cs confints 2/")
# require("reachR")
# # load data
#   data<-read.csv("./data/August_July_combined_clean_final.csv")
#   data<-data[data$screeners.verification.remember_date_dist=="remember",]
#
#   data<-lapply(data,function(x){
#     x[which(x=="N/A")]<-NA
#     x
#       }) %>% as.data.frame(stringsAsFactors=F)
#
# # fix sampling frame spelling
#   sf<-read.csv("./data/sf.csv")
#   sf$stratum<-tolower(sf$stratum)
#   sf$stratum<-gsub("july-","",sf$stratum)
#   sf$stratum<-gsub("august-","",sf$stratum)
#   # sf %>% aggregate.data.frame(list(sf$stratum),FUN = sumORmode) ->sf
#   # sf %>% write.csv("./data/sf.csv")
#
# # make stratum name column that maches sampling frame names
#   strata<-paste("MPCA",data$overview.payments_received,data$overview.governorate_office,sep="-") %>%
#     tolower %>% gsub("_payment","",.) %>% gsub("mpca-","mpca ",.) %>% gsub("basrah$","basra",.) %>%
#     gsub("thi_qar","thi qar",.)
#
#   data$stratum<-strata
#
#   # if in the output of this line anything in the left column is not NA,the stratum is missing from the sampling frame:
#   reachR:::untidy.cbind.list(list((strata %>% table %>% names)[!(strata %>% table %>% names) %in% (sf$stratum %>% table %>% names)],
#                            sf$stratum[sf$stratum!=""]))
#
#
# pops<-load_samplingframe("./data/sf.csv",return.stratum.populations = T)
#
#
#
#
#
#
# pops<-load_samplingframe("./data/sf.csv",return.stratum.populations = T)
#
# aggregate_percent_weighted_confints(data,pops = pops,strata = "stratum")
#
#
#
# aggregate_percent_weighted_confints<-function(data,pops,strata=NULL){
#   # get counts and percent
#   data %>% aggregate_count(split.by = strata) -> strata_answer_count
#   data %>% aggregate_percent_weighted(split.by=strata,ignore.missing.data = T) -> strata_answer_percent
#
#   strata_answer_count %>% melt ->meltedcount
#   strata_answer_percent %>% melt -> meltedpercent
#   meltedcount %>% head
#   perc_and_count<-data.frame(response=meltedcount$Var1,
#                              variable=meltedcount$L1,
#                              stratum=meltedcount$Var2,
#                              count=meltedcount$value,
#                              percent=meltedpercent$value)
#
#   perc_and_count$population<-pops[perc_and_count$stratum]
#   perc_and_count<-perc_and_count[which(perc_and_count$count>0),]
#
#   confints<-lapply(1:nrow(perc_and_count),function(x){
#     cfi<-stratified_confidence_interval(sample = perc_and_count[x,"count"],
#                                         population = perc_and_count[x,"population"],
#                                         p=perc_and_count[x,"percent"])
#     cfi
#   })
#
#   confints %>% do.call(rbind,.) ->confints
#   confints  %>% cbind(lower=.[,1],upper=.[,2],perc_and_count) -> reslts
#   reslts_filtered<-reslts[reslts[,"count"]>1,]
#   reslts_filtered[,"confidence range"]<-reslts_filtered$upper-reslts_filtered$lower
#   return(reslts_filtered)
#
# }
#
#
#
# aggregate_mean_weighted_confints<-function(data,pops,strata=NULL){
#   # get counts and percent
#   data %>% aggregate_count(split.by = strata) -> strata_answer_count
#
#   data %>% aggregate_percent_weighted(split.by=strata,ignore.missing.data = T) -> strata_answer_percent
#
#   strata_answer_count %>% melt ->meltedcount
#   strata_answer_percent %>% melt -> meltedpercent
#   meltedcount %>% head
#   perc_and_count<-data.frame(response=meltedcount$Var1,
#                              variable=meltedcount$L1,
#                              stratum=meltedcount$Var2,
#                              count=meltedcount$value,
#                              percent=meltedpercent$value)
#
#   perc_and_count$population<-pops[perc_and_count$stratum]
#   perc_and_count<-perc_and_count[which(perc_and_count$count>0),]
#
#   confints<-lapply(1:nrow(perc_and_count),function(x){
#     cfi<-stratified_confidence_interval(sample = perc_and_count[x,"count"],
#                                         population = perc_and_count[x,"population"],
#                                         p=perc_and_count[x,"percent"])
#     cfi
#   })
#
#   confints %>% do.call(rbind,.) ->confints
#   confints  %>% cbind(lower=.[,1],upper=.[,2],perc_and_count) -> reslts
#   reslts_filtered<-reslts[reslts[,"count"]>1,]
#   reslts_filtered[,"confidence range"]<-reslts_filtered$upper-reslts_filtered$lower
#   return(reslts_filtered)
#
# }
#
#
#
#
#
#
#
