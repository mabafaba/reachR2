#' barchart
#'
#' @param group
#' @param percent
#' @param error_min
#' @return error_max
#' @return a ggplot object. you can add/overwrite ggplot layers with the standard ggplot "+" operator
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
reach_style_barchart<-function(group,percent,error_min=NULL,error_max=NULL,horizontal=T){
  require('extrafont')
  require('ggplot2')
  require("ggthemes")
  require("dplyr")
  require('grid')

  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
  df<-data.frame(group=group,percent=percent,ymin=error_min,ymax=error_max,test = test)
  colnames(df)<-c('group','percent','ymin','ymax')
  theplot<-

    ggplot(df)+
    geom_bar(      aes(y=1
                       ,x=group),
                   stat='identity',
                   fill=reach_style_color_lightgrey()) +

    geom_bar(      aes(y=percent
                       ,x=group),
                   stat='identity',
                   fill=reach_style_color_red()) +

    geom_errorbar( aes(x=group,
                       y=percent,
                       ymin=ymin,
                       ymax=ymax),
                   stat='identity',
                   width=0) +

    geom_text(    aes(x=group,
                      y=(1),
                      label=paste0("  ", round(percent*100),"%"," ",group))
                  ,size=4,
                  family="Arial Narrow",
                  col='#000000',
                  hjust=0,
                  vjust=0.5
    )

    theme_tufte()+reachR:::reach_theme() +

    theme(
      text=element_text(family="Arial Narrow"),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.margin = unit(x = c(0,0,0,0),'null')
    ) +

    scale_y_continuous(labels = percent_formats,limits=c(0,3))

  if(horizontal){
    theplot<-theplot+coord_flip()}

  return(theplot)
}




#' Stacked line graph
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_line__stacked<-function(df,x.col.name,y.col.names,xlab,ylab,labels=NULL,highlight=NULL,write.to.file=NULL){
  # insure.string.is.column.header(df,x.col.name)
  # insure.string.is.column.header(df,y.col.names)
  # if(class(df[,x.col.name])=="Date"){df[,x.col.name]<-format(df[,x.col.name],"%B")}
  df_x<-df[,x.col.name]
  df_y<-df[,y.col.names]
  df_y<-df_y[,order(colSums(df_y))]
  # if(!is.null(highlight)){
  #   if(!(highlight %in% y.col.names)){warning('highlight column name not in y.col.names; highlight ignored.')
  #     highlight<-NULL}
  #
  # }else{
  #   df_y_cols<-reach_style_color_gradient(length(y.col.names))
  # }
  df_y_cols<-reach_style_color_gradient(length(y.col.names))
  df<-cbind(df_x,df_y)

  names(df)<-c(x.col.name,y.col.names)
  df<-melt(df,id.vars = x.col.name)
  ggplot( df,
          aes(
            x = df[[x.col.name]],
            y = value,
            fill = gsub("\\."," ",variable))) +
    geom_area(position = 'stack')+ theme_tufte(base_family = "")+
    xlab(xlab) +
    ylab(ylab)+scale_fill_manual(values = df_y_cols,name='')+reach_theme()



}






# RIBBON LINE GRAPH

#' Line graph with confidence band
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_line_with.band<-function(x,y,y.low,y.high,xlab="",ylab=""){
  df<-data.frame(x,y,y.low,y.high)
  # if(class(x)=="Date"){x<-format(x,"%B")}
  return(ggplot(df, aes(x,y)) + geom_ribbon(aes(ymin = y.low, ymax = y.high), fill = reach_style_color_lightgrey(3,transparent = F)) +
           geom_line(aes(y = y))+ theme_tufte(base_family = "")+geom_point(aes(x,y),fill=reach_style_color_darkgrey(1,transparent = F),
                                                                           colour=reach_style_color_darkgrey(1,transparent = F),
                                                                           pch=21
                                                                           ,size=2)+
           xlab(xlab) +
           ylab(ylab))+reach_theme()
}
