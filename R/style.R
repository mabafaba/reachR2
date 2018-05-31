

reach_theme<-function(){
  return(
    theme(panel.grid.major = element_line(colour=reach_style_color_lightgrey(3), size = (0.3))
         # ,panel.grid.minor = element_line(colour=reach_style_color_lightgrey(3), size = (0.3))
  ))
}




#' points with horizontal error bars
#'
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
reach_style_points_by_category_with_error_bars<-function(  group, value, value.low, value.high,xlab="",ylab=""){

  df <- data.frame(
    group =group,
    value = value,
    value.low = value.low,
    value.high = value.high
  )

  df<-df[rev(order(df$value)),]
  p <- ggplot(df, aes(value,factor(1:length(value))),color="black")

  p + geom_point() +
    geom_errorbarh(aes(xmax = value.high, xmin = value.low),height=0)+ theme_tufte(base_family = "")+
    xlab(xlab)+
    ylab(ylab)+
    scale_y_discrete(breaks=1:length(value),
                     labels=group)+reach_theme()
}








###### UNDER CONSTRUCTION
########################
# reach_style_line_with.ribbon<-function(x,y,y.low,y.high){
#   if(class(x)=="Date"){x<-format(x,"%B")}
#   df<-data.frame(x,y,y.low,y.high)
#
#
#
#   list(geom_ribbon(aes(x=x,y=y,ymin = y.low, ymax = y.high),
#                     data=df,
#                     fill = reach_style_color_lightgrey(3,transparent = T)),
#         geom_line(aes(y = y),data=df),
#         theme_tufte(base_family = ""),
#         geom_point(aes(x,y),data=df,fill=reach_style_color_darkgrey(1),
#           colour=reach_style_color_darkgrey(3),
#           pch=21
#           ,size=2)
#         )
# }


###### UNDER CONSTRUCTION
#
#
#
# reach_style_line_with.band2<-function(x,y,y.low,y.high,xlab,ylab){
#   df<-data.frame(x,y,y.low,y.high)
#   return(geom_ribbon(aes(ymin = y.low, ymax = y.high), fill = reach_style_color_lightgrey(3,transparent = T)) +
#            geom_line(aes(y = y))+ theme_tufte(base_family = "")+geom_point(aes(x,y),fill=reach_style_color_darkgrey(1),
#                                                                            colour=reach_style_color_darkgrey(3),
#                                                                            pch=21
#                                                                            ,size=2))
# }
#
# reach_style_line_with.band(x,y,y.low,y.high,'x','y')+reach_style_line_with.band2(x,y,y.low,y.high,'x','y')
#
#
#
# theme_reach<-function(x){
#
# }


#######################
# COLOUR RAMPS
#######################
# INDIVIDUAL COLORS
reach_style_color_generic.triplet<-function(r,g,b,lightness=1,transparent=F){
  if(!transparent){
  if      (lightness==1){rgba(r,g,b,1)}
  else if (lightness==2){rgba(r,g,b,0.5)}
  else if (lightness==3){rgba(r,g,b,0.3)}
  }else{
    if      (lightness==1){rgb(r,g,b,1)}
    else if (lightness==2){rgb(r,g,b,0.5)}
    else if (lightness==3){rgb(r,g,b,0.3)}
  }
    }

reach_style_color_red<-function(lightness=1,transparent=F){
  reach_style_color_generic.triplet(238/255,88/255,89/255,lightness,transparent)

}

reach_style_color_darkgrey<-function(lightness=1,transparent=F){
  reach_style_color_generic.triplet(88/255,88/255,90/255,lightness,transparent)
}

reach_style_color_lightgrey<-function(lightness=1,transparent=F){
  reach_style_color_generic.triplet(209/255,211/255,212/255,lightness,transparent)
}

reach_style_color_beige<-function(lightness=1,transparent=F){
  reach_style_color_generic.triplet(210/255,203/255,184/255,0.3,lightness,transparent)
}
#######################
# COLOUR TRIPLES
#######################
reach_style_color_reds<-function(transparent=F){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_red,transparent=transparent)
}

reach_style_color_darkgreys<-function(transparent=F){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_darkgrey,transparent=transparent)
}

reach_style_color_lightgreys<-function(transparent=F){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_lightgrey,transparent=transparent)
}

reach_style_color_beiges<-function(transparent=F){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_beige,transparent=transparent)
}


# COLOUR RAMPS
reach_style_color_rainbow<-function(n,highlight=0){
  cols<-c(reach_style_color_darkgreys(),
          reach_style_color_reds()[2:3],
          reach_style_color_lightgreys(),
          reach_style_color_reds()[1])



  if(n>9){stop("I don't do more than 9 colours, that's madness. (reach_style_color_gradient() might help out though.)")}



  reach_style_color_reds()

  # put red in highlight spto
  if(highlight!=0){
    cols[c(highlight,9)]<-cols[c(9,highlight)]
  }
  # cols<-  as.vector(rbind(reach_style_color_reds(),reach_style_color_darkgreys(),reach_style_color_beiges(),reach_style_color_lightgreys()))
  # rep(cols,ceiling(n/12))[1:n]

  return(cols[1:n])

}

reach_style_color_gradient<-function(n){

  colorRampPalette(c(reach_style_color_darkgreys(),rev(reach_style_color_reds())))(n)
}

# GGPLOT GRADIENTS
scale_fill_reach <- function(color=NULL){
  if(is.null(color)){
    structure(list(
      scale_fill_manual(values= reach_style_color_rainbow(12))
    ))
  }else{
    structure(list(
      scale_fill_manual(values= get(paste0('reach_style_color_',color,'s'))())

    ))}
}

scale_color_discrete_reach <- function(color='red'){

  structure(list(
    scale_color_manual(values= get(paste0('reach_style_color_',color,'s'))())
  ))
}

scale_color_continuous_reachn <- function(color='red'){

  structure(list(
    scale_color_gradientn(colours = get(paste0('reach_style_color_',color,'s'))())
  ))
}

rgba<-function(r,g,b,a,hex=T){
  colvalues<-  list(
    red=(1 - a) * 1 + a * r,
    green=(1 - a) * 1 + a * g,
    blue=(1 - a) * 1 + a * b)

  if(hex){return(do.call(rgb,colvalues))}
  else{return(unlist(colvalues))}

}

#######################
# EXAMPLES
#######################
# example data
# data<-data.frame('step'= 1:50, 'a' = runif(50),'b' = rnorm(50,mean = 10),'c' = rexp(50),'d' = rexp(50)
#                  ,'e' = rexp(50)
#                  ,'f' = rexp(50)
#                  ,'g' = rexp(50)
#                  ,'h' = rexp(50)
# )
#
#
#
#   reach_style_line__stacked(data,x.col.name = 'step',
#                           y.col.names = letters[1:5],'time','price')
#


  # reach_style_line_with.band(testdate,(1:9)+runif(9),(1:9)-runif(9),(1:9)+runif(9),xlab="month",ylab="price")


