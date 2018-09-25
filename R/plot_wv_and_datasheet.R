# Copyright (C) 2017 Philipp Clausen, Justin Lee, Stephane Guerrier, Roberto Molinari
#
# This file is part of gui4gmwm R Methods Package
#
# The `gui4gmwm` R package is free software: you can redistribute it and/or modify
# it under the terms of the CC BY-NC-SA 4.0 License.
#
# The `gui4gmwm` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' @title Plot WV with datasheet characteristics
#' @description Plot empirical WV of a sensors together the empirical WV implied by
#' the datasheet of the manufacture
#' @author Stephane Guerrier
#' @param object       A \code{gmwm} object.
#' @param datasheet    A \code{vector} with the implied WV of the datasheet.
#' @param datasheet_bi A \code{value} with the bias-instabulity value.
#' @param axis.x.label A \code{string} for the label of the "x-axis".
#' @param prov_title    A \code{string} for the title of the graph.
#' @export
#' @examples
#' Zt = rnorm(1000000)
#' wv = wvar(Zt, freq = 500)
#' datasheet = wn_to_wv(sigma2 = 1, tau = wv$scales)
#' plot_wv_and_datasheet(wv, datasheet/500)
plot_wv_and_datasheet <- function(wv,
                                  datasheet, 
                                  # datasheet_bi,
                                  axis.x.label,# = expression(paste("Scale ", tau)),
                                  axis.y.label,
                                  prov_title = NA){
  object = wv
  CI = T
  transparence = 0.1
  background = "white"
  bw = F
  CI.color = "#003C7D"
  line.type = NULL
  line.color = NULL
  point.size = NULL
  point.shape = NULL
  title = prov_title#NA
  title.size = 22# 15
  axis.label.size = 20 #13
  axis.tick.size = 17 #11
  # axis.x.label = expression(paste("Scale ", tau))
  #title.size = 20# 15
  #axis.label.size = 18 #13
  #axis.tick.size = 15 #11
  #axis.x.label = expression(paste("Scale ", tau))
  # axis.y.label = expression(paste("Wavelet Variance ", nu))
  legend.title.size = 19#13
  legend.text.size = 19#13
  legend.label = NULL
  legend.title = ""
  legend.key.size = 1


  params = "legend.label"
  if (CI){
    requireLength = 3
    legend.label.default = c(bquote("Empirical WV" ~ hat(nu)),
                             bquote("CI(" * hat(nu) * ", " * .(1 - object$alpha) *
                                      ")"),
                             "Datasheet")
  }else{
    requireLength = 2
    legend.label.default = c(bquote("Empirical WV" ~ hat(nu)), "Datasheet")
  }
  default = list(legend.label.default)
  nullIsFine = T
  object$dat = object$ci_high
  # object$dat = object$ci.high
  checkParams(params = params, require.len = requireLength,
              default = default, null.is.fine = nullIsFine)
  #check parameter
  params = c('line.type', 'line.color', 'point.size', 'point.shape')
  if(CI){
    requireLength = rep(4, times = 4)
    default = list(c('solid','solid','dotted','dotted'), c('#003C7D', 'red2', '#999999','#999999'),
                   c(5, 4, 0,0), c(20, 21, 46,7))
  }else{
    requireLength = rep(1, times = 4)
    default = list('solid', '#003C7D',  5, 20)
  }
  nullIsFine = rep(T,4)
  checkParams(params = params, require.len = requireLength, default = default, null.is.fine = nullIsFine)

  if(bw){
    if(CI){line.color = c("#000000", "#404040")}else{line.color = c("#000000")}
    CI.color = "grey50"
  }
  
  # datasheet values: WN+GM+.....
  WV = data.frame(var = object$variance, dat = datasheet, low = object$ci_low, high = object$ci_high,
                  scale = object$scales)
  # bias instability
  # WV2 = data.frame(var = object$variance, dat = rep.int(datasheet_bi, length(datasheet)), low = object$ci_low, high = object$ci_high,
  #                  scale = object$scales)

  if(CI){
    #process parameter (insert some values)
    params = params[-5]; from = 2; to = 3; times = 1;
    for(i in 1:length(params)){
      real_param = get(params[i])
      target = real_param[from]
      stuff = rep(target, times)
      one_param = params[i]

      assign(one_param, c(real_param, stuff))
    }

    #other parameter
    breaks = c('var', 'low', 'dat')
    legend.color = c(NA, alpha(CI.color, transparence), NA)
    legend.linetype = c(line.type[1], 'blank', line.type[1])
    legend.pointshape = c(point.shape[1], NA, point.shape[2])

    # put data in the desired format
    melt.wv = melt(WV, id.vars = 'scale')
    # melt.wv2 = melt(WV2, id.vars = 'scale')

  }else{
    #other parameter
    breaks = c('var')

    # put data in the desired format
    melt.wv = melt(WV, id.vars = 'scale', measure.vars = 'var')
    # melt.wv2 = melt(WV2, id.vars = 'scale', measure.vars = 'var')
  }

  p = ggplot()+
    
    geom_line(data = melt.wv, mapping = aes(x = scale, y = value, color = variable, linetype = variable)) +
    geom_point(data = melt.wv, mapping =aes(x = scale, y = value, color = variable, size = variable, shape = variable)) +
    
    # geom_line(data = melt.wv2, mapping = aes(x = scale, y = value, color = variable, linetype = variable)) +
    # geom_point(data = melt.wv2, mapping =aes(x = scale, y = value, color = variable, size = variable, shape = variable)) +

    scale_linetype_manual(name = legend.title, values = c(line.type), breaks = breaks, labels = legend.label ) +
    scale_shape_manual(name = legend.title, values = c(point.shape), breaks = breaks, labels = legend.label)+
    scale_size_manual(name = legend.title, values = c(point.size), breaks = breaks, labels = legend.label) +
    scale_color_manual(name = legend.title,values = c(line.color), breaks = breaks, labels = legend.label) +

    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))

  if(CI){
    p = p + geom_ribbon(data = WV, mapping = aes(ymin = low , ymax = high, x = scale, y = NULL), alpha = transparence, fill = CI.color, show.legend = T) +
      guides(colour = guide_legend(override.aes = list(fill = legend.color, linetype = legend.linetype, shape = legend.pointshape)))
  }

  if( background == 'white'||bw){
    p = p + theme_bw()
  }

  #decide where to place the legend
  legendPlace = placeLegend(WV$var[1], WV$low[ length(WV$low) ], WV$high[ length(WV$high)])
  p = p +
    xlab(axis.x.label) + ylab(axis.y.label) + ggtitle(title) +
    theme(
      plot.title = element_text(size= title.size),
      axis.title.y = element_text(size= axis.label.size),
      axis.text.y  = element_text(size= axis.tick.size),
      axis.title.x = element_text(size= axis.label.size),
      axis.text.x  = element_text(size= axis.tick.size),
      legend.key.size = unit(legend.key.size, "cm"),
      legend.text = element_text(size = legend.text.size),
      legend.title = element_text(size = legend.title.size),
      legend.background = element_rect(fill="transparent"),
      legend.justification=legendPlace[1:2], legend.position=legendPlace[3:4],
      legend.text.align = 0)

  if (is.na(title)) {
    name = if (object$robust){
      "Robust"
    }else{
      "Classic"
    }
    p = p + ggtitle(paste0("Haar Wavelet Variance Representation for ",
                           name, " Calculation"))
  }
  p
}

#' @title Plot GMWM with datasheet characteristics
#' @description Plot empirical WV of a sensors together the empirical WV implied by
#' the datasheet of the manufacture as well as GMWM solution
#' @author Stephane Guerrier
#' @param object       A \code{gmwm} object.
#' @param datasheet    A \code{vector} with the implied WV of the datasheet.
#' @param datasheet_bi A \code{value} with the bias-instabulity value.
#' @param axis.x.label A \code{string} for the label of the "x-axis".
#' @param prov_title    A \code{string} for the title of the graph.
#' @export
#' @examples
#' Zt = rnorm(1000000)
#' wv = gmwm(WN(), Zt, freq = 500)
#' datasheet = wn_to_wv(sigma2 = 1, tau = wv$scales)
#' plot_gmwm_and_datasheet(wv, datasheet/500)
plot_gmwm_and_datasheet <- function(object,
                                    datasheet, 
                                    # datasheet_bi,
                                    axis.x.label = expression(paste("Scale ", tau)),
                                    axis.y.label,
                                    prov_title = NULL){
  process.decomp = FALSE
  background = "white"
  CI = T
  transparence = 0.1
  bw = F
  CI.color = "#003C7D"
  line.type = NULL
  line.color = NULL
  point.size = NULL
  point.shape = NULL 
  title = prov_title 
  title.size = 22#15
  axis.label.size = 20#13
  axis.tick.size = 17#11 
  #axis.x.label = expression(paste("Scale ", tau))
  # axis.y.label = expression(paste("Wavelet Variance ",nu))
  legend.title = ""
  legend.label = NULL
  legend.key.size = 1
  legend.title.size = 19#13
  legend.text.size = 19#13
  
  #require pakage: scales, grid
  low=high=emp=theo=trans_breaks=trans_format=math_format=.x=value=variable=NULL
  
  temp = data.frame( emp = object$wv.empir,
                     dat = datasheet,
                     low = object$ci.low,
                     high = object$ci.high,
                     theo = object$theo,
                     scale = object$scales)
  # 
  # temp2 = data.frame( emp = object$wv.empir,
  #                    dat = rep.int(datasheet_bi, length(datasheet)),
  #                    low = object$ci.low,
  #                    high = object$ci.high,
  #                    theo = object$theo,
  #                    scale = object$scales)
  
  
  if(CI == T){
    if(is.null(line.type)){line.type = c('solid', 'solid', 'dotted', 'solid')}
    if(length(line.type)==4){line.type = c(line.type[1:3], line.type[3:4])}
    
    if(is.null(line.color)){
      line.color = c("#003C7D", "red2", "#999999" , "#F47F24")
    }
    
    if(bw){
      line.color = c("#b2b2b2", "#404040", "#000000")
      CI.color = "grey50"
    }
    if(length(line.color)==4){
      line.color = c(line.color[1:3],line.color[3:4])
    }
    
    if(is.null(point.size)){point.size = c(5, 5, 0, 5)}
    if(length(point.size)==4){point.size = c(point.size[1:3],point.size[3:4])}
    
    if(is.null(point.shape)){point.shape = c(20, 20, 46, 1) }
    if(length(point.shape)==4){point.shape = c(point.shape[1:3],point.shape[3:4])}
    
    if(is.null(legend.label)){
      #legend.label = c(expression(paste("Empirical WV ", hat(nu))), 
      #                                         expression(paste("CI(", hat(nu)," , 0.95)" )),
      #                                         expression(paste("Implied WV ", nu,"(",hat(theta),")")) )
      
      legend.label = c(bquote("Empirical WV "~hat(nu)), 
                       bquote("CI("*hat(nu)*", "*.(1 - object$alpha)*")" ),
                       bquote("Implied WV "~nu*"("*hat(theta)*")"), "Datasheet") 
    }
    
    WV = melt(temp, id.vars = 'scale')
    # WV2 = melt(temp2, id.vars = 'scale')
    breaks = c('emp','low','theo','dat')
    legend.fill = c(NA, CI.color, NA, NA)
    legend.linetype = c(line.type[1],'blank', "solid", 'solid')
    legend.pointshape = c(point.shape[1], NA, 1, 20)
    #legend.pointsize = c(point.size[1:2],0)
  }else{
    if(is.null(line.type)){line.type = c('solid','solid')}
    if(is.null(line.color)){line.color = c("#003C7D", "#F47F24")}
    if(bw){
      line.color = c("#b2b2b2", "#000000")}
    if(is.null(point.size)){point.size = c(5,5)}
    if(is.null(point.shape)){point.shape = c(20,1) }
    if(is.null(legend.label)){legend.label = c(expression(paste("Empirical WV ", hat(nu))),
                                               expression(paste("Implied WV ", nu,"(",hat(theta),")"))    )}
    
    WV = melt(temp, id.vars = 'scale', measure.vars = c('emp', 'theo'))
    # WV2 = melt(temp2, id.vars = 'scale', measure.vars = c('emp', 'theo'))
    breaks = c('emp','theo')
    #legend.color = c(NA,NA)
  }
  
  p = ggplot(data = WV, mapping = aes(x = scale)) +
      geom_line(aes(y = value, color = variable, linetype = variable)) +
    
      # geom_line(data = WV2, mapping = aes(x = scale, y = value, color = variable, linetype = variable)) +
      # geom_point(data = WV2, mapping =aes(x = scale, y = value, color = variable, size = variable, shape = variable)) +
    
      geom_point(aes(y = value, shape = variable, size = variable, color = variable)) + 
      scale_linetype_manual(name = legend.title, values = c(line.type), breaks = breaks, labels = legend.label) +
      scale_shape_manual(name = legend.title, values = c(point.shape), breaks = breaks,labels = legend.label)+
    
      scale_size_manual(name = legend.title, values = c(point.size),breaks = breaks,labels = legend.label) +
      scale_color_manual(name = legend.title, values = c(line.color), breaks = breaks, labels = legend.label) 
  
  if(CI){
    p = p +
      geom_ribbon(data = temp, mapping = aes(ymin = low, ymax = high),fill = CI.color, show.legend = T,alpha = transparence) +
      
      #scale_fill_manual(name = legend.title, values = c(color.CI,'red'), breaks = breaks, labels = legend.label) +
      guides(colour = guide_legend(override.aes = list(fill = legend.fill, linetype = legend.linetype, shape = legend.pointshape)))
  }
  
  #decide where to place the legend
  legendPlace = placeLegend(temp$emp[1], temp$low[ length(temp$low) ], temp$high[ length(temp$high)])    
  p = p + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))
  
  if( background == 'white' || bw){
    p = p + theme_bw() 
  }
  
  p = p +
    xlab(axis.x.label) + ylab(axis.y.label) + ggtitle(title) +
    theme(
      plot.title = element_text(size= title.size),
      axis.title.y = element_text(size= axis.label.size),
      axis.text.y  = element_text(size= axis.tick.size),
      axis.title.x = element_text(size= axis.label.size),
      axis.text.x  = element_text(size= axis.tick.size),
      legend.key.size = unit(legend.key.size, "cm"),
      legend.text = element_text(size = legend.text.size),  
      legend.title = element_text(size = legend.title.size),
      legend.background = element_rect(fill="transparent"),
      legend.justification=legendPlace[1:2], legend.position=legendPlace[3:4],
      legend.text.align = 0 )
  # if(!bw){p = p + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))}
  if (is.null(title)){
    if(object$robust){
      p = p + ggtitle("Haar Wavelet Variance Robust Representation")
    }
    else{
      p = p + ggtitle("Haar Wavelet Variance Classical Representation")
    }
  }
  p
}

#' @export
is.whole = function(x){ is.numeric(x) && all(floor(x)==x) }





