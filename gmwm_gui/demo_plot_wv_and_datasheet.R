library(gmwm)
library(reshape)
library(scales)
data("navchip")

# wv_and_datasheet = function(wv, datasheet){
#   object = wv
#   CI = T
#   transparence = 0.1
#   background = "white"
#   bw = F 
#   CI.color = "#003C7D"
#   line.type = NULL
#   line.color = NULL
#   point.size = NULL
#   point.shape = NULL
#   title = NA
#   title.size = 15 
#   axis.label.size = 13
#   axis.tick.size = 11
#   axis.x.label = expression(paste("Scale ", tau))
#   axis.y.label = expression(paste("Wavelet Variance ", nu))
#   legend.title.size = 13
#   legend.text.size = 13
#   legend.label = NULL
#   legend.title = ""
#   legend.key.size = 1
#   
#   
#   params = "legend.label"
#   if (CI){
#     requireLength = 3
#     legend.label.default = c(bquote("Empirical WV" ~ hat(nu)), 
#                              bquote("CI(" * hat(nu) * ", " * .(1 - object$alpha) * 
#                                       ")"),
#                              "Datasheet")
#   }else{
#     requireLength = 2
#     legend.label.default = c(bquote("Empirical WV" ~ hat(nu)), "Datasheet")
#   }
#   default = list(legend.label.default)
#   nullIsFine = T
#   object$dat = object$ci_high
#   checkParams(params = params, require.len = requireLength, 
#               default = default, null.is.fine = nullIsFine)
#   #check parameter
#   params = c('line.type', 'line.color', 'point.size', 'point.shape')
#   if(CI){
#     requireLength = rep(4, times = 4)
#     default = list(c('solid','solid','dotted','dotted'), c('#003C7D', 'red2', '#999999','#999999'),  
#                    c(5, 4, 0,0), c(20, 21, 46,7))
#   }else{
#     requireLength = rep(1, times = 4)
#     default = list('solid', '#003C7D',  5, 20)
#   }
#   nullIsFine = rep(T,4)
#   checkParams(params = params, require.len = requireLength, default = default, null.is.fine = nullIsFine)
#   
#   if(bw){
#     if(CI){line.color = c("#000000", "#404040")}else{line.color = c("#000000")}
#     CI.color = "grey50"
#   }
#   
#   WV = data.frame(var = object$variance, dat = datasheet, low = object$ci_low, high = object$ci_high, 
#                   scale = object$scales)
#   
#   
#   
#   if(CI){
#     #process parameter (insert some values)
#     params = params[-5]; from = 2; to = 3; times = 1;
#     for(i in 1:length(params)){
#       real_param = get(params[i])
#       target = real_param[from]
#       stuff = rep(target, times)
#       one_param = params[i]
#       
#       assign(one_param, c(real_param, stuff))
#     }
#     
#     #other parameter
#     breaks = c('var', 'low', 'dat')
#     legend.color = c(NA, alpha(CI.color, transparence), NA)
#     legend.linetype = c(line.type[1], 'blank', line.type[1])
#     legend.pointshape = c(point.shape[1], NA, point.shape[2])
#     
#     # put data in the desired format
#     melt.wv = melt(WV, id.vars = 'scale')
#     
#   }else{
#     #other parameter
#     breaks = c('var')
#     
#     # put data in the desired format
#     melt.wv = melt(WV, id.vars = 'scale', measure.vars = 'var')
#   }
#   
#   p = ggplot() + geom_line(data = melt.wv, mapping = aes(x = scale, y = value, color = variable, linetype = variable)) +
#     geom_point(data = melt.wv, mapping =aes(x = scale, y = value, color = variable, size = variable, shape = variable)) +
#     
#     scale_linetype_manual(name = legend.title, values = c(line.type), breaks = breaks, labels = legend.label ) +
#     scale_shape_manual(name = legend.title, values = c(point.shape), breaks = breaks, labels = legend.label)+
#     scale_size_manual(name = legend.title, values = c(point.size), breaks = breaks, labels = legend.label) +
#     scale_color_manual(name = legend.title,values = c(line.color), breaks = breaks, labels = legend.label) +
#     
#     scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x))) + 
#     scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))
#   
#   if(CI){
#     p = p + geom_ribbon(data = WV, mapping = aes(ymin = low , ymax = high, x = scale, y = NULL), alpha = transparence, fill = CI.color, show.legend = T) +
#       guides(colour = guide_legend(override.aes = list(fill = legend.color, linetype = legend.linetype, shape = legend.pointshape)))
#   }
#   
#   if( background == 'white'||bw){
#     p = p + theme_bw() 
#   }
#   
#   #decide where to place the legend
#   legendPlace = placeLegend(WV$var[1], WV$low[ length(WV$low) ], WV$high[ length(WV$high)])  
#   p = p +
#     xlab(axis.x.label) + ylab(axis.y.label) + ggtitle(title) +
#     theme(
#       plot.title = element_text(size= title.size),
#       axis.title.y = element_text(size= axis.label.size),
#       axis.text.y  = element_text(size= axis.tick.size),
#       axis.title.x = element_text(size= axis.label.size),
#       axis.text.x  = element_text(size= axis.tick.size),
#       legend.key.size = unit(legend.key.size, "cm"),
#       legend.text = element_text(size = legend.text.size),  
#       legend.title = element_text(size = legend.title.size),
#       legend.background = element_rect(fill="transparent"),
#       legend.justification=legendPlace[1:2], legend.position=legendPlace[3:4],
#       legend.text.align = 0)  
#   
#   if (is.na(title)) {
#     name = if (object$robust){
#       "Robust"
#     }else{
#       "Classic"
#     }
#     p = p + ggtitle(paste0("Haar Wavelet Variance Representation for ", 
#                            name, " Calculation"))
#   }
#   p
# }

setwd('/home/philipp/Desktop/padova/rcode_new/gmwm_gui/gmwm_gui')


source("plot_wv_and_datasheet.R")


## TEST 1 ##
Zt = rnorm(1000000)
wv = wvar(Zt, freq = 500)
datasheet = wn_to_wv(sigma2 = 1, tau = wv$scales)
wv_and_datasheet(wv, datasheet/500)

## TEST 2 ##
custom_freq = 500
custom_model = WN(sigma2 = 0.5) + QN(q2 = 0.21)
Xt = as.numeric(gen_gts(1000000, custom_model))
wv_xt = wvar(Xt, freq = custom_freq)
datasheet_xt = wn_to_wv(sigma2 = 0.5, tau = wv_xt$scales)/custom_freq + qn_to_wv(q2 = 0.21, tau = wv_xt$scales)/(custom_freq*custom_freq)
wv_and_datasheet(wv_xt, datasheet_xt)


## TEST 3 ##
nc_freq = 250
Yt = navchip[,3]
wv_yt = wvar(as.numeric(Yt), freq = nc_freq)
datasheet_yt = qn_to_wv(q2 = 1.95e-6, tau = wv_yt$scales)/nc_freq/nc_freq + wn_to_wv(sigma2 = 4.13e-7, tau = wv_yt$scales)/nc_freq
wv_and_datasheet(wv_yt, datasheet_yt)





