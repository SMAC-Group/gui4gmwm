library(gmwm)
library(reshape)
library(scales)
data("navchip")


#setwd('/home/philipp/Desktop/padova/rcode_new/gmwm_gui/to move')


# source("plot_wv_and_datasheet.R")

source("../R/plot_wv_and_datasheet.R", chdir = TRUE)



## TEST 1a ##
Zt = rnorm(1000000)
wv = wvar(Zt, freq = 500)
datasheet = wn_to_wv(sigma2 = 1, tau = wv$scales)
plot_wv_and_datasheet(wv, datasheet/500)

## TEST 1b ##
Zt = rnorm(1000000)
wv = wvar(Zt)
datasheet = wn_to_wv(sigma2 = 1, tau = wv$scales/500)
plot_wv_and_datasheet(wv, datasheet/500)

## TEST 2 ##
custom_freq = 500
custom_model = WN(sigma2 = 0.5) + QN(q2 = 0.21)
Xt = as.numeric(gen_gts(1000000, custom_model))
wv_xt = wvar(Xt, freq = custom_freq)
datasheet_xt = wn_to_wv(sigma2 = 0.5, tau = wv_xt$scales)/custom_freq + qn_to_wv(q2 = 0.21, tau = wv_xt$scales)/(custom_freq*custom_freq)
plot_wv_and_datasheet(wv_xt, datasheet_xt)


## TEST 3 ##
nc_freq = 250
Yt = navchip[,3]
wv_yt = wvar(as.numeric(Yt), freq = nc_freq)
datasheet_yt = qn_to_wv(q2 = 1.95e-6, tau = wv_yt$scales)/nc_freq/nc_freq + wn_to_wv(sigma2 = 4.13e-7, tau = wv_yt$scales)/nc_freq
plot_wv_and_datasheet(wv_yt, datasheet_yt)

## TEST 4 ##
nc_freq = 250
Yt = navchip[,3]
wv_yt = wvar(as.numeric(Yt), freq = nc_freq)

analysis = gmwm_imu(model = WN(), data = Yt)

datasheet_yt = wn_to_wv(sigma2 = 4.13e-7, tau = wv_yt$scales)/nc_freq

plot_wv_and_datasheet(wv_yt, datasheet_yt, "my personal title")

# plot_wv_and_datasheet_and_gmwm(wv_yt, datasheet_yn, analysis, my_title)





