

setwd("/home/philipp/Desktop/padova/rcode_new/gmwm_gui/gui4gmwm/initial_6_pack_plots/")

library(gui4gmwm)

data("navchip") # NAVCHIP
imu6 = imu(imu6, gyros = 1:3, accels = 4:6, axis = c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100) #MTIG
data("imar.gyro") #IMAR
data("ln200.gyro") #LN200
data("ln200.accel") #LN200

set_dpi <- 400
set_width <- 10
set_heigth <- 6

# HERE FOR MTIG
wv_imu_imu6 = wvar(imu6)
plot(wv_imu_imu6, axis.x.label = expression(paste("Scale ", tau, " [s]")))
ggsave("imu6.png", width=set_width, height=set_heigth, dpi=set_dpi)


# HERE FOR NAVCHIP
imu_navchip = imu(navchip, gyros = 1:3, accels = 4:6, axis = c('X', 'Y', 'Z'), freq = 250)
wv_imu_navchip = wvar(imu_navchip)
plot(wv_imu_navchip, axis.x.label = expression(paste("Scale ", tau, " [s]")))
ggsave("navchip.png", width=set_width, height=set_heigth, dpi=set_dpi)


# HERE FOR IMAR
imu_imar_gyro = imu(imar.gyro, gyros = 1:3, axis = c('X', 'Y', 'Z'), freq = 400)
wv_imu_imar_gyro = wvar(imu_imar_gyro)
plot(wv_imu_imar_gyro, axis.x.label = expression(paste("Scale ", tau, " [s]")))
ggsave("imar_gyro.png", width=set_width, height=set_heigth/3*2, dpi=set_dpi)


# HERE FOR LN200
imu_ln200 = imu(cbind(ln200.gyro, ln200.accel), gyros = 1:3, accels = 4:6, axis = c('X', 'Y', 'Z'), freq = 400)
wv_imu_ln200 = wvar(imu_ln200)
plot(wv_imu_ln200, axis.x.label = expression(paste("Scale ", tau, " [s]")))
ggsave("ln200.png", width=set_width, height=set_heigth, dpi=set_dpi)




