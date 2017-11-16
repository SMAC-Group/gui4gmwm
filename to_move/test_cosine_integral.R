rm(list=ls())
dev.off(dev.list())

setwd("/home/philipp/Desktop/padova/rcode_new/gmwm_gui/to_move/")

defined_scale <- 40
f0 <- 1/defined_scale # cutoff frequency
const.degps_2_radps = 1/360 * 2*pi
B <- (20 / 3600 * const.degps_2_radps )^2 # bias instability coeficient

cos_function <- function(t){cos(t)/t}
Ci <- function(x){-integrate(f = cos_function, lower = x, upper = 2e4, subdivisions=10000)$value}
VCi <- Vectorize(Ci, c("x"))
sigma2_T <- function(T){2*B/pi * ( log(2) - ( (sin(pi*f0*T))^3 ) / (2*(pi*f0*T)^2) * ( sin(pi*f0*T)+4*pi*f0*T*cos(pi*f0*T) ) + VCi(2*pi*f0*T) - VCi(4*pi*f0*T) )}

object_scales_T <- c(0.02, 0.04, 0.08, 0.16, 0.32, 0.64, 1.28, 2.56, 5.12, 10.24, 20.48, 40.96)

abs(sigma2_T(object_scales_T))

# plot(object_scales_T, sqrt(abs(sigma2_T(object_scales_T))), log = "xy", type = "l")








