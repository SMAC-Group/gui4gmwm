rm(list=ls())
dev.off(dev.list())

setwd("/home/philipp/Desktop/padova/rcode_new/gmwm_gui/to_move/")

defined_scale <- 2
f0 <- 1/defined_scale # cutoff frequency
B <- 1 # bias instability coeficient

cos_function <- function(t){cos(t)/t}
Ci <- function(x){-integrate(f = cos_function, lower = x, upper = 2e4, subdivisions=10000)$value}
VCi <- Vectorize(Ci, c("x"))

sigma2_T <- function(T){2*B*B/pi * ( log(2) - ( (sin(pi*f0*T))^3 ) / (2*(pi*f0*T)^2) * ( sin(pi*f0*T)+4*pi*f0*T*cos(pi*f0*T) ) + VCi(2*pi*f0*T) - VCi(4*pi*f0*T) )}

object_scales_T <- c(0.004, 0.005, 0.01, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 3, 5, 10, 30, 50, 100, 300, 500, 1000, 3000, 5000)

plot(object_scales_T, sqrt(abs(sigma2_T(object_scales_T))), log = "xy", type = "l")
