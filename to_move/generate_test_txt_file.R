library(gmwm)

# m1 = WN(sigma2 = 4.13e-7) # same as the noise found for a navchip gyroscope
# m2 = WN(sigma2 = 3.45e-5) # same as the noise found for a navchip accelerometer

# values baed on navchip gyro:
# Model Information: 
#   Estimates
# BETA      3.526037e-01
# SIGMA2_GM 7.348293e-09
# WN        5.304377e-07
# QN        1.681227e-06
# RW        1.268314e-12
# DR        3.913529e-09
# 


# values based on MTIG accel-x:
# Model Information: 
#   Estimates
# BETA      2.162742e-02
# SIGMA2_GM 1.373440e-07
# BETA      1.940672e+02
# SIGMA2_GM 7.501190e-05
# WN        7.838580e-06
# RW        9.883209e-12
# DR        7.234603e-09

# gyroscope
m1 = GM(beta = 3.526037e-01, sigma2_gm = 7.348293e-09) +
      WN(sigma2 = 5.304377e-07) +
      QN(q2 = 1.681227e-06) +
      RW(gamma2 = 1.268314e-12) +
      DR(omega = 3.913529e-09)

m12 = GM(beta = 3.526037e-01, sigma2_gm = 7.348293e-09) +
  WN(sigma2 = 5.304377e-07) +
  QN(q2 = 0) +
  RW(gamma2 = 1.268314e-12) +
  DR(omega = 3.913529e-09)

# m1 = WN(sigma2 = 5.304377e-07)
# m1 =  QN(q2 = 1.681227e-06)
# m1  = WN(sigma2 = 5.304377e-07) + QN(q2 = 1.681227e-06)
# m1  = WN(sigma2 = 5.304377e-07) + QN(q2 = 1.681227e-06) + RW(gamma2 = 1.268314e-12)
# m1  = WN(sigma2 = 5.304377e-07) + QN(q2 = 1.681227e-06) + RW(gamma2 = 1.268314e-12) + DR(omega = 3.913529e-09)
# m1 = GM(beta = 3.526037e-01, sigma2_gm = 7.348293e-09)
  
# accelerometer
m2 = GM(beta = 2.162742e+00, sigma2_gm = 1.373440e-07) + 
      WN(sigma2 = 7.838580e-06) + 
      DR(omega = 7.234603e-09)



my_freq = 200
number_of_datapoints = 1000000

Xt = gen_gts(n = number_of_datapoints, model = m1, freq = my_freq)
Yt = gen_gts(n = number_of_datapoints, model = m2, freq = my_freq)
n = length(Xt)

plot(wvar(Xt))
plot(wvar(Yt))

B = matrix( c(Xt, Yt), 
            nrow=length(Xt), 
            ncol=2)

write(format(t(B), nsmall = 19),
      file = "data.txt",
      ncolumns = 2,
      append = FALSE,
      sep = ",")



