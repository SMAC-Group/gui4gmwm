library(gmwm)

m1 = WN(sigma2 = 4.13e-7) # same as the noise found for a navchip gyroscope
m2 = WN(sigma2 = 3.45e-5) # same as the noise found for a navchip accelerometer

my_freq = 250
number_of_datapoints = 1000000

Xt = gen_gts(n = number_of_datapoints, model = m1, freq = my_freq)
Yt = gen_gts(n = number_of_datapoints, model = m2, freq = my_freq)
n = length(Xt)

B = matrix( c(Xt, Yt), 
            nrow=length(Xt), 
            ncol=2)

write(format(t(B), nsmall = 19),
      file = "data.txt",
      ncolumns = 2,
      append = FALSE,
      sep = ",")

## Calculating the wvar of white noise and comparing them 
a = wvar(Xt)
b = wvar(Yt)

compare_wvar(a, b)
