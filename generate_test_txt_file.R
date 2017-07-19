library(gmwm)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

m = WN(sigma2 = 4.13e-7)

Xt = gen_gts(100000, m)
n = length(Xt)

write(Xt,
      file = "data.txt",
      ncolumns = 1,
      append = FALSE,
      sep = " ")

