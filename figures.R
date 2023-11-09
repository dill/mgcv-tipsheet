# make figures for tipsheet

library(mgcv)
set.seed(2)

dat <- gamSim(1,n=400,dist="normal",scale=2)
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)

# gam.check plot
pdf("gamcheck.pdf", width=7, height=7)
gam.check(b)
dev.off()

# check.k output needs to be included
check.k(b)
