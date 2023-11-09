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




## basis functions

# B-splines
pdf("bsplines.pdf", width=5, height=5)
par(mar=c(5,4,0,0)) #bltr
k <- 10
sm <- smoothCon(s(x0, bs="bs", m=c(2,1), k=k), data=dat)[[1]]
Xp <- PredictMat(sm, data=data.frame(x0=seq(0,1,length.out=200)))

basis.coef <- rep(1, k)

weighted.bases <- Xp*0
for(i in 1:ncol(Xp)){
  weighted.bases[, i] <- Xp[, i] * basis.coef[i]
}
# plot outline
plot(dat$x0, dat$f0, main="", type="n",
     ylim=c(-0.025,0.9), axes=FALSE,
     xlab="s(x)",
     ylab="x")
axis(1)
axis(2)

# plot each basis functions
apply(weighted.bases, 2, lines, x=seq(0, 1, length.out=200))

dev.off()

