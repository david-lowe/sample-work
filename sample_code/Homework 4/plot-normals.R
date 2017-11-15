source("random-normals")
X <- r.norm(1000)

pdf("box-muller.pdf")
hist(X, probability=T, main="Box-Muller vs. Theoretical
Random Normal (X=1000)")
lines(density(X), lwd=3)
curve(dnorm(x),add=T,col="red",lwd=3)
dev.off()

