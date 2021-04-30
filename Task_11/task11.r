x <- rnorm(100, mean=5, sd=2)
x
var(x)
mean(x)
y <- ((x*5)+2)+runif(100,0,0.1)
y
pdf("graph1.pdf", height=4, width=4)
plot(x,y)
abline(lm(y~x), col="pink")
dev.off()
coef(lm(y~x))
z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i] <- runif(1)
	y <- (x*z[i]) + 2 + (rnorm(100, 0:0.1))
	l <- coef(lm(z[1:100]~y))
}
l
pdf("graph2.pdf", height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[e646    m
dev.off()
pdf("graph3.pdf", height=4, width=4)
plot(c(z, -0.029))
dev.off()

install.packages("meme")
library(meme)
u <- "https://upload.wikimedia.org/wikipedia/commons/3/3e/Charles_Robert_Darwin_by_John_Collier.jpg"
pdf("meme.pdf", height=6, width=6)
MyMeme <- meme(u, upper="Discovers the Vitality of Genetic Variation in Species", lower="Has 10 Kids with His First Cousin", color="white", size="1.5")
dev.off()
