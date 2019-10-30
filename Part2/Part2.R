install.packages("jpeg")
library(jpeg)
img1 <- readJPEG("C:/Program Files/araba512.jpg")
hist(img1)
mean1 = mean(img1)
median(img1)
sd(img1)

library(MASS)
set.seed(101)
fit <- fitdistr(img1, densfun="normal")
fit
hist(img1, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)
sd(img1)
var(img1)
mean(img1)

require(grDevices)
op <- par(bg = "magenta")
plot(c(0, 800), c(0, 800), type = "n", xlab = "", ylab = "")
rasterImage(img1, 100, 100, 612, 612, interpolate = FALSE)
par(op)

lowerbound <- qnorm(0.001)*sd(img1)+mean(img1)
upperbound <- qnorm(0.999)*sd(img1)+mean(img1)
imgTemp <- img1
imgTemp[imgTemp < lowerbound | imgTemp > upperbound] = 0
img2 <- imgTemp
mean2 = mean(img2)
hist(img2)
median(img2)
sd(img2)

fit <- fitdistr(img2, densfun="normal")
fit
hist(img2, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 1300), c(100, 712), type = "n", xlab = "", ylab = "")
rasterImage(img1, 100, 100, 612, 612, interpolate = FALSE)
rasterImage(img2, 700, 100, 1212, 612)
par(op)

img3 <- img1
hist(img3)
for (i in 1:10) {
  for(j in 1:10){
    tempImg2 <- img3[(1+(51*(i-1))):(51*i),(1+(51*(j-1))):(51*j),1:3]
    lowerbound <- qnorm(0.001)*sd(tempImg2)+mean(tempImg2)
    upperbound <- qnorm(0.999)*sd(tempImg2)+mean(tempImg2)
    tempImg2[tempImg2 < lowerbound | tempImg2 > upperbound] = 0
    img3[(1+(51*(i-1))):(51*i),(1+(51*(j-1))):(51*j),1:3] <- tempImg2[1:51,1:51,1:3]
  }
}
hist(img3)

fit <- fitdistr(img3, densfun="normal")
fit
hist(img3, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)
sd(img3)
var(img3)
mean(img3)
median(img3)

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 1300), c(100, 712), type = "n", xlab = "", ylab = "")
rasterImage(img2, 100, 100, 612, 612, interpolate = FALSE)
rasterImage(img3, 700, 100, 1212, 612)
par(op)

