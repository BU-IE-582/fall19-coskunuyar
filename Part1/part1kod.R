install.packages("jpeg")
library(jpeg)
setwd("Users/Yavuz/Desktop/Dersler/IE423/part1")
img <- readJPEG("/Users/Yavuz/Desktop/Dersler/IE423/part1/IMG.jpg")
# img <- readJPEG("/Users/Yavuz/Desktop/araba512.jpg")
str(img)
hist(img)
dim(img)
                ## Part 2
par(mfrow=c(1,1))
plot(c(0,512),c(0,512),type="n",xlab="",ylab="")
rasterImage(img,0,0,nrow(img),ncol(img))
matrice1 <- img[ , ,1]
matrice2 <- img[ , ,2]
matrice3 <- img[ , ,3]

rotate <- function(x) { 
    t(apply(x, 2, rev))  
  }

par(mfrow=c(3,1))
image(rotate(matrice1))
image(rotate(matrice2))
image(rotate(matrice3))
dev.off()

# as we can see, the sub images are all colored among red to yellow tones. To avoid that and show their own RGB contribution to original image, we can try manipulation below:
dummymatrix <- matrix(rep(0,262144),nrow=512,ncol=512)
submat1 <- array(data=c(matrice1,dummymatrix,dummymatrix),dim=c(512,512,3))
submat2 <- array(data=c(dummymatrix, matrice2,dummymatrix),dim=c(512,512,3))
submat3 <- array(data=c(dummymatrix,dummymatrix,matrice3),dim=c(512,512,3))

plot(c(0,512),c(0,512),type="n",xlab="",ylab="")
rasterImage(submat1,0,0,nrow(img),ncol(img))
plot(c(0,512),c(0,512),type="n",xlab="",ylab="")
rasterImage(submat2,0,0,nrow(img),ncol(img))
plot(c(0,512),c(0,512),type="n",xlab="",ylab="")
rasterImage(submat3,0,0,nrow(img),ncol(img))
dev.off()
                ## part 3
i <- 1
column1 <- matrix(,nrow = 1, ncol=512,byrow=TRUE)
column2 <- matrix(,nrow = 1, ncol=512)
column3 <- matrix(,nrow = 1, ncol=512)

for (i in 1:512) {
column1[1,i] <- mean(matrice1[ ,i])
}
i <- 1
for (i in 1:512) {
column2[1,i] <- mean(matrice2[ ,i])
}
i <- 1
for (i in 1:512) {
column3[1,i] <- mean(matrice3[ ,i])
}
par(mfrow=c(1,1))
x <- c(1:512)
plot(x,column1[1,],type="o",col="blue",pch="o",lty=1,lwd=2,xlab="col index",ylab="density")
points(x,column2[1,],col="red",pch="*") 
lines(x,column2[1,],col="red",lty=2,lwd=2)
points(x,column3[1,],col="green",pch="+") 
lines(x,column3[1,],col="green",lty=3,lwd=2)
legend(x="topleft",legend=c("Channel1","Channel2","Channel3"), col=c("blue","red","green")
       ,pch=c("o","*","+"),lty=c(1,2,3), ncol=1)
  
    ## part 4
i <- 1
j <- 1
submat1 <- matrice1
submat2 <- matrice2
submat3 <- matrice3

for (i in 1:256){ 
    for (j in 1:512) 
      {submat1[j,i] <- submat1[j,i]-submat1[j,i+256]}
  }
  i <- 1
  j <- 1
  for (i in 1:256){ 
    for (j in 1:512) 
    {submat2[j,i] <- submat2[j,i]-submat2[j,i+256]}
  }
  i <- 1
  j <- 1
  for (i in 1:256){ 
    for (j in 1:512) 
    {submat3[j,i] <- submat3[j,i]-submat3[j,i+256]}
  }
  i <- 1
  j <- 1
  

##normalizing
  dummysub1 <- submat1[1:512,1:256]
  dummysub2 <- submat2[1:512,1:256]
  dummysub3 <- submat3[1:512,1:256]
  min1<-min(dummysub1)
  min2<-min(dummysub2)
  min3<-min(dummysub3)
  max1<-max(dummysub1)
  max2<-max(dummysub2)
  max3<-max(dummysub3)
  
  for(i in 1:512){
    for(j in 1:256){
      dummysub1[i,j] <- ((dummysub1[i,j] - min1) / (max1-min1))
      dummysub2[i,j] <- ((dummysub2[i,j] - min2) / (max2-min2))
      dummysub3[i,j] <- ((dummysub3[i,j] - min3) / (max3-min3))
    }
  }

submat1[1:512,1:256] <- dummysub1
submat2[1:512,1:256] <- dummysub2
submat3[1:512,1:256] <- dummysub3

subimg <- array(data=c(submat1,submat2,submat3),dim=c(512,512,3))
dim(subimg)
plot(c(0,512),c(0,512),type="n",xlab="",ylab="")
rasterImage(subimg,0,0,nrow(subimg),ncol(subimg))

# part 5
install.packages("imagine")
library(imagine)
x=medianFilter(matrice1,radius=5,times=1)
y=medianFilter(matrice1,radius=11,times=1)
z=medianFilter(matrice1,radius=31,times=1)

k=medianFilter(matrice2,radius=5,times=1)
n=medianFilter(matrice2,radius=11,times=1)
m=medianFilter(matrice2,radius=31,times=1)

a=medianFilter(matrice3,radius=5,times=1)
b=medianFilter(matrice3,radius=11,times=1)
c=medianFilter(matrice3,radius=31,times=1)

require(grDevices)
op <- par(bg = "thistle")
plot(c(0, 2000), c(0, 2000), type = "n", xlab = "", ylab = "")
rasterImage(x, 100, 100, 612, 612, interpolate = FALSE)
rasterImage(y, 712, 100, 1224, 612, interpolate = FALSE)
rasterImage(z,1324 , 100, 1836,612, interpolate = FALSE)
rasterImage(a, 100, 712, 612, 1224, interpolate = FALSE)
rasterImage(b, 712, 712, 1224, 1224, interpolate = FALSE)
rasterImage(c,1324 , 712, 1836,1224, interpolate = FALSE)
rasterImage(k, 100, 1324, 612, 1836, interpolate = FALSE)
rasterImage(n, 712, 1324, 1224, 1836, interpolate = FALSE)
rasterImage(m,1324 , 1324, 1836,1836, interpolate = FALSE)
par(op)


