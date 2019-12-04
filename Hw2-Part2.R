library("jpeg")
library("imager")
library("broom")
library("EnvStats")
img <- readJPEG("C:/Users/kaank/Downloads/part2.jpeg")

initial_matrix <- matrix(,2600,4)
colnames(initial_matrix) = c("pxA","pxB","value","num")
prediction_matrix <- matrix(,350,350)
residual_matrix <- matrix(,350,350)



for(a in 1:350){
  for(b in 1:350){
    k <- 1
    for(i in a:(a+50)){
      for(j in b:(b+50)){
        if(i == a+25 & j == b+25){
        }else{
          initial_matrix[k,1] = i
          initial_matrix[k,2] = j
          initial_matrix[k,3] = img[i,j]
          if(k > 1299){
            initial_matrix[k,4] = k+1
          }else{
            initial_matrix[k,4] = k
          }
          k <- k + 1
        }
      }
    }
    
    initial_matrix_data_frame <- as.data.frame(initial_matrix)
    simple.fit = lm(value~num, data=initial_matrix_data_frame)
    prediction_matrix[a,b]<-predict(simple.fit,data.frame(num=1300))
    residual_matrix[(a),(b)]<- img[a+25,b+25]- predict(simple.fit,data.frame(num=1300))
    
  }
}

hist(residual_matrix)

res_mean <- mean(residual_matrix)
res_sd <- sd(residual_matrix)
res_UCL <- res_mean + (1.4*res_sd)
res_LCL <- res_mean - (1.4*res_sd)
plot(residual_matrix, ylim=c(-1,1))
abline(h = res_UCL)
abline(h = res_LCL)


imgPart2 <- img
for(i in 1:350){
  for(k in 1:350){
    if(residual_matrix[i,k] <  res_LCL| residual_matrix[i,k] > res_UCL ){
      imgPart2[(i+25),(k+25)] = 0;
    } 
  }
}

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 950), c(100, 510), type = "n", xlab = "", ylab = "")
rasterImage(img, 100, 105, 500, 505, interpolate = FALSE)
rasterImage(imgPart2, 540, 105, 940, 505, interpolate = FALSE)
par(op)


c<-1
acfmatrix<-matrix(,(350*350),1)
for(i in 1:350){
  for(k in 1:350){
    acfmatrix[c,]<-residual_matrix[i,k]
    c<-c+1
  }
}
acf(acfmatrix)


