library("jpeg")
library("matrixStats")
img <- readJPEG("C:/Users/kaank/Downloads/part2.jpeg")
mean_row <- rowMeans(img)
hist(mean_row)
std_row <- rowSds(img)
hist(std_row)

control_limits_row <- matrix(,400,3)

# control limits is a matrix whose first column is LCL, second column is CL, and 3rd column is UCL
control_limits_row[1:400,1] = mean_row[1:400]-(1.4)*std_row[1:400]
control_limits_row[1:400,2] = mean_row[1:400]
control_limits_row[1:400,3] = mean_row[1:400]+(1.4)*std_row[1:400]


imgPart1a <- img
for(i in 1:400){
  for(k in 1:400){
    if(imgPart1a[i,k] < control_limits_row[i,1] | imgPart1a[i,k] > control_limits_row[i,3] ){
      imgPart1a[i,k] = 0;
    } 
  }
}

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 950), c(100, 510), type = "n", xlab = "", ylab = "")
rasterImage(img, 100, 105, 500, 505, interpolate = FALSE)
rasterImage(imgPart1a, 540, 105, 940, 505, interpolate = FALSE)
par(op)


# Part 1 colum

mean_col <- colMeans(img)
hist(mean_col)
std_col <- colSds(img)
hist(std_col)

control_limits_col <- matrix(,400,3)

# control limits is a matrix whose first column is LCL, second column is CL, and 3rd column is UCL
control_limits_col[1:400,1] = mean_col[1:400]-(1.4)*std_col[1:400]
control_limits_col[1:400,2] = mean_col[1:400]
control_limits_col[1:400,3] = mean_col[1:400]+(1.4)*std_col[1:400]

imgPart1b <- img
for(i in 1:400){
  for(k in 1:400){
    if(imgPart1b[i,k] < control_limits_col[i,1] | imgPart1b[i,k] > control_limits_col[i,3] ){
      imgPart1b[i,k] = 0;
    } 
  }
}

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 950), c(100, 510), type = "n", xlab = "", ylab = "")
rasterImage(img, 100, 105, 500, 505, interpolate = FALSE)
rasterImage(imgPart1b, 540, 105, 940, 505, interpolate = FALSE)
par(op)

