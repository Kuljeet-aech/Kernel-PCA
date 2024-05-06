library(tidyverse)
library(kernlab)
library(RSSL)
library(scatterplot3d)
library(plotly)

####Example 1####
set.seed(123)
theta = runif(1000, -pi,pi)
x = cos(theta) + rnorm(200, 0, 0.3)
y = sin(theta) + rnorm(200, 0, 0.3)
u = 6*cos(theta) + rnorm(200, 0, 0.3)
v = 6*sin(theta) + rnorm(200, 0, 0.3)
plot(u, v, col='purple',pch = 19) 
points(x, y , col='red',pch=19)


x1=c(x,u)
x2=c(y,v)
groups <- rep(c("red", "purple"), each = length(x))
scatterplot3d(x1,x2, x1^2+x2^2,color=groups,pch=19)


####Example2####
set.seed(123)
phi1 = runif(200, -1,1)
phi2 = runif(200,-1,1)
y1 = phi1^3 + 0.5 + rnorm(200,0,0.05)
y2 = phi2^3 - 0.5 + rnorm(200,0,0.05)
y=c(y1,y2)
phi = c(phi1,phi2)
pca <- prcomp(cbind(phi,y))
summary(pca)
plot(phi,y)
data = data.frame(x=phi^3,y=y,z=((phi^3-y)^2))
plot_ly(data=data,x=~x,y=~y,z=~z,scatter="scatter")
kpca <- prcomp(data)
summary(kpca)
cumsum(prcomp(data)$sdev^2)/sum(prcomp(data)$sdev^2)
kpc <- kpca(~.,data = data.frame(x=phi, y=z),kernel='rbfdot',kpar=list(sigma=0.001),features=0)
cumsum(kpc@eig)/sum(kpc@eig)

####Spirals####
set.seed(123)
d2_1 <- generateSpirals(n=500)
d2_1$z <- (d2_1$z-2.5)
plot_ly(data = d2_1, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", marker=list(size=3),color=~Class, colors=c('#FF5B5B','#33CAD4'))
d2 <- d2_1 |> select(-Class)
p1 <- as.data.frame(predict(prcomp(d2))) |> ggplot(aes(PC1,PC2,color=d2_1$Class)) + geom_point() + labs (color='Class',title = 'Simple PCA')
p1
s1 <- cumsum(prcomp(d2)[[1]])/sum(prcomp(d2)[[1]])
data1 <- data.frame(Comp = paste("Comp.", 1:length(s1)),Variance_Explained= s1)
ggplot(data1, aes(x = Comp, y = s1)) + geom_bar(stat = "identity", fill = "violet") + labs(x = "Components", y = "Ratio of Variance") + geom_text(aes(label = sprintf("%.2f", s1)), vjust = -0.5) + ggtitle("Variance Explained by Simple PCA")

kpc <- kpca(~., data = as.data.frame(d2), kernel="polydot",kpar=list(degree=2, offset=-75), features=0)
p2 <- as.data.frame(kpc@pcv) |> ggplot(aes(V1,V2,color=d2_1$Class)) + geom_point() + labs(color='Class', title = 'Polynomial Kernel') + xlab('PC1') + ylab('PC2')
p2
s2 <- cumsum(kpc@eig)/sum(kpc@eig)
data2 <- data.frame(Comp = paste("Comp.", 1:length(s2)),Variance_Explained= s2)
ggplot(data2, aes(x = Comp, y = s2)) + geom_bar(stat = "identity", fill = "purple") + geom_text(aes(label = sprintf("%.2f", s2)), vjust = -0.5) + labs(x = "Components", y = "Ratio of Variance") + ggtitle("Variance Explained by Polynomial Kernel")

kpc <- kpca(~., data = as.data.frame(d2), kernel="rbfdot",kpar=list(sigma=0.4), features=0)
p3 <- as.data.frame(kpc@pcv) |> ggplot(aes(V1,V2,color=d2_1$Class)) + geom_point() + labs(color='Class', title = 'RBF Kernel') + xlab('PC1') + ylab('PC2')
p3
s3 <- cumsum(kpc@eig)/sum(kpc@eig)
data3 <- data.frame(Comp = 1:length(s3),Variance_Explained= s3)
ggplot(data3, aes(x = Comp, y = s3)) + geom_bar(stat = "identity", fill = "orange")+ geom_text(aes(label = sprintf("%.2f", s3)), vjust = -0.5) + labs(x = "Components", y = "Ratio of Variance") + ggtitle("Variance Explained by RBF Kernel")
