install.packages("plot3D")
library(plot3D)

x <- runif(3000, -5,5)
y <- runif(3000, -5,5)

z <- 2*x^2 +2*y^2 -4

z2 <- 2*x^2 +2*y^2 +4

A1 <- cbind(x,y,z)
A2 <- cbind(x,y,z2)

B <- rbind(A1,A2)

plot3D::scatter3D(B[,1],B[,2],B[,3])


plot()