install.packages("math")

x <- s
y <- tan(s)

w <- rnorm(50,2,0.3)
z <- rnorm(50,2,0.3)

xy <- cbind(x,y)
wz <- cbind(w,z)

whole <- rbind(xy,wz)

s<- seq(-2*pi, 2*pi,length.out =  1000)
s <- s[tan(s) <2 & tan(s) >-2]
plot(whole)
a <- runif(100,-2*pi,2*pi)

plot(s, tan(s))
par(new=TRUE)

plot(x,y)


y<- runif(20000,-5,5)
x<- runif(20000,-5,5)

x1 <- x[x^2+y^2 <= 0.5]
y1 <- y[x^2+y^2 <= 0.5]

xy1 <- cbind(x1,y1)

x2 <- x[x^2+y^2 >= 1 & x^2+y^2 <= 2 ]
y2 <- y[x^2+y^2 >= 1 & x^2+y^2 <= 2 ]

xy2 <- cbind(x2, y2)

x3 <- x[x^2+y^2 >= 4 & x^2+y^2 <= 5]
y3 <- y[x^2+y^2 >= 4 & x^2+y^2 <= 5]

xy3 <- cbind(x3,y3)

A <- rbind(xy1, xy2)
A <- rbind(A, xy3)

plot(A)


