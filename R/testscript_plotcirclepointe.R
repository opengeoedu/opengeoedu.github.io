x <- 3
y <- 4

n <- 12
r <- 4


plot(c(x, x+r*sin(0:20/10*pi)),c(y, y+r*cos(0:20/10*pi)), type="l")


vec <- seq(from=0, by=2*pi/n, length.out = n)
lon <- c(x+r*sin(vec))
lat <- c(y+r*cos(vec))

points(lon,lat, col="red")

