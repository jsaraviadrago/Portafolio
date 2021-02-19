library(sf)
# Coordinate seperate in grids for simplicity
#x <- c(0,1,2,3,1,2,4)
#y <- c(0,3,0,3,3,6,0)

# Plotting with grids so I can have an idea of measurement
#plot(x,y)

# Creating the triforce
multipoint <-  st_multipoint(matrix(c(0,1,2,3,1,2,4,0,3,0,3,3,6,0),
                                    ncol = 2))

# Creating the polygon
polyg <-  st_cast(multipoint, "POLYGON")

# Plotting the 
plot(polyg, col = "Gold")
