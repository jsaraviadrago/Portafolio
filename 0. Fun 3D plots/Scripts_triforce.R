library(ggplot2)
library(data.table)

dt.triangle <- data.table(group = c(1,1,1), 
                          polygon.x = c(2,4,3), 
                          polygon.y = c(1,1,3))

p <- ggplot()
p <- p + geom_polygon(
  data = dt.triangle
  ,aes(
    x=polygon.x
    ,y=polygon.y
    ,group=group
  )
)
p