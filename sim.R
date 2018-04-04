install.packages("tidyverse")
library(tidyverse)

# seq(11, 6441, 10)
seq.r <- function(x) seq(x + 10, x + 6440, 10)

# seq.r <- function(x) seq(x + 10, x + 200, 10)

# seq.r(1)
# seq.r(2)
# seq.r(3)
# seq.r(10)


# apply(1:3, 1, seq.r)

m <- matrix(0,644,644)
for(i in 1:644){
  m[,i] <- seq.r(i)
}

m[c(1,2,3,4,3,2,1),c(4,3,5,3,3,4,5)] %>% diag()

j = 0
v <- m[rep((1+j*10):(10+j*10),644),rep((10+j*10):(1+j*10),644)] %>% diag()

for(j in 1:63){
  v <- append(v, m[rep((1+j*10):(10+j*10),644),rep((10+j*10):(1+j*10),644)] %>% diag())
  print(j)
  }

v <- append(v, m[rep(641:644,644),rep(641:644,644)] %>% diag())

length(v)

644*644
