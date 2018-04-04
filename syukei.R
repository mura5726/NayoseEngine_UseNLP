library(tidyverse)
library(readxl)
x <- "all"
func.test <- 
  function(x){
    
    data.name.list <- c("all", "m1", "m2", "m3", "f1", "f2", "f3", "teen", "m20", "m30", "m40", "m50", "m60", "f20", "f30", "f40", "f50", "f60")

    mat.name <- read.csv("matrix_name.csv") %>% as.tibble() %>% mutate_if(is.factor, as.character)
    dat <- read_excel("重複率_20180402_add_readR.xlsx", 4 + which(data.name.list == x))[-c(1:13),]
    data.list <- list(dat)
    
    o <- mat.name %>% select(output)
    
    m.rep <- mat.name %>% select(メディア) %>% as.matrix() %>% rep(dim(mat.name)[1])
    m.rep.each <- mat.name %>% select(メディア) %>% as.matrix() %>% rep(each = dim(mat.name)[1])
    
    v.rep <- mat.name %>% select(ビークル) %>% as.matrix() %>% rep(dim(mat.name)[1])
    v.rep.each <- mat.name %>% select(ビークル) %>% as.matrix() %>% rep(each = dim(mat.name)[1])
    
    o.rep <- mat.name %>% select(output) %>% as.matrix() %>% rep(dim(mat.name)[1])
    o.rep.each <- mat.name %>% select(output) %>% as.matrix() %>% rep(each = dim(mat.name)[1])
    
    o_raw <- dat %>% select(X__3, X__4) %>% mutate(output_raw = paste(X__3, "_", X__4, sep = "")) %>% select(output_raw) %>% as.matrix()
    
    match.o <- data.frame(i = 1, which = ifelse(length(which(o == o_raw[1, ])) == 0,NA,which(o == o_raw[1, ])), o_raw = o_raw[1, ])
    
    for(i in 2:dim(o_raw)[1]){
      match.o <- rbind(match.o, data.frame(i = i, which = ifelse(length(which(o == o_raw[i, ])) == 0,NA,which(o == o_raw[i, ])), o_raw = o_raw[i, ]))
    }
    
    match.o <- 
      match.o %>% as.tibble() %>% mutate_if(is.integer ,as.character) %>% mutate_if(is.factor ,as.character) %>% mutate_if(is.double ,as.character)
    
    na.list <- match.o %>% as.tibble() %>% filter(is.na(which))
    
    o.rep <- mat.name %>% select(output) %>% as.matrix() %>% rep(dim(mat.name)[1])
    o.rep.each <- mat.name %>% select(output) %>% as.matrix() %>% rep(each = dim(mat.name)[1])
    
    d <- data.frame(m.rep.each, v.rep.each, o.rep.each, m.rep, v.rep, o.rep) %>% as.tibble() %>% mutate_if(is.factor, as.character)%>%
      left_join(match.o, by = c("o.rep.each" = "o_raw")) %>% rename(i.rep.each.mun = which, row = i) %>% 
      left_join(match.o, by = c("o.rep" = "o_raw")) %>% rename(i.rep.mun = which, col =i)
    
    rows <- d %>% select(row) %>% as.matrix() %>% as.numeric() 
    cols <- d %>% select(col) %>% as.matrix() %>% as.numeric() %>% head(644)
    
    ex.dat <- dat %>% as.matrix()
    sum_num <- dat[5,6] %>% as.numeric()
    
    j = 0
    v <- ex.dat[rep(cols[(1+j*10):(10+j*10)],644), rep(cols+1 ,each = 10)] %>% diag() %>% as.numeric()
    
    for(j in 1:63){
      v <- append(v, ex.dat[rep(cols[(1+j*10):(10+j*10)],644), rep(cols+1 ,each = 10)] %>% diag())
      print(j)
    }
    
    v <- append(v, ex.dat[rep(cols[(1+j*10):(10+j*10)],644), rep(cols+1 ,each = 10)] %>% diag())
  
    return(v)
    }

v <- func.test("all")
head(v)
v[645]

k = 1
ex.dat <- data.list[[k]][-c(1:4), -c(1:5)][-1,-1]
a = ex.dat[rows, cols] %>% diag()

mat.test <- matrix(c(11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55),5,5) %>% t()
mat.test[c(1,1,1,2,2,2),c(2,3,1,2,3,1)] %>% diag()

c(1,1,1,2,2,2) %>% as.tibble()

# for(k in 1:18){
#   ex.dat <- data.list[[k]][-c(1:4), -c(1:5)][-1,-1]
#   v <- array()
#   for(j in 1 : dim(d)[1]){
#     a = ex.dat[as.integer(rows[j, ]), as.integer(cols[j, ])] %>% as.integer()
#     v <- append(v, a)
#   }
#   vk <- v[-1] %>% as.tibble()
#   d <- d %>% mutate(vk)
# }

