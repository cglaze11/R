library(tidyverse)

# test data ---------------------------------------------------------------

m <- as.table(rbind(c(50, 250), c(5, 195)))
dimnames(m) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent"))

m <- as.matrix(m)

(Xsq <- chisq.test(m))

# formula - 1 2×2----------------------------------------------------------

exp <- tibble()
# degree of freedom
df <- (ncol(m)-1)*(nrow(m)-1)
# Adjustment of continuity
adj <- if (df == 1) 0.5 else 0
# compute the expected frequency value
for (i in 1:nrow(m) ) {   
  for (j in 1:ncol(m) ) { 
    .exp <- sum(m[i,1:ncol(m)])*sum(m[1:nrow(m),j])/sum(m)
    exp[i,j] <- (abs(m[i,j]-.exp)-adj)**2/.exp
  }
}

stat <- sum(exp)
p.value <- 1 - pchisq(stat, df=df, ncp=0, log = FALSE)

# formula - 2 2×2----------------------------------------------------------

exp <- tibble()
# degree of freedom
df <- (ncol(m)-1)*(nrow(m)-1)
# Adjustment of continuity
adj <- if (df == 1) 0.5 else 0
# compute the expected frequency value
for (i in 1:nrow(m) ) {   
  for (j in 1:ncol(m) ) { 
    .exp <- sum(m[i,1:ncol(m)])*sum(m[1:nrow(m),j])/sum(m)
    exp[i,j] <- .exp
  }
}

stat1 <- (abs(m[1,1]*m[2,2]-m[1,2]*m[2,1])-sum(m)/2)**2*sum(m)/+
  (sum(m[1,1:ncol(m)])*sum(m[2,1:ncol(m)])*sum(m[1:nrow(m),1]*sum(m[1:nrow(m),2])))
p.value1 <- 1 - pchisq(stat, df=df, ncp=0, log = FALSE)

# My Function -------------------------------------------------------------
# test 1
m <- as.table(rbind(c(50, 250), c(5, 195)))
dimnames(m) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent"))

m <- as.matrix(m)
# vector
c <- colSums(m)
c[1]

c <- as_vector(colSums(m))
c[1]
rowSums(m)
# test 2
m1 <- as.table(rbind(c(37,49,23), c(150,100,57)))
dimnames(m1) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent","Others"))

m1 <- as.matrix(m1)
# test 3
m2 <- as.table(rbind(c(67,9,10,5), c(32,23,20,4),c(10,11,23,5)))
dimnames(m2) <- list(gender = c("F", "M","O"),
                     party = c("Democrat","Independent","Others","None"))

m2 <- as.matrix(m2)

my_chisq <- function(x){
  # input:
  # x:a matrix
  n.row <- nrow(x)
  n.col <- ncol(x)
  df <- (n.row-1)*(n.col-1)
  # the sum of row
  r <- tibble()
  # the sum of col
  c <- tibble()
  
  for (i in 1:n.row ) {
    r[1,i] <- sum(x[i,1:n.col])
  }
  for (i in 1:n.col ) {
    c[1,i] <- sum(x[1:n.row,i])
  }
  total <- sum(x)
  
  # 2×2 tables
  if (n.row==2 & n.col==2) {
    stat <- ((abs(x[1,1]*x[2,2]-x[1,2]*x[2,1])-total/2)**2*total/(r[1,1]*r[1,2]*c[1,1]*c[1,2]))[1,1]
  }
  # 2×c tables
  else if(n.row==2 & n.col>=3) {
    oc <- tibble()
    for (j in 1:n.col){
      oc[1,j] <- x[1,j]**2/c[1,j]
    }
    stat <- ((total**2/r[1,1]/r[1,2])*(sum(oc)-r[1,1]**2/total))[1,1]
  }
  # r×c tables
  else if (n.row>=2 & n.col>=3) {
    orc <- tibble()
    for (i in 1:n.row){
      for (j in 1:n.col){
        orc[i,j] <- x[i,j]**2/r[1,i]/c[1,j]
      }
    }
    stat <- total*(sum(orc)-1)
  }

  p.value <- 1 - pchisq(stat, df=df, ncp=0, log = FALSE)
  # result <- list(stat=stat,pvalue=p.value)
  result <- c(stat,p.value)
  return(result)
}

my_chisq(m)
my_chisq(m1)
my_chisq(m2)

# My Function Advanced----------------------------------------------------------
# test 1
m <- as.table(rbind(c(60,32), c(3,11)))
dimnames(m) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent"))

m <- as.matrix(m)
# test 2
m1 <- as.table(rbind(c(37,49,23), c(150,100,57)))
dimnames(m1) <- list(gender = c("F", "M"),
                     party = c("Democrat","Independent","Others"))

m1 <- as.matrix(m1)
# test 3
m2 <- as.table(rbind(c(67,9,10,5), c(32,23,20,4),c(10,11,23,5)))
dimnames(m2) <- list(gender = c("F", "M","O"),
                     party = c("Democrat","Independent","Others","None"))

m2 <- as.matrix(m2)

my_chisq <- function(x){
  # input:
  # x:a matrix
  n.row <- nrow(x)
  n.col <- ncol(x)
  df <- (n.row-1)*(n.col-1)
  # the sum of row
  r <- rowSums(x)
  # the sum of col
  c <- colSums(x)
  total <- sum(x)
  
  # 2×2 tables
  if (n.row==2 & n.col==2) {
    stat <- ((abs(x[1,1]*x[2,2]-x[1,2]*x[2,1])-total/2)**2*total/(r[1]*r[2]*c[1]*c[2])) %>% as.vector()
  }
  # 2×c tables
  else if(n.row==2 & n.col>=3) {
    oc <- tibble()
    for (j in 1:n.col){
      oc[1,j] <- x[1,j]**2/c[j]
    }
    stat <- ((total**2/r[1]/r[2])*(sum(oc)-r[1]**2/total)) %>% as.vector()
  }
  # r×c tables
  else if (n.row>=2 & n.col>=3) {
    orc <- tibble()
    for (i in 1:n.row){
      for (j in 1:n.col){
        orc[i,j] <- x[i,j]**2/r[i]/c[j]
      }
    }
    stat <- total*(sum(orc)-1)
  }
  
  p.value <- 1 - pchisq(stat, df=df, ncp=0, log = FALSE)
  result <- c(stat,p.value)
  return(result)
}

my_chisq(m)
my_chisq(m1)
my_chisq(m2)
