
# Names And Values --------------------------------------------------------

a <- 1:10
b <- a
c <- b
d <- 1:10

obj_addr(a)
obj_addr(b)
obj_addr(c)
obj_addr(d)


x <- c(1L, 2L, 3L)
tracemem(x)
x
x[[3]] <- 4


mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)
#> ??? [1:0x7fe114251578] <chr> 
#> й└йд[2:0x7fe10ead1648] <string: "a"> 
#> й└йд[2:0x7fe10ead1648] 
#> й└йд[3:0x7fe11b27d670] <string: "abc"> 
#> й╕йд[4:0x7fe10eda4170] <string: "d">


a <- 1:2
b <- list(a, a)
c <- list(b, a, 1:2)
ref(a)
ref(b)
ref(c)


x <- list(1:10)
x[[2]] <- x
ref(x)

funs <- list(mean, sd, var)
obj_size(funs)
#> 17,608 B


a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

ref(b)
b[[1]][[1]] <- 10
ref(b)
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
ref(b)
obj_size(b)
obj_size(a, b)


x <- list()
ref(x)
obj_size(x)
x[[1]] <- x
ref(x)


c(1, FALSE)
c("a", 1)
c(TRUE, 1L)


x <- c("b", "c", "a")
ref(x)
y <- x[order(x)]
ref(y)


ifelse(TRUE, 1, "no")
ifelse(FALSE, 1, "no")
ifelse(NA, 1, "no")