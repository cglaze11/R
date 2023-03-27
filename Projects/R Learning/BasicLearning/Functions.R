
# HeaderComments ----------------------------------------------------------

# You need to pick a name for the function. Here I’ve used rescale01 because this function rescales a vector to lie between 0 and 1.
# 
# You list the inputs, or arguments, to the function inside function. Here we have just one argument. 
  # If we had more the call would look like function(x, y, z).
# 
# You place the code you have developed in body of the function, a { block that immediately follows function(...).

# Sample1 -----------------------------------------------------------------

# sample one
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

x <- c(1:10,NA)
rescale01 <- function(x) {
  # results will be NA when having NA value without na.rm=TRUE
  rng <- range(x, na.rm = TRUE)
  # rng <- range(x)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)


# eliminate long chains ---------------------------------------------------------------------

# long chains of if statements
func_cut <- function(temp) {
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 10) {
    "cold"
  } else if (temp <= 20) {
    "cool"
  } else if (temp <= 30) {
    "warm"
  } else {
    "hot"
  }
}
# eliminate long chains of if statements with cut
# cut(c(-40), breaks=10*(0:3),labels=c("cold","cool","warm"))
func_cut2 <- function(temp) {
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 30) {
    cut(temp, breaks=10*(0:3),labels=c("cold","cool","warm"))
  } else {
    "hot"
  }
}

func_cut(40)
func_cut2(40)


# CheckValues -------------------------------------------------------------

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")
#> Error in wt_mean(1:6, 6:1, na.rm = "foo"): is.logical(na.rm) is not TRUE


# Dot-dot-dot -------------------------------------------------------------

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#> [1] "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
#> Important output -----------------------------------------------------------


# ReturnValues ------------------------------------------------------------

# explicit reeturn
func_cut2 <- function(temp) {
  if (temp <= 0) {
    retun("freezing")
  } else if (temp <= 30) {
    return(cut(temp, breaks=10*(0:3),labels=c("cold","cool","warm")))
  } else {
    return("hot")
  }
}

ret <- func_cut2(40)
# pipable functions
mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 
#> Missing values: 0
#> Missing values: 18
