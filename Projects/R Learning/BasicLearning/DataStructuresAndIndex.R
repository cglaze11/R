
# Vector ------------------------------------------------------------------

# Vector
# one-dimensional data structure
# support index like c[i]
# when the number i is larger than the number of a vector,it returns NA
# null vector output as numeric(0) or character(0)

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)

1/x

c <- c(x,0,1/x)

c[1]
c[14]


# Vector Manipulation -----------------------------------------------------

# Vectors occurring in the same expression need not all be of the same length. 
#   If they are not, the value of the expression is a vector with the same length as the longest vector 
#   which occurs in the expression. Shorter vectors in the expression are recycled as often as 
#   need be (perhaps fractionally) until they match the length of the longest vector.



# Index Vectors -----------------------------------------------------------

# logical vector
x[!is.na(x)]

# A vector of positive integral quantities
x[1:10]
c("x","y")[rep(c(1,2,2,1), times=4)]
# rep(c(1,2,2,1), times=4) is same as c(1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1)
# index vector with another vector
c("x","y")[c(1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1)]

# A vector of negative integral quantities.
x[-(1:4)]

# A vector of character strings
fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "banana", "apple", "peach")
fruit[c("apple","orange")]


attr(fruit, "dim") <- c(2,2) %>% attributes()


# Array -------------------------------------------------------------------

# a vector
z <- c(1:15)
z
# convert a vector to a 3×5 array
# index with x[i,j,k...] due to the array dimension
dim(z) <- c(3,5)
z[1,2]
attributes(z)

# when an array is one-dimensional,it's usually treated to a vector.

a <- c(1:16)
dim(a) <- c(2,4,2)
c(a[2,1,1], a[2,2,1], a[2,3,1], a[2,4,1],
  a[2,1,2], a[2,2,2], a[2,3,2], a[2,4,2])


# Index Matrix ------------------------------------------------------------

x <- array(1:20, dim=c(4,5))   # Generate a 4 by 5 array.
x

i <- array(c(1:3,3:1), dim=c(3,2))
i                             # i is a 3 by 2 index array.

x[i]                          # Extract those elements

x[i] <- 0                     # Replace those elements by zeros.
x


# Recycling Rule ----------------------------------------------------------

# Mixed vector and array arithmetic. The recycling rule
# The precise rule affecting element by element mixed calculations with vectors and 
#   arrays is somewhat quirky and hard to find in the references. From experience we have found the following to be a reliable guide.

# 1.The expression is scanned from left to right.
# 2.Any short vector operands are extended by recycling their values until they match the size of any other operands.
# 3.As long as short vectors and arrays only are encountered, the arrays must all have the same dim attribute or an error results.
# 4.Any vector operand longer than a matrix or array operand generates an error.
# 5.If array structures are present and no error or coercion to vector has been precipitated, 
#     the result is an array structure with the common dim attribute of its array operands.


# Matrix ------------------------------------------------------------------

# a matrix is just an array with two subscripts



# List --------------------------------------------------------------------

# an object consisting of an ordered collection of objects known as its components.

# a list could consist of a numeric vector, a logical value, a matrix, a complex vector,
#   a character array, a function, and so on.

# index like x[[i]][j] or x$name

Lst <- list(name="Fred", wife="Mary", no.children=3,
            child.ages=c(4,7,9))

Lst[[4]][1]


# Data Frame --------------------------------------------------------------

# A data frame is a list with class "data.frame". 
#   There are restrictions on lists that may be made into data frames, namely

# The components must be vectors (numeric, character, or logical), 
#   factors, numeric matrices, lists, or other data frames.

# Matrices, lists, and data frames provide as many variables to the new data frame 
#   as they have columns, elements, or variables, respectively.

# Vector structures appearing as variables of the data frame must all have the same length,
#   and matrix structures must all have the same number of rows.

# x$var

# df["Column Name"][df["Column Name"] == "Old Value"] <- "New Value"

# Factor ------------------------------------------------------------------

# A factor is a vector object used to specify a discrete classification (grouping) of 
#   the components of other vectors of the same length
