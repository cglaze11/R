####################################################################################################
# https://r4ds.had.co.nz/relational-data.html
# the connect between mutiple tables
# this charpter is the same as proc sql in SAS,
# 1.mutate joins
#     -inner join
#     -outer join
#         -left join 
#         -right join
#         -full join
#     -key
#         -three kands of key format
# 2.filter joins
#     -semi_join
#     -anti_join
# the difference between filter joins and mutate joins is without duplicate observises.
# 3.joins problems
#     -Start by identifying the variables that form the primary key in each table while understanding the real mean of the variable
#     -Check that none of the variables in the primary key are missing. If a value is missing then it can’t identify an observation!
#     -Check that your foreign keys match primary keys in another table. The best way to do this is with an anti_join().
# 4.set operations
      # -intersect(x, y): return only observations in both x and y.
      # -union(x, y): return unique observations in x and y.
      # -setdiff(x, y): return observations in x, but not in y.
####################################################################################################
library(tidyverse)
library(nycflights13)


# relation
# Mutating joins, which add new variables to one data frame from matching observations in another.
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
# left join by key[carrier]:like left join in PROC SQL in SAS
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier") %>% 
  view()
# the secend method of left_join
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
# Filtering joins, which filter observations from one data frame based on whether or not they match an observation in the other table.

# Set operations, which treat observations as if they were set elements.


# keys
# A primary key uniquely identifies an observation in its own table. 

# A foreign key uniquely identifies an observation in another table. 

# a surrogate key : add one with mutate() and row_number()

####################################################################################################
# keys are unique
# four mutating join functions: the inner join, and the three outer joins.
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
# the simplest join:inner join
x %>% 
  inner_join(y, by = "key")
#> # A tibble: 2 x 3
#>     key val_x val_y
#>   <dbl> <chr> <chr>
#> 1     1 x1    y1   
#> 2     2 x2    y2
#################################################
# An inner join keeps observations that appear in both tables. 
# An outer join keeps observations that appear in at least one of the tables. 
# There are three types of outer joins:
#     A left join keeps all observations in x.
x %>% 
  left_join(y, by = "key")
#     A right join keeps all observations in y.
x %>% 
  right_join(y, by = "key")
#     A full join keeps all observations in x and y.
x %>% 
  full_join(y, by = "key")

####################################################################################################
# keys are not unique
# One table has duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key") 

# Both tables have duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

# three kands of key column
# by = NULL
# by = "variable"
# by = c("a" = "b")

####################################################################################################
# other method to attach the join
# mereg
# dplyr	              merge
# inner_join(x, y)	  merge(x, y)
# left_join(x, y)	    merge(x, y, all.x = TRUE)
# right_join(x, y)	  merge(x, y, all.y = TRUE),
# full_join(x, y)	    merge(x, y, all.x = TRUE, all.y = TRUE)

# SQL
# dplyr	                        SQL
# inner_join(x, y, by = "z")	  SELECT * FROM x INNER JOIN y USING (z)
# left_join(x, y, by = "z")	    SELECT * FROM x LEFT OUTER JOIN y USING (z)
# right_join(x, y, by = "z")	  SELECT * FROM x RIGHT OUTER JOIN y USING (z)
# full_join(x, y, by = "z")	    SELECT * FROM x FULL OUTER JOIN y USING (z)

####################################################################################################
# filter joins
# filtering joins never have duplicate rows like mutating joins
# semi_join(x, y) keeps all observations in x that have a match in y.
# anti_join(x, y) drops all observations in x that have a match in y.


####################################################################################################
# set operations
# These expect the x and y inputs to have the same variables, and treat the observations like sets
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)
# -intersect(x, y): return only observations in both x and y.
intersect(df1, df2)
# -union(x, y): return unique observations in x and y.
union(df1, df2)
# -setdiff(x, y): return observations in x, but not in y.
setdiff(df1, df2)
setdiff(df2, df1)


