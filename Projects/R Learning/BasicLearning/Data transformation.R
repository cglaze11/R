# https://r4ds.had.co.nz/transform.html

library(nycflights13)
library(tidyverse)

flights

#   To See the full dataset
view(flights)

###############################################################################################################
# Summary
# filter()  select rows
# arrange() order the data frame
# select()  select columns
# mutate()  create new variables
# function  :
    # arithmetic operators
    # modular operators
    # lag
    # offset:laed or lag
    # Cumulative and rolling aggregates : cumsum(), cumprod(), cummin(), cummax()
    # Logical comparisons, <, <=, >, >=, !=, and ==
    # Ranking
# summarize() Grouped summaries
    

###############################################################################################################

# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() which changes the scope of each function from operating on the entire dataset 
#   to operating on it group-by-group.

# print result
filter(flights, month == 1, day == 1)
# save to a variable
jan1 <- filter(flights, month == 1, day == 1)
# both print and save
(dec25 <- filter(flights, month == 12, day == 25))

sqrt(2) ^ 2 == 2
#> [1] FALSE
1 / 49 * 49 == 1
#> [1] FALSE
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE

#  %in% short-hand
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

# https://r4ds.had.co.nz/transform.html#exercises-8

# Find all flights that
#   Had an arrival delay of two or more hours
    filter(flights, dep_delay >= 120)
#   Flew to Houston (IAH or HOU)
    filter(flights, dest== 'IAH' | dest=='HOU')
    filter(flights, dest %in% c('IAH','HOU'))
#   Were operated by United, American, or Delta
    # filter(flights, dest %in% c('IAH','HOU'))
#   Departed in summer (July, August, and September)
    
#   Arrived more than two hours late, but didn’t leave late
#   Were delayed by at least an hour, but made up over 30 minutes in flight
#   Departed between midnight and 6am (inclusive)
#   Another useful dplyr filtering helper is between(). What does it do? 
#       Can you use it to simplify the code needed to answer the previous challenges?
#   How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
#   Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing?
#         Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

arrange(flights, desc(month),dep_delay = TRUE)
# arrange() orders the rows of a data frame by the values of selected columns.
# cending cyl and disp
arrange(mtcars, cyl, disp)
# cending cyl and descending disp
arrange(mtcars, cyl, desc(disp))


# grouped arrange ignores groups
by_cyl <- mtcars %>% group_by(cyl)
by_cyl
by_cy1 %>% arrange(desc(wt))
by_cy1
by_cyl %>% arrange(desc(wt), .by_group = TRUE)
by_cyl
# Unless you specifically ask:
by_cyl %>% arrange(desc(wt), .by_group = TRUE)

# use embracing when wrapping in a function;
# see ?dplyr_data_masking for more details
tidy_eval_arrange <- function(.data, var) {
  .data %>%
    arrange({{ var }})
}
tidy_eval_arrange(mtcars, mpg)

# use across() access select()-style semantics
iris %>% arrange(across(starts_with("Sepal")))
iris %>% arrange(across(starts_with("Sepal"), desc))

    
# Missing values are always sorted at the end:


daily <- group_by(flights, year, month, day)
daily
