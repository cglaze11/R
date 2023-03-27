
# Add Null Rows To Dada frame -----------------------------------------------
# Method 1:rbind()

library(berryFunctions)

# Method 2:
#    data2[nrow(data) + 1, ] <- new_row 

nl <- final %>% group_by(visitn,stra) %>% 
  mutate(
    n=row_number(),
    n2=n()
  )

nl2 <- nl

which(nl2$n == nl2$n2)

inst <- which(nl2$n == nl2$n2)+c(1:20) %>% as.vector()
inst[20]

# nl3 <- insertRows(nl2,inst,new=NA,rcurrent =FALSE)
nl3 <- insertRows(nl2,inst[1:19],new=NA,rcurrent =FALSE)

nl4 <- insertRows(nl3,100,new=NA,rcurrent =FALSE)

nl4 <- insertRows(nl2,81,new=NA,rcurrent =FALSE)

nl2$para[nl2$n == nl2$n2]

nl2[5 + 1, ] <- NA

nl2[5,1][nl2$n == nl2$n2] <- NA



# Fill missing value ------------------------------------------------------

# Value (year) is recorded only when it changes
sales <- tibble::tribble(
  ~quarter, ~year, ~sales,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197,
  "Q1",    2002,    39113,
  "Q2",      NA,    41668,
  "Q3",      NA,    30144,
  "Q4",      NA,    52897,
  "Q1",    2004,    32129,
  "Q2",      NA,    67686,
  "Q3",      NA,    31768,
  "Q4",      NA,    49094
)

# the data must be a data frame
sales %>% fill(year)

