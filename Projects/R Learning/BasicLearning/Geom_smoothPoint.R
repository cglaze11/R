

# https://r4ds.had.co.nz/data-visualisation.html#exercises-3
# Recreate the R code necessary to generate the following graphs.

# exercise 1
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy),se=FALSE,color="blue")

# exercise 2
ggplot() +
        geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
        geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, group = drv),se=FALSE,color="blue")

# exercise 3
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, color = drv),se=FALSE)

# exercise 4
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy),se=FALSE,color="blue")

# exercise 5
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy,linetype = drv),se=FALSE,color="blue")

# exercise 6
# Non Solved
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv,stroke=1),color="white") 

# https://r4ds.had.co.nz/data-visualisation.html#statistical-transformations
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
#display a bar chart of proportion
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

#
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))


# https://r4ds.had.co.nz/data-visualisation.html#position-adjustments

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# https://r4ds.had.co.nz/data-visualisation.html#coordinate-systems
# Coordinate systems
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# assignment the ggplot commands to the object :bar
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#   The layered grammar of graphics
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>, 
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>