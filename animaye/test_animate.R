pacman::p_load(gapminder,               # Spatial files
               ggplot2,        # File locator
               gganimate,    # data management + ggplot2 graphics
               gifski,
               png# handle time series datasets
          
               
)



data(gapminder)

p =  ggplot(
  gapminder,
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p + transition_time(year)
