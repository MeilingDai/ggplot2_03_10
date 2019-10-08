.libPaths(c("C:/Users/DAI035/Data School/Packages"))
library(tidyverse)
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv")
head(gapminder)
gapminder_1977 <- gapminder %>% filter(year == 1977)
gapminder_1977
ggplot (data = gapminder_1977)
ggplot(data = gapminder_1977, 
       mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop))
ggplot(data = gapminder_1977, 
       mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
       ) + geom_point()

glimpse(gapminder_1977)
str(gapminder_1977)

ggplot(data = gapminder_1977, 
       mapping = aes(x = pop, y = lifeExp, colour = continent, size = gdpPercap)
       ) + geom_point() +
  scale_x_log10()

ggplot(data = gapminder_1977, 
       mapping = aes(x = continent, y = lifeExp, colour = continent, size = gdpPercap)
) + geom_point() 

ggplot(data = gapminder_1977, 
       mapping = aes(x = country, y = lifeExp, colour = continent, size = gdpPercap)
) + geom_point() 

ggplot(data = gapminder_1977, 
       mapping = aes(x = continent, y = lifeExp, colour = gdpPercap )
) + geom_point() 
  
ggplot (data = gapminder_1977) + 
  geom_point (mapping = aes (x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
                scale_x_log10()
              ? geom_point()


ggplot(data = gapminder_1977, 
       mapping = aes(x = pop, y = lifeExp, colour = continent, size = gdpPercap)
) + geom_point() +
  scale_x_log10()

ggplot(data = gapminder_1977, aes(x = pop, y = lifeExp, colour = continent))+
  geom_point( alpha = 0.3) +
  scale_x_log10()


ggplot(data = gapminder_1977, aes(x = pop, y = lifeExp, colour = continent))+
  geom_point( ) +
  scale_x_log10()

ggplot(data = gapminder_1977, aes(x = pop, y = lifeExp, colour = continent))+
  geom_point( shape = "star", alpha = 0.3) +
  scale_x_log10()


ggplot(data = gapminder, 
       mapping = aes(x = year, y = lifeExp, colour = continent, size = pop)) +
  geom_point()

ggplot(data = gapminder, 
        aes(x = year, y = lifeExp)) +
                 geom_point()

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = continent, colour = continent)) +
  geom_point()

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = continent, color = continent)) +
  geom_line()+
  geom_point(colour = "black")
  

  
ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country, color = continent)) +
  geom_point() +
  geom_line()

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
  geom_point(alpha = 0.4)+
  scale_x_log10()
 

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, alpha = 0.4)) +
  geom_point(alpha = 1)+
  scale_x_log10()

gapminder_mean <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifeExp = mean (lifeExp))
gapminder_mean    


?geom_smooth

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, group = country)) +
  geom_line(aes(colour = continent))+
  geom_point()
  

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, group = country)) +
 geom_point()+
  geom_line(aes(colour = continent))

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, size = pop))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+ 
  geom_smooth(method = "lm", size = 2)


ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, size = pop))+
  geom_point(alpha = 0.5)+
  scale_x_log10()

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, size = pop))+
  geom_point(aes(colour = continent), alpha = 0.9)+
  scale_x_log10()+
  scale_colour_manual(values = c("red", "blue", "purple", "yellow", "black"))

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)


#challenge 9: Modify the color and size of the points on the point layer in the previous example. 
#Hint: do not use the aes function.
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = "green", size = 1.5) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)


#challenge 10: Modify your solution to Challenge 9 so that 
#the points are now a different shape and are colored by continent with new trendlines. 
#Hint: The color argument can be used inside the aesthetic
#solution 1: 
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent, shape = continent)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1)

#solution 2: 
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent, shape = continent)) +
  geom_point(size = 2) + 
  scale_x_log10() + 
  scale_colour_brewer (palette = "Set1")
  geom_smooth(method = "lm", size = 1.5)
  
  

#the points are now a different shape and are colored by continent but only with 1 trendline. 
  ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))+ 
    geom_point(aes(colour = continent), size = 2) + 
    scale_x_log10() + 
    scale_shape_manual(values = c(0, 5, 8, 9, 12))
    geom_smooth(method = "lm", size = 1.5)
  
  
#chanllenhe 11: Try modifying the plot above by changing some colours in the scale to see if you can find a pleasing combination. Run the colours() function if you want to see a list of colour names R can use.
  ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
    geom_point(size = 2, shape = "square") + 
    scale_x_log10() +
    scale_colour_brewer(palette = "Set1"  )
    geom_smooth(method = "lm", size = 1.5)
    

    # break data into several small datasets by facet
    a_countries <- gapminder %>% 
      filter(str_starts(country, "A"))
    a_countries
ggplot(data = a_countries, 
       mapping = aes(x = year, y = lifeExp, colour = continent, group = country)
       ) + geom_line()


ggplot(data = a_countries, 
       mapping = aes(x = year, y = lifeExp, colour = continent, group = country)
) + geom_line()+
  facet_wrap(~country)

gapminder_1977
ggplot(data = gapminder_1977, mapping = aes(x= gdpPercap, y = lifeExp, colour = continent, size = pop)
       )+ geom_point()+
  scale_x_log10()


#chanllenge 12: #seperate data by year
head(gapminder)
ggplot( data = gapminder, mapping = aes(x= gdpPercap, y = lifeExp, colour = continent, size = pop)
)+ geom_point()+
  scale_x_log10()+
  facet_wrap(~year)

#seperate data by continent:
ggplot( data = gapminder, mapping = aes(x= gdpPercap, y = lifeExp, colour = continent, size = pop)
)+ geom_point()+
  scale_x_log10()+
  facet_wrap(~continent)



ggplot( data = gapminder_1977, mapping = aes(x= gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
)+ geom_point()+
  scale_x_log10()+
  geom_text()
  
  
gapminder_rich <- filter(gapminder_1977, gdpPercap > 30000)
gapminder_rich
ggplot( data = gapminder_1977, mapping = aes(x= gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
)+ geom_point()+
  scale_x_log10()+
  geom_text(data = gapminder_rich)


rough_plot <- ggplot(data = a_countries, 
       mapping = aes(x = year, y = lifeExp, colour = continent, group = country)
) + geom_line()+
  facet_wrap(~country)
rough_plot

rough_plot + scale_colour_brewer(palette = "Dark2") 
rough_plot + labs (title = "Fig1", 
                   x = "year",
                   y = "life Expectancy",
                   colour = "continent")


rough_plot + labs (title = "Fig1 correlation of year and life expectancy for A-countries", 
                   x = "year",
                   y = "life Expectancy",
                   caption = "Data source: Gapminder",
                   colour = "continent")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 6))

rough_plot + labs (title = "Fig1 correlation of year and life expectancy for A-countries", 
                   x = "year",
                   y = "life Expectancy",
                   caption = "Data source: Gapminder",
                   colour = "continent")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 12))
