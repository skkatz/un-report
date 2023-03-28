library(tidyverse)

#loading in data set and name it
gapminder_1997 <- read_csv("gapminder_1997.csv")
#summery statistics
str(gapminder_1997)

?read_csv

sum(5,6)
round(3.1415, 3)
round(2, 3.1415)
round(x = 3.1415, digits = 2)
#ways to make positional arguments

#reading an excel file
#readxl
  
#builing a plot and creating layers
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) + 
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)")

#All together
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title =  "Do people in wealthy countries live longer?", 
       size = "Population (in millions)")


# Load in a larger data set
gapminder_data <- read_csv("gapminder_data.csv")

dim(gapminder_data)


ggplot(data = gapminder_data) +
  aes(x= year, y = lifeExp, grouo = country, color = continent) +
  geom_line()


# Discrete Plots
ggplot(data = gapminder_1997) + 
  aes(x= continent, y = lifeExp) + 
  geom_boxplot()

ggplot(data = gapminder_1997) + 
  aes(x= continent, y = lifeExp) + 
  geom_violin() + 
  geom_point()
#can add another geom layer

ggplot(data = gapminder_1997) + 
  aes(x= continent, y = lifeExp) + 
  geom_violin() + 
  geom_jitter()
#can see trends along with plotted points
#order you feed layers in affects the plot
ggplot(data = gapminder_1997) + 
  aes(x= continent, y = lifeExp) + 
  geom_jitter() +
  geom_violin()
#can ascribe aes to specfific geom

# Master Aesthetics
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin() +
  geom_jitter(aes(size = pop), color = "pink")

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(fill = "pink")

#now assigning colors to variable in data (continent)
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = continent))

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(fill = "springgreen")


#Univariate plots

ggplot(gapminder_1997) + 
  aes(x = lifeExp) +
  geom_histogram(bins = 15)
# Bins() --> affect how many bins data is put into
# binwidth --> how wide bins are

# Plot themes

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic() +
  theme(axis.text.x = 
          element_text(angle = 90, 
                       vjust = 0.5, 
                       hjust = 1))

# FACET WRAP
# ways to separate out data by variable and wrapping sub plot around each other
# good way tease out data
ggplot(gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))
# vars --> variable function

# facet grid
awesome_plot <- ggplot(gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))


# save plot as object 
ggsave("awesome_plot.jpg", width = 6, height = 4)
ggsave(awesome_plot, file = "awesome_plot.jpg", width = 6, height = 4)

#Exercise:
# -violin plot of continent and life expectancy
# color mapped to continent
# assigned to an object "violin plot"
# black and white theme
# save as "awesome_violin_plot.jpg"

awesome_violin_plot <- ggplot(gapminder_1997) +
  aes(x = continent, y = lifeExp) + 
  labs(y = "Life Expectancy", x = "Continent") +
  geom_violin(aes(fill = continent)) +
  theme_bw()

ggsave(awesome_violin_plot, file = "awesome_violin_plot.jpg", width = 6, height = 4)

 

