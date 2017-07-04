library(ggplot2) # or: library(tidyverse)
?mpg

g <- ggplot(mpg, aes(x = displ, y = hwy))
g + geom_point(aes(colour = drv))

g + geom_point() + facet_wrap(~drv)

ggplot(mpg, aes(x = displ, y = hwy, colour=factor(cyl)))+
  geom_point()+facet_wrap(~year)+geom_smooth(method='lm')+
  scale_y_log10(breaks = c(20, 30, 40))+scale_x_log10(breaks = c(2, 3, 4, 5, 6, 7))

library(dplyr)

library(tidyverse)
data(mpg, package = "ggplot2")
mpg %>% tbl_df

mpg %>% select(manufacturer, model, displ, year, cyl, trans, cty, hwy)

mpg2 <- mpg %>% select(manufacturer, model, displ, year, cyl, trans, cty, hwy)

mpg2 %>% mutate(displ2 = displ * displ, vol_per_cyl = round(displ / cyl, 2))

mpg3 <- mpg2 %>% mutate(displ2 = displ * displ, vol_per_cyl = round(displ / cyl, 2))

mpg3 %>% filter(manufacturer == 'chevrolet') %>% arrange(desc(vol_per_cyl))

mpg3 %>% group_by(manufacturer, year) %>% summarise(max_vol_per_cyl = max(vol_per_cyl))

mpg4 <- mpg3 %>% group_by(manufacturer, year) %>% summarise(max_vol_per_cyl = max(vol_per_cyl))

mpg4 %>% spread(year, max_vol_per_cyl)

mpg5 <- mpg4 %>% spread(year, max_vol_per_cyl)

mpg5 %>% mutate(change = `2008`-`1999`)

mpg6 <- mpg5 %>% mutate(change = `2008`-`1999`)

mpg6 %>% rename(max_vpc_2008 = `2008`, max_vpc_1999 = `1999`)

mpg6 %>% rename(max_vpc_2008 = `2008`, max_vpc_1999 = `1999`) %>% gather(variable, value, max_vpc_1999, max_vpc_2008) %>% select(everything(), -change) %>% View

install.packages("nycflights13")
library(nycflights13)

flights %>% tbl_df
airlines %>% tbl_df
weather %>% tbl_df

flights2 <- flights %>% select(origin, year, month, day, hour, 
                               sched_dep_time, dep_delay, carrier)
weather2 <- weather %>% select(origin, year, month, day, hour, 
                               precip, wind_speed, visib)
inner_join(flights, airlines, by = "carrier")
flights2 %>% full_join(weather2)

weather2 %>% summarise(min_precip = min(precip, na.rm = TRUE), 
                       min_wind = min(wind_speed, na.rm = TRUE), 
                       max_visib = max(visib, na.rm = TRUE))                                     

good_weather_delays <- flights2 %>% inner_join(weather2) %>% filter(visib == 10, wind_speed == 0, precip==0)

good_weather_delays %>% group_by(carrier) %>% summarise(dep_delay = 
                                                          mean(dep_delay,na.rm = TRUE)) %>% arrange(desc(dep_delay)) %>% left_join(airlines)


