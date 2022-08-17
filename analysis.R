library(ggplot2)
library(dplyr)
library(usdata)
library(plotly)
library(maps)
library(stats)
library(mapproj)

setwd("~/Documents/a3---data-visualization-and-applications-itsjasminesky")
incarceration_trends<- read.csv("~/Documents/a3---data-visualization-and-applications-itsjasminesky/incarceration_trends.csv")
mean_black_prison_pop <-mean(incarceration_trends[["black_prison_pop"]], na.rm = TRUE) #247.166
mean_native_prison_pop <- mean(incarceration_trends[["native_prison_pop"]], na.rm = TRUE) #7.29707
mean_latinx_prison_pop <- mean(incarceration_trends[["latinx_prison_pop"]], na.rm = TRUE) #101.4304
mean_white_prison_pop <- mean(incarceration_trends[["white_prison_pop"]], na.rm = TRUE) #162.7287
mean_aapi_prison_pop <- mean(incarceration_trends[["aapi_prison_pop"]], na.rm = TRUE) #7.345527
mean_total_prison_pop <- mean(incarceration_trends[["total_prison_pop"]], na.rm = TRUE) #361.3607
mean_black_prison_pop/mean_total_prison_pop #0.683987, 68.4%
mean_native_prison_pop/mean_total_prison_pop #0.02019331, 2%
mean_aapi_prison_pop/mean_total_prison_pop #0.020
mean_latinx_prison_pop/mean_total_prison_pop #0.28
mean_white_prison_pop/mean_total_prison_pop #0,45
state_highest_black_incarceration <- incarceration_trends %>%
  select(state, black_jail_pop, black_prison_pop) %>% replace(is.na(.), 0) %>%
  mutate(sum = black_jail_pop + black_prison_pop) %>% group_by(state) %>%
  summarise(total_incarceration = sum(sum, na.rm = TRUE)) %>% 
  filter(total_incarceration == max(total_incarceration, na.rm = TRUE))%>% 
  pull(state) #CA
county_with_highest_prisoners_in_WA <- incarceration_trends %>%
  filter(state == "WA") %>%   
  filter(year == max(year)) %>% 
  filter(total_pop == max(total_pop)) %>% 
  pull(county_name)
#King County

#Black incarceration per state

black_incarceration_year <- incarceration_trends %>%
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL", "OH", "GA", "NC", "MI")) %>%
  ggplot(aes(year,(black_prison_pop + black_jail_pop), color = state))+
  geom_point() +
  geom_path() +
  facet_wrap(~state, nrow = 2) +
  ggtitle("Black Incarceration (State) per year") +
  ylab("Black Incarceration")
  options(repr.plot.width =9, repr.plot.height = 9)

black_incarceration_year


#variable comparison

incarceration_trends %>%
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL", "OH", "GA", "NC", "MI")) %>%
  ggplot(aes(black_prison_pop, white_prison_pop, color = state), position = "fill") +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~state)

incarceration_white_vs_black <- incarceration_trends %>%
  select(year, black_jail_pop, black_prison_pop, white_jail_pop, white_prison_pop) %>% 
  replace(is.na(.), 0) %>%
  mutate(black_sum = black_jail_pop + black_prison_pop,
  white_sum = white_jail_pop + white_prison_pop) %>% 
  group_by(year) %>%
  summarise(black_sum = sum(black_sum), white_sum = sum(white_sum))

black_white_incarceration <- plot_ly(data = incarceration_white_vs_black, x = ~year,
   y = ~black_sum,
  name = "Black",
  type = 'scatter',
  mode = 'lines',
  width = "900") %>% 
  add_trace(y = ~white_sum, name = "White", mode = 'lines') %>% 
  layout(title = "Black vs. White Incarceration (Years)", xaxis = list(title = "Year"),
  yaxis = list(title = "Total Incarceration(#)")
)

black_white_incarceration

#filter

black_white_state <- incarceration_trends %>%
  select(state, black_jail_pop, black_prison_pop, white_jail_pop, white_prison_pop) %>%
  replace(is.na(.), 0) %>%
  group_by(state) %>%
  mutate(black_sum = black_jail_pop + black_prison_pop,
  white_sum = white_jail_pop + white_prison_pop) %>% 
  summarise(black_sum = sum(black_sum), white_sum = sum(white_sum)) %>% 
  mutate(rate = black_sum/white_sum) %>%
  select(-black_sum, -white_sum) %>%
  arrange(-rate) %>%
  slice(-c(1))

#map

state_shape <- map_data("state")

state_abbrevs <- data.frame(state.abb, state.name)

us_map <- left_join(black_white_state, state_abbrevs, by = c('state' = 'state.abb'))

us_map<- us_map %>% mutate(region = tolower(state.name))

us_map <- left_join(state_shape, us_map)

#minimalist theme map
  
  map_incarceration <- ggplot(us_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = rate),
    color = "white",
    size =.1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red", labels = 
  scales::label_number_si()) +
  labs(fill = "Ratio")+
  ggtitle("Incarceration Black / white ratio per state")
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  )

  map_incarceration



  


