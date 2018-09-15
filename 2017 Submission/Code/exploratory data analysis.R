---
  title: "EDA"
output: html_notebook
---
  
  ```{r}
library(tidyverse)
```

```{r}
library(readr)
ridership <- read_csv("~/Desktop/Datathon/ridership_new_new.csv")

hour_segment <- function(hour) {
  t = "Null"
  if (hour > 6 && hour < 10) {
    t = "morning"
  } else if (hour > 17 && hour < 9) {
    t = "evening"
  }
  return (t)
}


is_morning = rep("Other", dim(ridership)[1])
is_morning[ridership$hour > 5 & ridership$hour < 10] =  "morning"
is_morning[ridership$hour > 17 & ridership$hour < 21] =  "evening"

p = ridership %>% 
  mutate(Year = year(day), morning = is_morning) %>%
  group_by(day_of_week, Year, morning) %>%
  summarise(total_trip = sum(trip_count))


pp = p %>%
  ggplot(aes(x = Year, y = total_trip)) + 
  geom_bar(stat = "identity", aes(fill = morning), position = "dodge") + 
  facet_wrap(~ day_of_week)


per_day = ridership %>% 
  mutate(Year = year(day), morning = is_morning) %>%
  group_by(day, Year, morning) %>%
  summarise(total_trip = sum(trip_count))

per_day_plot = per_day %>%
  gplot(aes(x = day, y = total_trip)) + 
  geom_line(aes(color = morning)) + 
  facet_wrap(~ Year, scales = "free")

station = read_csv("stations.csv")

SF_set = station$abbr[station$city == "San Francisco"]

go_to_sf_morning_weekday = ridership %>% 
  mutate(Year = year(day), Month = month(day), go_to_SF = exit_abbr %in% SF_set, morning = is_morning) %>%
  filter(morning == "morning", day_of_week < 5) %>%
  group_by(Year, go_to_SF, day) %>%
  summarise(total_trip = sum(trip_count))

per_evening_plot = go_to_sf_morning_weekday %>%
  ggplot(aes(x = day, y = total_trip)) + 
  geom_line(aes(color = go_to_SF)) + 
  facet_wrap(~ Year, scales = "free_x")

go_to_sf_morning_weekday_from_county = ridership %>% 
  mutate(Year = year(day), Month = month(day), go_to_SF = exit_abbr %in% SF_set, morning = is_morning) %>%
  filter(morning == "morning", day_of_week < 5, go_to_SF == TRUE)



by_zipcode = go_to_sf_morning_weekday_from_county %>%
group_by(Year, go_to_SF, enter_abbr) %>%
summarise(total_trip = sum(trip_count))

p = by_zipcode %>%
ggplot() + 
geom_bar(stat = "identity", aes(y = total_trip, x = enter_abbr)) + 
facet_wrap(~ Year, scales = "free_x") + theme(axis.title.x=element_blank(),
axis.text.x=element_blank(),
axis.ticks.x=element_blank())
p


# 
station_with_zip = station %>% mutate(enter_abbr = abbr) %>%select(enter_abbr, zipcode) 
combined= right_join(ridership, station_with_zip, by = "enter_abbr")

# Estimate 
ggplot(demo) + geom_line(aes(x = year, y = totalpop)) + facet_wrap(~zipcode, scales = "free")

ggplot(demo) + geom_line(aes(x = year, y = gini)) + facet_wrap(~zipcode, scales = "free")

demo = demo[demo$zipcode != 94128,]
# 1
df1 = demo[demo$year == 2011, ]
# 2
df2 = demo[demo$year == 2012, ]
# 3
df3 = demo[demo$year == 2013, ]
# 4
df4 = demo[demo$year == 2014, ]

df5 = df4 + (1/2)*(df4 - df3) + (1/3)*(df3 - df2) + (1/6)*(df2 - df1)
df5 = df5 %>% mutate(year = 2015)

df6 = df5 + (1/2)*(df5 - df4) + (1/3)*(df4 - df3) + (1/6)*(df3 - df2)
df6 = df6 %>% mutate(year = 2016)

df7 = df6 + (1/2)*(df6 - df5) + (1/3)*(df5 - df4) + (1/6)*(df4 - df3)
df7 = df7 %>% mutate(year = 2017)

df8 = df7 + (1/2)*(df7 - df6) + (1/3)*(df6 - df5) + (1/6)*(df5 - df4)
df8 = df8 %>% mutate(year = 2018)

df9 = df8 + (1/2)*(df8 - df7) + (1/3)*(df7 - df6) + (1/6)*(df6 - df5)
df9 = df9 %>% mutate(year = 2019)

df10 = df9 + (1/2)*(df9 - df8) + (1/3)*(df8 - df7) + (1/6)*(df7 - df6)
df10 = df10 %>% mutate(year = 2020)


estimated_demo_2015_2020 = bind_rows(df5, df6, df7, df8, df9, df10)

write.csv(estimated_demo_2015_2020, "estimated_demo_2015_2020.csv")

bind_rows(demo, estimated_demo_2015_2020) %>%
  ggplot() + geom_line(aes(x = year, y = gini)) + facet_wrap(~zipcode, scales = "free")

