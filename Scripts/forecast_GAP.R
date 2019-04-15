# GOAL: regression GAP energy consumption
# DEFINITION: create and analyse the prediction of energy consumption to regression algorithms
# AUTHOR: Joan Claverol 


# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, lubridate, fastDummies, magrittr, modelr)

# load data ---------------------------------------------------------------

df <- read_rds("Datasets/CleanTotalData.rds")
# df_temp <- read_rds("Datasets/CleanWeatherData.rds")

# 1st pre-process ---------------------------------------

# selecting relevant variables 
df_frcst <- df %>%
  select(DateTime,ActiveEnergy,season,wday,Period.Day)

# edefining main predictors
df_frcst %<>% 
  mutate(second = second(DateTime), minute = minute(DateTime), 
         hour = hour(DateTime), week = week(DateTime), 
         month = month(DateTime), year = year(DateTime))
  
# filtering 2007
df_frcst_2007 <- df_frcst %>% 
  filter(year == 2007) %>% 
  group_by(date = date(DateTime), Period.Day, wday, season) %>% 
  summarise(avg_energy = mean(ActiveEnergy))

# visualizing the consumption and hypothesis creation, week days and months 
# really affects the data
df_frcst_2007 %>% 
  ggplot(aes(x = date, y = avg_energy)) + 
    geom_point(aes(color = season)) +
    geom_smooth(se = F) +
    facet_wrap(~Period.Day)
    

# let's visualize the distribtuion of energy in relation to the week day
df_frcst_2007 %>% 
  ggplot(aes(x = wday, y = avg_energy)) +
    geom_boxplot(aes(color = season)) + facet_wrap(~Period.Day)
# sunday is the day with more consumption + other insights

# modeling with lm --------------------------------------------------------

# creating the linear model 
mod_lm <- lm(avg_energy ~ wday + week + season, 
             data = df_frcst_2007)

# visualizing the errors
grid <- df_frcst_2007 %>% 
  data_grid(df_frcst_2007) %>% 
  add_predictions(model = mod_lm, var = "avg_energy")

df_frcst_2007 <- df_frcst_2007 %>% 
  add_residuals(model = mod_lm, var = "resid_mod1")

df_frcst_2007 %>% 
  ggplot(aes(x = date, y = resid_mod1, color = season)) +
    geom_ref_line(h = 0) +
    geom_line()
# In summer and at the end of the sprint we have better predictions

# Problems defining the outliers in relation to the season
# df_frcst_2007 %>%
df_frcst_2007 %>%   
  ggplot(aes(wday, avg_energy)) +
    geom_boxplot() +
    geom_point(data = grid, color = "red") +
    facet_wrap(~season)

# notes to improve my model: 
# add temperature
# add more data

