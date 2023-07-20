# Import library
library(tidyverse)

# ================================
# Import data
sales_train <- read_csv('D:/Coursera/Pacman Course/Data Science/Day 3/m5-forecasting-accuracy/sales_train_validation.csv')
sales_train %>% head() %>% View()

calendar <- read_csv('D:/Coursera/Pacman Course/Data Science/Day 3/m5-forecasting-accuracy/calendar.csv')
calendar %>% head() %>% View()


# ================================
# Reference vector for daily data
col_date <- seq(1, 1913)
col_date_d <- rep('d_', length(col_date))
col_date_f <- paste0(col_date_d, col_date)
col_date_f

# ================================
# Overall Wallmart
sales_train_overall <- sales_train %>% 
  select(all_of(col_date_f)) %>% 
  apply(2, sum) 

sales_train_overall <- tibble(
  d = col_date_f,
  total_unit_sold = sales_train_overall
)

# Join the data to get date
sales_train_overall <- sales_train_overall %>% 
  left_join(calendar %>% select(date, d), by = 'd')
sales_train_overall

# Add the month and year column
sales_train_overall <- sales_train_overall %>% 
  mutate(month = floor_date(date, unit = 'mon'),
         year = floor_date(date, unit = 'year'))

# Time series plot for overall Wallmart
# Daily
sales_train_overall %>% ggplot(aes(x = date, y = total_unit_sold)) +
  geom_line()
# Monthly
sales_train_overall %>% 
  group_by(month) %>% 
  summarise(total_unit_sold = sum(total_unit_sold)) %>% 
  ggplot(aes(x = month, y = total_unit_sold)) +
  geom_line()
# Yearly
sales_train_overall %>% 
  group_by(year) %>% 
  summarise(total_unit_sold = sum(total_unit_sold)) %>% 
  ggplot(aes(x = year, y = total_unit_sold)) +
  geom_line()

# ================================
# Wallmart per state
sales_train_state <- sales_train %>% 
  group_by(state_id) %>% 
  summarise(across(all_of(col_date_f), sum))

sales_train_state <- sales_train_state %>% 
  pivot_longer(all_of(col_date_f), names_to = 'd', values_to = 'total_unit_sold')

sales_train_state <- sales_train_state %>% 
  left_join(calendar %>% select(date, d), by = 'd') %>% 
  mutate(month = floor_date(date, unit = 'mon'),
         year = floor_date(date, unit = 'year'))

# Time series plot for overall Wallmart
# Daily
sales_train_state %>% 
  ggplot(aes(x = date, y = total_unit_sold, color = state_id)) +
  geom_line()
# Monthly
sales_train_state %>% 
  group_by(state_id, month) %>% 
  summarise(total_unit_sold = sum(total_unit_sold)) %>% 
  ggplot(aes(x = month, y = total_unit_sold, color = state_id)) +
  geom_line() +
  geom_smooth()
# Yearly
sales_train_state %>% 
  group_by(state_id, year) %>% 
  summarise(total_unit_sold = sum(total_unit_sold)) %>% 
  ggplot(aes(x = year, y = total_unit_sold, color = state_id)) +
  geom_line()

