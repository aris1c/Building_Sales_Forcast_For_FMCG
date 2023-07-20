# Import library
library(tidyverse)

# Import data
sales_train <- read_csv('data/sales_train_validation.csv')
calendar <- read_csv('data/calendar.csv')

sales_train %>% head() %>% View()

# Data preparation for state x store x cat x day level
init_agg <- sales_train %>% 
  group_by(state_id, store_id, cat_id) %>% 
  summarise(across(contains('d_'), sum))

init_agg <- init_agg %>% 
  pivot_longer(contains('d_'), names_to = 'd', values_to = 'total_unit_sold')

init_agg

# Aggregate cat
agg_cat <- init_agg %>% 
  group_by(cat_id) %>% 
  summarise(total_unit_sold = sum(total_unit_sold))

agg_cat

# Graph aggregate cat
agg_cat %>% ggplot(aes(x = cat_id, y = total_unit_sold)) +
  geom_bar(stat = 'identity', position = 'dodge')

# Aggregate state x cat
agg_state_cat <- init_agg %>% 
  group_by(state_id, cat_id) %>% 
  summarise(total_unit_sold = sum(total_unit_sold),
            total_store_sell = n_distinct(store_id)) %>% 
  mutate(avg_total_unit_sold_per_store = total_unit_sold / total_store_sell)

agg_state_cat

# Graph aggregate state x cat
agg_state_cat %>% ggplot(aes(x = state_id, y = total_unit_sold, fill = cat_id)) +
  geom_bar(stat = 'identity', position = 'dodge')

agg_state_cat %>% ggplot(aes(x = state_id, y = avg_total_unit_sold_per_store, fill = cat_id)) +
  geom_bar(stat = 'identity', position = 'dodge')

# Aggregate store x cat
agg_store_cat <- init_agg %>% 
  group_by(store_id, cat_id) %>% 
  summarise(total_unit_sold = sum(total_unit_sold))

agg_store_cat

# Graph aggregate store x cat
agg_store_cat %>% ggplot(aes(x = store_id, y = total_unit_sold, fill = cat_id)) +
  geom_bar(stat = 'identity', position = 'dodge')

agg_store_cat %>% ggplot(aes(x = store_id, y = total_unit_sold, fill = cat_id)) +
  geom_bar(stat = 'identity', position = 'stack')

