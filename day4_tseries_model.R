# ==================================================
# Import library
# Install package first, you can run the code in line 3 by deleting the hashtag for comment
# install.packages(c("fpp3", "tidyverse"))
library(tidyverse)
library(fpp3)



# ==================================================
# Import data
sales_train <- read_csv('data/sales_train_validation.csv')
sales_test <- read_csv('data/sales_train_evaluation.csv')
calendar <- read_csv('data/calendar.csv')



# ==================================================
# Overall Wallmart data for training
sales_train_overall <- sales_train %>% 
  select(contains('d_')) %>% 
  apply(2, sum) 

sales_train_overall <- tibble(
  d = sales_train %>% select(contains('d_')) %>% colnames(),
  total_unit_sold = sales_train_overall
)

sales_train_overall

# Join with calendar to get date
sales_train_overall <- sales_train_overall %>% 
  left_join(calendar %>% select(d, date) %>% distinct()) %>%
  select(date, total_unit_sold)

sales_train_overall

# Change the data to tsibble, i.e. tibble for time series data
sales_train_overall <- sales_train_overall %>% 
  as_tsibble(index = date)

# Line plot
sales_train_overall %>% 
  autoplot(total_unit_sold)

# Box cox transformation
lambda <- sales_train_overall %>% 
  features(total_unit_sold, features = guerrero) %>% 
  pull(lambda_guerrero)

lambda

sales_train_overall <- sales_train_overall %>% 
  mutate(total_unit_sold_t = box_cox(total_unit_sold, lambda))



# ==================================================
# Overall Wallmart data for testing
sales_test_overall <- sales_test %>% 
  select(contains('d_')) %>% 
  apply(2, sum) 

sales_test_overall

sales_test_overall <- tibble(
  d = sales_test %>% select(contains('d_')) %>% colnames(),
  total_unit_sold = sales_test_overall
)

sales_test_overall

# Join with calendar to get date
sales_test_overall <- sales_test_overall %>% 
  left_join(calendar %>% select(d, date) %>% distinct()) %>%
  select(date, total_unit_sold)

sales_test_overall

# Change the data to tsibble, i.e. tibble for time series data
sales_test_overall <- sales_test_overall %>% 
  as_tsibble(index = date)

# Box cox transformation
sales_test_overall <- sales_test_overall %>% 
  mutate(total_unit_sold_t = box_cox(total_unit_sold, lambda))

# Line plot
sales_test_overall %>% 
  autoplot(total_unit_sold)



# ==================================================
# ACF
sales_train_overall %>% 
  ACF(total_unit_sold_t) %>% 
  autoplot()

# PACF
sales_train_overall %>% 
  PACF(total_unit_sold_t) %>% 
  autoplot()

# Test for differencing
sales_train_overall %>% 
  features(total_unit_sold_t, unitroot_ndiffs)
sales_train_overall %>% 
  features(total_unit_sold_t, unitroot_nsdiffs)

sales_train_overall %>% 
  mutate(total_unit_sold_t_d = difference(total_unit_sold_t, 7)) %>% 
  features(total_unit_sold_t_d, unitroot_nsdiffs)

sales_train_overall %>% 
  mutate(total_unit_sold_t_d = difference(total_unit_sold_t, 7)) %>% 
  features(total_unit_sold_t_d, unitroot_ndiffs)

sales_train_overall <- sales_train_overall %>% 
  mutate(total_unit_sold_t_d = difference(total_unit_sold_t, 7))

# Check ACF and PACF again
sales_train_overall %>% 
  ACF(total_unit_sold_t_d) %>% 
  autoplot()

sales_train_overall %>% 
  PACF(total_unit_sold_t_d) %>% 
  autoplot()

sales_train_overall %>% gg_tsdisplay(
  total_unit_sold_t_d,
  plot_type='partial', lag_max = 24
)

sales_train_overall[is.na(sales_train_overall$total_unit_sold_t_d),]

# Combination of possible model
forecast_model <- sales_train_overall %>% 
  model(search = ARIMA(total_unit_sold_t, stepwise=FALSE),
        search_2 = ARIMA(total_unit_sold_t, stepwise=TRUE),
        arima300411 = ARIMA(total_unit_sold_t ~ pdq(2,0,0) + PDQ(3,1,1)),
        ets_model = ETS(total_unit_sold_t))

forecast_model %>% pivot_longer(everything(), names_to = "Model name",
                                values_to = "Orders")

# Check the performance
glance(forecast_model) %>% arrange(AIC, BIC) %>% select(.model, AIC, BIC) # Which one is the best model?

# Check the assumption
forecast_model %>% select(search) %>% gg_tsresiduals()

augment(forecast_model) %>% 
  filter(.model == "search") %>% 
  features(.innov, ljung_box, lag = 24, dof = 6) # degree of freedom should be related to the number of parameters

# Check the accuracy on training data
forecast_model %>% accuracy() %>% select(.model, .type, RMSE, MAPE)

# Let's look on the forecast to the next 28 days
forecast_model %>% forecast(h=28) %>% autoplot(sales_train_overall) 
forecast_model %>% select(search) %>% forecast(h=28) %>% autoplot(sales_train_overall) 

forecast_model %>% select(search) %>% forecast(h=28) %>% accuracy(sales_test_overall)

# Convert to initial data
temp <- forecast_model %>% select(search) %>% forecast(h=28)
temp <- temp[,c(2,4)]
colnames(temp) <- c('date', 'total_unit_sold_t_forecast')

point_forecast_result <- forecast_model %>% 
  select(search) %>% 
  fitted() %>% 
  select(date, .fitted) %>% 
  rename(
    total_unit_sold_t_forecast = .fitted
  ) %>% 
  bind_rows(temp) %>% 
  mutate(
    total_unit_sold_forecast = inv_box_cox(total_unit_sold_t_forecast, lambda)
  ) %>% 
  left_join(sales_test_overall, by ='date') %>% 
  select(date, total_unit_sold, total_unit_sold_forecast)

point_forecast_result %>% 
  pivot_longer(contains('total')) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()



# ==================================================
# Save point forecast
write_csv(point_forecast_result, 'output/forecast_result.csv')

# Save best model
saveRDS(forecast_model %>% select(search), 'output/model.rda')



# ==================================================
load_model <- readRDS('output/model.rda')

load_model %>% forecast(h=100) %>% autoplot(sales_train_overall)




