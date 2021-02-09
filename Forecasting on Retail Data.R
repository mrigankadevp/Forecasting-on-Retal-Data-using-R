library(fpp3)
library(readabs)

retail_abs<-read_abs("8501.0", tables = 11)
retail_abs

retail_abs <- retail_abs %>%
  mutate(Month = yearmonth(date)) %>%
  rename(Turnover = value, `Series ID` = series_id) %>%
  select(Month, `Series ID`, series, Turnover)
retail_abs

retail_abs <- retail_abs %>%
  separate(series, c("Category", "State", "Industry"), sep = ";", extra = "drop") %>%
  mutate(
    State = trimws(State),
    Industry = trimws(Industry),
  ) %>%
  select(-Category) 

retail_abs <- retail_abs %>%
  as_tsibble(index = Month, key = c(State, Industry)) %>%
  filter(!is.na(Turnover), Month <= yearmonth("2019 Dec")) 
retail_abs1 <- retail_abs %>% 
  filter(`Series ID`=="A3349881A") 
#Familarize with data
retail_abs1%>% 
  autoplot(Turnover)

retail_abs1 %>%
  gg_season(Turnover, labels = "both")+
  ggtitle("Seasonal plot: Turnover of other retailing n.e.c in Queensland")
  
retail_abs1 %>%
  gg_subseries(Turnover)+
  ylab("Turnover")+
  xlab("Year")+
  ggtitle("Seasonal subseries plot:Turnover of other retailing n.e.c in Queensland")

retail_abs1 %>%
  gg_lag(Turnover, geom = 'point')+
  ggtitle("Lag Plot")# lags = 1:12)

retail_abs1 %>%
  ACF(Turnover)%>%
  autoplot()+
  ggtitle("ACF Plot")


#Transformation Comparisons
retail_abs1 %>%
  summarise(Turnover = sum(Turnover))

retail_abs1 %>%
  autoplot(log(Turnover))+
  ggtitle("Log Transformation")

retail_abs1 %>%
  autoplot(Turnover^(1/3))+
  ggtitle("Cube Root Transformation")

retail_abs1 %>%
  autoplot(-1/Turnover)+
  ggtitle("Inverse Transformation")


transformed_retail_abs <-retail_abs1 %>%
  autoplot(box_cox(Turnover, 0.174))+
  ggtitle("Box-Cox Transformation")

#Split data into training and test sets
training_set <- retail_abs1 %>%
  slice(1:(n()-24))

retail_abs1%>%
  autoplot(Turnover)+
  geom_line(aes(color="Training"), data=training_set)+
  xlab("Years")+
  ggtitle("Traing and Test sets data plots")

#Applying two Benchmark methods to forecast
fit_model <- training_set%>%
  model(snaive=SNAIVE(Turnover),
        drift=RW(Turnover~drift())) %>%
  forecast(h=24)

fit_model %>%
  autoplot(training_set)+
  ylab("Turnover")+
  xlab("Year")+
  ggtitle("Snaive and Drift Forecasting Methods")


 accuracy_assign1<- accuracy(fit_model, retail_abs)
 accuracy_assign1

#Best method residual analysis
best_method_assign1 <- retail_abs1 %>%
  model(snaive=SNAIVE(Turnover))
aug <- augment(best_method_assign1)
aug%>%
  autoplot(.resid)
aug%>%
  gg_tsdisplay(.resid, plot_type = "histogram")
best_method_assign1%>%
  select(snaive)%>%
  gg_tsresiduals()+
  ggtitle("Residual Analysis")

#Generate point forecasts using benchmark method

library(fable)
benchmark_assign1<-retail_abs1 %>%
  model(SNAIVE(Turnover))%>%
  forecast(h="2 years")%>%
  hilo()%>%
  interval()
benchmark_assign1
retail_abs1 %>%
  model(SNAIVE(Turnover))%>%
  forecast(h="2 years")%>%
  interval()

retail_abs1 %>%
  model(SNAIVE(Turnover))%>%
  forecast(h=24)%>%
  autoplot(retail_abs1)+
  ylab("Turnover")+
  xlab("Year")+
  ggtitle("Point forecast for next 2 years(future)")

#Plot time-series and select ETS Model
retail_abs1 %>% 
  autoplot(Turnover) + xlab("Year") +ylab("Turnover") + ggtitle("Time-Series Plot for Turnover")


training_set <- retail_abs1 %>%
  slice(1:(n()-24))

#Estimate ETS model
fit_model_assign2 <- retail_abs1%>%
  model(hwdamped = ETS(Turnover ~ error("M")+trend("Ad")+season("M"))) %>%
  report(fit_model_assign2)
tidy(fit_model_assign2)
glance(fit_model_assign2)
accuracy(fit_model_assign2)
components(fit_model_assign2) %>%
  autoplot()

#Plot residuals and perform diagnostic checks
fit_model_assign2 %>%
  augment()%>%
  features(.resid, ljung_box, lag=24, dof=5)

augment(fit_model_assign2)%>%
  gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")+ggtitle("Residual Analysis")

#Let R select ETS model
selected_r_model <- retail_abs1 %>%
  model(ETS(Turnover)) %>%
  report()

components(selected_r_model)%>%
  autoplot()+
  ggtitle("ETS(M,A,M) components")

residuals(selected_r_model)
residuals(selected_r_model, type = "response")

#Generate forecasts using ETS model
training_set <- retail_abs1 %>%
  slice(1:(n()-24))
#selected model
training_fit_model_selected <- training_set%>%
  model(hwdamped = ETS(Turnover ~ error("M")+trend("Ad")+season("M")))%>%
  report()
forecast_selected <- training_fit_model_selected %>% 
  forecast(h = "24 months")
forecast_selected%>%tail()
forecast_selected %>%
  autoplot(filter(retail_abs1, Month >= yearmonth("2018 Jan")))
#r model
training_fit_model_r <- training_set%>%
  model(ETS(Turnover))%>%
  report()
forecast_r <- training_fit_model_r %>% 
  forecast(h = "24 months")
forecast_r%>%tail()
forecast_r %>%
  autoplot(filter(retail_abs1, Month >= yearmonth("2018 Jan")))

# 95% interval by chosen
interval_95 <- augment(fit_model_assign2)%>%
  pull(.resid) %>%
  sd()

yhat <- forecast_selected %>%
  pull(Turnover) %>%
  head(1)

yhat + c(-1, 1) * 1.96 * interval_95

forecast_selected %>%
  mutate(interval = hilo(.distribution, 95)) %>% pull(interval)

interval_95 = sqrt(sum((augment(training_fit_model_selected)$.resid)^2)/(length(augment(training_fit_model_selected)$.resid)-length(tidy(training_fit_model_selected)$estimate)))

yhat + c(-1, 1)*qnorm(0.975) * interval_95

# 95% interval by R
interval_95 <- augment(fit_model_assign2)%>%
  pull(.resid) %>%
  sd()

yhat <- forecast_r %>%
  pull(Turnover) %>%
  head(1)

yhat + c(-1, 1) * 1.96 * interval_95

forecast_r %>%
  mutate(interval = hilo(.distribution, 95)) %>% pull(interval)

interval_95 = sqrt(sum((augment(training_fit_model_r)$.resid)^2)/(length(augment(training_fit_model_r)$.resid)-length(tidy(training_fit_model_r)$estimate)))

yhat + c(-1, 1)*qnorm(0.975) * interval_95

#Q7
#Generate forecasts for two years after
new_forecast_model <- retail_abs1%>%
  model(additive = ETS(Turnover ~ error("M")+trend("A")+season("M")))%>%
  report()

fc_selected<- new_forecast_model %>% 
  forecast(h = "24 months")

fc_selected%>%tail()

fc_selected %>%
  autoplot(filter(retail_abs1, Month >= yearmonth("2018 Jan")))

retail_abs1 <- retail_abs %>% 
  filter(`Series ID`=="A3349881A") 

#Stabilize the data
#KPSS testing
#Box-Cox transformation(as selected)
#Seasonal Differencing used
retail_abs1%>% 
  autoplot(Turnover)

retail_abs1 %>%
  features(Turnover, features = guerrero)


transformed_retail_abs <-retail_abs1 %>%
  autoplot(box_cox(Turnover, 0.174))+
  ggtitle("Box-Cox Transformation")
transformed_retail_abs

retail_abs1 %>%
  features(Turnover, unitroot_kpss)


retail_abs1 %>%
    autoplot(
      box_cox(Turnover, 0.174) %>% difference(12))+ ggtitle("Seasonal Differencing")

retail_abs1 %>% autoplot(
  box_cox(Turnover, 0.174) %>% difference(12) %>% difference(1))+ ggtitle("Seasonal differencing with first difference")

retail_abs1 %>%
  mutate(diff_turnover= difference(difference(box_cox(Turnover, 0.174), 12),1)) %>%
  features(diff_turnover, unitroot_kpss)

#PACF and ACF plots

retail_abs1 %>% gg_tsdisplay(
  box_cox(Turnover, 0.174) %>% difference(12) %>% difference(1),
  plot_type = "partial")

fit_ARIMA<-retail_abs1%>%
  model(
    fitted_model=ARIMA(box_cox(Turnover, 0.174)~0+pdq(3,1,2)+PDQ(0,1,1)))

fit_ARIMA_glance<- fit_ARIMA%>%glance()
fit_ARIMA_glance

#Check whiteness of residuals of ARIMA model
fit_ARIMA%>%
  select(fitted_model)%>%
  gg_tsresiduals()

#Considered three alternative ARIMA models
fit_compare<-retail_abs1%>%
  
  model(
    
    md1=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,1,2)+PDQ(0,1,1)),
    
    md2=ARIMA(box_cox(Turnover,0.174)~0+pdq(2,1,2)+PDQ(0,1,1)),
    
    md3=ARIMA(box_cox(Turnover,0.174)~0+pdq(2,1,1)+PDQ(0,1,1)),
    
    md4=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,1,1)+PDQ(0,1,1)))



fit_compare_glance<-fit_compare%>%glance()
fit_compare_glance
fit_compare_ljung<-fit_compare%>%augment()%>%
  features(.resid,ljung_box,dof=12,lag=24)
fit_compare_ljung
#Let ARIMA choose a model
#Let ARIMA look harder
fit_ARIMA_R<-retail_abs1 %>%
  model(arima = ARIMA(box_cox(Turnover, 0.174),stepwise = FALSE,order_constraint = 10, approximation = FALSE)) %>%
  report()
fit_ARIMA_byR<- retail_abs1 %>%
  model(arima = ARIMA(box_cox(Turnover, 0.174)))%>%
  report()
fit_ARIMA_byR
fit_ARIMA_ljung<-fit_ARIMA_byR%>%augment()%>%
  features(.resid,ljung_box,dof=12,lag=24)
fit_ARIMA_ljung
fit_ARIMA_glance_1<- fit_ARIMA_R%>%glance()
fit_ARIMA_glance_1
fit_ARIMA_R%>%
  gg_tsresiduals()

fit_ARIMA_R%>%augment()%>%
  features(.resid,ljung_box,dof=12,lag=24)

#compare ARIMA models accuracy
comparison_models <- retail_abs1 %>%
  model(
    md1=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,1,2)+PDQ(0,1,1)),
    
    md2=ARIMA(box_cox(Turnover,0.174)~0+pdq(2,1,2)+PDQ(0,1,1)),
    
    md3=ARIMA(box_cox(Turnover,0.174)~0+pdq(2,1,1)+PDQ(0,1,1)),
    
    md4=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,1,1)+PDQ(0,1,1)),
    
    arima=ARIMA(box_cox(Turnover, 0.174)~0+pdq(3,0,0)+PDQ(1,1,1))
  )
accuracy_comparison <-accuracy(comparison_models)
accuracy_comparison

#Generate and plot forecasts and forecast intervals by chosen ARIMA model
forecast_model <- retail_abs1 %>%
  model( arima = ARIMA(box_cox(Turnover, 0.174)~pdq(3,0,0)+PDQ(1,1,1))) %>%
  report()
forecast_model 
forecast_by_model <-forecast_model%>%
  forecast(h = 24)
forecast_by_model%>%
  autoplot(retail_abs1)+ggtitle("Forecast for two years using ARIMA")
forecast_by_model
#95% interval by R

interval_95 <- augment(forecast_model)%>%
  pull(.resid) %>%
  sd()

yhat <- forecast_by_model %>%
  pull(Turnover) %>%
  head(1)

yhat + c(-1, 1) * 1.96 * interval_95


interval_95 = sqrt(sum((augment(forecast_model)$.resid)^2)/(length(augment(forecast_model)$.resid)-length(tidy(forecast_model)$estimate)))

yhat + c(-1, 1)*qnorm(0.975) * interval_95

#Split data into training and test set
#Compare between chosen ARIMA and ETS model
training_set <- retail_abs1 %>%
  slice(1:(n()-24))

fit_compare_model<-training_set%>%
  model(
    Multiplicative = ETS(Turnover ~ error("M")+trend("A")+season("M")),
    Arima=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,0,0)+PDQ(1,1,1)))

compare_glance<-fit_compare_model%>%glance()
compare_glance
accuracy_ARETS<- accuracy(fit_compare_model)
accuracy_ARETS
training_set %>%
  model(
    Multiplicative = ETS(Turnover ~ error("M")+trend("A")+season("M")),
    Arima=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,0,0)+PDQ(1,1,1)))%>%
forecast(h = 24)%>%
  autoplot(retail_abs1)+ggtitle("Forecasts using ETS(M,A,M) and ARIMA(3,0,0)(1,1,1)[12]")
           
#Removing the filter


retail_abs<-read_abs("8501.0", tables = 11)
retail_abs

retail_abs <- retail_abs %>%
  mutate(Month = yearmonth(date)) %>%
  rename(Turnover = value, `Series ID` = series_id) %>%
  select(Month, `Series ID`, series, Turnover)
retail_abs

retail_abs <- retail_abs %>%
  separate(series, c("Category", "State", "Industry"), sep = ";", extra = "drop") %>%
  mutate(
    State = trimws(State),
    Industry = trimws(Industry),
  ) %>%
  select(-Category) 

retail_abs <- retail_abs %>%
  as_tsibble(index = Month, key = c(State, Industry)) %>%
  filter(!is.na(Turnover))



retail_COVID19 <- retail_abs %>%
  filter(`Series ID`=="A3349881A")

tail_COVID <-tail(retail_COVID19)
tail_COVID

retail_COVID19%>%
  autoplot(Turnover)+ggtitle("TimeSeries after removing Filter")

#ARIMA model forecast
forecast_new_ARIMA<-retail_COVID19 %>%
  model(    
    Arima=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,0,0)+PDQ(1,1,1))) %>%
    report()

forecast_newdata<-  forecast_new_ARIMA %>%
  forecast(h = 24)

forecast_newdata%>%
  autoplot(retail_abs1)+ggtitle("ARIMA Model")

#95% interval for ARIMA model
interval_95 <- augment(forecast_new_ARIMA)%>%
  pull(.resid) %>%
  sd()

yhat <- forecast_newdata %>%
  pull(Turnover) %>%
  head(1)

yhat + c(-1, 1) * 1.96 * interval_95

interval_95 = sqrt(sum((augment(forecast_new_ARIMA)$.resid)^2)/(length(augment(forecast_new_ARIMA)$.resid)-length(tidy(forecast_new_ARIMA)$estimate)))

yhat + c(-1, 1)*qnorm(0.975) * interval_95

#ETS model forecast
forecast_new_ETS<-retail_COVID19 %>%
  model( Multiplicative = ETS(Turnover ~ error("M")+trend("A")+season("M")))%>%
  report()
forecast_data_new<- forecast_new_ETS %>%
  forecast(h = 24) 
forecast_data_new%>%
  autoplot(retail_abs1)+ggtitle("ETS Multiplicative Model")
#95% interval for ETS Model
interval_95 <- augment(forecast_new_ETS)%>%
  pull(.resid) %>%
  sd()

yhat <- forecast_data_new %>%
  pull(Turnover) %>%
  head(1)

yhat + c(-1, 1) * 1.96 * interval_95

interval_95 = sqrt(sum((augment(forecast_new_ETS)$.resid)^2)/(length(augment(forecast_new_ETS)$.resid)-length(tidy(forecast_new_ETS)$estimate)))

yhat + c(-1, 1)*qnorm(0.975) * interval_95

# ARIMA and ETS together
retail_COVID19 %>%
  model(
    Arima=ARIMA(box_cox(Turnover,0.174)~0+pdq(3,0,0)+PDQ(1,1,1)),
    Multiplicative = ETS(Turnover ~ error("M")+trend("A")+season("M")))%>%
    forecast(h = 24) %>%
    autoplot(retail_abs1)+ggtitle("ARIMA and ETS Forecast")
