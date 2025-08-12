library(dplyr)
censor <-
year_hour <- censor %>% 
             group_by(Year, Hour) %>%
             summarise(avg = mean(PRKG_ABLE_TRCN), .groups = "drop")

library(ggplot2)
ggplot(year_hour, aes(x=Hour, y=avg, fill=as.factor(Year))) + geom_col(position = "dodge")+
  scale_fill_manual(values=c("2021"="gold4","2020"="gold3", "2019"= "gold"))+
  labs(fill = "Year")+
  theme_minimal()

result = censor$PRKG_ABLE_TRCN
reason = censor$Hour
data_name = censor
bartlett.test(result ~ reason, data = data_name)

#result <-  mctp(result ~ reson, data=data_name)
#summary(result)
## <<시간 따른 주차가능대수 차이, 통계적 유의미함 :: p-value가 거의 0에 수렴.
## Bartlett test of homogeneity of variances

##data:  result by reason
##Bartlett's K-squared = 747.12, df = 23,
##p-value < 2.2e-16

result = censor$PRKG_ABLE_TRCN
reason = censor$Year
data_name = censor
bartlett.test(result ~ reason, data = data_name)
##<<연도 따른 주차가능대수 차이, 통계적 유의미함 :: p-value가 거의 0에 수렴.
##Bartlett test of homogeneity of variances

##data:  result by reason
##Bartlett's K-squared = 1090.3, df = 2,
##p-value < 2.2e-16

# 데이터 전처리
#censor$Hour <- censor$newTime 
library(tidyverse)
library(caret)
library(dplyr)

processed_data <- censor %>%
  select(PRKG_ABLE_TRCN, Year,Hour) %>%
  mutate(year=as.factor(Year))
  
#데이터 분활
set.seed(123)
train_indices <- createDataPartition(processed_data$PRKG_ABLE_TRCN, p=0.8, list=FALSE)
train_data <- processed_data[train_indices, ]
test_data <- processed_data[-train_indices, ]

# 선형회귀 모델 생성 및 훈련
linear_model <- lm(PRKG_ABLE_TRCN ~., data = train_data)
summary(linear_model)

# 분석 결과 : 
Call:
  lm(formula = PRKG_ABLE_TRCN ~ ., data = train_data)

Residuals:
  Min      1Q  Median      3Q     Max 
-5.3400 -0.9201  0.0799  1.0554  3.3003 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)
(Intercept)  5.3400466  0.0102606  520.44   <2e-16
year2020    -1.0772159  0.0076931 -140.02   <2e-16
year2021    -0.3832385  0.0079692  -48.09   <2e-16
hour        -0.0244821  0.0006127  -39.96   <2e-16

(Intercept) ***
  year2020    ***
  year2021    ***
  hour        ***
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.38 on 189077 degrees of freedom
Multiple R-squared:  0.1039,	Adjusted R-squared:  0.1039 
F-statistic:  7310 on 3 and 189077 DF,  p-value: < 2.2e-16

## 선형회귀분석, 모델 설명력 낮음. 다음 선형모델 결과 해석참고
predictions <- predict(linear_model, newdata = test_data)
mse <- mean((test_data$PRKG_ABLE_TRCN - predictions)^2)
r_squared <- cor(test_data$PRKG_ABLE_TRCN, predictions)^2

cat("Mean Squared Error:", mse,"\n")
cat("R-squared:",r_squared,"\n")
coefficients <- coef(linear_model)[-1] # Intercept 제외
barplot(coefficients, col = "skyblue", main = "Linear Regression Coefficients", cex.names = 0.8)
plot(test_data$PRKG_ABLE_TRCN, predictions, col = "blue", pch = 16,
     xlab = "Actual PRKG_ABLE_TRCN", ylab = "Predicted PRKG_ABLE_TRCN",
     main = "Actual vs. Predicted")
abline(0, 1, col = "red", lty = 2)

## 랜덤포레스트 시도
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(PRKG_ABLE_TRCN ~ ., data = train_data)

# 모델 예측 및 평가
rf_predictions <- predict(rf_model, newdata = test_data)
rf_mse <- mean((test_data$PRKG_ABLE_TRCN - rf_predictions)^2)
rf_r_squared <- cor(test_data$PRKG_ABLE_TRCN, rf_predictions)^2

cat("Random Forest Mean Squared Error:", rf_mse, "\n")
cat("Random Forest R-squared:", rf_r_squared, "\n")

#랜덤포레스트 시각화
# 1. 변수 중요도 시각화
varImpPlot(rf_model)
plot(rf_model$forest[[1]], main = "First Tree in Random Forest")
rf_predictions <- predict(rf_model, newdata = test_data)
plot(test_data$PRKG_ABLE_TRCN, rf_predictions, col = "blue", pch = 16,
     xlab = "Actual PRKG_ABLE_TRCN", ylab = "Predicted PRKG_ABLE_TRCN",
     main = "Random Forest: Actual vs. Predicted")
abline(0, 1, col = "red", lty = 2)

#그래디언트 부스팅
install.packages("gbm")
library(gbm)
gb_model <- gbm(PRKG_ABLE_TRCN ~ ., data = train_data, distribution = "gaussian", n.trees = 100, interaction.depth = 4)

# 모델 예측 및 평가
gb_predictions <- predict(gb_model, newdata = test_data, n.trees = 100)
gb_mse <- mean((test_data$PRKG_ABLE_TRCN - gb_predictions)^2)
gb_r_squared <- cor(test_data$PRKG_ABLE_TRCN, gb_predictions)^2

cat("Gradient Boosting Mean Squared Error:", gb_mse, "\n")
cat("Gradient Boosting R-squared:", gb_r_squared, "\n")

# ARIMA 모델 피팅
install.packages("forecast")
library(forecast)
train_time_series <- ts(train_data$PRKG_ABLE_TRCN,frequency =1)

#time_series_data <- ts(processed_data$PRKG_ABLE_TRCN, frequency = 1)
plot(train_time_series)
arima_model <- auto.arima(train_time_series)
test_time_series <- ts(test_data$PRKG_ABLE_TRCN, frequency = 1)
plot(test_time_series, main = "주차 가능 대수의 시계열 데이터", xlab = "시간", ylab = "주차 가능 대수")

# 안정적 시계열로 변환하기 위해 로그변환 이용
diff_train <- diff(train_time_series)
plot(differenced_data, main = "Differenced Parking Availability Over Time", ylab = "Differenced Parking Availability", xlab = "Time")
diff2_train <- diff(diff_train)
plot(diff2_train,main="2차 차분")

# 차분 결과 안정성 점검 : adf 검정
library(tseries)
adf_test_result <- adf.test(diff_train)
print(adf_test_result)

# 1차 차분 결과 < 0.05로 시계열 데이터 안정적.
library(forecast)

# ACF와 PACF 시각화
tsdisplay(diff_train)
auto_arima_model <- auto.arima(train_time_series, seasonal = FALSE, stepwise = TRUE)
print(auto_arima_model)

arima_model <- Arima(train_time_series, order = c(2,1,1))
forecast_result <- forecast(arima_model, h = length(test_time_series))
plot(forecast_result, main = "ARIMA(2,1,1) 모델 예측", xlab = "시간", ylab = "주차 가능 대수")
lines(test_time_series, col = "blue", lty = 2)

accuracy(forecast_result)
plot(forecast_result, main = "Parking Availability Forecast with Confidence Intervals", ylab = "Parking Availability")
head(forecast_result$mean)

#ARIMA(2,1,1) 모델 이용
arima_model <- Arima(test_time_series, order = c(2,1,1))
forecast_result <- forecast(arima_model, h = length(test_time_series))
plot(forecast_result, main = "ARIMA(2,1,1) 모델 예측", xlab = "시간", ylab = "주차 가능 대수", xlim = c(2020, 2030))
lines(test_time_series, col = "blue", lty = 2)  # 실제 테스트 데이터 표시