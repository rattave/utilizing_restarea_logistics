library(forecast)

# 날짜 형식으로 변환
censor$FSTTM_RGST_DTTM <- as.POSIXct(censor$FSTTM_RGST_DTTM, format='%Y')

# FSTTM_RGST_DTTM을 인덱스로 설정
ts_data <- ts(censor$PRKG_ABLE_TRCN, frequency = 1, start = 2019)
#log_ts_data <- log(ts_data)
log_ts_data <- log(ts_data)
diff_ts_data <- diff(log_ts_data)
diff2_ts_data <- diff(diff_ts_data)
#diff3_ts_data <- diff(diff2_ts_data)
diff_data <- data.frame(
  date = time(diff_ts_data),
  diff_value = diff_ts_data
)
diff_data <- na.omit(diff_data)

install.packages("tseries")
library(tseries)
diff_ts_data <- diff(ts_data, na.diff = FALSE) 
adf_test_diff <- adf.test(diff2_ts_data)
print("ADF Test - Differenced Data:")
print(adf_test_diff)

acf_ts_data <- acf(diff2_ts_data)
pacf_ts_data <- pacf(diff2_ts_data)

# ARIMA 모형 적용
library(forecast)
arima_model <- auto.arima(diff_ts_data)
print(auto_arima_model)
forecast_auto_arima <- forecast(auto_arima_model, h=10)

# 결과 시각화
plot(forecast_auto_arima, main="Auto ARIMA 모델을 통한 주차가능대수 예측 결과", xlab="연도", ylab="주차가능대수", xlim=c(2019,2030),col="darkblue",lwd=3)
points(censor$FSTTM_RGST_DTTM, censor$PRKG_ABLE_TRCN, col="red")
lines(forecast_auto_arima$mean, col="blue")
forecast_result <- forecast(arima_model, h=10)  # n은 예측할 기간
print(forecast_result)
plot(forecast_result, main="주차가능대수 예측 결과", xlab="연도", ylab="주차가능대수", xlim=c(2019,2030))

#axis(1, at=seq(2019, 2030, by=1), labels=seq(2019, 2030, by=1))
points(censor$FSTTM_RGST_DTTM, censor$PRKG_ABLE_TRCN, col="red")
lines(forecast_result$mean, col="blue") 

forecast_values <- forecast(forecast_auto_arima, h = length(actual_values))$mean

# 평균 제곱 오차 계산
mse <- mean((forecast_values - actual_values)^2)

# 정확도 출력
cat("평균 제곱 오차 (MSE):", mse, "\n")

library(forecast)

# 예측 모델 평가
accuracy_metrics <- accuracy(forecast_auto_arima, actual = actual_values)

# 출력
print(accuracy_metrics)

library(ggplot2)

# 모델 평가 지표 데이터프레임 생성
evaluation_metrics <- data.frame(
  Metric = c("ME", "RMSE", "MAE", "MAPE", "ACF1"),
  Value = c(6.11617e-05, 1.162663, 0.9248394, 0.8756566, -0.006582834)
)

# 시각화
ggplot(evaluation_metrics, aes(x = Value, y = Metric)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "모델 평가 지표", x = "평가 지표", y = "값") +
  theme_minimal()