# Analizy szeregów czasowych dotyczących kształtowania się CPI Meksyku oraz liczby wyszukiwań w Google koszykarza Lebrona James’a

- For the prediction of the non-seasonal series (Mexico's CPI), 3 extrapolation methods were used: exponential smoothing, the Holt model and ARIMA
- For the seasonal series (number of Lebron James's google searches), the SARIMA model was used
- Both series, were examined in terms of stationarity (Dickey-Fuller test, KPSS test) and seasonality (augmented Dickey, Hasha, Fuller test)
- A graphical interpretation of the ACF and PACF functions was used to verify the AR and MA orders
- The AIC and BIC information criteria were used to select the best models
- The work used R (modelling) and Demetra (series decomposition) software

#### Average forecast error estimates for models estimating non-seasonal series
![](images/Project_2/mean_errors.png](https://github.com/askovr0n/Portfolio/blob/main/images/Project_2/mean_errors.png)
