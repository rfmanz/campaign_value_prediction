Attempt to predict campaign value for better budget allocation via multi-series recursive forecasting. 

Currently using: 
- gn_fb_tonic data
- predicting ROAS for differing time horizons

<br/>
A time series is a sequence of data arranged chronologically and spaced at equal or irregular intervals. The forecasting process consists of predicting the future value of a time series, either by modeling the series solely based on its past behavior (autoregressive) or by incorporating other external variables.

To apply machine learning models to forecasting problems, the time series needs to be transformed into a matrix where each value is associated with a specific time window (known as lags) that precedes it. In the context of time series, a lag with respect to a time step t is defined as the value of the series at previous time steps. For instance, lag 1 represents the value at time step t-1, while lag m represents the value at time step t-m.

In independent multi-series forecasting a single model is trained for all time series, but each time series remains independent of the others, meaning that past values of one series are not used as predictors of other series. However, modeling them together is useful because the series may follow the same intrinsic pattern regarding their past and future values. For instance, the sales of products A and B in the same store may not be related, but they follow the same dynamics, that of the store.

![Alt text](/assets/image.png)

Since the value t(n-1) is required to predict t(n), and t(n-1) is unknown, a recursive process is applied in which, each new prediction, is based on the previous one. This process is known as recursive forecasting or recursive multi-step forecasting

![Alt text](/assets/image-1.png)


References:
- https://skforecast.org/0.11.0/introduction-forecasting/introduction-forecasting.html
- https://unit8co.github.io/darts/examples/20-RegressionModel-examples.html


