## R Codes


- Codes used in the paper <a href = https://arxiv.org/abs/2004.07977 > Short-Term Covid-19 Forecast for Latecomers </a> and to create the forecasts in the <a href = https://covid19analytics.com.br/> covid19analytics.com.br </a> website.
- Run setup.R before trying to run the error correction model. Always keep the packages in the script loaded to use the codes.
- The data folder has a sample dataset. The Error Correction Function must receive data on that format. 
- The data must count the days after the case X (for example 100). This is represented in the variable day. Observations before this day for each country must be removed from the panel. 
- The data folder has an example of a function that takes the John Hopkins data and transforms it in to the correct format. This function will work as long as John Hopkins keep the data in the same format. 
- These codes represent our best effort to forecast the number of cases and deaths from Covid-19 in the very short-run in Brazil. As we explain in the companion <a href = https://arxiv.org/abs/2004.07977 >paper</a>, given the short sample, measurement errors, structural breaks, we cannot guarantee that the model will perform well under all contingencies possible. Needless to say, we are not responsible for any misuse of these codes and/or the forecasts. We aim to keep developing the forecasting method, and updating these codes. Any suggestion on how to improve the codes and/or the forecasting method is more than welcome.


## ECM example

- If you used the John Hopkins script in the data folder do download and format the panel you can run the ECM with the following code:

<code>run_ecm(modelpanel = panel, M = 14, target = "Brazil", ssize = 30, inflate = 5, alpha_s = 1)</code>

- Note that the country you select as target must have enough countries ahead of it in the epidemic cycle. If you use <code>M=14</code> you will need to have a good number of countries at least 14 days ahead in the cycle than the target country.  
