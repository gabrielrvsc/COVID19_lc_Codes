## R Codes


- Codes used in the paper <a href = https://arxiv.org/abs/2004.07977 > Short-Term Covid-19 Forecast for Latecomers </a> and to create the forecasts in the <a href = https://covid19analytics.com.br/> covid19analytics.com.br </a> website.
- Run setup.R before trying to run the error correction model. Always keep the packages in the script loaded to use the codes.
- The Data folder has a sample dataset. The Error Correction Function must receive data on that format. 
- The data must count the days after the case X (for example 100). This is represented in the variable day. Observations before this day for each country must be removed from the panel. 
- The Data folder has an example of a function that takes the John Hopkins data and transforms it in to the correct format. This function will work as long as John Hopkins keep the data in the same format. 
- 
