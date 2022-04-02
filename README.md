# Europe-Electricity-Consumption-prediction-project
AUthors: Wayne Chiu, Filippos Polyzos
By using the ENTSO-E Transparency Platform and collect historical electricity load data to predict electricity load for below 10 Europe Countries:
Belgium	Finland	France	Germany	Greece Italy	Poland	Romania	Spain	Sweden


There are two R files: 
1. Clear R - to collect our data source
2. Forecast submission - to apply model selection, validation and generate forecast. 

My goal: To create a model that can constanyly beat naive method


# Methodology
**Model Selection**

Use 4 weeks of historical data to calculate below 4 models which has the lowest MAPEs for each country
1. ETS
2. Tbats
3. MAPA
4. Thief    

**Period selection**

After we have selected the model , I determine what is the amount of data by calculating the MAPEs of the forecast generated from  
1. 4-weeks
2. 5-months
3. Combination of 4-weeks and 5-months data (equal weight , w=0.5)

The periods that have lowest MAPEs will be selected. 

# Results


![image](https://user-images.githubusercontent.com/99564835/161405326-e63e8d1a-5544-4ef5-ab27-017ad2ab757b.png)
Above graphs is an example of using our forecast model to forecast the next 7 days. It shows that our model can predict the seasonality and trend more closely than the simple naive method

![image](https://user-images.githubusercontent.com/99564835/161404943-d957dc52-4f0e-43ad-95c0-abcf3fb15cde.png)
Our model currently beats a naive forecast in all countries in the 2-week forecast (Mar-10 to Mar-24) as it has a lower MAE in all countries


![image](https://user-images.githubusercontent.com/99564835/161404861-8fa8786c-d365-457d-bc9c-0bc1be2b1051.png)
MAPEs of 2-week forecast (Mar-10 to Mar-24)



