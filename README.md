# Europe-Electricity-Consumption-prediction-project
By using the ENTSO-E Transparency Platform and collect historical electricity load data to predict electricity load for below 10 Europe Countries:
Belgium	Finland	France	Germany	Greece Italy	Poland	Romania	Spain	Sweden


There are two R files: 
1. Clear R - to collect our data source
2. Forecast submission - to apply model selection, validation and generate forecast. 

My goal: To Create a model that can constanyly beat naive method


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
Our methods generate about 20% better than the naive methods when comparing forecast of 1 week.

