# Statement of data 

## Simulated data

We generate the simulated data based on the three factors (Inter-Demand Interval (IDI), squared Coefficient of Variation (CV<sup>2</sup>) and the number of observations). Each factor is varied around four levels. Considering 4<sup>3</sup> = 64 combinations and 1,000 series at each level, we produce in total 64,000 simulated time series for simulation study. R code is available from https://github.com/lily940703/fide/blob/main/simulation_generate.R. 

## Real dataset
The RAF dataset used for the empirical evaluation has been previously investigated in the literature(Kourentzes and Athanasopoulos 2021; Petropoulos and Kourentzes
2015; Teunter and Duncan 2009). It contains 5000 monthly time series, with 84 observations each. The RAF dataset can be freely available by the authors upon request.


## References

- Kourentzes, Nikolaos, and George Athanasopoulos. 2021. "Elucidate Structure in Intermittent Demand Series." European Journal of Operational Research 288 (1): 141–152.
- Petropoulos, Fotios, and Nikolaos Kourentzes. 2015. "Forecast Combinations for Intermittent Demand." Journal of the Operational Research Society 66 (6): 914–924.
- Teunter, Ruud H, and Laura Duncan. 2009. "Forecasting Intermittent Demand: A Comparative Study." Journal of the Operational Research Society 60 (3): 321–329.
