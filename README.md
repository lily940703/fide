# `fide`: Feature-based Intermittent DEmand forecasting

`fide` provides a feature-based forecasting method for intermittent demand proposed by [Li Li, Yanfei Kang, Fotios Petropoulos, and Feng Li](https://arxiv.org/abs/2204.08283). The package aims to facilitate reproducing the results of our paper, and can also be applied to other intermittent demand forecasting problems. 

## Installation
You can install `fide` from github with:
```
devtools::install_github("lily940703/fide")
```

## Load the package
```
library(M4metalearning)
library(tsintermittent)
```
## Data
Simulated data and a real dataset are used in this paper, please see [this page](https://github.com/lily940703/fide/blob/main/docs/Statement%20of%20data.md) for details.

## Usage
An example of using the package based on simulated data is shown on [this page](https://github.com/lily940703/FIDE/blob/main/docs/Usage%20example.md).

## References
- Li, Li, [Yanfei Kang](https://yanfei.site), [Fotios Petropoulos](https://researchportal.bath.ac.uk/en/persons/fotios-petropoulos), and [Feng Li](http://feng.li/). 2022. "Feature-based Intermittent Demand Forecast Combinations: Bias, Accuracy and Inventory Implications." [*_Working Paper_*](https://arxiv.org/abs/2204.08283). 
