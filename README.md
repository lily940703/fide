# `fide`: Feature-based Intermittent DEmand forecasting

`fide` provides a feature-based forecasting method for intermittent demand proposed by [Li Li, Yanfei Kang, Fotios Petropoulos, and Feng Li](https://doi.org/10.1080/00207543.2022.2153941). The package aims to facilitate reproducing the results of our paper, and can also be applied to other intermittent demand forecasting problems. 

## Installation
You can install `fide` from github with:
```
devtools::install_github("lily940703/fide")
```

## Load required packages
```
library(M4metalearning)
library(tsintermittent)
library(fide)
```
## Data
We provide simulated data and two real datasets for intermittent demand forecasting, please see [this page](https://github.com/lily940703/fide/blob/main/docs/Statement%20of%20data.md) for details.

## Usage
An example of using the package based on simulated data is shown on [this page](https://github.com/lily940703/FIDE/blob/main/docs/Usage%20example.md).

## References
- Li, Li, [Yanfei Kang](https://yanfei.site), [Fotios Petropoulos](https://researchportal.bath.ac.uk/en/persons/fotios-petropoulos), and [Feng Li](http://feng.li/) (2023). "Feature-based Intermittent Demand Forecast Combinations: Accuracy and Inventory Implications." [*_International Journal of Production Research_*](https://doi.org/10.1080/00207543.2022.2153941), 61(22), pp. 7557-7572.

```
@article{LiL2023FeaturebasedIntermittent,
	title = {Feature-based intermittent demand forecast combinations: accuracy and inventory implications},
	volume = {61},
	url = {https://arxiv.org/abs/2204.08283},
	doi = {10.1080/00207543.2022.2153941},
	pages = {7557--7572},
	number = {22},
	journaltitle = {International Journal of Production Research},
	author = {Li, Li and Kang, Yanfei and Petropoulos, Fotios and Li, Feng},
	date = {2023-11},
}
```
