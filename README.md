# sBGmodel: Estimating Customer Lifetime Value

The *sBGmodel* package provides functions to estimate the customer lifetime value (short: CLV) for contractual discrete transactions.
The contractual discrete setting means that the customer can only come back or leave after the end of one period (e.g. by cancelling the subscription).
This method uses the shifted-beta-geometric (short: sBG) distribution to estimates parameters for a model of customer contract duration. 
Using these estimated parameters the discounted expected residual lifetime (short: DERL) for a customer can be calculated. 
It is applicable for single as well as for multicohort data of customers.

The implementation is based on the work of Peter S. Fader and Bruce G.S. Hardie (see References). 

It also provides the functionalities for customers with different behaviors up to a specific period. 
This can be handled by the parameters cp_period, cp_alpha and cp_beta.
Basically, it estimates two different pairs of alpha and beta - one for the time frame before the given cp_period and one afterwards.
These additional parameters can also be used for calculating the DERL just by including them to the function.


## Function Overview

The package offers the following functions:


| Function  | Short Description | 
| :-------- | :---------------- | 
| sBG_model | Initializes the sBG model with the given parameter list (alpha, beta). |
| fit | Fits the sBG model for single- or multi-cohort data. Returns a list containing the fitted sBG model and the input data frame. | 
| derl | Calculates the DERL using the given sBG model. | 
| absolute_error | Computes the absolute error between the model fit and the actual values per period and cohort and returns it as a matrix. | 
| performance_metrics | Computes performance metrics per period for given input data (currently only absolute error is available). |
| customer_rates | Calculates churn, retention or survival rates for a given period range using the provided sBG model. | 


## Installation in R

```
> devtools::install_github('runtastic/sBGmodel')
```

## Example Code


### Estimate model parameters

Parameters for sBG model can be estimated with the `fit` function for a single cohort or multiple cohorts.

#### Single-cohort

```
> df_input <- data.frame(cohort=rep(1, 8), 
                         period = 1:8, 
                         survived_customers = c(1000, 869, 743, 653, 593, 551, 517, 491))
> model <- fit(df_input)
```

#### Multi-cohort

```
> df_input <- data.frame(cohort = c(1, 1, 1, 2, 2, 3),  
                         period = c(1, 2, 3, 1, 2, 1), 
                         survived_customers = c(158, 98, 53, 118, 85, 109))
> model <- fit(df_input)
```


### Estimate sBG parameters

Parameters for sBG model can also be created manually, for example if you have previously estimated model parameters
available.

### Evaluate model
You can evaluate your model by looking at the absolute error.
```
error_per_period <- absolute_error(model)
```

### Calculate DERL
You can calculate the discounted expected residual lifetime with the `derl` function.
```
> model <- sBG_model(alpha = 2, beta = 3)
> derl <- derl(model, period = 5, discount_rate = 0.75) # calculates DERL for fifth period
```

## Contributing
Bug reports and pull requests are welcome on GitHub at https://github.com/runtastic/sBGmodel. This project is 
intended to be a safe, welcoming space for collaboration, and contributors are expected to adhere to the 
code of conduct.

## Changelog
### Version 1.0 (2019-08-08)
Initial release

## Future improvements
 * support sBG parameter estimation for multi-cohorts with limited information
 * support for further performance metrics (e.g. relative error)
 * model fit and performance visualisations

## References
 * Fader, Peter & G. S. Hardie, Bruce. (2019). Computing DERL for the sBG Model Using Excel. 
 * Fader, Peter & G. S. Hardie, Bruce. (2010). Customer-Base Valuation in a Contractual Setting: The Perils of Ignoring Heterogeneity. Marketing Science. 29. 85-93. 10.2139/ssrn.908802. 
 * Fader, Peter & G. S. Hardie, Bruce. (2019). Fitting the sBG Model to Multi-Cohort Data. 
 * Fader, Peter & G. S. Hardie, Bruce. (2006). How to Project Customer Retention. Journal of Interactive Marketing. 21. 10.1002/dir.20074. 
 
# License
The sBGModel is available as open source under [the terms of the MIT License][mit].

[mit]: https://choosealicense.com/licenses/mit/