# model_parameters

#### [RW1972](https://victornavarro.org/calmr/articles/RW1972.md)

``` r
model_parameters("RW1972")
```

    ## $name
    ## [1] "alphas"    "betas_on"  "betas_off" "lambdas"  
    ## 
    ## $default_value
    ## [1] 0.4 0.4 0.4 1.0
    ## 
    ## $is_global
    ## [1] FALSE FALSE FALSE FALSE

| Name                |          Symbol          | Description                              |
|:--------------------|:------------------------:|:-----------------------------------------|
| alphas              |         $\alpha$         | Learning rate for presented stimulus     |
| betas_on, betas_off | $\beta_{on},\beta_{off}$ | Intensity of presented and absent target |
| lambdas             |        $\lambda$         | Maximum learning supported by target     |

#### [MAC1975](https://victornavarro.org/calmr/articles/MAC1975.md)

``` r
model_parameters("MAC1975")
```

    ## $name
    ## [1] "alphas"     "min_alphas" "max_alphas" "betas_on"   "betas_off" 
    ## [6] "lambdas"    "thetas"     "gammas"    
    ## 
    ## $default_value
    ## [1] 0.4 0.1 1.0 0.4 0.4 1.0 0.2 0.3
    ## 
    ## $is_global
    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

| Name                   |           Symbol            | Description                                                   |
|:-----------------------|:---------------------------:|:--------------------------------------------------------------|
| alphas                 |          $\alpha$           | Starting associability (learning rate) for presented stimulus |
| min_alphas, max_alphas | $\alpha_{min},\alpha_{max}$ | Minimum and maximum associability for stimulus                |
| betas_on, betas_off    |  $\beta_{on},\beta_{off}$   | Intensity of presented and absent target                      |
| lambdas                |          $\lambda$          | Maximum learning supported by target                          |
| thetas                 |          $\theta$           | Attentional learning rate parameter for stimulus              |
| gammas                 |          $\gamma$           | Attentional learning weight for stimulus                      |

#### [PKH1982](https://victornavarro.org/calmr/articles/PKH1982.md)

``` r
model_parameters("PKH1982")
```

    ## $name
    ## [1] "alphas"     "min_alphas" "max_alphas" "betas_ex"   "betas_in"  
    ## [6] "lambdas"    "thetas"     "gammas"    
    ## 
    ## $default_value
    ## [1] 0.4 0.1 1.0 0.4 0.3 1.0 1.0 0.3
    ## 
    ## $is_global
    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

| Name                   |           Symbol            | Description                                                   |
|:-----------------------|:---------------------------:|:--------------------------------------------------------------|
| alphas                 |          $\alpha$           | Learning rate for presented stimulus                          |
| min_alphas, max_alphas | $\alpha_{min},\alpha_{max}$ | Minimum and maximum associability for stimulus                |
| betas_in, betas_ex     |   $\beta_{in},\beta_{ex}$   | Learning rates for inhibitory and excitatory associations     |
| lambdas                |          $\lambda$          | Maximum learning supported by target                          |
| thetas                 |          $\theta$           | Decay/strengthening associability rate parameter for stimulus |
| gammas                 |          $\gamma$           | Attentional learning weight for stimulus                      |

#### [SM2007](https://victornavarro.org/calmr/articles/SM2007.md)

``` r
model_parameters("SM2007")
```

    ## $name
    ## [1] "alphas"  "lambdas" "omegas"  "rhos"    "gammas"  "taus"    "order"  
    ## 
    ## $default_value
    ## [1] 0.4 1.0 0.2 1.0 1.0 0.2 1.0
    ## 
    ## $is_global
    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

| Name    |  Symbol   | Description                                                  |
|:--------|:---------:|:-------------------------------------------------------------|
| alphas  | $\alpha$  | Learning rate for presented stimulus                         |
| lambdas | $\lambda$ | Maximum learning supported by target                         |
| omegas  | $\omega$  | Weakening rate for presented stimulus                        |
| rhos    |  $\rho$   | Salience contribution for unconditioned activation of target |
| gammas  | $\gamma$  | Contribution of stimulus to comparison process               |
| taus    |  $\tau$   | Learning rate for operator switch                            |
| order   |  $order$  | Order for the comparison process                             |

#### [HDI2020/HD2022](https://victornavarro.org/calmr/articles/HD2022.md)

``` r
model_parameters("HDI2020")
```

    ## $name
    ## [1] "alphas"
    ## 
    ## $default_value
    ## [1] 0.4
    ## 
    ## $is_global
    ## [1] FALSE

``` r
model_parameters("HD2022")
```

    ## $name
    ## [1] "alphas"
    ## 
    ## $default_value
    ## [1] 0.4
    ## 
    ## $is_global
    ## [1] FALSE

| Name   |  Symbol  | Description                          |
|:-------|:--------:|:-------------------------------------|
| alphas | $\alpha$ | Learning rate for presented stimulus |

#### [TD](https://victornavarro.org/calmr/articles/TD.md)

``` r
model_parameters("TD")
```

    ## $name
    ## [1] "alphas"    "betas_on"  "betas_off" "lambdas"   "gamma"     "sigma"    
    ## 
    ## $default_value
    ## [1] 0.05 0.40 0.40 1.00 0.95 0.90
    ## 
    ## $is_global
    ## [1] FALSE FALSE FALSE FALSE  TRUE  TRUE

| Name                |          Symbol          | Description                              |
|:--------------------|:------------------------:|:-----------------------------------------|
| alphas              |         $\alpha$         | Learning rate for presented stimulus     |
| betas_on, betas_off | $\beta_{on},\beta_{off}$ | Intensity of presented and absent target |
| lambdas             |        $\lambda$         | Maximum learning supported by target     |
| gamma               |         $\gamma$         | Temporal discount parameter              |
| sigma               |         $\sigma$         | Rate of decay for eligibility traces     |

#### [ANCCR](https://victornavarro.org/calmr/articles/ANCCR.md)

``` r
model_parameters("ANCCR")
```

    ## $name
    ##  [1] "reward_magnitude"  "betas"             "cost"             
    ##  [4] "temperature"       "threshold"         "k"                
    ##  [7] "w"                 "minimum_rate"      "sampling_interval"
    ## [10] "use_exact_mean"    "t_ratio"           "t_constant"       
    ## [13] "alpha"             "alpha_reward"      "use_timed_alpha"  
    ## [16] "alpha_exponent"    "alpha_init"        "alpha_min"        
    ## [19] "add_beta"          "jitter"           
    ## 
    ## $default_value
    ##  [1] 1.000 1.000 0.000 1.000 0.600 1.000 0.500 0.001 0.200 0.000 1.200    NA
    ## [13] 0.020 0.200 0.000 1.000 1.000 0.000 0.000 1.000
    ## 
    ## $is_global
    ##  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [13]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

| Name                                  |                   Symbol                    | Description                                                                                                |
|:--------------------------------------|:-------------------------------------------:|:-----------------------------------------------------------------------------------------------------------|
| reward_magnitude                      |                 $CW_{j,j}$                  | Reward magnitude for target                                                                                |
| betas                                 |                   $\beta$                   | Unconditional value for target                                                                             |
| cost                                  |                   $cost$                    | Response cost                                                                                              |
| temperature                           |                $temperature$                | Temperature for softmax function                                                                           |
| threshold                             |                  $\theta$                   | Threshold to become meaningful causal target/putative cause                                                |
| k,alpha,alpha_reward                  |         $k,\alpha,\alpha_{reward}$          | Learning rates for predecessor representation, predecessor representation contingency, and causal weights. |
| w                                     |                     $w$                     | Weight for net contingency computation                                                                     |
| minimum_rate                          |              $minimum\_ rate$               | Lower bound on perceivable event rates                                                                     |
| sampling_interval                     |            $sampling\_ interval$            | Time interval to update base rate calculations                                                             |
| use_exact_mean                        |            $use\_ exact\_ mean$             | Whether to use exact mean calculations for $\alpha$                                                        |
| t_ratio                               |                 $t\_ ratio$                 | Ratio to calculate time constant                                                                           |
| use_timed_alpha                       |            $use\_ timed\_ alpha$            | Whether to use exponential decay for $\alpha$                                                              |
| alpha_exponent, alpha_init, alpha_min | $alpha\_ exponent,alpha\_ init,alpha\_ min$ | Parameters for exponential decay of $\alpha$                                                               |
| add_beta                              |                $add\_ beta$                 | Whether to add $\beta$ to dopaminergic activity                                                            |
| jitter                                |                  $jitter$                   | Magnitude of perceptual noise for simultaneous events                                                      |

#### [RAND](https://victornavarro.org/calmr/articles/RAND.md)

``` r
model_parameters("RAND")
```

    ## $name
    ## [1] "alphas"
    ## 
    ## $default_value
    ## [1] 0.4
    ## 
    ## $is_global
    ## [1] FALSE

| Name   |  Symbol  | Description              |
|:-------|:--------:|:-------------------------|
| alphas | $\alpha$ | Placeholder; no meaning. |
