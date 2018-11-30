# Planning

## Quicktasks

* finish preprocessings -> Rdata/csv, not all...
* lagged y
* training function:
  * select columns of df according to model (sop, sop+error, error)
  * split train, val, test
  * train
  * rmse val + plot val
  * save rmse val to file results?

## Tasks

### 0. determine if new Y samples have less noise -> decide if changing dataset or applying mov.avg
  a. __y -> new dataset comparison__
  b. __y -> remove noise with a moving average__

###  2. determine if the errors (y target) are actually independent?
  * this is related to regression samples ->for each t there's x and xt, xt-1,..,xt-w and yt
  * can it be considered independent?
  * covariance/correlation with sop vars? (will only extract linear relationship)

###  1. y() as a value or as an increment on the interval?
  prepare both:
  * for different intervals 10ms 20ms 50ms 100ms 500ms 1000ms
  * with t-w and t+d but for each t
  * ok-- with y(t+d)

###  3. x: construct feature space (possibly using ts() then -> matrix of values)
  * option 1: window w. for different w intervals 10ms, 20ms, 50ms, 100ms, 500ms, 1000ms
  * option 2: dilated window n*w? only taking value every n values? 
  * option 3: dilated decreaseing window: like every 5, every 10, every 20, 40, 80, 160,..

### 3b. save training data versions x+y for the different combinations
done
  

  
pending
  0. fix preprocessing -> by each group trim beginning and end 
  1. test all preprocessings on a small/mid dataset (10 experiments)
  1. x+future_y for w=50,100,500  d=20,100,1000 dsw=33,100,330 maw=100
  1. x+y+future_y w=10,20,50,100,500,1000ms and d=10,20,50,100,500,1000ms
  2. x_dilated window+y  for n=10,100 w=10,20,.1000 d=10,20,..1000
  3. x+y_ini-present-futurey for n=.. w=.. d=..
  4. x_dilated_window+y_ini-last for n=.. w=.. d=..


### 4. sample dataset: train, validation & test

### 5. error/accuracy measure: 
  *      RMSE?, 
  *      accuracy on error up/down class given dt,
  *      accuracy = 1 - predicted_error/error_budget(mean error)  

### 6. experiment design: 
  - downsampling/aggregation: 4,33,300..? verify this!
  - w: different time window -> number of features
  - d: different prediction window
  - kernel hyperparams
  - different kernels
  - different loss functions?
  - different model inputs: 
    - only sops
    - only error
    - both error & sops

### 7. start training:
  - follow experiment design scheduling


## Experiment design:

### hyperparams/experiments to perform:

* downsampling: 4,33,40,330
* w: time window, how many backward values of each variable to consider
  * w range: 10ms to 1000ms
* d: predict window, how far away in time we will try to predict the error?
  * d range: 10ms to 1000ms
* kernels: 
  * linear kernel
  * polynomial kernel, translation invariant, radial
  * Gaussian RBF kernel
  * look throught the paper kernels for ts analysis
* kernel hyperparams: e for rvm margin, sigma for rbf, etc...
  * Gaussian RBF : sigma 
  * linear kernel: a, c
  * polynomial kernel: a, c, q
* model type based on inputs: 
  * sops only,
  * error only(ts forecast) 
  * sops+error, 
  * sops+initial error in w
* loss functions:?

### tentative experiments design 1:

Initial hyper params list yields around 12 "factors", simplify to 2 values each -> 2^12 ...
Tentative 1:
  * downsampling: 4,33,100,330,1000 + ma for error
  * w: 10, 100, 1000
  * d: 10, 100, 1000
  * linear kernel a:  0.01 0.1 1 10 100
  * linear kernel c:  0.01 0.1 1 10 100
  * RBF kernel sigma? 0.01 0.1 1 10 100
  * input type: sops, error, sops+error, sops+initial_error

For a total of 5*3*3*5*5*4(linear kernel) + 5*3*3*5*4(Gaussian kernel) = 5400 model fittings 


### tentative experiments design 2:

Do a global search first, then do a local search approach. Meaning that w,d, input type will be searched more broadly and shallowly. Then once those hyperparams have some optimal values, we will intensify our search for the best kernels and kernel params

Tentative 2.1:
  * downsampling: 4,33,100,330,1000
  * w: 10, 100, 1000
  * d: 10, 100, 1000
  * linear kernel a?  1
  * linear kernel c?  1
  * RBF kernel sigma? 1
  * input type: sops, error, sops+error, sops+initial_error (model input will select them)
Tentative 2.2:
  * w: 10
  * d: 10
  * linear kernel a?  0.01 0.1 1 10 100
  * linear kernel c?  0.01 0.1 1 10 100
  * Polynomial kernel q: 2 3 4 7 11
  * RBF kernel sigma: 0.01 0.1 1 10 100 + 0.1 0.2 0.3 .. 1 (total 15)
  * input type: sops+error 

So now it is 3*3*4 (2.1) + 4*4(linear 2.2) + 4*4*5(poly 2.2) + 15(Gaussian 2.2) = 147 model fittings