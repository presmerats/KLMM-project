# Planning

## Tasks

0. determine if new Y samples have less noise -> decide if changing dataset or applying mov.avg

  a. y -> new dataset
  b. y -> remove noise with a moving average

1. y() as a value or as an increment on the interval?
2. determine if the errors (y target) are actually independent?

3. x: construct feature space (possibly using ts() then -> matrix of values)
  * option 1:  window w
  * option 2: dilated window n*w? only taking value every n values?


4. sample dataset: train, validation & test

5. error/accuracy measure: 
  *      RMSE?, 
  *      accuracy on error up/down class given dt,
  *      accuracy = 1 - predicted_error/error_budget(mean error)  

6. experiment design: 
  - w: different time window -> number of features
  - d: different prediction window
  - kernel hyperparams
  - different kernels
  - different loss functions?
  - different model inputs: 
    - only sops
    - only error
    - both error & sops

7. start training:
  - follow experiment design scheduling


## Experiment design:

hyperparams/experiments to perform:

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

tentative experiments design 1:

Initial hyper params list yields around 12 "factors", simplify to 2 values each -> 2^12 ...
Tentative 1:
  * w: 10, 100, 1000
  * d: 10, 100, 1000
  * linear kernel a:  0.01 0.1 1 10 100
  * linear kernel c:  0.01 0.1 1 10 100
  * RBF kernel sigma? 0.01 0.1 1 10 100
  * input type: sops, error, sops+error, sops+initial_error

For a total of 3*3*5*5*4(linear kernel) + 3*3*5*4(Gaussian kernel) = 1080 model fittings 

tentative experiments design 2:

Do a global search first, then do a local search approach. Meaning that w,d, input type will be searched more broadly and shallowly. Then once those hyperparams have some optimal values, we will intensify our search for the best kernels and kernel params

Tentative 2.1:
  * w: 10, 100, 1000
  * d: 10, 100, 1000
  * linear kernel a?  1
  * linear kernel c?  1
  * RBF kernel sigma? 1
  * input type: sops, error, sops+error, sops+initial_error
Tentative 2.2:
  * w: 10
  * d: 10
  * linear kernel a?  0.01 0.1 1 10 100
  * linear kernel c?  0.01 0.1 1 10 100
  * Polynomial kernel q: 2 3 4 7 11
  * RBF kernel sigma: 0.01 0.1 1 10 100 + 0.1 0.2 0.3 .. 1 (total 15)
  * input type: sops+error 

So now it is 3*3*4 (2.1) + 4*4(linear 2.2) + 4*4*5(poly 2.2) + 15(Gaussian 2.2) = 147 model fittings