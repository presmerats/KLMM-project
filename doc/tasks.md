# Planning

## Quicktasks

done:

* finish preprocessings -> Rdata/csv, not all...
* lagged y
* training function:
  * select columns of df according to model (sop, sop+error, error)
  * split train, val, test
  * train
  * rmse val + plot val
  * save rmse val to file results?
* improve results.txt -> add total time of training, make oneliner, model_params
* find a good seeting of rvm on a small and working sample -> rbf!
* fix preprocessing -> by each group trim beginning and end  -> verify
    * any value of group n does not have values of previous group m  
* save plot to disk
  
* translate w,d from ms to time points
    dsw=n, 1 point = 0.25ms (4sec/16000=0.00025)
    w=100ms -> w'*n*0.25=w -> w'=w/0.25/n 
    d=50ms  -> d'=d*4/n
    data.preparation -> computation with w' and d'
    ->plot, write, print, change w,d to ms

  * study the ma values vs downsampling  -> not too smooth!
    ok- ideally: w=500-100ms, d=50-200ms, dsw=1-10ms, ma=10ms? -> see suspected noise frequency
    ok- w=200ms, d=100ms, dsw=40(10ms), ma=100(1s avg! too much)
    ok-> print comparison original sampled vs downsampled vs downsampled+ma
      (copy values in the downsampled to have same num of data points)
    ok-> x-> from data points to ms
    ok- ma and dsw must be diffrent!
    ok- some setups put values much  much higher! investigate why
    ok- dwsampled corresponds to last setup?
    ok- decide the final intervals
        dws: 10ms
        ma:  250ms


  * w,d
      w=50,100,200,500, 1000ms
      d=50,100,200ms

  * write in the report schematically with images and tables
    - preprocessing steps
    - selecting downsampling and moving average rates
    - models 
    - model selection procedure
    - comparison with previous work
    - results


  * not necessary! (kernels dont suffer from redundant features I think):
    * x+future_y          for w=50,100,500  d=20,100,1000 dsw=33,100,330 maw=100
    * x+y+future_y        for w=10,20,50,100,500,1000ms and d=10,20,50,100,500,1000ms
    * x_dilated window+y  for n=10,100 w=10,20,.1000 d=10,20,..1000
    * x+y_ini-present-futurey for n=.. w=.. d=..
    * x_dilated_window+y_ini-last for n=.. w=.. d=..

  ok- plot model results (make a func to plot from a data on directory)
      ok- test.model()
      - save model name and plots with dim of the data input


    ok- read Bishop
        ok- SVM for regression read
        ok- SVM for regression code
          https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html
        ok- RVM read

    ok- training/testing func skeletonç
        ok- skeleton pass training func as param
        ok- adapt current rvm code
        ok- svm code
        ok- clean code
        


    ok- fix model name when saving
      svm.rbf__n_1500_sigma_1e-06_e_0.1_C_100_dim__1500___4004_csv_small5_w1000ms_d100ms_wp400_dp40_dsw10_maw250._1000ms_100ms_400_40_10ms_250ms

    ok- verification of w,d,dsw,maw versus datafile name

    ok- training plot -> save name

   ok- test.model
      ok- skeleton
      ok- rvm prediction  -> needs same data size!!!
        ok-pad with data from training???
        ok-pad with repeated data??  -> NOT NECESSARY
        ok-test without padding but colum dim ok
      ok- control column size! extract from model name the dims
      ok- plot and save result to txt

   ok- cross-validation implement
      ok- normal cross-validation
        current scheme
          train(xtain,ytrain)
          predict(xval, yval)

        cross-validation
          ok-1)compute indices xtrains,ytrains,xvals,yvals,xtest,ytest
              ok-> take into account the features length? like nrow(xtest) =ncol(xtest)*0.8
              ok-> test
         ok- 2)foreach j i 1:k
            train(xtrain_j,ytrain_j)
            predict(xval_j, yval_j)
            save(RMSE_j)
          ok-3)mean(RMSE)
          ok-4)save model hyperparams(and datafile and w,d,dsw,maw)
          ok-5)plot?....maybe not
      ok- train again model on all training dataset and return that model!

    ok- Email prof  
        - where to focus testing efforts (kernels?, hyperparams) what else?
            idea e-SVM, nu-SVM, RVM comparison
        - question about n>d what to do? sampling or selection

    ok- present first working experiments (model, RMSE, 2 plots)
        ok- initial explanation
        ok- explain trainin with cross-validation procedure
        ok- train 2 svm model with 2 dif values of sigma, + 2 rvm models

      
    ok- enhance plot
      ok- test.model
      ok- larger, 
      ko- png
      ok- legend
      ok- make doulbe plot write to disk

  ok- sops plot -> select sop by name not num position (will change with other model)


    ok- sop plot -> plot the complete time series for one value and position
        ok-for first group
        ok-avoid ylim to take the 0  of ytest.pred into account

        ok-add raw      
      
        ok-for all groups

        ok-faster for all groups
          save indexes and do a rbind at the end

        ok-verification of predicted/smoothed error
          - xlim of 2 plots the same  

        
    ok- add previous values of fututey 
          to be able to plot previous ytest smoothed

          down.sample.avg
            -> result goes to df
          smoothing
            -> result goes to y
          features.time.window
            -> must do the processing for error but also for y
            -> we add here the features time window for the y

          fix the mess with the columns

    ok- add the possibility to train/test with y_raw and y_smooth

    ok- problem test.model -> second experiment ytest is lost!
        -> problem with rbind ..fixed

    ok- xlab in 2 rows




    goals: 
      sop plot for error signal vs noise
      sop plot for prediction of signal 
      test models with other data (non cv data)
      table of best models
      compute test error also

  ok- KLMM sop plot fix sop range
    ok-0- verify the SOP vars between -2/2
    ok-2- start point manually instead and random if none random  
  ok- KLMM plot x range fix!!
    ok-subsel corresponds to plot range?
      due to w and d
      the plot size is
        range/4000*w + range + d
    ok->we could remove the datapoints > range
  ok- KLMM plot x range in ms instead of data points?
    dsw=1 and dsw=10 must be almost identical, only predicted error or smoothed error must change
    
    ok->xvalues*dsw verify it's correct

    ok-> second plot does not adjust xrange?

    ok->ylim problem...

    ok->with dsw and xvalue the x range changes!
      dsw=1 -> 1 exp lasts 4000ms
      dsw=10 ->1 exp lasts 3000ms

      ok the last w or d ms are dropped in dsw=10

      comes like this in sop.plot
      data.preparation review
     
    ok->predict is plotted at the same time? translation of d required
    
    
  ok- KLMM solve the n in train.model
    ok-> renamed to max_num_rows
    ok-> test without using it -> automatically corrected

  ok- KLMM test other datasets investigate strange behaviour in test time!
    -> not a strange behavior
    -> at least it doesnt seem to be so
  ------
  ok- plot of w and d window from previous paper
  ok- naive bayes results from previous paper
  
  ok- plots of SOP and error to understand problem


  ok- plots of sop and error pred 
    for dsw=10 and dsw=1
    for 2 combs of C,e,sigma

  ok- KLMM plot of error with rectangle where noise and rectangle where signal (dsw=10ms, dsw=1ms, dsw=0)
    ->plot dsw=10, dsw=1, dsw=0.25 in the same plot?

  ok- table with RMSE
  

20181213  --------------------------

  - repeat previous work (read paper or ask)
      discretization
        p for SOP vars
        q for BER target
      input
        s1.1= s1 at t
        s1.2= slope from t-w to t ofs1
        s2.1
        ...
      model
        naive-bayes = table of prob for each "setup"(=input and target case)
        s1.1s1.1s2.1s2.2s3.1s3.2e1 = counts
        for each "config" of s1.1s1.2.. we count the num of e1,e2,...eq 

      test setup
        every experiment:
          2000 samples before event
          14000 samples after
        selection of 100 exp for training
        selection of 100 exp for testing

      error
        Relative prediction error (RPE) = accuracy
        RPE = (predictedBER - realBER)/realBER

        aim
          predict ber balues hifgher than mean + 3 times sd

          fig3
            mean RPE
            max RPE (worst case)

      error2
        predict threshold violation
        accuracy:
        compute mean and sd for all measurements
        threshold = mean + 6*sd

        first test for w -> best w is 150ms
        second test for d -> best d is 100ms
          doubts:
            1-when w=150ms we get threshold anticipation 45% faster? WHAT DOES THIS MEAN?
            2-how is the threshold anticipated?
            3-when d=100ms -> 80ms threshold anticipation? what is this?  

    - next steps
        ok-ask for p and q values -> p=16 and q=8
        ok- w,d ranges w in [0,200] d in [50,300]

        ok- compute the slope in t-w,t for each sop var in each sample (for different values of w)
          w [10,20,30,40,50,100,150,200]
          d [10,50,100,200,300]

        ok- discretize sop values, sop trends, and ber error
          ok- review sxi values!
          ok- slope xi are quite small -> adapt slope discretization ranges
  
pending:        
        - use the seleccio as input data
          ok- first build
          - review NA's on x1

        - ask 
          - only 100 for test and train? or 100 and 100? ~50
          - ask for the slope discretization
            -> max and min slope given fixed w! -> done  linearly
          - ask for the error discretization (normalization or not?)
          - model2
              fixed accuracy and increment delta
          - option ber discr 
              avg de la key[sop config]
              key[sop] non observed -> put mean of all BER observed
          - how are the experiments mixed?  
              custom
              70 30 pero mirar que testing siguin similar

        
        - renameRanges?  
        - rebuild dataset
        - build the df for different values of w and d


    - naive bayes -> review + train with df of each w-d combination
    - compute the error as in the paper
    - comparison

    - svm implementation
    - svm optimization e,C,sigma other kernels...
    - svm for regression, + optimize, see how to compare with the results
    - rvm
    - play with signal processing



  ---------------------
  
  - KLMM correct small glitch at the beginning of new group
    forloop that looks for jumps of more than 20,100,200 in the sop vars
      -> sop.plot
      -> sop.plot.prepare
      -> data.preparation
  
  - KLMM data.preparation improve memory usage
  

  ------
  - KLMM review CV tests
  ko- KLMM review code
  ko- KLMM change the model to max in an interval?

    - downsampling and smoothing is TOO much -> losing some signal
        ok-test
          dsw 10 ma 250  sigma 1e-7
          dsw 1 ma 50  - sigma 1e-7 e 0.1 C 100
          dsw 1 ma 10    sigma 1e-9 very bad
          dsw 1 ma 10    sigma 1e-7 e 0.01 C 100 a bit better
          dsw 1 ma 10    sigma 1e-7 e 0.1 C 10
          dsw 1 ma 10    sigma 1e-7 e 0.01 C 10
        
        report
          1) compare for same dataset, same num data points the 2 dsw/models
            -> conclude about the dsw
            ko.. not conclusive

          2) redo the initial downsampling plots with just one/two files also
          ko no change

          3) do a SOP+error with the downsampling to see that 1ms is probably good accurate
            + add ma in blue




    - create new preprocessed FULL datasets
      dsw1 ma50
      dsw1 ma10
      dsw1 ma5

    - SOP plot sop vars between -2 and 2 ??
      - redo all plots once this is fixed

    - test normalization also

    
    
    
    - problem when raw=FALSE -> some ytest have jumps...
        -> verify error on second file (around 2000)


    - limits in the beginning and in the end
          xtests$y_0 must be downsampled also...
          (removal of future values)
          features.prediction
            -> does the removal of last rows

        
    - in one sample there's the full plot! 
          or use values of a csv in order instead of sampling 
            -> ok but then initial values are cropped bcs of w window (1000ms it's one full second)
          combine both approaches
            -> csv in order
            -> for first sample of a different group(file) plot the complete time series without prediction
      
    - sops plot -> in the top with smaller scale

    - automate results gathering  
      - table with RMSE
          num model, model_type, w,d,dsq,maw,  kernel, params, RMSE
      - for each model
          - plot for first fold validation
          - plot of sop + predictiontest data (in a small range to be able to see it)
      

    - total data transformation (dsw=10, maw=250 and raw)
    
    - write down code explanation
  
    - more models
      -> think a way to dsitribute the data for doing cross validation of so many models (easy-> total rows/ row limitation = num of different models that we could fit)


    - verify the dim verificatino?

  


    - models
      ok- fix svm 
      - nu svm

    - goals
          - how to train,
          - how to predict
          - how to cross val (less needed in RVM)
          - size limitations ?








  * noise vs precision experiment (remove downsampling & ma and train rbf with sigma=1e-3)
  



  * use rbf working kernel -> increase data size until it stops working or takes too long training
    - prepare sizes 5,50,75,100,150,200,
    - could also play with the downsampling... -> better not!
  

  * try other kernels: polynomial, string, laplace


  * comparison with previuos work
    * discretize y values (using the paper q value) <- and use the same error measure as in the paper
    * train svm's on small sizes <- RVM works quite well!
    
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