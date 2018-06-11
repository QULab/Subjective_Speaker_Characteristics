Correlations between speech features and speaker characteristics / voice descriptions
================
Laura Fernández Gallardo
October 2017

-   [Objectives](#objectives)
-   [Load speech features](#load-speech-features)
-   [Ratings of voice descriptions](#ratings-of-voice-descriptions)
-   [Predicting: "melodisch" for male speakers](#predicting-melodisch-for-male-speakers)
    -   [Pre-processing of speech features](#pre-processing-of-speech-features)
    -   [mlr regression task](#mlr-regression-task)
-   [Conclusions](#conclusions)

``` r
# clear
rm(list=ls())

# Libraries needed:

library(RCurl) # to read raw data from repo
library(caret) # pre-processing and feat imp
library(usdm) #  for variance inflation factor
library(mlr) # regression
```

Objectives
----------

Predicing voice descriptions (VD) from speech features, [extracted with OpenSMILE](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/data/speech_features).

Load speech features
--------------------

``` r
path_github <- "https://raw.githubusercontent.com/laufergall/Subjective_Speaker_Characteristics/master/data/speech_features"

gemaps_m <- read.csv(text=getURL(paste0(path_github,"/eGeMAPSv01a_88_malespk.csv")), header=TRUE, sep=",")
gemaps_f <- read.csv(text=getURL(paste0(path_github,"/eGeMAPSv01a_88_femalespk.csv")), header=TRUE, sep=",")
```

Ratings of voice descriptions
-----------------------------

Subjective ratings were given to 20 speakers: 10 males and 10 females.

``` r
path_github2 <- "https://raw.githubusercontent.com/laufergall/Subjective_Speaker_Characteristics/master/data/subjective_ratings"

# load raw ratings
ratings <- read.csv(text=getURL(paste0(path_github2,"/VD_ratings.csv")), header=TRUE, sep=",")


# mean of ratings and split by gender
ratings_a <- aggregate(ratings[16:(16+33)], by=list(ratings$sample_heard), mean)
names(ratings_a)[1] <- 'sample_heard'

# merge ratings and features (10 samples only for each gender)
data_m <- merge(ratings_a, gemaps_m)
data_f <- merge(ratings_a, gemaps_f)

# just the 88 speech features
feats_m <- data_m[,(ncol(data_m)-88+1):ncol(data_m)]
feats_f <- data_f[,(ncol(data_f)-88+1):ncol(data_f)]
```

Predicting: "melodisch" for male speakers
-----------------------------------------

The item "melodisch" is relevant for the prediction of males' "mitfuehlend", "gelangweilt", "distanziert", "gleichgueltig", "freundlich" , "maennlich", and of females' "gleichgueltig" "ruhig" as seen [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/blob/master/analysis/relationships_SC_VD/linearregression_VD_SC.md).

Additionally, it seems to be easyly separable according to speakers' WAAT, as seen [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/blob/master/analysis/voice_descriptions/explorative_analysis_ratings/analysis_ratings_cleanspeech.md)

### Pre-processing of speech features

Same as done [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/analysis/features_SC_VD).

Using the caret package: Box-Cox transformation to reduce skewness, centering and scaling.

``` r
trans_m <- caret::preProcess(feats_m,
                            method = c('BoxCox','center','scale'))

transformed_m <- predict(trans_m, feats_m)
```

Calculate variance inflation factor (VIF) to deal with multicollinearity problems:

Naimi, B., Hamm, N. A. S., Groen, T. A., Skidmore, A. K. and Toxopeus, A. G.,"Where is Positional Uncertainty a Problem for Species Distribution Modelling?," Ecography, 37, pp. 191-203, 2014.

Identify collinear variables that should be excluded.

From the usdm package: "A VIF greater than 10 is a signal that the model has a collinearity problem".

``` r
# vifcor
#v1 <- vifcor(transformed_m_2, th=0.5) 

# vifstep
v2_m <- vifstep(transformed_m, th=10)

transformed_m_v2 <- exclude(transformed_m,v2_m)
```

### mlr regression task

Predicting "melodisch" for male speakers.

``` r
# just 10 observations and 6 features

mdata <- cbind(transformed_m_v2, data_m$melodisch)
names(mdata)[ncol(mdata)]<-'target'

# data for regression model

regrTask <- mlr::makeRegrTask(data = mdata, target = 'target')   
```

Function to perform regression with different learners and tuning hyperparameters (given as input parameters).

Learner fused with filter method for calculating the feature importance, based on linear correlation (as done [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/blob/master/analysis/features_SC_VD/feature_importance_caret.md)).

Leave-one-out cross-validation as resampling strategy, since we only have 10 observations.

I set the performance metric to be: median(abs(response - truth)).

``` r
regr_fw_LOO_medae <- function(regr.lrn, ps) {

# learner fused with filter method for feature selection
lrn = mlr::makeFilterWrapper(learner = regr.lrn, fw.method = "linear.correlation")

# search space: going through different sizes of the feature subset 
# tuning by gridsearch
# resampling strategy: Leave-one-out Cv
# performance measure: medae

rdesc = mlr::makeResampleDesc("LOO")

res = mlr::tuneParams(lrn, 
                      task = regrTask, 
                      resampling = rdesc, 
                      par.set = ps, 
                      control = makeTuneControlGrid(),
                      measures = list(medae)
                      )

# train model with the optimal feature set found

lrn2 = makeFilterWrapper(learner = regr.lrn, 
                        fw.method = "linear.correlation", 
                        fw.abs = res$x$fw.abs)

mod = train(lrn2, regrTask)

print('selected features:')
cbind(getFilteredFeatures(mod))

}



# interesting plot to check correlations
# plotLearnerPrediction("regr.lm", features = "loudnessPeaksPerSec", task =regrTask)
```

Creating different learners and checking the regession performance.

#### regr.featureless

``` r
regr.lrn = makeLearner("regr.featureless", method = 'mean')
# regr.lrn$par.set

# no features are selected for this regressor anyway
ps = makeParamSet(
  makeDiscreteParam("fw.abs", values = 1)
  )

regr_fw_LOO_medae(regr.lrn, ps)
```

    ## [Tune] Started tuning learner regr.featureless.filtered for parameter set:

    ##            Type len Def Constr Req Tunable Trafo
    ## fw.abs discrete   -   -      1   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: Inf

    ## [Tune-x] 1: fw.abs=1

    ## [Tune-y] 1: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune] Result: fw.abs=1 : medae.test.mean=16.7777778

    ## [1] "selected features:"

    ##      [,1]                 
    ## [1,] "loudnessPeaksPerSec"

#### regr.lm

Linear regression

Parameters: selecting a number of features

``` r
regr.lrn = makeLearner("regr.lm")
# regr.lrn$par.set

# parameters: selecting a number of features
ps = makeParamSet(
  makeDiscreteParam("fw.abs", values = seq(1, ncol(mdata)-1))
  )

regr_fw_LOO_medae(regr.lrn, ps)
```

    ## [Tune] Started tuning learner regr.lm.filtered for parameter set:

    ##            Type len Def      Constr Req Tunable Trafo
    ## fw.abs discrete   -   - 1,2,3,4,5,6   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: Inf

    ## [Tune-x] 1: fw.abs=1

    ## [Tune-y] 1: medae.test.mean=11.2705882; time: 0.0 min

    ## [Tune-x] 2: fw.abs=2

    ## [Tune-y] 2: medae.test.mean=11.9010228; time: 0.0 min

    ## [Tune-x] 3: fw.abs=3

    ## [Tune-y] 3: medae.test.mean=13.0714011; time: 0.0 min

    ## [Tune-x] 4: fw.abs=4

    ## [Tune-y] 4: medae.test.mean=11.0429252; time: 0.0 min

    ## [Tune-x] 5: fw.abs=5

    ## [Tune-y] 5: medae.test.mean=11.6525080; time: 0.0 min

    ## [Tune-x] 6: fw.abs=6

    ## [Tune-y] 6: medae.test.mean=11.5797885; time: 0.0 min

    ## [Tune] Result: fw.abs=4 : medae.test.mean=11.0429252

    ## [1] "selected features:"

    ##      [,1]                         
    ## [1,] "loudnessPeaksPerSec"        
    ## [2,] "MeanVoicedSegmentLengthSec" 
    ## [3,] "StddevUnvoicedSegmentLength"
    ## [4,] "equivalentSoundLevel_dBp"

#### regr.randomForest

Parameters: mtry: Number of variables randomly sampled as candidates at each split ntree: Number of trees to grow

``` r
regr.lrn = makeLearner("regr.randomForest")
regr.lrn$par.set
```

    ##                      Type  len   Def                 Constr Req Tunable
    ## ntree             integer    -   500               1 to Inf   -    TRUE
    ## se.ntree          integer    -   100               1 to Inf   Y    TRUE
    ## se.method        discrete    -    sd bootstrap,jackknife,sd   Y    TRUE
    ## se.boot           integer    -    50               1 to Inf   -    TRUE
    ## mtry              integer    -     -               1 to Inf   -    TRUE
    ## replace           logical    -  TRUE                      -   -    TRUE
    ## strata            untyped    -     -                      -   -   FALSE
    ## sampsize    integervector <NA>     -               1 to Inf   -    TRUE
    ## nodesize          integer    -     5               1 to Inf   -    TRUE
    ## maxnodes          integer    -     -               1 to Inf   -    TRUE
    ## importance        logical    - FALSE                      -   -    TRUE
    ## localImp          logical    - FALSE                      -   -    TRUE
    ## nPerm             integer    -     1            -Inf to Inf   -    TRUE
    ## proximity         logical    - FALSE                      -   -   FALSE
    ## oob.prox          logical    -     -                      -   Y   FALSE
    ## do.trace          logical    - FALSE                      -   -   FALSE
    ## keep.forest       logical    -  TRUE                      -   -   FALSE
    ## keep.inbag        logical    - FALSE                      -   -   FALSE
    ##             Trafo
    ## ntree           -
    ## se.ntree        -
    ## se.method       -
    ## se.boot         -
    ## mtry            -
    ## replace         -
    ## strata          -
    ## sampsize        -
    ## nodesize        -
    ## maxnodes        -
    ## importance      -
    ## localImp        -
    ## nPerm           -
    ## proximity       -
    ## oob.prox        -
    ## do.trace        -
    ## keep.forest     -
    ## keep.inbag      -

``` r
# parameters: selecting a number of features
ps = makeParamSet(
  makeDiscreteParam("fw.abs", values = seq(2, ncol(mdata)-1)),
  makeDiscreteParam("mtry", values = seq(1, 2)),
  makeDiscreteParam("ntree", values = seq(1, 10)) 
  )

regr_fw_LOO_medae(regr.lrn, ps)
```

    ## [Tune] Started tuning learner regr.randomForest.filtered for parameter set:

    ##            Type len Def               Constr Req Tunable Trafo
    ## fw.abs discrete   -   -            2,3,4,5,6   -    TRUE     -
    ## mtry   discrete   -   -                  1,2   -    TRUE     -
    ## ntree  discrete   -   - 1,2,3,4,5,6,7,8,9,10   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: Inf

    ## [Tune-x] 1: fw.abs=2; mtry=1; ntree=1

    ## [Tune-y] 1: medae.test.mean=13.4682692; time: 0.0 min

    ## [Tune-x] 2: fw.abs=3; mtry=1; ntree=1

    ## [Tune-y] 2: medae.test.mean=14.1480769; time: 0.0 min

    ## [Tune-x] 3: fw.abs=4; mtry=1; ntree=1

    ## [Tune-y] 3: medae.test.mean=21.0703846; time: 0.0 min

    ## [Tune-x] 4: fw.abs=5; mtry=1; ntree=1

    ## [Tune-y] 4: medae.test.mean=23.5551282; time: 0.0 min

    ## [Tune-x] 5: fw.abs=6; mtry=1; ntree=1

    ## [Tune-y] 5: medae.test.mean=20.3945513; time: 0.0 min

    ## [Tune-x] 6: fw.abs=2; mtry=2; ntree=1

    ## [Tune-y] 6: medae.test.mean=10.1219231; time: 0.0 min

    ## [Tune-x] 7: fw.abs=3; mtry=2; ntree=1

    ## [Tune-y] 7: medae.test.mean=22.6069872; time: 0.0 min

    ## [Tune-x] 8: fw.abs=4; mtry=2; ntree=1

    ## [Tune-y] 8: medae.test.mean=15.5855128; time: 0.0 min

    ## [Tune-x] 9: fw.abs=5; mtry=2; ntree=1

    ## [Tune-y] 9: medae.test.mean=16.1058333; time: 0.0 min

    ## [Tune-x] 10: fw.abs=6; mtry=2; ntree=1

    ## [Tune-y] 10: medae.test.mean=19.6332051; time: 0.0 min

    ## [Tune-x] 11: fw.abs=2; mtry=1; ntree=2

    ## [Tune-y] 11: medae.test.mean=12.4357372; time: 0.0 min

    ## [Tune-x] 12: fw.abs=3; mtry=1; ntree=2

    ## [Tune-y] 12: medae.test.mean=17.7240064; time: 0.0 min

    ## [Tune-x] 13: fw.abs=4; mtry=1; ntree=2

    ## [Tune-y] 13: medae.test.mean=11.7753526; time: 0.0 min

    ## [Tune-x] 14: fw.abs=5; mtry=1; ntree=2

    ## [Tune-y] 14: medae.test.mean=19.5728846; time: 0.0 min

    ## [Tune-x] 15: fw.abs=6; mtry=1; ntree=2

    ## [Tune-y] 15: medae.test.mean=18.7297756; time: 0.0 min

    ## [Tune-x] 16: fw.abs=2; mtry=2; ntree=2

    ## [Tune-y] 16: medae.test.mean=8.3436538; time: 0.0 min

    ## [Tune-x] 17: fw.abs=3; mtry=2; ntree=2

    ## [Tune-y] 17: medae.test.mean=16.0762821; time: 0.0 min

    ## [Tune-x] 18: fw.abs=4; mtry=2; ntree=2

    ## [Tune-y] 18: medae.test.mean=16.4755449; time: 0.0 min

    ## [Tune-x] 19: fw.abs=5; mtry=2; ntree=2

    ## [Tune-y] 19: medae.test.mean=16.7079167; time: 0.0 min

    ## [Tune-x] 20: fw.abs=6; mtry=2; ntree=2

    ## [Tune-y] 20: medae.test.mean=17.0590064; time: 0.0 min

    ## [Tune-x] 21: fw.abs=2; mtry=1; ntree=3

    ## [Tune-y] 21: medae.test.mean=12.6958333; time: 0.0 min

    ## [Tune-x] 22: fw.abs=3; mtry=1; ntree=3

    ## [Tune-y] 22: medae.test.mean=9.5056624; time: 0.0 min

    ## [Tune-x] 23: fw.abs=4; mtry=1; ntree=3

    ## [Tune-y] 23: medae.test.mean=19.4530769; time: 0.0 min

    ## [Tune-x] 24: fw.abs=5; mtry=1; ntree=3

    ## [Tune-y] 24: medae.test.mean=15.7633761; time: 0.0 min

    ## [Tune-x] 25: fw.abs=6; mtry=1; ntree=3

    ## [Tune-y] 25: medae.test.mean=18.3785470; time: 0.0 min

    ## [Tune-x] 26: fw.abs=2; mtry=2; ntree=3

    ## [Tune-y] 26: medae.test.mean=11.4802564; time: 0.0 min

    ## [Tune-x] 27: fw.abs=3; mtry=2; ntree=3

    ## [Tune-y] 27: medae.test.mean=13.5920940; time: 0.0 min

    ## [Tune-x] 28: fw.abs=4; mtry=2; ntree=3

    ## [Tune-y] 28: medae.test.mean=14.0703205; time: 0.0 min

    ## [Tune-x] 29: fw.abs=5; mtry=2; ntree=3

    ## [Tune-y] 29: medae.test.mean=17.7629060; time: 0.0 min

    ## [Tune-x] 30: fw.abs=6; mtry=2; ntree=3

    ## [Tune-y] 30: medae.test.mean=15.7003632; time: 0.0 min

    ## [Tune-x] 31: fw.abs=2; mtry=1; ntree=4

    ## [Tune-y] 31: medae.test.mean=14.4855609; time: 0.0 min

    ## [Tune-x] 32: fw.abs=3; mtry=1; ntree=4

    ## [Tune-y] 32: medae.test.mean=16.1745833; time: 0.0 min

    ## [Tune-x] 33: fw.abs=4; mtry=1; ntree=4

    ## [Tune-y] 33: medae.test.mean=16.7404647; time: 0.0 min

    ## [Tune-x] 34: fw.abs=5; mtry=1; ntree=4

    ## [Tune-y] 34: medae.test.mean=15.4827083; time: 0.0 min

    ## [Tune-x] 35: fw.abs=6; mtry=1; ntree=4

    ## [Tune-y] 35: medae.test.mean=15.5300962; time: 0.0 min

    ## [Tune-x] 36: fw.abs=2; mtry=2; ntree=4

    ## [Tune-y] 36: medae.test.mean=13.1596154; time: 0.0 min

    ## [Tune-x] 37: fw.abs=3; mtry=2; ntree=4

    ## [Tune-y] 37: medae.test.mean=13.9660897; time: 0.0 min

    ## [Tune-x] 38: fw.abs=4; mtry=2; ntree=4

    ## [Tune-y] 38: medae.test.mean=14.9963622; time: 0.0 min

    ## [Tune-x] 39: fw.abs=5; mtry=2; ntree=4

    ## [Tune-y] 39: medae.test.mean=12.9985256; time: 0.0 min

    ## [Tune-x] 40: fw.abs=6; mtry=2; ntree=4

    ## [Tune-y] 40: medae.test.mean=13.1716346; time: 0.0 min

    ## [Tune-x] 41: fw.abs=2; mtry=1; ntree=5

    ## [Tune-y] 41: medae.test.mean=15.7930385; time: 0.0 min

    ## [Tune-x] 42: fw.abs=3; mtry=1; ntree=5

    ## [Tune-y] 42: medae.test.mean=14.7051026; time: 0.0 min

    ## [Tune-x] 43: fw.abs=4; mtry=1; ntree=5

    ## [Tune-y] 43: medae.test.mean=15.1418974; time: 0.0 min

    ## [Tune-x] 44: fw.abs=5; mtry=1; ntree=5

    ## [Tune-y] 44: medae.test.mean=15.6290769; time: 0.0 min

    ## [Tune-x] 45: fw.abs=6; mtry=1; ntree=5

    ## [Tune-y] 45: medae.test.mean=13.2041282; time: 0.0 min

    ## [Tune-x] 46: fw.abs=2; mtry=2; ntree=5

    ## [Tune-y] 46: medae.test.mean=11.8606154; time: 0.0 min

    ## [Tune-x] 47: fw.abs=3; mtry=2; ntree=5

    ## [Tune-y] 47: medae.test.mean=14.2245000; time: 0.0 min

    ## [Tune-x] 48: fw.abs=4; mtry=2; ntree=5

    ## [Tune-y] 48: medae.test.mean=15.7298462; time: 0.0 min

    ## [Tune-x] 49: fw.abs=5; mtry=2; ntree=5

    ## [Tune-y] 49: medae.test.mean=15.1176538; time: 0.0 min

    ## [Tune-x] 50: fw.abs=6; mtry=2; ntree=5

    ## [Tune-y] 50: medae.test.mean=16.2271923; time: 0.0 min

    ## [Tune-x] 51: fw.abs=2; mtry=1; ntree=6

    ## [Tune-y] 51: medae.test.mean=14.6224252; time: 0.0 min

    ## [Tune-x] 52: fw.abs=3; mtry=1; ntree=6

    ## [Tune-y] 52: medae.test.mean=18.4520085; time: 0.0 min

    ## [Tune-x] 53: fw.abs=4; mtry=1; ntree=6

    ## [Tune-y] 53: medae.test.mean=17.8951389; time: 0.0 min

    ## [Tune-x] 54: fw.abs=5; mtry=1; ntree=6

    ## [Tune-y] 54: medae.test.mean=15.8300000; time: 0.0 min

    ## [Tune-x] 55: fw.abs=6; mtry=1; ntree=6

    ## [Tune-y] 55: medae.test.mean=16.8498504; time: 0.0 min

    ## [Tune-x] 56: fw.abs=2; mtry=2; ntree=6

    ## [Tune-y] 56: medae.test.mean=12.8758868; time: 0.0 min

    ## [Tune-x] 57: fw.abs=3; mtry=2; ntree=6

    ## [Tune-y] 57: medae.test.mean=14.5684081; time: 0.0 min

    ## [Tune-x] 58: fw.abs=4; mtry=2; ntree=6

    ## [Tune-y] 58: medae.test.mean=17.1264530; time: 0.0 min

    ## [Tune-x] 59: fw.abs=5; mtry=2; ntree=6

    ## [Tune-y] 59: medae.test.mean=13.6409936; time: 0.0 min

    ## [Tune-x] 60: fw.abs=6; mtry=2; ntree=6

    ## [Tune-y] 60: medae.test.mean=16.5464530; time: 0.0 min

    ## [Tune-x] 61: fw.abs=2; mtry=1; ntree=7

    ## [Tune-y] 61: medae.test.mean=15.0370421; time: 0.0 min

    ## [Tune-x] 62: fw.abs=3; mtry=1; ntree=7

    ## [Tune-y] 62: medae.test.mean=15.2293223; time: 0.0 min

    ## [Tune-x] 63: fw.abs=4; mtry=1; ntree=7

    ## [Tune-y] 63: medae.test.mean=16.8828388; time: 0.0 min

    ## [Tune-x] 64: fw.abs=5; mtry=1; ntree=7

    ## [Tune-y] 64: medae.test.mean=13.9721978; time: 0.0 min

    ## [Tune-x] 65: fw.abs=6; mtry=1; ntree=7

    ## [Tune-y] 65: medae.test.mean=18.8976374; time: 0.0 min

    ## [Tune-x] 66: fw.abs=2; mtry=2; ntree=7

    ## [Tune-y] 66: medae.test.mean=10.9294048; time: 0.0 min

    ## [Tune-x] 67: fw.abs=3; mtry=2; ntree=7

    ## [Tune-y] 67: medae.test.mean=17.2040476; time: 0.0 min

    ## [Tune-x] 68: fw.abs=4; mtry=2; ntree=7

    ## [Tune-y] 68: medae.test.mean=16.6868407; time: 0.0 min

    ## [Tune-x] 69: fw.abs=5; mtry=2; ntree=7

    ## [Tune-y] 69: medae.test.mean=15.2564103; time: 0.0 min

    ## [Tune-x] 70: fw.abs=6; mtry=2; ntree=7

    ## [Tune-y] 70: medae.test.mean=16.9050641; time: 0.0 min

    ## [Tune-x] 71: fw.abs=2; mtry=1; ntree=8

    ## [Tune-y] 71: medae.test.mean=15.5250721; time: 0.0 min

    ## [Tune-x] 72: fw.abs=3; mtry=1; ntree=8

    ## [Tune-y] 72: medae.test.mean=17.4994471; time: 0.0 min

    ## [Tune-x] 73: fw.abs=4; mtry=1; ntree=8

    ## [Tune-y] 73: medae.test.mean=15.7489583; time: 0.0 min

    ## [Tune-x] 74: fw.abs=5; mtry=1; ntree=8

    ## [Tune-y] 74: medae.test.mean=14.6366426; time: 0.0 min

    ## [Tune-x] 75: fw.abs=6; mtry=1; ntree=8

    ## [Tune-y] 75: medae.test.mean=15.8153125; time: 0.0 min

    ## [Tune-x] 76: fw.abs=2; mtry=2; ntree=8

    ## [Tune-y] 76: medae.test.mean=12.6890064; time: 0.0 min

    ## [Tune-x] 77: fw.abs=3; mtry=2; ntree=8

    ## [Tune-y] 77: medae.test.mean=15.1934375; time: 0.0 min

    ## [Tune-x] 78: fw.abs=4; mtry=2; ntree=8

    ## [Tune-y] 78: medae.test.mean=16.5680929; time: 0.0 min

    ## [Tune-x] 79: fw.abs=5; mtry=2; ntree=8

    ## [Tune-y] 79: medae.test.mean=15.3141266; time: 0.0 min

    ## [Tune-x] 80: fw.abs=6; mtry=2; ntree=8

    ## [Tune-y] 80: medae.test.mean=17.7259295; time: 0.0 min

    ## [Tune-x] 81: fw.abs=2; mtry=1; ntree=9

    ## [Tune-y] 81: medae.test.mean=16.1094302; time: 0.0 min

    ## [Tune-x] 82: fw.abs=3; mtry=1; ntree=9

    ## [Tune-y] 82: medae.test.mean=15.6320513; time: 0.0 min

    ## [Tune-x] 83: fw.abs=4; mtry=1; ntree=9

    ## [Tune-y] 83: medae.test.mean=16.7592379; time: 0.0 min

    ## [Tune-x] 84: fw.abs=5; mtry=1; ntree=9

    ## [Tune-y] 84: medae.test.mean=16.5192593; time: 0.0 min

    ## [Tune-x] 85: fw.abs=6; mtry=1; ntree=9

    ## [Tune-y] 85: medae.test.mean=18.0826140; time: 0.0 min

    ## [Tune-x] 86: fw.abs=2; mtry=2; ntree=9

    ## [Tune-y] 86: medae.test.mean=13.4697436; time: 0.0 min

    ## [Tune-x] 87: fw.abs=3; mtry=2; ntree=9

    ## [Tune-y] 87: medae.test.mean=15.8650427; time: 0.0 min

    ## [Tune-x] 88: fw.abs=4; mtry=2; ntree=9

    ## [Tune-y] 88: medae.test.mean=16.3243519; time: 0.0 min

    ## [Tune-x] 89: fw.abs=5; mtry=2; ntree=9

    ## [Tune-y] 89: medae.test.mean=16.0369373; time: 0.0 min

    ## [Tune-x] 90: fw.abs=6; mtry=2; ntree=9

    ## [Tune-y] 90: medae.test.mean=14.5051923; time: 0.0 min

    ## [Tune-x] 91: fw.abs=2; mtry=1; ntree=10

    ## [Tune-y] 91: medae.test.mean=15.1752564; time: 0.0 min

    ## [Tune-x] 92: fw.abs=3; mtry=1; ntree=10

    ## [Tune-y] 92: medae.test.mean=15.9538782; time: 0.0 min

    ## [Tune-x] 93: fw.abs=4; mtry=1; ntree=10

    ## [Tune-y] 93: medae.test.mean=14.7955641; time: 0.0 min

    ## [Tune-x] 94: fw.abs=5; mtry=1; ntree=10

    ## [Tune-y] 94: medae.test.mean=14.6556090; time: 0.0 min

    ## [Tune-x] 95: fw.abs=6; mtry=1; ntree=10

    ## [Tune-y] 95: medae.test.mean=16.9049103; time: 0.0 min

    ## [Tune-x] 96: fw.abs=2; mtry=2; ntree=10

    ## [Tune-y] 96: medae.test.mean=11.6077436; time: 0.0 min

    ## [Tune-x] 97: fw.abs=3; mtry=2; ntree=10

    ## [Tune-y] 97: medae.test.mean=16.4023974; time: 0.0 min

    ## [Tune-x] 98: fw.abs=4; mtry=2; ntree=10

    ## [Tune-y] 98: medae.test.mean=15.8783205; time: 0.0 min

    ## [Tune-x] 99: fw.abs=5; mtry=2; ntree=10

    ## [Tune-y] 99: medae.test.mean=15.8991410; time: 0.0 min

    ## [Tune-x] 100: fw.abs=6; mtry=2; ntree=10

    ## [Tune-y] 100: medae.test.mean=14.8619359; time: 0.0 min

    ## [Tune] Result: fw.abs=2; mtry=2; ntree=2 : medae.test.mean=8.3436538

    ## [1] "selected features:"

    ##      [,1]                      
    ## [1,] "loudnessPeaksPerSec"     
    ## [2,] "equivalentSoundLevel_dBp"

#### regr.rknn

Random k-Nearest-Neighbors Parameters: selecting a number of features and k

``` r
regr.lrn = makeLearner("regr.rknn")
regr.lrn$par.set
```

    ##            Type len    Def   Constr Req Tunable Trafo
    ## k       integer   -      1 1 to Inf   -    TRUE     -
    ## r       integer   -    500 1 to Inf   -    TRUE     -
    ## mtry    integer   -      - 1 to Inf   -    TRUE     -
    ## seed    integer   -   2015 1 to Inf   -    TRUE     -
    ## cluster untyped   - <NULL>        -   -    TRUE     -

``` r
ps = makeParamSet(
  makeDiscreteParam("fw.abs", values = seq(2, ncol(mdata)-1)),
  makeDiscreteParam("k", values = seq(1, 5))
  )

regr_fw_LOO_medae(regr.lrn, ps)
```

    ## [Tune] Started tuning learner regr.rknn.filtered for parameter set:

    ##            Type len Def    Constr Req Tunable Trafo
    ## fw.abs discrete   -   - 2,3,4,5,6   -    TRUE     -
    ## k      discrete   -   - 1,2,3,4,5   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: Inf

    ## [Tune-x] 1: fw.abs=2; k=1

    ## [Tune-y] 1: medae.test.mean=14.8542154; time: 0.0 min

    ## [Tune-x] 2: fw.abs=3; k=1

    ## [Tune-y] 2: medae.test.mean=16.7276000; time: 0.0 min

    ## [Tune-x] 3: fw.abs=4; k=1

    ## [Tune-y] 3: medae.test.mean=17.4817846; time: 0.0 min

    ## [Tune-x] 4: fw.abs=5; k=1

    ## [Tune-y] 4: medae.test.mean=15.4727846; time: 0.0 min

    ## [Tune-x] 5: fw.abs=6; k=1

    ## [Tune-y] 5: medae.test.mean=16.2864615; time: 0.0 min

    ## [Tune-x] 6: fw.abs=2; k=2

    ## [Tune-y] 6: medae.test.mean=14.5290538; time: 0.0 min

    ## [Tune-x] 7: fw.abs=3; k=2

    ## [Tune-y] 7: medae.test.mean=15.6910000; time: 0.0 min

    ## [Tune-x] 8: fw.abs=4; k=2

    ## [Tune-y] 8: medae.test.mean=15.8657769; time: 0.0 min

    ## [Tune-x] 9: fw.abs=5; k=2

    ## [Tune-y] 9: medae.test.mean=15.1174923; time: 0.0 min

    ## [Tune-x] 10: fw.abs=6; k=2

    ## [Tune-y] 10: medae.test.mean=15.2423885; time: 0.0 min

    ## [Tune-x] 11: fw.abs=2; k=3

    ## [Tune-y] 11: medae.test.mean=13.4296872; time: 0.0 min

    ## [Tune-x] 12: fw.abs=3; k=3

    ## [Tune-y] 12: medae.test.mean=15.2451179; time: 0.0 min

    ## [Tune-x] 13: fw.abs=4; k=3

    ## [Tune-y] 13: medae.test.mean=16.5984231; time: 0.0 min

    ## [Tune-x] 14: fw.abs=5; k=3

    ## [Tune-y] 14: medae.test.mean=16.1415231; time: 0.0 min

    ## [Tune-x] 15: fw.abs=6; k=3

    ## [Tune-y] 15: medae.test.mean=16.1764744; time: 0.0 min

    ## [Tune-x] 16: fw.abs=2; k=4

    ## [Tune-y] 16: medae.test.mean=14.2716962; time: 0.0 min

    ## [Tune-x] 17: fw.abs=3; k=4

    ## [Tune-y] 17: medae.test.mean=16.0973327; time: 0.0 min

    ## [Tune-x] 18: fw.abs=4; k=4

    ## [Tune-y] 18: medae.test.mean=16.1411212; time: 0.0 min

    ## [Tune-x] 19: fw.abs=5; k=4

    ## [Tune-y] 19: medae.test.mean=16.1055192; time: 0.0 min

    ## [Tune-x] 20: fw.abs=6; k=4

    ## [Tune-y] 20: medae.test.mean=16.0767346; time: 0.0 min

    ## [Tune-x] 21: fw.abs=2; k=5

    ## [Tune-y] 21: medae.test.mean=14.3637985; time: 0.0 min

    ## [Tune-x] 22: fw.abs=3; k=5

    ## [Tune-y] 22: medae.test.mean=15.8016554; time: 0.0 min

    ## [Tune-x] 23: fw.abs=4; k=5

    ## [Tune-y] 23: medae.test.mean=16.3935508; time: 0.0 min

    ## [Tune-x] 24: fw.abs=5; k=5

    ## [Tune-y] 24: medae.test.mean=16.1769954; time: 0.0 min

    ## [Tune-x] 25: fw.abs=6; k=5

    ## [Tune-y] 25: medae.test.mean=16.1821723; time: 0.0 min

    ## [Tune] Result: fw.abs=2; k=3 : medae.test.mean=13.4296872

    ## [1] "selected features:"

    ##      [,1]                      
    ## [1,] "loudnessPeaksPerSec"     
    ## [2,] "equivalentSoundLevel_dBp"

#### regr.rpart

Decision Tree

Parameters: selecting a number of feature, minsplit

``` r
regr.lrn = makeLearner("regr.rpart")
regr.lrn$par.set
```

    ##                    Type len  Def   Constr Req Tunable Trafo
    ## minsplit        integer   -   20 1 to Inf   -    TRUE     -
    ## minbucket       integer   -    - 1 to Inf   -    TRUE     -
    ## cp              numeric   - 0.01   0 to 1   -    TRUE     -
    ## maxcompete      integer   -    4 0 to Inf   -    TRUE     -
    ## maxsurrogate    integer   -    5 0 to Inf   -    TRUE     -
    ## usesurrogate   discrete   -    2    0,1,2   -    TRUE     -
    ## surrogatestyle discrete   -    0      0,1   -    TRUE     -
    ## maxdepth        integer   -   30  1 to 30   -    TRUE     -
    ## xval            integer   -   10 0 to Inf   -   FALSE     -

``` r
ps = makeParamSet(
  makeDiscreteParam("fw.abs", values = seq(1, ncol(mdata)-1)),
  makeDiscreteParam("minsplit", values = seq(1, 10))
  )

regr_fw_LOO_medae(regr.lrn, ps)
```

    ## [Tune] Started tuning learner regr.rpart.filtered for parameter set:

    ##              Type len Def               Constr Req Tunable Trafo
    ## fw.abs   discrete   -   -          1,2,3,4,5,6   -    TRUE     -
    ## minsplit discrete   -   - 1,2,3,4,5,6,7,8,9,10   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: Inf

    ## [Tune-x] 1: fw.abs=1; minsplit=1

    ## [Tune-y] 1: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 2: fw.abs=2; minsplit=1

    ## [Tune-y] 2: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 3: fw.abs=3; minsplit=1

    ## [Tune-y] 3: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 4: fw.abs=4; minsplit=1

    ## [Tune-y] 4: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 5: fw.abs=5; minsplit=1

    ## [Tune-y] 5: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 6: fw.abs=6; minsplit=1

    ## [Tune-y] 6: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 7: fw.abs=1; minsplit=2

    ## [Tune-y] 7: medae.test.mean=14.6490385; time: 0.0 min

    ## [Tune-x] 8: fw.abs=2; minsplit=2

    ## [Tune-y] 8: medae.test.mean=14.4746795; time: 0.0 min

    ## [Tune-x] 9: fw.abs=3; minsplit=2

    ## [Tune-y] 9: medae.test.mean=17.5849359; time: 0.0 min

    ## [Tune-x] 10: fw.abs=4; minsplit=2

    ## [Tune-y] 10: medae.test.mean=14.5913462; time: 0.0 min

    ## [Tune-x] 11: fw.abs=5; minsplit=2

    ## [Tune-y] 11: medae.test.mean=16.1964744; time: 0.0 min

    ## [Tune-x] 12: fw.abs=6; minsplit=2

    ## [Tune-y] 12: medae.test.mean=12.8349359; time: 0.0 min

    ## [Tune-x] 13: fw.abs=1; minsplit=3

    ## [Tune-y] 13: medae.test.mean=14.3798077; time: 0.0 min

    ## [Tune-x] 14: fw.abs=2; minsplit=3

    ## [Tune-y] 14: medae.test.mean=17.6939103; time: 0.0 min

    ## [Tune-x] 15: fw.abs=3; minsplit=3

    ## [Tune-y] 15: medae.test.mean=19.1945513; time: 0.0 min

    ## [Tune-x] 16: fw.abs=4; minsplit=3

    ## [Tune-y] 16: medae.test.mean=16.2009615; time: 0.0 min

    ## [Tune-x] 17: fw.abs=5; minsplit=3

    ## [Tune-y] 17: medae.test.mean=16.1964744; time: 0.0 min

    ## [Tune-x] 18: fw.abs=6; minsplit=3

    ## [Tune-y] 18: medae.test.mean=12.8349359; time: 0.0 min

    ## [Tune-x] 19: fw.abs=1; minsplit=4

    ## [Tune-y] 19: medae.test.mean=13.3054487; time: 0.0 min

    ## [Tune-x] 20: fw.abs=2; minsplit=4

    ## [Tune-y] 20: medae.test.mean=13.2945513; time: 0.0 min

    ## [Tune-x] 21: fw.abs=3; minsplit=4

    ## [Tune-y] 21: medae.test.mean=16.8939103; time: 0.0 min

    ## [Tune-x] 22: fw.abs=4; minsplit=4

    ## [Tune-y] 22: medae.test.mean=13.9003205; time: 0.0 min

    ## [Tune-x] 23: fw.abs=5; minsplit=4

    ## [Tune-y] 23: medae.test.mean=15.7727564; time: 0.0 min

    ## [Tune-x] 24: fw.abs=6; minsplit=4

    ## [Tune-y] 24: medae.test.mean=12.4112179; time: 0.0 min

    ## [Tune-x] 25: fw.abs=1; minsplit=5

    ## [Tune-y] 25: medae.test.mean=13.3541667; time: 0.0 min

    ## [Tune-x] 26: fw.abs=2; minsplit=5

    ## [Tune-y] 26: medae.test.mean=13.3432692; time: 0.0 min

    ## [Tune-x] 27: fw.abs=3; minsplit=5

    ## [Tune-y] 27: medae.test.mean=13.4560897; time: 0.0 min

    ## [Tune-x] 28: fw.abs=4; minsplit=5

    ## [Tune-y] 28: medae.test.mean=13.4560897; time: 0.0 min

    ## [Tune-x] 29: fw.abs=5; minsplit=5

    ## [Tune-y] 29: medae.test.mean=11.3894231; time: 0.0 min

    ## [Tune-x] 30: fw.abs=6; minsplit=5

    ## [Tune-y] 30: medae.test.mean=12.7400641; time: 0.0 min

    ## [Tune-x] 31: fw.abs=1; minsplit=6

    ## [Tune-y] 31: medae.test.mean=10.2873718; time: 0.0 min

    ## [Tune-x] 32: fw.abs=2; minsplit=6

    ## [Tune-y] 32: medae.test.mean=11.5751923; time: 0.0 min

    ## [Tune-x] 33: fw.abs=3; minsplit=6

    ## [Tune-y] 33: medae.test.mean=11.5751923; time: 0.0 min

    ## [Tune-x] 34: fw.abs=4; minsplit=6

    ## [Tune-y] 34: medae.test.mean=11.5751923; time: 0.0 min

    ## [Tune-x] 35: fw.abs=5; minsplit=6

    ## [Tune-y] 35: medae.test.mean=11.5751923; time: 0.0 min

    ## [Tune-x] 36: fw.abs=6; minsplit=6

    ## [Tune-y] 36: medae.test.mean=13.3540385; time: 0.0 min

    ## [Tune-x] 37: fw.abs=1; minsplit=7

    ## [Tune-y] 37: medae.test.mean=10.2873718; time: 0.0 min

    ## [Tune-x] 38: fw.abs=2; minsplit=7

    ## [Tune-y] 38: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 39: fw.abs=3; minsplit=7

    ## [Tune-y] 39: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 40: fw.abs=4; minsplit=7

    ## [Tune-y] 40: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 41: fw.abs=5; minsplit=7

    ## [Tune-y] 41: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 42: fw.abs=6; minsplit=7

    ## [Tune-y] 42: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 43: fw.abs=1; minsplit=8

    ## [Tune-y] 43: medae.test.mean=10.2873718; time: 0.0 min

    ## [Tune-x] 44: fw.abs=2; minsplit=8

    ## [Tune-y] 44: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 45: fw.abs=3; minsplit=8

    ## [Tune-y] 45: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 46: fw.abs=4; minsplit=8

    ## [Tune-y] 46: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 47: fw.abs=5; minsplit=8

    ## [Tune-y] 47: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 48: fw.abs=6; minsplit=8

    ## [Tune-y] 48: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 49: fw.abs=1; minsplit=9

    ## [Tune-y] 49: medae.test.mean=10.2873718; time: 0.0 min

    ## [Tune-x] 50: fw.abs=2; minsplit=9

    ## [Tune-y] 50: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 51: fw.abs=3; minsplit=9

    ## [Tune-y] 51: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 52: fw.abs=4; minsplit=9

    ## [Tune-y] 52: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 53: fw.abs=5; minsplit=9

    ## [Tune-y] 53: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 54: fw.abs=6; minsplit=9

    ## [Tune-y] 54: medae.test.mean=12.6226282; time: 0.0 min

    ## [Tune-x] 55: fw.abs=1; minsplit=10

    ## [Tune-y] 55: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 56: fw.abs=2; minsplit=10

    ## [Tune-y] 56: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 57: fw.abs=3; minsplit=10

    ## [Tune-y] 57: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 58: fw.abs=4; minsplit=10

    ## [Tune-y] 58: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 59: fw.abs=5; minsplit=10

    ## [Tune-y] 59: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune-x] 60: fw.abs=6; minsplit=10

    ## [Tune-y] 60: medae.test.mean=16.7777778; time: 0.0 min

    ## [Tune] Result: fw.abs=1; minsplit=9 : medae.test.mean=10.2873718

    ## [1] "selected features:"

    ##      [,1]                 
    ## [1,] "loudnessPeaksPerSec"

Conclusions
-----------

-   Performance (medae) on the regression task predicting subjective ratings given to "melodisch" for male speakers:

    -   Baseline (selects mean): 16.8
    -   Linear Regression: 11.0
    -   Random Forest: 7.5 &lt;- best performance, with just 2 selected features: "loudnessPeaksPerSec" and "equivalentSoundLevel\_dBp"
    -   Random k-Nearest-Neighbors: 13.6
    -   Decision Tree: 10.3

-   This work contributes to the Lens model for the prediction of speaker characteristics given speech segments.

Following the Brunswik Lens Model revised by Scherer (1978), the features that can be extracted from the speech signal (e.g. pitch and formant frequencies, speech tempo, etc.) can be seen as "Distal Cues", whereas the collected VC-subjective labels represent the "Proximal Percepts" that directly account for the final listeners' impressions of speakers (speaker characteristics). The NSC data facilitates the research necessary to clarify the relationship between "Distal Cues" and "Proximal Percepts", which should lead to machines reaching the human performance in the attribution of speaker social characteristics.

-   The 2 steps are:

    -   from measurable acoustic speech parameters to voice desctiptions: this script shows that this can be done. Instead of "melodisch", we can also try to predict any of the other 33 subjective voice descriptions, such as "gepresst", "mit akzent", "unbetont", ... for male and for female speakers.
    -   from voice descritions to speaker characteristics: there exist linear relationships that permit accurate predictions as shown [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/blob/master/analysis/relationships_SC_VD/linearregression_VD_SC.md)

-   Future work: more data needs to be collected to corroborate this, especially more subjective voice descriptions from other speakers. Here, we only have 20. The NSC database can be used to this aim.
