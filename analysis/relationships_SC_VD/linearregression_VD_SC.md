Linear models from voice descriptions to speaker characteristics
================
Laura Fernández Gallardo
December 2017

-   [Objectives](#objectives)
-   [Loading data](#loading-data)
-   [Linear regression with caret](#linear-regression-with-caret)
    -   [Predicting SC from VD of male speakers](#predicting-sc-from-vd-of-male-speakers)
    -   [Predicting SC from VD of female speakers](#predicting-sc-from-vd-of-female-speakers)
-   [Conclusions](#conclusions)

``` r
# Libraries needed:

library(RCurl) # to read raw data from repo
library(leaps) # for feature selection
library(knitr) # for kable
```

Objectives
----------

We are building simple linear regression models to relate speaker characteristics (SC) to voice descriptions (VD) items. SC are to be predicted given VD as descriptor.

Loading data
------------

Setting paths and loading subjective ratings from [listening tests](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/doc/listening_tests).

From the SC data, pick only the speakers for which we have VD ratings.

``` r
sc <- sc[sc$speaker_ID %in% unique(vd$speaker_ID),]
```

Linear regression with caret
----------------------------

Descriptors are subjective ratings of voice descriptions, from 0 to 100. Hence, we do not pro-process with center + scaling.

Function to:

-   take input parameters (SC and VD of speakers of the same gender, 10 instances):
-   prepare descriptors (address multicollinearity)
-   fit linear model for each speaker characteristic - performing rfe
-   output models' coefficients, r-squared and p-value of F-statistic for each lm fit

``` r
fit.linearmodels <- function(sc_sub, vd_sub){
  
  # Our descriptors: VD ratings (from 0 to 100). Aggregate by speaker ID
  
  desc <- aggregate(vd_sub[,c(16:(16+33))], by = list(vd_sub$speaker_ID),mean)
  desc <- desc[,-1]
  
  # Center and scale descriptors
  # desc <- as.data.frame(lapply(desc, scale))
  
  # Out targets:  mean rating across listeners for each of the 34 SC 
  
  all.targets <- aggregate(sc_sub[,10:(10+33)], by = list(sc_sub$speaker_ID),mean, na.rm=T)
  all.targets <- all.targets[,-1]
  
  
  set.seed(2302)
  
  options(digits=2)
  
  all.lm <- lapply(all.targets, function(x){
    
    # finding the best descriptors (model selection)
    result.regsubsets <- leaps::regsubsets(desc,x)
    
    # we select the model with 2 descriptors
    selmodel <- summary(result.regsubsets)$which[2,]
    desc.selmodel <- names(selmodel[which(selmodel==TRUE)])[2:3]
    
    # fit the selected model and retrieve models' coefficients, descriptiors, r-squared, p-value of F-statistic
    
    fit <- lm(x ~ desc[,desc.selmodel[1]] + desc[,desc.selmodel[2]])
    
    f <- summary(fit)$fstatistic
    
    tmp <- coef(fit)
    tmp["r.squared"] <- summary(fit)$r.squared
    tmp["p.value"] <- pf(f[1],f[2],f[3],lower.tail=F)
    
    # build lm equation
    
    # rounded coefficients for better output
    cf <- round(coef(fit), 2) 
    
    # sign check to avoid having plus followed by minus for negative coefficients
    tmp["eq"] <- paste(cf[1],
                       ifelse(sign(cf[2])==1, "+", "-"), abs(cf[2]), desc.selmodel[1],
                       ifelse(sign(cf[3])==1, "+", "-"), abs(cf[3]), desc.selmodel[2])
    
    return(tmp)
  })
  
  return(all.lm)
  
}
```

### Predicting SC from VD of male speakers

They are 10 male speakers: 5 of the high WAAT class (warmth-attractiveness) and 5 of the low WAAT class.

``` r
# Select male speakers from dataframes SC and VD
sc_sub <- sc[sc$speaker_gender=='male',]
vd_sub <- vd[vd$speaker_gender=='m',]

# call our function and view the models' coefficients, r-squared and p-value of F-statistic
all.lm_males <- fit.linearmodels(sc_sub, vd_sub)


# create dataframe with the output of each model
all.lm_males <- data.frame(t(as.data.frame(all.lm_males)))

# view table
all.lm_males$r.squared <- as.numeric(as.matrix(all.lm_males$r.squared))
kable(all.lm_males[order(all.lm_males$r.squared, decreasing = T), c(6,4)])
```

|                 | eq                                           |  r.squared|
|-----------------|:---------------------------------------------|----------:|
| mitfuehlend     | 16.57 + 1.26 melodisch - 0.52 gleitend       |       0.99|
| intelligent     | 26.64 + 0.92 scharf - 0.38 schrill           |       0.99|
| gelangweilt     | 125.6 - 1.16 melodisch - 0.42 schrill        |       0.99|
| unsympathisch   | 67.93 - 0.94 scharf + 0.54 gepresst          |       0.99|
| haesslich       | 73.61 - 1.26 scharf + 0.57 hell              |       0.98|
| herzlich        | 58.68 - 0.69 klanglos + 0.5 scharf           |       0.98|
| distanziert     | 139.31 - 1.27 melodisch - 0.69 stockend      |       0.98|
| verstaendnislos | 132.75 - 0.6 hoch - 1.12 warm                |       0.98|
| gleichgueltig   | 120.04 - 1.05 melodisch - 0.48 schrill       |       0.98|
| angenehm        | -19.58 + 0.8 scharf + 0.76 warm              |       0.97|
| aktiv           | 237.82 - 1.71 unangenehm - 1.67 natuerlich   |       0.96|
| inkompetent     | 116.29 - 0.49 hoch - 0.82 fest               |       0.96|
| gesellig        | 73.57 - 0.76 monoton + 0.37 auffaellig       |       0.96|
| nicht\_genervt  | -29.6 + 0.57 hoch + 1.13 warm                |       0.96|
| emotional       | 157.16 - 1.23 klanglos - 0.81 natuerlich     |       0.96|
| freundlich      | 36.34 + 1.11 melodisch - 0.77 hart           |       0.96|
| interessant     | -78.77 + 1.66 glatt + 0.5 entspannt          |       0.95|
| charakterlos    | 39.79 + 1.71 nicht\_nasal - 1.96 scharf      |       0.94|
| attraktiv       | -82.23 + 1.58 glatt + 0.72 entspannt         |       0.92|
| entspannt       | 190.34 - 1.37 laut - 0.86 unprofessionell    |       0.91|
| sicher          | 74.09 + 1.05 heiser - 1.18 kraftlos          |       0.91|
| ruhig           | 187.83 - 1.39 laut - 0.74 unprofessionell    |       0.90|
| unentschieden   | -52.03 + 1.02 zittrig + 1.07 verbunden       |       0.90|
| gehorsam        | 34.88 - 0.65 klanglos + 1.21 behaucht        |       0.85|
| bescheiden      | -21.73 + 2.61 nicht\_nasal - 1.5 scharf      |       0.79|
| zynisch         | 191.31 - 1.79 nicht\_knarrend - 0.52 langsam |       0.79|
| maennlich       | 150.31 - 1.05 undeutlich - 0.69 melodisch    |       0.78|
| dominant        | 152.85 - 0.73 nicht\_nasal - 1.52 behaucht   |       0.78|
| unaufdringlich  | 178.5 - 1.31 laut - 0.93 heiser              |       0.76|
| unsachlich      | -81.52 + 1.09 stockend + 1.46 verbunden      |       0.75|
| aufgesetzt      | 128.6 - 1 nicht\_nasal - 0.95 heiser         |       0.73|
| alt             | 103.19 + 2.27 nicht\_nasal - 3.07 glatt      |       0.72|
| unaffektiert    | 3.47 + 1.74 natuerlich - 0.99 praezise       |       0.68|
| kindlich        | -35 - 1.48 scharf + 2.32 glatt               |       0.67|

### Predicting SC from VD of female speakers

They are 10 female speakers: 5 of the high WAAT class (warmth-attractiveness) and 5 of the low WAAT class.

``` r
# Select male speakers from dataframes SC and VD
sc_sub <- sc[sc$speaker_gender=='female',]
vd_sub <- vd[vd$speaker_gender=='w',]

# call our function and view the models' coefficients, r-squared and p-value of F-statistic
all.lm_females <- fit.linearmodels(sc_sub, vd_sub)

# create dataframe with the output of each model
all.lm_females <- data.frame(t(as.data.frame(all.lm_females)))

# view table
all.lm_females$r.squared <- as.numeric(as.matrix(all.lm_females$r.squared))
kable(all.lm_females[order(all.lm_females$r.squared, decreasing = T), c(6,4)])
```

|                 | eq                                           |  r.squared|
|-----------------|:---------------------------------------------|----------:|
| gleichgueltig   | 143.3 - 1.36 melodisch - 0.66 gepresst       |       0.99|
| distanziert     | 4.87 - 0.27 mit\_Akzent + 1.2 unbetont       |       0.99|
| gelangweilt     | 22.38 - 0.27 hell + 0.84 monoton             |       0.98|
| freundlich      | -80.78 + 1.47 nicht\_nasal + 1.05 auffaellig |       0.98|
| unsympathisch   | 152.05 - 0.87 scharf - 1.02 entspannt        |       0.98|
| mitfuehlend     | 22.65 + 1.12 nicht\_knarrend - 1.1 monoton   |       0.98|
| interessant     | 11.72 - 0.66 klanglos + 1.09 gleitend        |       0.98|
| haesslich       | -5.22 + 0.9 mit\_Akzent + 0.73 unbetont      |       0.98|
| attraktiv       | -2.79 - 0.7 klanglos + 1.4 gleitend          |       0.97|
| emotional       | -85.1 + 0.52 auffaellig + 1.79 gleitend      |       0.97|
| herzlich        | 70.04 + 0.54 warm - 1.12 unbetont            |       0.97|
| gesellig        | -12.39 + 0.72 scharf + 0.48 laut             |       0.97|
| intelligent     | -10.68 + 0.41 scharf + 0.75 warm             |       0.96|
| nicht\_genervt  | 86.42 + 0.54 auffaellig - 1.54 stockend      |       0.95|
| angenehm        | -51.02 + 0.7 scharf + 1.17 entspannt         |       0.95|
| verstaendnislos | 42.22 - 0.5 hoch + 0.62 unangenehm           |       0.94|
| charakterlos    | 118.63 - 0.85 scharf - 0.72 behaucht         |       0.92|
| inkompetent     | -65.08 + 1.46 undeutlich + 1.03 praezise     |       0.92|
| maennlich       | -80.35 + 1.34 stockend + 0.92 verbunden      |       0.87|
| unentschieden   | 38.35 - 0.66 hart + 0.52 unprofessionell     |       0.87|
| aktiv           | 7.05 + 1.25 laut - 0.91 zittrig              |       0.84|
| zynisch         | -27.74 + 0.91 heiser + 0.74 kurz             |       0.81|
| bescheiden      | -38.38 + 0.67 hell + 1.17 langsam            |       0.80|
| alt             | 176.22 - 1.22 kurz - 1.22 verbunden          |       0.79|
| gehorsam        | 97.04 - 0.37 scharf - 0.68 heiser            |       0.75|
| kindlich        | -48.08 + 0.99 kurz + 0.62 verbunden          |       0.73|
| unsachlich      | 37.01 - 1.87 monoton + 2.09 kraftlos         |       0.72|
| sicher          | 164.96 - 0.92 klanglos - 1.01 natuerlich     |       0.71|
| entspannt       | 62.08 + 1 glatt - 1.08 gleitend              |       0.70|
| unaufdringlich  | -9.92 + 0.5 hell + 0.83 langsam              |       0.69|
| aufgesetzt      | 23.99 - 0.62 undeutlich + 0.93 stockend      |       0.69|
| dominant        | -2.81 + 0.48 scharf + 0.88 heiser            |       0.67|
| unaffektiert    | 118.26 - 0.56 gleitend - 0.59 behaucht       |       0.51|
| ruhig           | 179.87 - 1.23 melodisch - 1.12 kraftlos      |       0.48|

Conclusions
-----------

Following the Brunswik Lens Model revised by \[1\], the features that can be extracted from the speech signal (e.g. pitch and formant frequencies, speech tempo, etc.) can be seen as "Distal Cues", whereas the collected VC-subjective labels represent the "Proximal Percepts" that directly account for the final listeners' impressions of speakers (i.e. the SC dimensions).

The [NSC](http://www.qu.tu-berlin.de/?id=nsc-corpus) data facilitates the research necessary to clarify the relationship between "Distal Cues" and "Proximal Percepts", which should lead to machines reaching the human performance in the attribution of speaker social characteristics.

In this script, we have built simple relationships between "Proximal Percepts" and speaker chatacteristics: items from the SC-Questionnaire (such as mitfühlend, intelligent, gelangweilt, etc.). Interestingly, r-squared is quite high. The next (and more tricky) step is to predict the "Proximal Percepts" from the "Distal Cues", that is, to build models able to predict voice description items (klanglos, melodisch, scharf, etc.) from measurable speeach features.

(See translations of the item names from German to English [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/blob/master/doc/NSC-documentation_slides_v04.pdf), slides 15 and 18, or [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/data/subjective_ratings))

We should however highlight the main limitation of this study: we have only 20 voices labelled in terms of voice descriptions. More listening tests (such as [Listening Test 2](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/doc/listening_tests)) would be needed in order to collect more subjective ratings from the 300 NSC speakers.

\[1\] Scherer, K. R., "Personality Inference from Voice Quality: the Loud Voice of Extroversion," European Journal of Social Psychology, 8:467-487, 1978.
