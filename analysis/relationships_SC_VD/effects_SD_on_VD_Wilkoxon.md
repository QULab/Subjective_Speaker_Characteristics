Testing for significant effects of SC WAAT on VD factors
================
Laura Fernández Gallardo
August 2017

-   [Objectives](#objectives)
-   [Load data](#load-data)
-   [Wilkoxon test](#wilkoxon-test)
    -   [Male speakers](#male-speakers)
    -   [Female speakers](#female-speakers)
-   [Conclusions](#conclusions)

``` r
# Libraries needed:

library(RCurl) # to read raw data from repo
library(stringi) # install.packages('stringi')
library(stats) # for wilcox.test
```

Objectives
----------

Insights into subjective voice descriptions of extreme warm-attractive speakers.

To assess whether speaker characteristics (SC), namely warmth and attractiveness (WAAT), affect significantly the perceptions of voice descriptions (VD). The factors derived by factor analysis are considered (see [SC factor analysis](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/analysis/speaker_characteristics/factor_analysis) and see [VD factor analysis](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/analysis/voice_descriptions/factor_analysis))

We conduct Wilcoxon rank-sum tests.

Load data
---------

Set paths and read subjective ratings to voice descriptions.

Load factor scores of voice descriptions calculated in [VD factor analysis](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/analysis/voice_descriptions/factor_analysis)).

``` r
path_factorscores <- '../../data/generated_data'

fs_m <- read.csv(paste0(path_factorscores,'/factorscores_VD_malespk.csv'))
names(fs_m) <- c("sample_heard", "dim1_vd","dim2_vd","dim3_vd","dim4_vd" ) # voice description dimensions

fs_f <- read.csv(paste0(path_factorscores,'/factorscores_VD_femalespk.csv'))
names(fs_f) <- c("sample_heard", "dim1_vd","dim2_vd","dim3_vd","dim4_vd" ) # voice description dimensions

fs_vd <- rbind(fs_m, fs_f) # join male and female speakers
fs_vd$speaker_pseudonym <- stri_sub(fs_vd$sample_heard,6,-14)
fs_vd<-fs_vd[,-1] # remove "sample_heard"
```

As a reminder, the factors found for voice descriptions were:

-   Male speech
    -   Dimension 1: proficiency (precision / fluency) (negative)
    -   Dimension 2: tension
    -   Dimension 3: melody
    -   Dimension 4: brightness
-   Female speech
    -   Dimension 1: fluency (not connected to precision in contrast to male speech) (negative)
    -   Dimension 2: brightness ( and also, smoothness with lower loading - can be neglected )
    -   Dimension 3: proficiency precision (negative)
    -   Dimension 4: shrillness (related to tension dim of male speech)

Prepare dataframe with SC and VD factors for each of the 20 speakers.

``` r
# merging dfs

mydata_factors <- merge(fs_vd, 
                        data_raw[data_raw$listener_pseudonym == data_raw$listener_pseudonym[1] , 6:15], 
                        by = 'speaker_pseudonym')

head(mydata_factors)
```

    ##   speaker_pseudonym    dim1_vd    dim2_vd   dim3_vd    dim4_vd speaker_ID
    ## 1          alrayyan -2.7990369 -1.7800925  1.015643 -0.3498631         61
    ## 2    andorralavella  0.9022884  1.3338408 -1.116270 -0.3651595        263
    ## 3       barentsburg  1.5280131  0.1951973 -1.606340 -1.3508458        171
    ## 4        basseterre -2.4144344 -0.1542567  1.031051  1.4150970         97
    ## 5        birmingham -1.8787370  1.1246618 -1.423784 -0.8249679         40
    ## 6         bucharest -2.5669615 -0.6637726  1.063094  0.3091249         41
    ##   speaker_gender speaker_age speaker_dim1_SC speaker_dim2_SC
    ## 1              m          30        3.309936        3.129257
    ## 2              m          26       -4.861186       -1.683500
    ## 3              m          34       -5.175979       -2.546008
    ## 4              m          26        3.325969        2.397315
    ## 5              w          29        2.806899        1.638903
    ## 6              m          24        3.292849        2.620515
    ##   speaker_dim3_SC speaker_dim4_SC speaker_dim5_SC speaker_highlow_SC
    ## 1      1.15714351      0.08381383       0.8657708               high
    ## 2      0.13424320     -1.04523924      -0.5029408                low
    ## 3     -0.03107284     -0.05489620       2.5516748                low
    ## 4      1.05451378      0.35383790       1.0852559               high
    ## 5      0.61193717      1.09080941       0.1900481               high
    ## 6      1.27680865      0.20336541      -0.1957567               high

Wilkoxon test
-------------

Wilkoxon test with VD factors. Are there significant effects of speaker WAAT on factor value?

(averaging across listeners)

``` r
# function in utils_R
source('../utils_R/getsigcode.R')
```

### Male speakers

``` r
# separate high and low WAAT speakers
l <- mydata_factors[mydata_factors$speaker_highlow_SC=='low' & mydata_factors$speaker_gender=='m',]
h <- mydata_factors[mydata_factors$speaker_highlow_SC=='high' & mydata_factors$speaker_gender=='m',]


for (i in c(1:4)){ # for each VD factor

  a <- l[,i+1]
  b <- h[,i+1]

  
  #---wilcox.test
  # Wilcoxon rank-sum test -> equivalent to the Mann-Whitney test
  # paired: each subject or entity is measured twice, resulting in pairs of observations <- this is not our case
  wtest <- wilcox.test(a, b, paired=FALSE)
  
  cat(paste0(names(mydata_factors)[9+i], ': ', ", p-val: ", round(wtest$p.value,4),getsigcode(wtest$p.value), '   mean low=',round(mean(a),3),', mean high=',round(mean(b),3),'\n'))
  
}
```

    ## speaker_dim2_SC: , p-val: 0.0556.   mean low=1.691, mean high=-1.691
    ## speaker_dim3_SC: , p-val: 0.5476   mean low=0.278, mean high=-0.278
    ## speaker_dim4_SC: , p-val: 0.0079**   mean low=-1.079, mean high=1.079
    ## speaker_dim5_SC: , p-val: 0.0159*   mean low=-1.005, mean high=1.005

### Female speakers

``` r
# separate high and low WAAT speakers
l <- mydata_factors[mydata_factors$speaker_highlow_SC=='low' & mydata_factors$speaker_gender=='w',]
h <- mydata_factors[mydata_factors$speaker_highlow_SC=='high' & mydata_factors$speaker_gender=='w',]


for (i in c(1:4)){ # for each VD factor
  
  a <- l[,i+1]
  b <- h[,i+1]
  
  
  #---wilcox.test
  # Wilcoxon rank-sum test -> equivalent to the Mann-Whitney test
  # paired: each subject or entity is measured twice, resulting in pairs of observations <- this is not our case
  wtest <- wilcox.test(a, b, paired=FALSE)
  
  cat(paste0(names(mydata_factors)[9+i], ': ', ", p-val: ", round(wtest$p.value,4),getsigcode(wtest$p.value), '   mean low=',round(mean(a),3),', mean high=',round(mean(b),3),'\n'))
  
}
```

    ## speaker_dim2_SC: , p-val: 0.0079**   mean low=0.866, mean high=-0.866
    ## speaker_dim3_SC: , p-val: 0.0159*   mean low=-1.219, mean high=1.219
    ## speaker_dim4_SC: , p-val: 0.0079**   mean low=1.008, mean high=-1.008
    ## speaker_dim5_SC: , p-val: 0.4206   mean low=0.134, mean high=-0.134

Conclusions
-----------

We examine the effects of speakers' warmth--attractiveness on the obtained VD factor scores.

Conducted Wilcoxon rank-sum tests suggested that, for male speakers, *melody* and *brightness* factor scores differ significantly for perceived low warm--attractive speakers compared to perceived high warm--attractive speakers (*p* &lt; .01 and *p* &lt; .05, respectively).

For female speakers, this statistical significant difference has been found for *fluency* (*p* &lt; .01), *brightness* (*p* &lt; .05), and *proficiency precision* (*p* &lt; .01).

These findings indicate the plausibility to classify perceived speaker traits based on speech features related to their voice descriptions. However, the voice descriptions of more speakers need to be analyzed in order to better determine the statistical effects between the SC and VD dimensions.
