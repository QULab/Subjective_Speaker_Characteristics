Effect of channel distorions on the SRM variances
================
Laura Fernández Gallardo
April 2016

-   [Objectives](#objectives)
-   [Load data from listening test](#load-data-from-listening-test)
-   [Mutual ratings of voice likability](#mutual-ratings-of-voice-likability)
-   [Round Robin - univariate analysis for voice likability](#round-robin---univariate-analysis-for-voice-likability)
-   [Gender effects](#gender-effects)

Clear and set path.

``` r
# Libraries needed:

library(RCurl) # to read raw data from repo
```

    ## Warning: package 'RCurl' was built under R version 3.4.3

    ## Loading required package: bitops

``` r
library(TripleR) # analysis of SRM variance
```

    ## Warning: package 'TripleR' was built under R version 3.4.4

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.1

``` r
library(knitr) # for kable
```

    ## Warning: package 'knitr' was built under R version 3.4.1

Objectives
----------

We examine speech-based interpersonal perceptions. Here, by analyzing listeners' responses from the round robin experiment. Assessing the effects of channel distorions on the [SRM](http://davidakenny.net/srm/soremo.htm) variances.

SRM model: D. A. Kenny, "Interpersonal Perception: A Social Relations Analysis," New York, U. S.: Guilford Press, 1994.

Load data from listening test
-----------------------------

``` r
## Read raw results

path_github <- "https://raw.githubusercontent.com/laufergall/Subjective_Speaker_Characteristics/master/data/subjective_ratings"
  
data_self <- read.csv(text=getURL(paste0(path_github,"/data_listeningtest6_selfRatings.csv")), header=TRUE, sep=",")  

data_lik <- read.csv(text=getURL(paste0(path_github,"/data_listeningtest6_likabilityRatings_NAs.csv")), header=TRUE, sep=",")  

data_likper <- read.csv(text=getURL(paste0(path_github,"/data_listeningtest6_likabilityPersonalityRatings_NAs.csv")), header=TRUE, sep=",")  

participants <- read.csv(text=getURL(paste0(path_github,"/data_listeningtest6_participants.csv")), header=TRUE, sep=";")  
```

Mutual ratings of voice likability
----------------------------------

Split data\_lik per channel condition: narrowband (NB) or wideband (WB).

``` r
data_lik_split<-split(data_lik, data_lik$stimulusDistortion)
data_lik_NB<-data_lik_split[[1]]
data_lik_WB<-data_lik_split[[2]]
```

Mean of likability ratings in NB and in WB, for each of the 30 speakers. WB voices tend to be rated with higher likability.

The influence of NB and WB communication channels on voice likability were analyzed employing the ratings of the first part of the listening test. Considering a scale from 0 to 100, where greater numbers would represent higher perceived likability, the average rating was 45.47 and 53.30 for NB and WB speech, respectively.

``` r
mean_NB <- mean(data_lik_NB$Rating_Likability, na.rm=TRUE)
stdev_NB<- sd(data_lik_NB$Rating_Likability, na.rm=TRUE)
ratingsSpk_NB <- aggregate(data_lik_NB$Rating_Likability, by=list(data_lik_NB$IDSpeaker), mean, na.rm=T) # per speaker

mean_WB <- mean(data_lik_WB$Rating_Likability, na.rm=TRUE)
stdev_WB <- sd(data_lik_WB$Rating_Likability, na.rm=TRUE)
ratingsSpk_WB <- aggregate(data_lik_WB$Rating_Likability, by=list(data_lik_WB$IDSpeaker), mean, na.rm=T) # per speaker

# mean ratings
mean(ratingsSpk_NB$x)
```

    ## [1] 45.46006

``` r
mean(ratingsSpk_WB$x)
```

    ## [1] 53.29224

``` r
# mean ratings per speaker
df <- cbind(ratingsSpk_NB, ratingsSpk_WB$x)
names(df) <- c('speaker_ID', 'rating_NB', 'rating_WB')
kable(df) # view
```

|  speaker\_ID|  rating\_NB|  rating\_WB|
|------------:|-----------:|-----------:|
|            1|    46.79310|    46.58621|
|            2|    49.62069|    57.13793|
|            3|    35.20690|    31.03448|
|            4|    49.89655|    60.10345|
|            5|    42.89655|    57.51724|
|            6|    37.21429|    35.60714|
|            7|    48.75862|    62.58621|
|            8|    46.10345|    49.96552|
|            9|    36.62069|    41.79310|
|           10|    46.00000|    54.37931|
|           11|    61.27586|    70.34483|
|           12|    45.24138|    49.89655|
|           13|    45.68966|    57.86207|
|           14|    45.00000|    47.93103|
|           15|    44.03571|    60.64286|
|           16|    36.55172|    52.00000|
|           17|    56.41379|    66.00000|
|           18|    53.44828|    70.51724|
|           19|    45.48276|    53.62069|
|           20|    37.10345|    39.06897|
|           21|    47.27586|    54.06897|
|           22|    41.51724|    48.89655|
|           23|    56.10345|    65.13793|
|           24|    33.55172|    39.31034|
|           25|    39.41379|    46.27586|
|           26|    42.17241|    52.86207|
|           27|    57.10345|    70.13793|
|           28|    43.10345|    51.34483|
|           29|    53.65517|    52.79310|
|           30|    40.55172|    53.34483|

T-test to check the significance of the mean differences between NB and WB:

``` r
# Two Sample t-test:
t.test(data_lik_NB$Rating_Likability, data_lik_WB$Rating_Likability, alternative="less")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  data_lik_NB$Rating_Likability and data_lik_WB$Rating_Likability
    ## t = -7.354, df = 1718.9, p-value = 1.482e-13
    ## alternative hypothesis: true difference in means is less than 0
    ## 95 percent confidence interval:
    ##       -Inf -6.080014
    ## sample estimates:
    ## mean of x mean of y 
    ##  45.47120  53.30415

A two-sample t-test shows that the WB mean is significantly higher than the NB mean; t(1718) = -7.354, p &lt; .001, revealing listeners' predilection for WB voices.

Round Robin - univariate analysis for voice likability
------------------------------------------------------

``` r
# Variance components, NB
RR1_NB <- RR(Rating_Likability ~ IDListener*IDSpeaker, data=data_lik_NB, na.rm=TRUE) 
RR1_NB$varComp
```

    ##                             type   estimate standardized       se    SEVAR
    ## sesaa2            actor variance  96.519934  0.214575890 27.27664 744.0150
    ## sesbb2          partner variance  39.141575  0.087016618 12.70860 161.5086
    ## sescc2     relationship variance 314.155727  0.698407492 15.56279 242.2005
    ##                   error variance         NA           NA       NA       NA
    ## sesab2  actor-partner covariance   3.682166  0.059906746 13.42129 180.1310
    ## sesccs2  relationship covariance   1.318373  0.004196558 15.56279 242.2005
    ##             t.value      p.value
    ## sesaa2   3.53855688 0.0006886514
    ## sesbb2   3.07992776 0.0022502042
    ## sescc2  20.18633235 0.0000000000
    ##                  NA           NA
    ## sesab2   0.27435267 0.7857585343
    ## sesccs2  0.08471312 0.4665357945

``` r
# Variance components, WB
RR1_WB <- RR(Rating_Likability ~ IDListener*IDSpeaker, data=data_lik_WB, na.rm=TRUE)
RR1_WB$varComp
```

    ##                             type   estimate standardized       se    SEVAR
    ## sesaa2            actor variance  77.592415   0.14312213 23.03169 530.4587
    ## sesbb2          partner variance  86.766456   0.16004399 25.36106 643.1834
    ## sescc2     relationship variance 377.782406   0.69683387 18.71599 350.2883
    ##                   error variance         NA           NA       NA       NA
    ## sesab2  actor-partner covariance  -6.712459  -0.08180806 17.43123 303.8479
    ## sesccs2  relationship covariance   4.327087   0.01145391 18.71599 350.2883
    ##            t.value      p.value
    ## sesaa2   3.3689417 0.0010736005
    ## sesbb2   3.4212470 0.0009368697
    ## sescc2  20.1850064 0.0000000000
    ##                 NA           NA
    ## sesab2  -0.3850823 0.7029864124
    ## sesccs2  0.2311973 0.4093924952

These are the SRM variance components computed from the NB and WB ratings separately. Interestingly, large perceiver variance has been found with respect to the target variance in NB, while in WB both perceiver and target variance are similar.

Two deductions can then be made: First, perceivers differ in "being a liker" to a greater extent in NB than in WB. This implies more variation among perceivers considering the average of their likability ratings of NB voices compared to WB. In other words, there is a wider range of positive and negative liking tendencies in NB, probably due to the perceivers' different degrees of tolerance to the NB distortion. And second, because targets differ in "being liked" more in WB than in NB, the differences between non-likable and likable voices appear to be better detected with the extended bandwidth.

Gender effects
--------------

Gender effects in NB, WB, and full band. Mean likability per listener gender and speaker gender.

``` r
IDMales <- participants$ID[participants$gender == 'm'] 
IDFemales <- participants$ID[participants$gender == 'f'] 

# Subsetting according to listeners IDs and speakers IDs
data_lik_NB_only_m_m <- subset(data_lik_NB, IDListener%in% IDMales & IDSpeaker %in% IDMales)
data_lik_NB_only_f_f <- subset(data_lik_NB, IDListener%in% IDFemales & IDSpeaker %in% IDFemales)
data_lik_NB_only_m_f <- subset(data_lik_NB, IDListener%in% IDMales & IDSpeaker %in% IDFemales)
data_lik_NB_only_f_m <- subset(data_lik_NB, IDListener%in% IDFemales & IDSpeaker %in% IDMales)

data_lik_WB_only_m_m <- subset(data_lik_WB, IDListener%in% IDMales & IDSpeaker %in% IDMales)
data_lik_WB_only_f_f <- subset(data_lik_WB, IDListener%in% IDFemales & IDSpeaker %in% IDFemales)
data_lik_WB_only_m_f <- subset(data_lik_WB, IDListener%in% IDMales & IDSpeaker %in% IDFemales)
data_lik_WB_only_f_m <- subset(data_lik_WB, IDListener%in% IDFemales & IDSpeaker %in% IDMales)

data_lik_44100 <- data_likper
data_lik_44100_only_m_m <- subset(data_lik_44100, IDListener%in% IDMales & IDSpeaker %in% IDMales)
data_lik_44100_only_f_f <- subset(data_lik_44100, IDListener%in% IDFemales & IDSpeaker %in% IDFemales)
data_lik_44100_only_m_f <- subset(data_lik_44100, IDListener%in% IDMales & IDSpeaker %in% IDFemales)
data_lik_44100_only_f_m <- subset(data_lik_44100, IDListener%in% IDFemales & IDSpeaker %in% IDMales)

# males rate males
mean(data_lik_NB_only_m_m$Rating_Likability, na.rm=TRUE) # 43.05238
```

    ## [1] 43.05238

``` r
mean(data_lik_WB_only_m_m$Rating_Likability, na.rm=TRUE) # 52.86667
```

    ## [1] 52.86667

``` r
mean(data_lik_44100_only_m_m$Rating_Likability, na.rm=TRUE) # 50.0619
```

    ## [1] 50.0619

``` r
# females rate females
mean(data_lik_NB_only_f_f$Rating_Likability, na.rm=TRUE) # 47.35714  
```

    ## [1] 47.35714

``` r
mean(data_lik_WB_only_f_f$Rating_Likability, na.rm=TRUE) # 54.76667
```

    ## [1] 54.76667

``` r
mean(data_lik_44100_only_f_f$Rating_Likability, na.rm=TRUE) # 53.67143
```

    ## [1] 53.67143

``` r
# males rate females
mean(data_lik_NB_only_m_f$Rating_Likability, na.rm=TRUE)  # 47.44643
```

    ## [1] 47.44643

``` r
mean(data_lik_WB_only_m_f$Rating_Likability, na.rm=TRUE) # 53.16964
```

    ## [1] 53.16964

``` r
mean(data_lik_44100_only_m_f$Rating_Likability, na.rm=TRUE) # 56.63839
```

    ## [1] 56.63839

``` r
# females rate males
mean(data_lik_NB_only_f_m$Rating_Likability, na.rm=TRUE)  # 43.99554
```

    ## [1] 43.99554

``` r
mean(data_lik_WB_only_f_m$Rating_Likability, na.rm=TRUE) # 52.47768
```

    ## [1] 52.47768

``` r
mean(data_lik_44100_only_f_m$Rating_Likability, na.rm=TRUE)  # 49.36607
```

    ## [1] 49.36607

In this case, female spekers are generally more liked, by males and by females.
