## Analysis Listening Test 8: Paired Comparison

The data from Listening Test 8 is analyzed (see [listening_tests](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/doc/listening_tests)).



### Matlab

*Note that the Matlab scripts have not been edited with the correct paths in this repository.*

**"answers2matrix.m"**: calls **"f_read_answers157.m"** to transform crowdwourcing answers into a better format indicating the presented stimulus pair and value selected in slider by crowdsourcing workers. Generates the 'pair_val__Day2510_Hour1105.mat' file

**"main_generate_matrices_from_CS_answers.m"** 

* reads the crowdsourcing answers 'pair_val__Day2510_Hour1105.mat' and generates the matrices: 'preferencematrix_onlyaccepted.csv' and 'dissimilaritymatrix_onlyaccepted.csv'. Containing only the 1365 accepted answers.
* reads the crowdsourcing answers 'pair_val__Day0202_Hour1611.mat' and generates the matrices: 'preferencematrix_ALL.csv' and 'dissimilaritymatrix_ALL.csv'. Containing all 1682 accepted and rejected answers

These data are found in [this](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/data/subjective_ratings/data_listeningtest8) folder.

The matrix 'preferencematrix_onlyaccepted.csv' is then used by the R script.



### R

**"applying_BTL_crowdsourcing.Rmd"**: Apply the BTL model to paired-comparison data to estimate ratio-scaled preference scores from data:

* from Listening Test 8
* from Listening Test 7

The sets of scores are then compared.



## Analysis Listening Test 9: Direct scaling

### Matlab

*Note that the Matlab scripts have not been edited with the correct paths in this repository.*

**"saving_crowdee_finalRatings_allworkers.m"**: calls **"f_read_answers189.m"** to read answers from the qualification job and **"f_read_answers190_191.m"** to read answers from the likability rating job. Generates the files:

* 'crowdee_results_Answers_Job_190_2017-02-06T09-26-10.345Z.mat' (female speakers) 
* 'crowdee_results_Answers_Job_191_2017-02-06T09-26-07.946Z.mat' (male speakers)

**"comparing_crowdee_lab.m"**: loads generated mat files (Listening Test 9) and mat files corresponding to results from Listening Test 7: 'Lab_results_males.mat' and 'Lab_results_females.mat'.

The mean likability scores from Listening Test 9 (crowdsourcing) and Listening Test 7 (laboratory) correlated highly: Pearson r = 0.68, p<0.005, SE = 0.20 and Pearson r = 0.89, p <0.001, SE = 0.13 for male and for female speakers, respectively.

Generated figure: 'likabilityscale_lab_crowdee_all.pdf'. Calls **"rotateXLabels.m"** to rotate x labels in the plot.

The data files are found in [this](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/data/subjective_ratings/data_listeningtest9) folder.