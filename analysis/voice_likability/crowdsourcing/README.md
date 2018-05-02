## Analysis: Paired Comparison

The data from Listening Test 8 is analyzed (see [listening_tests](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/doc/listening_tests)).



### Matlab

*Note that the Matlab scripts have not been edited with the correct paths in this repository.*

**answers2matrix.m**: calls **f_read_answers157.m** to transform crowdwourcing answers into a better format indicating the presented stimulus pair and value selected in slider by crowdsourcing workers. Generates the 'pair_val__Day2510_Hour1105.mat' file

**"main_generate_matrices_from_CS_answers.m"** reads the crowdsourcing answers 'pair_val__Day2510_Hour1105.mat' and generates the matrices: 'preferencematrix_onlyaccepted.csv' and 'dissimilaritymatrix_onlyaccepted.csv'.

These data are found in [this](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/data/subjective_ratings/data_listeningtest8) folder.

The matrix 'preferencematrix_onlyaccepted.csv' is then used by the R script.



### R

**"applying_BTL_crowdsourcing.Rmd"**: 





Apply the BTL model to paired-comparison data to estimate ratio-scaled preference for voices. Then, this model is also applied to the "simulated" paired comparison data. Finally, correlations are obtained from the 3 sets of scores: 

* utility scale values from paired-comparison test
* utility scale values from direct scaling test (after simulating paired-comparison data)
* raw direct scaling test data (0 - 100), averaged across listeners



