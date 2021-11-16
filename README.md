Music Genre Wise Classification.

Problem Statement:
Given the increasing content of music in the web by artists and also on social platforms by enthusiasts. There is a requirement to tag the songs appropriately for customisation by reccomendation engines. To recognize the musical genre of a piece of music of which only a recording is available. The data is from https://github.com/mdeff/fma

Data:
The data frame has 35000 rows and 519 columns as features and 1 column as label one genre for each track. It’s a Multi Class Problem with 16 genres, with 35,000 records. The features correspond to Pitch Class intensities (Chromagram) and Centroid frequency. The features used have been extracted from chromagrams of the songs.

Approach:
1. The features have been extracted using librosa package for all the mp3 files.
2. The dataset was split into train, validatio and test.
3. Preporcessing has been done on the featuers.
4. Baseline model was built with all features included. - Accuracy of 24.15%
5. Corelation Coefficient was used to select features which are highly correlated to genre.
6. Classification models have been built on multiple sets of features.
7. Naives Bayes model was built with features related to Vocal. Which gave accuracy of:
            Training - 41.03%
            Validation - 42.25%
            Test - 41.55%
8.Naives Bayes model was built with features coming from Signal Processing. Which gave accuracy of:
            Training - 38.32%
            Validation - 39.16%
            Test - 38.78%
9. Naives Bayes model was built with features related to chromagram. Which gave accuracy of:
            Training - 11.87%
            Validation - 12.82%
            Test - 11.97%
10. Naives Bayes model was built with features related to Tones. Which gave accuracy of:
            Training - 30%
            Validation - 29.77%
            Test - 30.3%
11. SVM model with combined features from Vocals and Signal Processing. Which gave accuracy of:
            Training - 99.93%
            Validation - 28.82%
            Test - 28.72%
12. Random Forest Model with combined features from Vocals and Signal Processing. Which gave accuracy of:
            Validation - 61.88%
            Test - 61.39%
            
This Project was done as part of reaserch internship at INSOFE. Unfortunately this is where I had to part ways with the project.  

The code is written in R language in RStudio.
