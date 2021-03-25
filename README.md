# "How Index Selection, Compression and Recording Schedule Impact the description of Ecological Soundscapes" Data Analysis Scripts
 
 
 ## Prior to these Scripts: 
 
1) AudioMoth filename converted from Hexadecimal to Date and Time
2) Recording location code and recording frequency appended to filename
3) Audio Compressed (using fre:ac gui) and corresponding compression appended to filename
4) Compressed audio split into different lengths (using pydub) and the length (frame size) appended to file name
5) Audio (all locations, lengths, compression levels) run through the AudioSet CNN, fingerprint vector outputted to csv with filename used to describe metadata
    this forms AudioSet raw data: "Data_AudioSet_Fingerprint.csv"
6) Audio (all locations, lengths, compression levels) analysed in R to extract Analytical Indices, outputted to csv with filename used to describe metadata
    this forms Analytical Indices raw data: "Data_Analytical_Indices.csv"

 ## Steps of Analysis covered in this repo
 
 ### Find Value of Difference as a Result of Compression
 
 #### append_ID_and_find_difference.r
 1) Each index reading given an ID number (ignoring compression) so all the same recordings of different compression level compared 
 2) Absolute difference found 
 3) Q-Q plots used to determine difference distribution

 #### "merging_data_and_bland_altman.r"
 1) Generate Bland-Altman plots from scratch (presented as hexbin plots) 

 #### Median_and_quartiles_Analytical_and_AudioSet.r
 1) Calulates the mean and quartiles for each compression (as a % of the range) for both Analytical Indices and AudioSet Fingerprint

### Find the Effect of the altered factors on the indices descriptive abilities

#### "random_forest_both_CM_gen_and_accuracy_output.r" 
1) Defines the functions which perform temporal splitting of the data 
2) Runs a random forest for each test case 
3) Saves the Confusion Matix (for plotting) of each test case as well as the overall Accuracy, Precision and Recall 

#### "Confusion_Matrix_Dataframe_gen.r"
1) Loads in all the confusion matrix data and outputs a single dataframe which captures all of the relevant CM information. _Note: individual confusion matrices haven't been saved in this repo to save space but are easily regerated by the prior (random_forest_both_CM_gen_and_accuracy_output.r) script._
 
 ### Manuscript Meta-Statistics and Figure Generation: 
 
 #### "Full_Stats_and_Figures.r"
 1) Loads in all the data, runs statstical tests and plots graphs
 
