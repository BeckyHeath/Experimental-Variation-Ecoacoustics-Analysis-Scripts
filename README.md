# Compression_Paper_Data_Analysis_Scripts
 
 
 ## Prior to these Scripts: 
 
1) AudioMoth filename converted from Hexadecimal to Date and Time
2) Recording location code and recording frequency appended to filename
3) Audio Compressed (using fre:ac gui) and corresponding compression appended to filename
4) Compressed audio split into different lengths (using pydub) and the length (frame size) appended to file name
5) Audio (all locations, lengths, compression levels) run through the AudioSet CNN, fingerprint vector outputted to csv with filename used to describe metadata
    this forms AudioSet raw data 
6) Audio (all locations, lengths, compression levels) analysed in R to extract Analytical Indices, outputted to csv with filename used to describe metadata
    this forms Analytical Indices raw data

 ## Steps of Analysis covered here
 Raw Analytical Index Data: "Data_Analytical_Indices.csv"
 Raw AudioSet Data: "Data_AudioSet_Fingerprint.csv"
 
 ## Other Scripts
 ### AGB Generation 
 TBC
