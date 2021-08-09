#!/usr/bin/env python3

################################################################################################
#
# Script to derive add additional descriptive data to the filenames (manually inputted)
#
# Data order Convention: "CompLevel_RecLength_Location_RecFreq_Date_T=Time.wav"
# 
# Becky Heath 2019
#
################################################################################################


# Imports
import sys
import time
import os
                    #Move To Folder with Audio Files in 

for f in os.listdir():
    f_name, f_ext = os.path.splitext(f)
    CompLevel = str("RAW")
    RecLength = str("20min")
    Location = str("E")
    RecFreq = int(48)
    
    f_new=  '{}_{}_{}_{}_{}{}'.format(CompLevel, RecLength, Location, RecFreq, f_name, f_ext)

    os.rename(f, f_new)