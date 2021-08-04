#!/usr/bin/env python3

################################################################################################
#
# Script to derive recording dates and times from the Hexadecimal filenames given by the 
# Audiomoth system. 
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
    asdig = int(f_name, 16)             #Convert To Decimal
    malay= asdig + 28800                #Convert to Malay Local Time (28800 = 8 Hours)
    asdate = time.localtime(malay)      #Date+TimeLable
    f_hour=str(asdate.tm_hour).zfill(2)
    f_min=str(asdate.tm_min).zfill(2)
    
    f_new=  '{}_{}_{}_T={}{}{}'.format(asdate.tm_year, asdate.tm_mon, asdate.tm_mday, f_hour, f_min, f_ext)

    os.rename(f, f_new)