# Eye Movement Data analysis (03/24/2020 updated by Yi-Lun Weng)
  

## **Overview** 

1.  Mouse click results: Identify which picture participants clicked on in each condition

   => Input data:  play_mouseclick.csv & play_mouseclick_plot.csv
   => R script:    mouseclick_lmer.R & mouseclick_plot.R
   => Output data: play_mouseclick_lmer_result.xlsx & play_mouseclick_plot.eps

2.  Eye movement results: Calculate proportion of fixation on each target in each condition

   => Input data:  play_eye_fixation_proportion.csv & play_eye_fixation_proportion_plot.csv
   => R script:    fixation_lmer.R & fixation_plot.R
   => Output data: play_fixation_lmer_result.xlsx & fixation_plot.eps

3.  First fixation results: Identify the first fixation after the verb offset

   => Input data:  play_firstlook_proportion.csv & play_firstlook_proportion_lmer.csv
   => R script:    firstfix_lmer.R & firstfix_plot.R
   => Output data: play_firstfix_lmer_result.xlsx & firstfix_plot.eps
  
#### == All the data files and scripts are saved on the NAS: \projects\play\analysis\eyemovement ==
  
  
### **1. Mouse click results**  

Goal: To know participants click on which region.

(1) Extract the coordinate information using the "Clickx" and "Clicky" columns from the raw data file (e.g., play_a_xxx.csv).
    We have to code them into Top Left (TL), Lower Left (LL), Top Right (TR), and Lower Right (LR).  
    The computer screen size is 1080 x 1920. The top left is [0,0].

     TL  = Click x < 960 & Click y < 540
     LL  = Click x < 960 & Click y > 540
     TR  = Click x > 960 & Click y < 540
     LR  = Click x > 960 & Click y > 540

(2) After identifying the location of the click, identify the location of each picture in each trial (which is random).  
    Then, using the "TrialID", "Tloc", "Tiloc", "Cloc", and "Ciloc" columns, identify for each trial 
    whether the "TL" location contains the target animal, instrument, or distractor picture.

     
(3) For each trial, using the mouse click response, create four columns "ta","ti","da","di" (one for each possible response). 
    For each column set the value to 1 if the participant clicked on that picture; otherwise, set the value to 0.

     ta = Target animal
     ti = Target instrument
     da = Distractor animal
     di = Distractor instrument

(4) Create a bar plot showing the mouse click results (i.e., the percentage of mouse clicks on the target instrument picture).

### **2. Eye movement results** 

Goal: To know participants' eye movement patterns in each trial.

(1) Align the fixation time to the onset of the verb in each trial. 
    Add three columns: (a) time length and (b) time accumulation (c) time window into the eye movement raw data file.

(2) Use the "DeviceTimeStamp" column to calculate the time difference between two fixations in the "time length" column.  
    Next, for each trial, subtract the value in "time length" column from that of the previous trial and 
    add this value to the previous value in the accumulation column.
    The time accumulation column allows for analysis of fixations across the duration of the sentence.

(3) Using the "time accumulation" column and "onsets.txt" file, determine in which time window each fixation took place.  
    The "onsets.txt" contains the information such as the onset of the verb, first noun and second noun in each sentence.
    There are three possible time windows, 
    1. between the onset of the verb and the first noun,  
    2. between the onset of the first noun and the second noun, and  
    3. from the onset of the 2nd noun until the end of the sentence (approximately 1500ms). 

   Consider the time windows for the sentence “Pet the duck with the hat.”, illustrated in the table below:  
    Time window 1 = Starting at the onset of the verb (e.g. Pet) and ended at the onset of 1st noun (e.g. duck)  
    Time window 2 = Starting at the onset of the 1st noun (e.g. duck) and ended at the onset of the 2nd noun (e.g. hat)  
    Time window 3 = Starting at the onset of the 2nd noun and ended 1,500 ms later  

(4) Add three columns for linear-mixed models analysis:      
    1. tafixation  (whether the participant looked at the target animal): 0 / 1 / NA  
    2. tifixation  (whether the participant looked at the target instrument): 0 / 1 / NA  
    3. talag1      (whether the participant looked at the previous target animal): 0 / 1 / NA  

(5) Visualize the eye movement results in a spaghetti plot.


### **3. First fixation results** 

Goal: To find the first anticipatory look after the verb offset.


(1) Add a column "firstfix" in the data file.

(2) All time windows are offset by 200 milliseconds to account for the time needed to program and launch an eye movement (Hallett, 1986)  

(3) Use the "TrialID", "time accumulation" and "time window" column to determine the first true fixation.
    The criterion for the first true fixation is 25 consecutive fixations on the same image.  
    For both time window 1 (the verb region) and time window 2 (the N1 region), find the first 25 consecutive fixations on a single image.   
    Where these 25 consecutive fixations are found in each time window, set the value of "firstfix" to "1".  
    Set all other values in the "firstfix" column to "0".  

