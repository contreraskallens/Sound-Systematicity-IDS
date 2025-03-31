# Sound systematicity in the IDS
Sound systematicity in the Intercontinental Dictionary Series

Script for the article "Systematic patterns of sound and meaning can support early stages of word learning".

All scripts can be found in the "Scripts" folder. The "Data" folder contains both the input data (e.g., IDS, WALS) and the processed data allocated in the course of the analyses. All figures, including some extra figures, can be found in the "Figures" folder. The "Results" folder contains all the results from the RNN simulations.

To obtain the results from the script, run the scripts "main_pub.R" and "supplementary_control.R". To reproduce the results, uncomment all lines in "wata_wrangling.R", and then run the python script rnn_data.py. This will re-generate all data used in the article and analysed in the main scripts. Note that this takes a considerable time to run.
