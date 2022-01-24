# Weak signals on Social networks
This method aims to detect weak signals in networks (social networks in particular) via  the study of graphlets.
From the original temporal dataset, it splits the data into snapshots of same size each.
It counts the number of graphlets in each snapshot, then apply statistical methods that identify a selection of graphlets as precursors to events.
From the list of identified precursor graphlets, it apply a ratio calculation to detect those who can be qualified as weak signals.

## Datafiles for three different social networks
Twitter\\
Facebook\\
MathOverFlow

### Experience in the jupyter notebook is based on the Twitter dataset

### Steps:
  Download the corresponding CSV file, and go to jupyter then import the .ipynb file\n
  Launch the jupyter uploaded notebook in an R kernel

### Additional experimentation can be applied on facebook_data.csv, and sx-mathoverflow.txt datafiles.
