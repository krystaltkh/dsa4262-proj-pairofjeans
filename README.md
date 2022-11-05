# Genomics Project: Prediction of m6A RNA modifications from direct RNA-Seq data
By pairofjeans group for our DSA4262 Sense-making Case Analysis: Health and Medicine (in Genomics) module.

## For Project evaluation

For ease of evaluation, following just the steps listed here will be sufficient to produce predictions on a sample dataset using our trained model. Instructions here will assume the use of an Ubuntu machine.

### Getting started
```
sudo apt update
sudo apt install r-base-core
git clone https://github.com/krystaltkh/dsa4262-proj-pairofjeans.git
cd dsa4262-proj-pairofjeans/sample_data
```
The /sample_data directory contains the saved trained model (trained_rf.rds), sample data of 100 rows (sample_data.csv) and the R script to do predictions (do_predictions.R). The R script can be used to predict other datasets but will require running the data processing steps first to generate new features from the raw dataset.
```
sudo Rscript do_prediction.R trained_rf.rds sample_data.csv
```
Once the script completes, the predictions are saved as a file titled predictions.csv. The predictions.csv file contains (transcript_id, position, segment and predicted_label) for each row in the sample data. To view the file from the terminal, run 
```
vi predictions.csv
```

## Users' Guide
The following are steps on how our group worked on the project which includes dataset preparations and pre-processing, model training and predictions. Sample data is provided in the /sample_data folder to reproduce results.

### Getting started
```
git clone https://github.com/krystaltkh/dsa4262-proj-pairofjeans.git
```
For users running on Ubuntu, python and pandas are required.
```
sudo apt update
sudo apt install python3
sudo apt install python3-pandas
```
### JSON parsing
The json parser is written in python and can be ran as a python notebook or as a script.
In Ubuntu, using screens, run
```
screen -S parser
python3 parser_script.py
```
The script will then prompt you for the JSON dataset file path as well as the output file path to save the parsed data to. Enter the file paths (with appropriate file extensions eg. .json) accordingly and the parsing will begin.

### Data processing and Model training
Our group utilises R to generate new features and train our models. All R scripts can be found in the r-scripts directory. Instructions to follow and documentations are found in the r scripts. For adding new features, look at adding-features.R. The R script for models trained are appropriately titled i.e. smote+knn.R for kNN model, smote+xgboost.R for XGBoost model.

