# Genomics Project: Prediction of m6A RNA modifications from direct RNA-Seq data
By pairofjeans group for our DSA4262 Sense-making Case Analysis: Health and Medicine (in Genomics) module.

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
The script will then prompt you for the JSON dataset file path as well as the output file path to save the parsed data to. Enter the file paths accordingly and the parsing will begin.

### Data pre-processing

### Model training and predictions
