# Approaches for dealing with seasonality in clinical prediction models for infections

## Introduction

We performed an analysis of different methods to deal with seasonality in clinical prediciton models. See more details at:

Cánovas-Segura B, Morales A, Juarez JM, Campos M.
Approaches for Dealing with Seasonality in Clinical Prediction Models for Infections.
Applied Sciences. 2023; 13(14):8317. https://doi.org/10.3390/app13148317")

## Synthetic dataset generation

The script in `./src/syntheticDatasetGenerator.r` generates the two synthectic datasets commented on the paper and stores them in `./data/synth_condensed.rds` and `./data/synth_sinusoidal.rds`.

## Experiment launcher

The script in `./src/experimentLauncher.r` can be used to execute the different experiments commented in the paper. It creates the models that result from each experiment in `./models`, as well as the resulting predictions. 

## Source code

The rest of the code can be found in the `./src/functions` folder. It is structured as follows:

- `./src/functions/framework.r` contains all the code related with the proposed models based on sliding windows and ensembles.
- `./src/functions/preprocessing.r` contains methods to perform the feature selection and balancing strategies used. 
- `./src/functions/utils.r` contains multiple function required in the rest of the scripts.
- `./src/functions/prettyprint.r` contains functions to improve the output for reports and similar.

## Reports

Some R markdown reports are available in `./src/reports`:

- `./src/reports/syntheticDatasetAnalysis.Rmd`: Analysis of the synthetic datasets. It requires the datasets to be available in the `./data/` folder.


