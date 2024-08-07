# Business Cycle Analysis Using Text-based Indicators

## Getting started
Start by cloning the Git repsitory: 
```sh
git clone https://github.com/lena-will/master-thesis.git
```

## Introduction

## Code Structure
+ Text pre-processing is done in python and can be found in ```preprocessing.py```
+ For computational efficiency the LDA is done using the R package ``topicmodels``` which was built in C.
+ However, ```gibbs_sampling_lda.R``` offers code to do the inference to Latent Dirichlet ALlocation from scratch using the Gibbs sampling algorithm introduced by Griffiths and Steyvers (2004).
+ Code for any plots can be found in the ``plots code```folder. 
