# Business Cycle Analysis Using Text-based Indicators

## Getting started
Start by cloning the Git repsitory: 
```sh
git clone https://github.com/lena-will/master-thesis.git
```

## Introduction

## Data
+ The text data are articles form the German newspaper "Frankfurter Allgemeine Sonntagszeitung" starting in 2001 and including articles up until April 2024. The data can be aquired from the "Frankfurter Allgemeine Zeitung".
+ Previously dated German recessions in ```recessions_germany.csv``` are based on the "German Council of Economic Experts"'s business cycle dating (see [https://www.sachverstaendigenrat-wirtschaft.de/en/topics/business-cycles-and-growth/konjunkturzyklus-datierung.html](https://www.sachverstaendigenrat-wirtschaft.de/en/topics/business-cycles-and-growth/konjunkturzyklus-datierung.html)).
+ The macroeconomic indicators for Germany are available via the following sources:
  + [Quarterly GDP](https://www-genesis.destatis.de/genesis//online?operation=table&code=81000-0002&bypass=true&levelindex=1&levelid=1685634675885#abreadcrumb)
  + [IP Index](https://www-genesis.destatis.de/genesis//online?operation=table&code=42153-0001&bypass=true&levelindex=0&levelid=1685634299865#abreadcrumb)        (Produktion im produzierenden Gewerbe ohne Bau)
  + [Economic Sentiment Index](https://economy-finance.ec.europa.eu/economic-forecast-and-surveys/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en)
  + [CPI](https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0002&bypass=true&levelindex=0&levelid=1723382986798#abreadcrumb)
  + [Vacancies](https://statistik.arbeitsagentur.de/SiteGlobals/Forms/Suche/Einzelheftsuche_Formular.html?nn=1459928&topic_f=zr-stea-1950)
  + [Long-term Intrest Rate](https://fred.stlouisfed.org/graph/?id=IRLTLT01DEM156N,#0) (10 years)
  + [Short-term Interest Rate](https://fred.stlouisfed.org/series/IR3TIB01DEM156N) (3 months)

## Code Structure
+ Text pre-processing is done in python and can be found in ```preprocessing.py```
+ For computational efficiency the LDA is run using the R package ```topicmodels``` which was built in C.
+ However, ```gibbs_sampling_lda.R``` offers code to do the inference to Latent Dirichlet Allocation from scratch using the Gibbs sampling algorithm introduced by Griffiths and Steyvers (2004).
+ Code for any plots can be found in the ```plots code``` folder.
+ All functions to the weekly bridge models are in the ```functions``` folder. ```weekly_models_elastic_net.R``` runs the models for all periods of the business cycle.
