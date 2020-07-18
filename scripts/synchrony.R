## synchrony

## packages
library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
# install.packages("synchrony")
# install.packages("codyn")
# install.packages("rfishbase")
# install.packages("munfold")
# install.packages("gdata")
library(synchrony)
library(codyn)
library(rfishbase)
library(munfold)
library(data.table)
library(gdata)

## upload pca scores for synchrony

pca_scores <- read.csv("output_data/trait_pca_scores_july2020.csv")
head(pca_scores)

## which axis?
## dimension 1 explains 50.5%
## traits that describe fish size are along this axis, e.g. eggsize, max length, age at maturity etc
## for interpretation, we can describe trait synchrony in terms of fish size in relation to climate anomonlies.
## dimension 2 explains 22.8%
## traits that describe reproduction e.g. reproductive guild, fecundity
## use both axes

## which synchrony?
## site is unit of analysis as pca scores are effectively an index score of that site
## synchrony between sites
## synchrony between basins

## Synchrony between sites
## between sites within basins

## axis 1 (fish size)

## group by basin, change name of syngeo id

pca_basins <- pca_scores %>%
  group_by(MAIN_BAS) %>%
  rename(sYNGEO_ID = sINGEO_ID)
pca_basins


