# Russian-Ukrainian War

Created: 2022-05-26

Updated: 2026-06-23

  - First Inspired by [`Lena Sokol's Kaggle`](https://www.kaggle.com/code/sokolheavy/2022-ukraine-russia-war-visualization).

  - Data sources:
    - [`Petro Ivanyuk Repository`](https://github.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset).
    - [`Armed Forces of Ukraine`](https://www.zsu.gov.ua/en/estimated-enemy-losses)

  - From 2026-01-19, data is read directly from [`Petro Ivanyuk's GitHub`](https://github.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset) in JSON format. I will no longer keep a local copy of the CSV data.

## Background of the Conflict

On 24 February, 2022, the Russian Federation launched a full-scale invasion of Ukraine, escalating a conflict that originally began in 2014. What initially started as a swift campaign aimed at decapitating the Ukrainian government quickly devolved into the largest conventional war in Europe since World War II. The conflict has since transitioned through several distinct phases: early mechanized offensives, deep defensive fortifications, and finally, a prolonged war of attrition characterized by trench warfare, use of drones, and massive artillery exchanges [`Institute for the Study of War`](https://understandingwar.org/analysis/russia-ukraine/) 

## Rationale and Evolving Research Direction

### The Problem with Cumulative Data

When I first started this project in May of 2022, I was merely aggregating, and visualizing data cumulatively. While tracking total losses reflects the massive scale of the war, its toll and costs, cumulative charts just trend "up and to the right". After some time, I realized I was not learning anything new from what was going on in the battlefield. I also could not state any conclusions to questions such as, "Should Russia be concerned about their casualty numbers, and their rate of equipment loss?".

### The Methodological Shift (Velocity & Probability)

I changed the analysis from cumulative counting to analyzing conflict velocity and quantifying uncertainty with probability.

  - Time-Series & Rate-of-Change: Using daily reported casualty numbers, and calculating a 7-day rolling average to see if the offensive is intensifying or not
  
  - Bayesian Analysis: Data from on-going conflicts is noisy, chaotic and subject to the "fog of war". The accuracy of the data is uncertain, and also depends on the source (with quite different figures). By adding Bayesian Inference, I try to mathematically quantify the uncertainty of the data, and establishing ranges were we are more confident of the numbers.

## Exploratory Insights

I still used cumulative plots to have a overview of the data, to see if there are any interesting things that stand out. Then, I shifted to the rate-of-change analysis.

End