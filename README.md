# Russian-Ukrainian War 2022

Created: 2022-05-26

Updated: 2026-07-13

  - First Inspired by [`Lena Sokol`](https://www.kaggle.com/code/sokolheavy/2022-ukraine-russia-war-visualization)'s project on Kaggle

  - Data sources:
    - [`Petro Ivanyuk Repository`](https://github.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset)
    - [`Armed Forces of Ukraine`](https://www.zsu.gov.ua/en/estimated-enemy-losses)

  - From 2026-01-19, data is read directly from [`Petro Ivanyuk's GitHub`](https://github.com/PetroIvaniuk/2022-Ukraine-Russia-War-Dataset) in JSON format. I will no longer keep a local copy of the CSV data.

## Quick Navigation

- [Background of the Conflict](#background-of-the-conflict)
- [Rationale and Evolving Research Direction](#rationale-and-evolving-research-direction)
  - [The Problem with Cumulative Data](#the-problem-with-cumulative-data)
  - [The Methodological Shift(Conflict Velocity & Probability)](#the-methodological-shift-conflict-velocity--probability)
- [Exploratory Insights](#exploratory-insights)
  - [Exploratory Plots](#exploratory-plots)
- [Data Analysis](#data-analysis)
  - [Casualties](#casualties)
  - [Equipment](#equipment)
- [Final Conclusions](#final-conclusions)

## Background of the Conflict

On 24 February, 2022, the Russian Federation launched a full-scale invasion of Ukraine, escalating a conflict that originally began in 2014. What initially started as a swift campaign aimed at decapitating the Ukrainian government quickly devolved into the largest conventional war in Europe since World War II. The conflict has since transitioned through several distinct phases: early mechanized offensives, deep defensive fortifications, and finally, a prolonged war of attrition characterized by trench warfare, use of drones, and massive artillery exchanges. [(`Institute for the Study of War`)](https://understandingwar.org/analysis/russia-ukraine/) 

## Rationale and Evolving Research Direction

### The Problem with Cumulative Data

When I first started this project in May of 2022, I was merely aggregating, and visualizing data cumulatively. While tracking total losses reflects the massive scale of the war, its toll and costs, cumulative charts just trend "up and to the right". After some time, I realized I was not learning anything new from what was going on in the battlefield. I also could not state any conclusions to questions such as, "Should Russia be concerned about their casualty numbers, and their rate of equipment loss?".

### The Methodological Shift (Conflict Velocity & Probability)

I changed the analysis from cumulative counting to analyzing conflict velocity and quantifying uncertainty with probability.

  - Rate-of-Change Analysis: Using daily reported casualty numbers, and calculating a 7-day rolling average to see if the offensive is intensifying or not.
  
  - Bayesian Analysis: Data from on-going conflicts is noisy, chaotic and subject to the "fog of war". The accuracy of the data is uncertain, and also depends on the source (with quite different figures). By adding Bayesian Inference, I try to mathematically quantify the uncertainty of the data, and establishing ranges were we are more confident of the numbers.

## Exploratory Insights

I still used cumulative plots to have a overview of the data. Sometimes just from the cumulative plots, there are interesting things that stand out, like the recent parabolic rise in the number of drones used. Then, I shifted to the rate-of-change analysis, and using Bayesian inference.

### Exploratory Plots

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/01-casualty-exploratory.png)

The cumulative number of Russian casualties since the beginning of the war is over a million. But, there is limited insight from this chart.

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/02-equipment-exploratory.png)

The cumulative number of Russian equipment loss also shows trends moving up and to the right. In the initial phases of war, the standout losses were the number of tanks. Now, it's the number of drones.

## Data Analysis

### Casualties

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/03-conflict-intensity.png)

The recent 7-day rolling average of daily reported casualties is between 1,000 to 1,500.

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/04-bayes-casualty.png)

Adding Bayesian Analysis, the estimated true mean of daily casualties is around 650.

For context, according to declassified [`CIA documents`](https://www.google.com/url?sa=t&source=web&rct=j&opi=89978449&url=https://www.cia.gov/readingroom/docs/CIA-RDP89T01451R000100090001-5.pdf&ved=2ahUKEwi4_rPtq5uVAxXx2TgGHbDZD5MQFnoECDMQAQ&usg=AOvVaw2N6B0dtQQrrNpRS28ZyCGn), it was estimated that the Soviet Union lost more than 12,000 lives over 10 years (from 1979 to 1989). That means, in roughly 2 weeks, Russian would have lost the same number of soldiers as over 10 years in Afghanistan.

### Equipment

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/05-equipment-shift.png)

There is a shift in the usage of equipment, from the use of Armor in the early stages of war, followed by the transition to Artillery as conflict transitioned to trench warfare, and the recent parabolic rise in the use of drones.

![](https://github.com/weiyuet/russo-ukrainian-war/blob/main/figures/06-bayes-artillery.png)

From Bayesian Analysis, Russia is losing around 40 field artillery units daily. Is this a sustainable burn rate?

## Final Conclusions

The analysis reveal structural shifts in the conflict. Early war was characterized by heavy use of armor. Then as defense positions and frontlines became more entrenched, artillery came to dominate. Finally, the use of drones has evolved from just merely reconnaissance, to industrialized frontline equipment. Being relatively cheap, and yet lethal, they have effectively replaced artillery for precision frontline strikes. Based on the rate-of-change analysis, the intensity of the conflict is not slowing down.

End