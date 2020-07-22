# Understanding the role of Stylochus ellipticus as a predator of Crassostrea virginica in Chesapeake Bay tributaries

R script and associated figures and tables for 2014 Master's Thesis [DOI](https://doi.org/10.25772/K42A-DE54)

## Abstract

Predation may be a key component of the unsuccessful restoration of the Eastern Oyster (Crassostrea virginica), a former keystone species in Chesapeake Bay. Here, I examine the polyclad flatworm Stylochus ellipticus and its potential role as an important predator of C. virginica. Using small-fragment size C. virginica specific DNA primers, oyster DNA was successfully detected in whole organisms homogenates of wild-caught S. ellipticus individuals. Of the 1,575 individuals tested, 68.1% tested positive, thus predation occurred. Predation did not appear to be affected by salinity or temperature; however, season did appear to have an effect on both predation and S. ellipticus abundance (p-value: <0.05). The findings also imply that S. ellipticus are highly mobile, entering the water column to reach hard substrate at various depths, whereas previous studies suggest otherwise. These findings are useful in the planning and management of oyster cultivation and restoration. Furthermore, this study outlines a method of diet study that may be more sensitive than traditional DNA-based techniques.

## Statistical Analysis

### Seasonal, temporal, and spatial influence of Stylochus abundance
* Analysis of Variance (ANOVA)
* Kruskal-Wallis
* Wilcoxon Rank Sum

### Stylochus predation of Oyster
* Parsed data (abundance = 0 removed) analyzed using "lme4" package (Bates et al. 2013)
* Logistic regression
