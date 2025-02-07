# Annotated bibliography

## @hudakMappingForestStructure2016

Used random forest models to predict dominant species, BA, and tree density as
either univariate and multivariate, knn models. They chose the imputation method
(knn) because it preserves the covariance relationship of the response
variables, and permits prediction of ancillary variables that were also
collected at the same plots. They discuss the role of plot size and GPS location
error in estimating relationships.

Plot variables were imputed on a 30 m grid and then averaged at the block level
(average 439 ha) and correlations between variables were computed across blocks
(n = 425). Found [[surface fuel correlations]] with BA. Elgin AFB
is 186,000 ha!

## @duffPredictingContinuousVariation2012 

Argues for the utility of predicting continuous fuel variation using site,
vegetation, and forest structure metrics, as opposed to using fuel classes which
do not capture fine scale variability. Fuels are a function of production,
decomposition.

Community analysis was performed, but was not used in spatial fuels prediction
because it was not continuous. A redundancy analysis found vegetation could
explain 80% of fuel variation. Final models were GAMs using continuous
environmental predictors. Environmental factors explained 30 - 50% of variance.

I don't think tree species were taken into account. Over half of the shrub
species had significant correlation with fuel load of various classes.


## @chenDevelopmentPredictiveModel2017

Predicted litter load based on terrestrial lidar scan estimates of fuel depth.
Litter load here refers to all fuels smaller than 6 mm. Pairwise interactions of
predictor variables were tested with step-wise linear regression. The best model
used years-since-fire * litter-depth, canopy-density * elevation, and
litter-depth * fuel-type (damp/dry). Other variables tested were:
litter-bed-coverage (%), aspect, slope, and burn-type.

## @galeForestFireFuel2021

This review offers a mostly external perspective of fuel modeling. They point
out that little work has focused on predicting sub-canopy fuels (small tree,
shrub, and surface fuels), perhaps because of inputs for specific fire models.
They advocate for adoption of quickly advancing remote sensing technology to
overcome difficulties in remote sensing of fuels (lower strata) as well as the
collection of the full range of data that are significant to physical fire
behavior--not just those required by simplified models.

## @mutluMappingSurfaceFuel2008

Created maps of fuel models using combined RGB imagery and binned lidar data
with supervised classification. Found Minimum Noise Fraction performed better
than Principal Component Analysis for dimensionality reduction and Mahalanobis
distance did better than Maximum Likelihood. Used the Andersen 1982 fuel models.

## @collinsVariabilityVegetationSurface2016

Assess the influence of many site, composition, and structure variables on
surface fuel loading. Structural variables included four rough dbh classes and
%BA by species. Used conditional inference trees to identify influential
factors. Used K-means partitioning to classify cover types using the same set of
variables. C-trees of site characteristics explained little of the partitioning
of cover types, except site productivity (as actual Actual Evapo-transpiration).
C-trees selected only BA of fir and %CC* as influential on fine fuels, and no
variables for course fuels. Goal was to make inferences about Sierran forest
reference conditions, with an intact fire regime.



