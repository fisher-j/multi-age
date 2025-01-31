# Reading notes

## 2025-01-17

@linSaplingGrowthSurvivorship2002
- observation of the cross over effect of shade tolerance theory

@hornAdaptiveGeometryTrees1971
- basis of theory of shade tolerance

@kobeJuvenileTreeSurvivorship1995
- also cited in @forresterResponseTreeRegeneration2014 as a reference for cross
  over in shade tolerance.

So, where as Kobe found that shade tolerance consisted of a trade off between
high-light growth and low-light mortality, lin found that for forests not
affected by long winters, the trade offs growth to growth, and growth to
mortality were equivalent.

## 2025-01-10

@pillersLeaflitterAccretionDecomposition1993
- surface fuel load and decomposition in redwood

@mcdonaldAdaptationsWoodyShrubs1981:
- cited in FEIS as a source supporting the general flammability of tanoak

@kuljianEffectsSuddenOak2010:
- an inappropriate model supports high flammability of tanoak
- tanoak has low foliar moisture content

@forrestelDiseaseFuelsPotential2015:
- effects of sudden oak death on surface fuels



## 2024-12-06

@halpernInitialResponsesForest2005
- few (no) differences between aggregated and dispersed overstory for understory
  growth response

@halpernLevelPatternOverstory2012:

@palikSpatialDistributionOverstory2003:


## 2024-11-08

@kolaksEffectThinningPrescribed2004
- effect of aspect and slope on fuel loading: significant differences.

## 2024-05-02

@hanProductivityCostWholetree2020:
- whole tree removal more expensive than leaving residuals

@stephensEffectsForestFuelreduction2012:
- overview of all thinning prescribed fire effects

## 2024-04-25

@barrettModelThirdGrowth1988:
- (powers and wiant, 1970): 
  - greatest sprouting on stumps 200 - 400 years age, decreasing thereafter
- (Barrette, 1966):
  - age/diameter does not affects sprouting
  - second growth redwood sprout 95% of time

@gouldModelingSproutoriginOak2007:
- Stocking as the response metric of choice
- 30 years after treatment, eventual stocking given original tree size and
  species

@keyserStumpSprouting192015:
- discuss self thinning of sprout clumps
  - importance of time since initiation
- 19 eastern hardwood species
- percent sprouting, sprout density, and height 1 year after harvest
- many species specific results
- percent sprouting inversely related to size for some
  - unexpected, possibly confounded by diameter distributions present at site
- sprout density (weakly) positively related to size for some
  - differing from other studies, that found positive or negative relationship
    between sprout density and stump size
- sprout height relationship varied by species, but included dbh, site-index,
  and basal area in various combinations
- contradicts studies from other regions, suggests need for regional sprouting models
- for most species, no correlation between percent sprouting and tree
  size
- carbohydrate reserves obscures effect of overstory density
  - Percent sprouting (in the short term) not related to overstory density
  - overstory density probably affects survival of sprouts over time: Rydberg
    2000; Atwood et al., 2009, Johnson, 1975;
  -  overstory density not related to height growth: Del Tredici, 2001

## 2024-04-24

@boeNaturalSeedlingsSprouts1975:
- percent sprouting at 5 years:
  clearcut: 56
  shelterwood: 42
  selection: 53

@lindquistSproutRegenerationYounggrowth1979
- sprouting response 1 year after treatment
- measured number of sprout clumps, number of sprouts, percent stumps sprouting
- raw data suggests number of clumps per stump, sprouts per stump and percent
  sprouting increase with understory light
- Weak evidence for relationship between stand density and percent sprouting
- Boe, Neal studied sprouting in old-growth stands
- percent sprouting not related to stump size

```r
tribble(
  ~resid, ~stumps, ~clumps, ~sprouts, ~pct_sprouted,
  25,     282,     980,     6842,     98.2,
  25,     765,     2068,    10550,    94.4,
  25,     420,     1184,    6977,     89.2,
  75,     318,     615,     2200,     84.2,
  50,     428,     1022,    4360,     89.5,
  50,     395,     968,     5705,     87.3
)|> 
dplyr::summarize(
  .by = resid,
  clumps = mean(clumps / stumps),
  sprouts = mean(sprouts / stumps),
  pct_sprout = mean(pct_sprouted)
) |> 
dplyr::arrange(resid)

#   resid clumps sprouts pct_sprout
# 1    25   3.00   18.2        93.9
# 2    50   2.42   12.3        88.4
# 3    75   1.93    6.92       84.2
```

@nealSproutingOldgrowthRedwood1967:
- old growth, Humboldt, Del Norte, 1 year after logging
- Treatments: [shelterwood, selection, clearcut], but treatments could not be
  tested due to differing diameter distributions
- number of sprouts and height not related to tree size and age
- percent sprouting and number of sprouts inversely related to tree size
  and age
- barrette 1966:
  - no relationship between percent sprouting and age/diameter
  - average dia of Barrette 25 cm smaller
- all point to the complexity of sprout response
- Studies sampled different age sprouts

## 2024-04-22

@mumaDynamicsStumpSprout2022:
- Our study
- post-treatment basal area:
  GS: 0
  LD: 22 m2 ha-1
  HD + HA: 39 m2 ha-1

@forresterResponseTreeRegeneration2014:
- hardwoods in north-central Wisconsin
- shade tolerant vs. mid-tolerant did not behave according to traditional
  theory
  - indicates importance of site and climate, 
  - above vs below-ground competition
- year 4 annual height increments:
  control: 13 cm
  small gap: 18
  medium gap: 26
  large gap: 30
- Sugar maple not affected by overstory light
- Church, 1960: drop in percent sprouting 5 years after logging
  15cm stumps: 94% to 58%
  75 cm stumps: 38% to 6%
- Richard, 2003: below-ground competition becomes dominant in poor soils

@keyserStumpSproutDynamics2014:
- address rapid growth of less desirable species
- 90, 80, 70, 60% retention
- upland hardwoods, north Carolina
- average height significantly less in the 2 higher density vs 2 lower density
  treatments. 
- Significant drop above 70% retention

@oharaDynamicsCoastRedwood2009:
- rapid self-thinning of stump sprouts

@rongSurvivalDevelopmentLiaodong2013:
- 30%, 60%, and 75% retention
- growth slowed from year 1 to 5
- taller sprouts in higher light:
  30%: 125 cm
  60%: 134
  75%: 141

@rydbergInitialSproutingGrowth2000:
- greater height growth in clearcut vs. selection cut (coppice, birch and
  poplar)

@berrillUnderstoryLightPredicts2018:
- Redwood height increment doubles from 5 to 150 cm stump diameter
- Redwood transitions to light dependence in year 2? Passes equilibrium, relies
  less on stored carbohydrate
- increasing dependence on light with age
- recommend separating above and below-ground competition

## 2024-04-19

@mcdonaldSilvicultureecologyThreeNative1978:
- Challenge, Yuba County, CA
- tanoak heights at 10 years:
  clearcut: 19 ft
  shelterwood: 8 ft

@knappEarlyStumpSprout2017:
- bottomland hardwood: 11 spp
- 2 and 8 m2 ha-1 retention treatments
- height growth greater for all species with lower retention
- many species showed reduced sprout survival with increasing stump diameter
- several species showed no relationship between stump size and percent
  sprouting
- weak relationship between stump size and dominant sprout height
- no relationship between treatment and number of sprouts

@nievesStandSiteCharacteristics2022:
- percent sprouting given dbh, species, moisture index, harvest treatment
- "well-known tendency for sprouting to vary among species and decline as tree
  size (or age) increases," but this phenomena is highly variable
- increased percent sprouting with overstory openness
  - primarily for non shade tolerant species
- found additional site-level variability in sprout response, not accounted for
  in model variables
- red oak in some areas did not lose sprouting ability with size
- sprouting in response to overstory density literature scarce
- Importance of the range of residual basal area for detection of an effect
- percent sprouting affected by site quality, site history, management...
- "Light is known to induce epicormic sprouting from dormant buds on the
  trunk (Blum 1963, Smith 1965)"

@gardinerDevelopmentWaterOak1997:
- rapid self-thinning of sprout clumps not related to overstory retention
- most rapid growth in first year
- 2.3 m ht after first year, regardless of treatment

value   | Heavy Thin | Light Thin
 -------|------------|-----------
inc 1-5 |        0.51|       0.38
inc 5-7 |        0.23|       0.19
ht yr 7 |          5 |        4.2

- managing coppice beneath partial overstory research is limited
- increasing variability over time (yr 7), especially in light thin obscuring
  statistical differences

@johnsonPredictingOakStump1977:
- Percent sprouting ≥ 2.9 m, 5 years after cutting.
- decreasing likelihood of sprouting with stump size and tree age.
- Percent sprouting related to season of cutting

## 2024-04-18

@harringtonPredictingCrownSizes1992:
- tanoak, madrone, and giant chinkapin 
- predictive equations for hardwood sprout attributes
- applicable to clear cuts
- 20 cm parent tree, at 10 years: 4.3 m 

```r
# max height (cm) from diameter (cm) and age (years)
# based on basal area in dm2
htmax <- function(DIA, AGE) {
    PBA <- DIA^2 * pi / 100
    78 * (PBA^0.1462) * (AGE^0.5804)
}
```

@wilkinsonTanoakSproutDevelopment1997:
- in the shade after 4 years:
  after cutting: 23 in.  
  after repeated burning: 15 in. 
- open grown: compare to 79 in. (Harrington 1992)
- tender green shoots after repeated burning, all were browsed, no browsing in
  other treatments
- Much tanoak sprout development data for open grown and in competition with
  conifers, less known about development in shade.

@oharaUnderstoryStumpSprout2007:
Once a sprout clump becomes self-sustaining, subsequent sprout growth is
primarily a function of the light regime:
  In redwood: Boe, 1975; Lindquist, 2004; Barrett, 1988 Webb, 2012
  Other species: Larsen et al., 1997; Gardiner and Helmig, 1997

age: 5 years

value  | low density | med density | high density
-------|-------------|-------------|------------
ba     | 43          | 77          | 110
ht-inc | 0.95        | 0.41        | 0.22

Asymptote at 50 pacl, low density treatment had average of around 24 pacl.


@barrettModelThirdGrowth1988:
  height when ba resid < 20.5 m2 ha-1: 
    HT = 0.6602 * AGE + 0.5529
  height when ba resid ≥ 20.5 m2 ha-1: 
    HT = (31.5289 * (ln(AGE)^2) / RESBA) - (0.04486 * AGE + 0.9451)

@berrillUnderstoryLightPredicts2018 (our study):
- post treatment SDI (tph):
    GS: 0
    LD: 330
    HA: 530
    HD: 530
    pre-harvest: 710 to 1640

@oliverYounggrowthRedwoodStands1994:
He just studies residual tree growth, not sprout growth. Comments on potential
for below-ground competition for resources.
- post treatment SDI (tph):
    25%: 393 
    50%: 809
    75%: 1236
    100%: 1825
- post treatment BA:
    25%: 110 
    50%: 220
    75%: 330
    100%: 442
- 1-year percent sprouting: 89 (lindquist, 1979)
  10-year percent sprouting: 69 (Barrett, 1985)

## 2024-04-17

@barrettModelThirdGrowth1988:
- Barrette 1966: sprout height growth 0.63 m/yr in open, 0.3 m/yr suppressed

@vila-vilardellPrescribedFireThinning2023:
- 5 years after treatment similar 1-hr and 10-hr fuels, but higher 100-hr
- REDUCED shrub load compared to control
- Increased herbaceous cover

@jimenezMidtermFuelStructure2016:
- canopy fuels still reduced 5 years after treatment
- fine surface fuels returned to pre-treatment conditions

@fajardoTenyearResponsesPonderosa2007:
- Douglas-fir recruits not reduced
- burning reduced residual tree vigor
- burning promoted recruitment, particularly Douglas-fir

@kilgoreCrownfirePotentialSequoia1975:
- fuel loads before and after burning in giant sequoia

## 2024-04-15

@hoodFuelTreatmentLongevity2020:
- No statistical differences in fine woody fuels, but there was a generally
  decreasing trend over time.
- Differences in fuel loading across treatments were relatively small, 12 years
  after treatment
- increased sunlight exposure and precip through fall increase surface
  decomposition rates
  - @keaneBiophysicalControlsSurface2008
- Most significant outcome was advanced regeneration and vegetation recruitment
  which contributed to ladder fuels, but also less desirable stand structure
- thinning and burning most effective: several references
- conditions:
  Thinning:
    initial:
      tph: 370
      qmd: 27 cm
      ba: 21 m2 ha-1
      comp: [93% PIPO]
    post-harvest:
      tph: 180
      ba: 13
      qmd: 31
  Shelterwood:
    initial:
      tph: 434
      qmd: 28.2
      ba: 26.9 m2 ha-1
      comp: [13% PSME, 86% PIPO]
    post-harvest:
      tph: 140
      ba: 12
      qmd: 35

@stephensFireTreatmentEffects2009:
- fuel load increase following treatments that did not remove whole trees
  - whole tree recommended in @ageeBasicPrinciplesForest2005

@knappEfficacyVariableDensity2017:
- After burning, only 100 h and litter differed among treatments: thinned
  treatments had more 100 h fuel and less litter than unthinned controls.

TO READ:

- [X] @stephensFireTreatmentEffects2009
- [X] @schwilkNationalFireFire2009
- [ ] @berrillDevelopmentRedwoodRegeneration2021

@burtonFineFuelChanges2022:
- fine twigs increase, then decreased (after 20 years) in harvested plots
- leaf mass lower in harvest plots until 15 years after fire
- increase in vegetation in harvested plots, peaking at 20 years
- timber harvest resulted in higher fine fuel loads in the short term (<10 years)
- fine fuels continued increasing (post fire) for 30 years -> achieving steady state
### more 10-hr fuels, less 1-hr fuels following harvest
- 10-hr fuels 1.4 times greater in harvested, compare to:
  - @ageeThinningPrescribedFire2006 - Western US forests
  - @stephensFireTreatmentEffects2009
- less leaves and fine twigs for 10 years after thinning
  - also: @johnstonMechanicalThinningPrescribed2021
### After 15 years, harvest plots had more 1-hr fuels
- possibly due to re-sprout mortality?
- convergence of fine fuel loads after 20 years:
  -@hoodFuelTreatmentLongevity2020 
### Shrub regrowth
- Dense
  - @odlandPlantCommunityResponse2021 
  - @keyesPitfallsSilviculturalTreatment2006
- Discuss overstory and azimuth effects on fuel moisture and thus decomposition
### Limitations
- Need to address harvest intensity and method
  - @mccawStandCharacteristicsFuel2002
- slope and topographical effects

Redwood slash depths after variable density thinning, 3 feet initially, reduced
by 1/3 in four years. Negligible after 10 years [@oharaRestorationOldForest2010,
@oharaRegenerationDynamicsCoast2017]

## 2024-04-12

@burtonFineFuelChanges2022 discuss changes in fuels due to timber harvest in
Eucalypt forests.

@parresolEffectsOverstoryComposition2012 lists fuel loading values for SE
forests:
- "Stand age and basal area were generally more important than recent fire
  history for predicting fuel loads." 
- "Age and basal area were related to live fuels in a complex manner that is
  likely confounded with periodic disturbances that disrupt stand dynamics."

## 2024-02-28

I'm still trying to figure out the best way to model species specific diameter
distribution models. I came across @delimaModelingTreeDiameter2015 who fit 10
different statistical models. Then, curious about Bayesian methods, found:
@bullockDerivingTreeDiameter2007. Both of these find different parametric models
perform better under different conditions. After these, I had a brief look at
@bollandsasComparingParametricNonparametric2013, who found non-parametric method
(Most Similar Neighbor) to be more reliable than parametric (Seemingly Unrelated
Regression)

I talked to Ethan about his paper and figured that I should probably read it as
well

The author of the R package, DHARMa has a 2012 paper about Bayesian methods in
ecology: @hartigConnectingDynamicVegetation2012. Therein is a figure refereing
to predicted forest size structure, and a couple of references for the same.
This led to @vanoijenBayesianCalibrationProcessbased2005, *Bayesian calibration
of process based forest models* that I wish I had time to read

## 2024-01-30

I started reading about community analysis in case I need to analyze composition
data for regenerating sprouts for my thesis. I quickly came to the ideas of
[Bray-Curtis dissimilarity](https://stats.stackexchange.com/questions/9281/what-test-to-compare-community-composition)
and testing these with
[PERMANOVA](https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/),
@andersonPermutationalMultivariateAnalysis2017.

## 2023-12-21

### Multi-aged management

@mitchellRetentionSystemReconciling2002 discuss variable retention as it is
practiced in Canada. They emphasize the need for updated silvicultural practices
to match the shift in forest management objectives towards ecosystem function.

@noletComparingEffectsEven2018 indicates that there is not much support the
ecological benefits of multi-aged silviculture, and echos some of the same
sentiments as @apletEvenVsUnevenAged1994 regarding the need for both approaches
as well as protected areas. This same sentiment can be found in
@schutzSilviculturalToolsDevelop2002

I started off my reading in multi-aged management with:

@mumaDynamicsStumpSprout2022 followed by @oharaUnderstoryStumpSprout2007 and
finally @oharaDynamicsCoastRedwood2010, which seems to be basically a copy of
the 2007 paper.


## 2023-11-27

residence time: @cruzAssessingCrownFire2010 point to two competing sources for
residence time calculation. @andersonHeatTransferFire1969 is used in most models
for calculating Byrams fire line intensity, but likely underpredicts.
@nelsonReactionTimesBurning2003 is proposed as a better model, but it is not
used.

emperical lenght to breadth ratio elmfire reference:
@finneyChapterLandscapeFire2004, @andersonPredictingWindDrivenWild1983

huygens principal elmfire reference: @richardsGeneralMathematicalFramework1995

@sardoyNumericalStudyGroundlevel2008 and @perrymanCellularAutomataModel2012 form
the basis of the ELMFIRE spotting model.

## 2023-11-22

@finneySimulationProbabilisticWildfire2011 Only models fires on ERC(G) above the
80th percentile. Fuel moistures are taken from a lookup table of average fuel
moistures (for the year?) "for each percentile range": 80th 90th and 97th
percentiles.

The [IFTDSS](https://iftdss.firenet.gov/firenetHelp/help/pageHelp/content/home.htm) is another source for online documentation of fire models

@liuStudyingEffectsFuel2013 fuel treatment allocation based on burn probability
resulted in lower simulated burn probability in comparison to allocation basesed
on fuel load

@agerWildfireExposureAnalysis2013 uses Fsim for burn probability and fire risk.

## 2023-11-21

@lautenbergerMappingAreasElevated2017
- long-term vs short-term fire risk planning
- references @agerWildfireRiskModeling2006 as a model monte carlo risk modeling
  system
- 250 ignitions km^-2
- One-hour fire spread simulation...
@kearnsConstructionProbabilisticWildfire2022 
- using elmfire, fires burn for a random period, up to seven days to approxiamte
  the duration of observed fires.
- suppression is not modeled explicity, but implied to some degree in how
  ignitions are modeled (based on occurences of fires >100 acres)
- use NOAA RTMA hourly time series for weather input

A host of fire models area available online as a service at the [fire modeling
services framework](https://fmsf.firenet.gov/fmsf_ui/)

@wangProjectedChangesDaily2017 predicts climate change impacts on fire spread

Just learned about the Fire Program Analysis Fire Observation Database
<https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0009.6>

@parisienApplicationsSimulationbasedBurn2019 good brief history of uses of
spatial fire spread models including fuel treatment effectiveness.
- Box 1 addresess burn probability model calibration and emphasizes the
  importance of consulting with local fire managers.
- @scottWildfireRiskAssessment2013 The diverse uses and the limitations of
  different approaches to BP modelling are discussed.
- polygon based summaries of outputs
- fire hazzard: burn probability and some metric of fire behavior
- fire risk: burn probability and potential impacts to valued resources

## 2023-11-20

I missed the Big Basin presentation about the CZU fire on 11/15, but here is the
youtube link: <https://www.youtube.com/watch?v=DtdmH3KCU8s>

Criticism of the generalized liklihood uncertainty estimation procedure
@stedingerAppraisalGeneralizedLikelihood2008, led to probably its first use in
wildfire modleing: @bianchiniImprovedPredictionMethods2006. The criticism is
basically that without a formal liklihood function, the predictions are just
arbitrary.

@oakleyBayesianInferenceUncertainty2002 cited in
@volodinaImportanceUncertaintyQuantification2021 one of the papers that form the
basis of simulation model uncertainty quantification. Volodina also advocate for
the gaussian process emulator approach. This seems like the appropriate way
forward with uncertainty quantification. They cite
@kennedyBayesianCalibrationComputer2001 heavily.

@refsgaardUncertaintyEnvironmentalModelling2007 describe 14 methods of
uncertainty analysis and where they apply. Focus on simulation, but briefly
address broader (social) issues. Important techniques addressed for my project
are:
- Inverse modeling (parameter estimation): the calibration is only valid for
  predicting the outcome used in the objective function that is used for
  calibration
- Monte Carlo: genearl applicability, but distribution for all inputs and
  parameters, as well as their covariance structure must be specified.

@brownIntegratedMethodologyRecording2005 defines categories of uncertainty and a
frame work for recording and documenting them. Each data type is classified by
how it varies in time and space.

@jonesWhereFireQuantifying2004 Assesses effect of spatially explicit error in
wildfire threat indicies. Effect of DEM is low, but fuel classification and
especially weater input error have large effects.

## 2023-11-17

@bevenFutureDistributedModels1992 Describe the GLUE methodology which allows for
model calibration and uncertainty quantification in a Bayesian context for
deterministic models.

@abdalhaqEnhancingWildlandFire2005 early paper on evolutionary algorithms for
fire modeling.

@bevenFutureDistributedModels1992 Explanation of the glue methodology.

@benaliFireSpreadPredictions2017 shows that calibration on past fires can
improve predictions for subsequent fires. Only tunes rate of spread adjustment
factor. Results indicate remaining "strong limitations in modeling system." Uses
Generalized Liklihood Uncertainty Estimation.

@rochouxRegionalscaleSimulationsWildland2013 cited by Lautenberger for
"real-time data assimilation"

The Hausdorff distance is used in image detection/matching. It is the maximum
distance from any point in a set to the nearest point in another set. It is
assymetric, or directional, so the h *between* two sets is the maxium of h(a,b)
an h(b,a).

@denhamDynamicDataDrivenGenetic2012 cited by Lautenberger as first use of
Genetic Algorithm

@jahnForecastingFireGrowth2011 cited by Lautenberger defining model calibration
as inverse modeling.

## 2023-11-16

@simardMoistureContentForest1968 This is used by Lautenberger as a reference for
predicting fuel moisture. He also uses the national live fuels database for
assigining live fuel moisture.

@liuGlobalSensitivityAnalysis2015 seems to indicate that Surface fuel load is
does not have significant uncertainty for all fuel models. And found relatively
small effects for fuel moisture as well. They found consisten effects of low
heat content and surface area to volume ratio. This seems contradictory, could
it have to do with how they grouped individual components?

## 2023-11-14

@liuParametricUncertaintyQuantification2015 give a concise overview of the 24
input parameters to the Rothermel fire spread model. I should do something
similar.

## 2023-11-10

most cited paper in the geosciences is about data assimilation:
@kalnayNCEPNCAR40Year1996 very basically, data assimilation is an iterative
process of updating a prediction based on an initial estimate and estimates of
errors in observations as well as in the model.

On parametric uncertainty, three new papers, cited in both
@ervilhaParametricUncertaintyQuantification2017 and
@yuanPhysicalModelWildland2020 
- @jimenezQuantifyingParametricUncertainty2008 
- @bachmannUncertaintyPropagationWildland2002
- @salvadorGlobalSensitivityAnalysis2001 

@bachmannUncertaintyPropagationWildland2002 wonders what the comparison between
stochastic input - determinisic spread, vs models with deterministic input -
stochastic spread, and finally, stochastic - stochastic models. Uncertainty in
this study is the observed spatial variability in fuel attributes within a fuel
model classificaiton. Uncertainty calculated for each cell using partial
derivative of rate of spread with respect to each predictor (taylor series?)
resulting in a SD for each cell. Total variance was the average of variances for
each cell in a fuel type. Thus, using a combination of observed and assumed
variances on input variables, total variance in the output was estimated,
analytically, without Monte Carlo simulation.

General reading on uncertainty quantification in environmental modeling
@thompsonUncertaintyProbabilityWildfire2016,
@uusitaloOverviewMethodsEvaluate2015,
@ohaganProbabilisticUncertaintySpecification2012,
@volodinaImportanceUncertaintyQuantification2021,
@refsgaardUncertaintyEnvironmentalModelling2007

@ohaganProbabilisticUncertaintySpecification2012 focus on expert opinion

I now have the textbook: @wikleSpatiotemporalStatistics2019 

@yooBayesianSpatiotemporalLevel2022 second bayesian level-set model? Description
of level-set method. They infer fire spread rate from data rather than from a
spread model.

@dabrowskiDataAssimilationLevelSet2022 first bayesian level-set model

generalized uncertainty liklihood estimation (GLUE) can be used as both a
calibration method and an uncertainty propagation method
[@refsgaardUncertaintyEnvironmentalModelling2007]

calibration with a single model wrongly attributes model structure error to
parameter uncertainty. Because model structure is not accounted for, predictions
of outputs other than the one used in calibration are unreliable
[@refsgaardUncertaintyEnvironmentalModelling2007] 

@fujiokaNewMethodAnalysis2002 quantifies uncertainty in predictions of fire
spread, providing 95% confidence limits of perimeter.

@elhami-khorasaniConceptualizingProbabilisticRisk2022 discusses probabibilistic
fire model predictions as one step in fire risk assessment.

## 2023-11-09

@volodinaImportanceUncertaintyQuantification2021 What the title says, didn't
read it, but I'm trying to write about uncertainty in a genearl way, so here you
go. Gaussian process emulators to speed up calculations and quantify uncertainty
at the same time. In fact, this paper seems to be about the same process used by
@allaireNovelMethodPosteriori2021 and cites some of the same papers regarding
the esimation of uncertainty starting with a deterministic model. 

## 2023-11-08

@liuParametricUncertaintyQuantification2015 compares various methods of
parametric uncertainty estimation.

@filippiEvaluationForestFire2014 compares various models "out of the box" on a
large set of wildfires. They seem to perform rather similarly.

@fujiokaNewMethodAnalysis2002 early probabilistic estiates of model uncertainty.

@yuanPhysicalModelWildland2020 parametric uncertainty in phyusical model. Higher
slopes result in increased concective heat transfer

@alexanderLimitationsAccuracyModel2013 cited in 
@cruzUncertaintyAssociatedModel2013:
- half of studies had mean absolute percent error between 51 and 75%
- studies with the rothermel surface model 20-310%
- No evidence of more error with crown fire predictions,
  - van wagner said crown fire may be easier - narrower range
- Does not address error propagation involving multiple model linkages, which
  can be much higher.

## 2023-11-07

landfire: in 2019 landfire introduced capable fuels which models fuel condition
after disturbance.

@nelsonLANDFIRERefreshStrategy2013 landfire data was updated with new
techniques. "incorporates adjustments that were required with previous versions
of the data, as outlined in @strattonGuidebookLANDFIREFuels2009" 

@scottReviewAssessmentLANDFIRE2008 assesses problems with landfire crown fuel
data. Calls for the need to include fire model calibration parameters, rather
than adjustment of sound input data. This would be nice. This document
references @scottComparisonCrownFire2006 which compares crown fire models and I
believe identifies crown fraction burned as the main cause of discrepency
between nexus and flammap.

@strattonGuidanceSpatialWildland2006 has a lot of guidance on model input
calibration, but not a lot of references.

@krasnowForestFuelMapping2009 built custum landscape files, but used "an
iterative approach to match historical fires" so it was not surprising they
achieved better results. Basically, they used calibration techniques.

## 2023-11-06

A new parameter uncertainty paper: @alessandriParameterEstimationFire2021 

a couple more papers to read:
@hiltonEffectsSpatialTemporal2015
@refsgaardUncertaintyEnvironmentalModelling2007 reference in above for general
uncertainty in modeling guidance.
@sullivanEstimatingErrorWind2001

@pintoProbabilisticFireSpread2016 hindcast a single fire. They use wrf
forecasted weather data and windninja. Fuel moisture was constant (6, 7, 8, 60,
and 90%). Wind direction, speed and relative humidity were assigned normal
distributions. Distribution of ROS adjustment factors was determined from 8 fire
perimeters where ROS adjustment was varied for each fuel model independently
(between 0.33 and 3). I believe that sampled values were for a given model run
were added globally i.e., constant added to every hour of weather data. Fuel
model perturbations seem to have been sampled independlty for each fuel model
present. They perform many runs of different simulated time lengths. They don't
clearly state how their probabilities are calculated, which is confusing because
there are many non-coincident simulated burn periods. In any case, they found
that re-initilizing siulations from observed fire locations did not improve
predictions. They didn't model spotting, which was observed as a major driver of
fire behavior.

## 2023-11-03

@caiAnalysisUncertaintyFuel2019 attempted to calibrate a fuel model for their
forsts in China in @caiDevelopmentStandardFuel2014 but met with unsatisfactory
results and so undertook to determine the primary sources of fuelmodel
uncertainty through multiple sensitivity and uncertainty analyses. They show
that a sensitvity analysis is does not capture a parameters effect on the
output, because in reality, some potentially sensitive inputs are not very
uncertain. They show that fireline intensity has the greatest percent variation,
followed by rate of spread and flame length. While shrub surface area to volume
ratio was identified as an inflential variable, it does not vary greatly across
the landscape, and so was omited from final evaluation. The final values chosen
for calibration were:
- fuel bed depth
- 1-hr loading
- shrub loading
They claim that few (no) studies assess individual fuel model parameters
separtely, but rather assess sensitivtiy of outcomes to fuel models as a whole.
It is difficult to tell in this study, how exactly parameters were varied for
simulations, by pixel, or globally across the domain.

@allaireGenerationEvaluationEnsemble2020 is the precursur to
@allaireNovelMethodPosteriori2021, which is well written and covers the ensemble
simulation literature in a straight forward way.

@andersonFiregrowthModellingUsing2007 random and systematic perturbation of
weather data. They are trying to determine the effect of various perturbation
techniques on forecasted weather, using simulated fire from actual weather as
the ground truth. Random weather perturbations of forecasted weather led to
20-30% more area burned than actual weather. Overall, I'm not sure if any of
this is relevant to my study.

@parksWarmerDrierFire2020 builds on fire severity research to calculate regional
time series trends.

@samSimulatingBurnSeverity2022 Recent article refering to many recent burn
severity literature. They use a multi-step modeling process with GAMS to
classify burn severity and produce stochastic realizations of burn severity
maps. Framed as an alternative to process based simulations.

@estesFactorsInfluencingFire2017 Include existence of inversion layer and solar
radiation model in statistical model of fire severtiy under moderate conditions.
Inversion layers were determined from 3 RAWS stations above and 3 below 1300 m
elevation. The difference was calculated and daily difference significace was
assessed with a statistical test. Solar radiation was calculated using the
algorithm of Kumar et al. (1997).

## 2023-11-02

@cochraneEstimationWildfireSize2012 calibrates farsite first to produce
realistic fire perimeters. Cite error and lack of validation in input datasets.
Proportional modifiers used to adjust CBH, CBD, FMC and CC) and weather
(primarily wind) "similar to stratton 2006, 2009". Calibration also accounted
for observed NBR and spread rate. Spatial probability of fire burning as a
resutl of fuel treatments. Cites Finney et al. (2007) a lot. Once calibrated,
the model was used to produce two sets of simulations (about 30 runs each) for
each fire. One with actual (treated) stands, the other with inputs modified to
represent a counter-factual no treatment scenario.

@parksFireActivitySeverity2014 1st time emphasis on "broad scale" severtiy
across ecological gradient. Focus on proxies for fuel ammount and fuel moisture.
50,000 hectare "hexels". National parks and wilderness areas. Explores the idea
that fire is limited at the extremes of ecological gradients of moisture and
productivity.

@keyserClimateDrivesInterannual2017 make strong case for climatological controls
on high severity fire, refuing @dillonBothTopographyClimate2011. Their results
are conditional on large fire occurence, which would need to be predcited
separtely.

@parksSpatialBottomupControls2012 use Burn Probability to assess the effect of
bottom-up controls including fuel arrangement, topography, and ignition
patterns. Fuel arrangement had a strong effect on spatial pattern of Burn
Probability. Parisien, Parks, and Miller are doing lots of Fire simulation
around this time, It makes me wonder if they've moved on for a particular
reason. Here is a similar paper from Parks (2011):

@parksMultiscaleEvaluationEnvironmental2011: Use randig, a version of flammap
that includes non-random ignitions and variable winds and fuel moisture. Explain
the differenece between spatial and non-spatial inputs. Suggest that burn
probability is best analyzed at very large spatial scales 2000-3000 ha. Compare
and contrast correlations in fire behavior with mechanistic controls (i.e.,
slope and apsects effect on vegetation and moisture, and fire season length).
They also address colinearity in predictors. The scale of the spatial inputs
represents an assumption and conditions our interpretation of the results.
Randig does not include spatial varrying winds, or fuel moisture.

@zinckUnifyingWildfireModels2009 I'm not sure why I read this. I guess I had
never heard of any of these fire spread models before, but apprently fire spread
models that exist in the realms of statistical physics and ecological succession
are similar. In ecology there are over 40 fire models describing landscape level
fire patterns. Mostly they depend on time since last fire.

@finneySimulationProbabilisticWildfire2011 mentions statistical modeling with
historic data as another approach to burn severity and probability mapping, and
lists several references, but these don't seem to overlap with any of the
references I found for burn severity mapping like dillon, parks, or harvey. This
is interesting. Following up on one of the incldued references for burn
probability derived from statistical modeling:
@preislerStatisticalModelForecasting2007 and searching for citations led me to
recent burn probability from a bayesian statistical model:
@pimontPredictionRegionalWildfire2021.

@estesFactorsInfluencingFire2017 statistical modeling of burn severity during an
average year (moderate conditions). Can't use data from immediately after the
fire because low sun angle late in the year obscures north slopes. Uses
relationship between dnbr and percent canopy cover change developed by
@millerCalibrationValidationRelative2009 

## 2023-11-01

@lautenbergerMappingAreasElevated2017 good background information on monte carlo
wildland fire spread modeling.

@andersonFiregrowthModellingUsing2007 perhaps one of the first studies to employ
monte carlo simulation in fire spread modeling.

@cardilPerformanceOperationalFire2023 uses Wildfire Analyst Enterprise, and
aparently has access to lots of proprietary data products, like technosylvas
improved fuel models and live fuel moisture data, as well as calibrated Weather
Research Forecast model data. A little discouraging...

@jonesWhereFireQuantifying2004 sensitivity analysis of fire model to quantify
uncertainty due to inputs

These were two papers listed on the homepage for farsite under the heading "Fuel
Treatment Evaluation using FARSITE":
- @strattonAssessingEffectivenessLandscape2004 Described landscape fire
  simulation to assess fuel treatment effectiveness

## 2023-10-31

@mchughConsiderationsUseModels2006 and @strattonGuidanceSpatialWildland2006
provide guidance on calibrating spatial fire models (FARSITE). 

## 2023-10-30

@ottFuelTreatmentEffectiveness2023 checking out references within. I realize I'm
not familiar with the modeling approaches. This paper focuses on the landscape
level effect of treatment extent, placement, size, prescription, and timing.
These are different from studies that focus on local fuel treatment
effectiveness.

More literature review:
- @parisienApplicationsSimulationbasedBurn2019 
  Introduces the development of the idea of burn probability modeling (BP) and
  its current applications. Included is an exhaustive list of BP modeling
  studies.
- @whiteEcologistsShouldNot2014: don't interpret simulation studies with
  p-values. 
- @kaliesTammReviewAre2016: emperical evidence of fuel treatment effectiveness
- @mckinneySystematicReviewEmpirical2022 emperical evidence for landscape level
  fuel treatment effectiveness
- @parisienCommentaryArticleBurn2020 erroneous interpretations of burn
  probability 
- @agerIntegratingFireBehavior2011 describes early system to implement a
  burn probability GIS system, using flammap, arcgis, and fvs-ffe

## 2023-10-27

@parksPreviousFiresModerate2014 methods for computing RBR.

## 2023-10-25

@veirsCoastalRedwoodFire1985 Live fuel load is very important to "natural," or
higher severity fire in redwoods.

## 2023-10-23

@hawbakerMappingBurnedAreas2017 is often cited in burned area mapping studies.
Their product detected more burned areas than the Monitoring Trends in Burn
Severity database. This is the "Burned area essential climate variable"
algorithm developed for USGS. The more recent algorithm is @hawbakerLandsatBurnedArea2020. These only report the probability that a pixel burned,

@collinsUtilityRandomForests2018 demonstrate accurate burn severity
classification using random forests and several landsat bands/indices. They use
imagery very near to the fire occurence: *60 days before and after the fire*. They
use surface reflectance products and mask water (and shadows?).

@daldeganSpectralMixtureAnalysis2019 use a multiple end-member spectral mixture
analysis to delineate burned areas in the tropics.

## 2023-10-18

Chris Lautenberger has documented [ELMFIRE](https://elmfire.io/getting_started.html)

and the University of Nevada is working with wrf-fire: <https://unr-wrf-fire.readthedocs.io/en/latest/#>

I'm realizing that there are multipe versions of wrf-fire, the first appears to
have been developed by @mandelCoupledAtmospherewildlandFire2011 and is
maintained separately. It is not clear what fire model is included in the
official repository.

@munoz-esparzaAccurateFireSpreadAlgorithm2018 Improvement to the wrf-fire model

@coenWRFFireCoupledWeather2013 describes some of the complexities of coupled
atmospheric fire modeling and distinguishes between two classes of computational
fluid dynamics models, namely, those that employ numerical weather predictions,
and those that are concerend with small domains, <1km and don't account for
interactions with the atmosphere on larger scales, i.e., those that produce
weather.
- "the winds over a strong heat source tend to be upward"
- 

## 2023-10-16

@hewardBurnSeverityRelated2013 "fire severity" is used to indicate immediate
fire effects, whereas "burn severity" is used to denote long term impacts.
Provide a discussion and references for "linking fire intensity and burn
severity." I was not impressed with their plots of the relationship. There are
apparnetly very few studies relating fire behavior to fire effects.

## 2023-10-15

@kolaitisComparativeAssessmentWildland2023 found that the model of
@rossaEmpiricalModelingFire2018 combined with the rothermel wind adjustment
factor produced the most accurate results across 166 laboaratory tests provided
in the literature. 

@rossaEmpiricalModelingFire2018 uses fuel moisture, fuel height, and surface
area to mass ratio (specific area) in an emperical model of no-wind, no-slope
fire spread. They use a wide range of laboratory, field, and some wildfire data
to fit their model.

## 2023-10-14

@olbrichUnderstandingFireBehavior validated improved canopy base height
[@engelstadEstimatingCanopyFuel2019], surface fuel model selection (national
forest staff) and canopy bulk density @wolterModelingSubborealForest2021. 

It would seem that Wolter had two grad students parsing remote sensing data and
running farsite models, only to find that the "new" inputs had little effect.

The improved canopy base height data did not improve predictions of fire spread
(on average the "improved" data were very close to the land fire estimates).
Surface fuel models were selected by managers and these performed better than
thos provided by landfire, although the two estimates were correlated. Despspite
improvements, modeled fire still failed to accurately characterize crown fire
spread, especially spotting.

looking at the modeled fire perimeters, canopy bulk density improvements did not
appear to be any better than landfire data.

Searching for bayesian random forests led me to 
[this site](https://discourse.mc-stan.org/t/bayesian-random-forest/11795/2)
where an alternative is proposed. Gaussian processes are also suggested as the
non-parametric model of choice.

## 2023-10-13

@parksHighseverityFireEvaluating2018 incorporate vegetation estimate into fire
severity prediction. 
- Suggest their results can be combined with fire behavior predictions to better
  inform management. 
- Incorporate weather variability by resampling weather for each pixel. 
- Use RBR cutoff to categorize high-severity fire.

@goudieJoiningSplittingModels2019 describe something similar to Bayesian
melding, where complex models can be broken into similar sub-models, and this
can be applied to deterministic models as well.

## 2023-10-11

@coenRequirementsSimulatingWildland2018 advocates for CAWFE and points out the
limitations of other coupled weather-fire models.

## 2023-10-10

@hewardBurnSeverityRelated2013 finds fire severity is related to burn intensity
as measured from modis FRP.

@scanlonPredictingPostFireSeverity2006 Used farsite to model fire effects at the
canoe fire and found no correlation between composite burn index and farsite
modeled outputs.

## 2023-10-09

@harveyIncorporatingBiophysicalGradients2019 test sensitivity of spectral
indicies to canopy burn severity and find biophysical gradients are important to
account for is estimating the correlation. Readily available extensive data like
topopgraphy and prefire spectral data improved fit for RdNBR. In addition, stand
structure data also affects the relationship between observed fire effects and
RdNBR. Accounting for error in relationship between satelite indicies and field
burn severity encompasses as much variability as different components of burn
severity. On this point, they recommend testing burn severity relationship among
different quantiles of predicted burn severtiy posterior distribution. 

Heat index (topographic variable) has similar effect of latitude, owing to the
interception of suns rays throught the canopy.

60 - 70% explained variability seems to be the max for NBR dereived indicies for
field observed burn seveirty metrics.

They also list the sources for prepared RdNBR data.

@fernandez-guisuragaCautionNeededMediterranean2023 found the individual burn
severity metrics were better correlated with spectral indicies, depending on
vegetation community. Thus, composite burn indicies can obscure relationships
between indices and fire effects.

@thompsonVegetationWeatherExplain2009 case study of drivers of crown damage in
the biscuit fire. Manually digitized aerial photos.

@soverelEstimatingBurnSeverity2010 RdNBR does not perform better than dNBR.

@millerCalibrationValidationRelative2009 use FVS data to genearte canopy cover
estiates. Use @bigingEstimationCrownForm1990 for crown diameters. Percent canopy
change and BA change don't have a strong relationship with NBR, but this could
have to do error in their estiamtes. 

## 2023-10-06

I think that @pooleInferenceDeterministicSimulation2000 might hold the key to my
problem of having priors for both model inputs and outputs of a deterministic
model, as well as data pertaining to outputs?

## 2023-10-04

@borsukBayesianHierarchicalModel2001 promote a hierachical model for estimating
parameters from data across systems. I would apply this idea estimate key
parameters for fire effects prediction across multiple fires in the redwood
region. 

Sidetracked by remote sensing burn severity: 
- @birchVegetationTopographyDaily2015 find no effect of windspeed on dNBR,
  corroborated by @dillonBothTopographyClimate2011, who found topography and
  moisture to be the predominant predictors of dNBR. They also cautioned against
  directly interpreting dNBR as burn severity, which was confusing, because
  thats exactly what they did in the article, they point to
  @morganChallengesAssessingFire2014 on this point.
  *Basically, topography and fuels are found to have the strongest association
  with dNBR.*

- @healeyComparisonTasseledCapbased2005 compare different landsat derived
  idicies of canopy loss, an example of implementation: A First Assessment of
  Canopy Cover Loss in Germany’s Forests after the 2018–2020 Drought Years 

reading @pooleInferenceDeterministicSimulation2000, discussing the advent of
statistical inference with deterministic models. 

@uusitaloOverviewMethodsEvaluate2015 help me understand the difference between
Monte Carlo, ensemble, and Bayesian methods for quantifying uncertainty in
deterministic models. Some highlights:
- the importance of uncertainty quantification in decision making
- the cost/benefit of converting deterministic models to probabalistic framework
- Can spatio-temporal variance be used to quantify the full range of variation?
- Senstivity analysis and Uncertainty analysis both revolve around generating
  many model runs, with parameters varied either singly, or randomly drawn from
  independent or joint distributions.
  - Sensititivity is a form of uncertainty:
    @pooleInferenceDeterministicSimulation2000 
- Data-based options include bayesian networks:
  @garroteProbabilisticForecastsUsing2007 used this approach, but validated the
  model using "ensemble" prediction techniques.

@parksGivingEcologicalMeaning2019 build a model to measure Composite Burn Index
from spectral indicies. I would like to do somethig similar with crown loss.
They differentiate their study from those that try to determine the drivers of
burn severity, suggesting that their model be used to define the response
variable used in studies testing drivers of burn severity.

## 2023-10-03

I added @andrewsExaminationWindSpeed2013 where the idea of a wind limit is
criticized 

Sam Hillamn in @viegasAdvancesForestFire2022 is working on a "framework for fuel
inputs to furture fire behavior models" from remote sensing.

## 2023-10-02

@vanellaMultiFidelityFrameworkWildland2021 describe using FDS with the level set
mehtod on complex terrain.

@phelpsClassificationForestFuels2022 used surface fuel load, canopy base height,
canopy bulk density to classify fuel types and define probabilities of crown
fire ignition using @cruzModelingLikelihoodCrown2004 and
@cruzDevelopmentTestingModels2005. This seems pretty simple, and fairly
reasonable. I should at least try it out.

@guoTransitionSurfaceFire2022 validated the van wagern transition model as
presented in @scottAssessingCrownFire2001, in a laboratory setting with dry
trees.

@ritterFinescaleFirePatterns2020 found fire intensity for crown initiation 4-12%
greater than reported by @vanwagnerConditionsStartSpread1977 

@safford2020CaliforniaFire2022 point towards recent advancements towards
wildfire use and strong criticism regarding lack of progress. Used RAVG RdNBR

@lautenbergerWildlandFireModeling2013 presents an Eulerian level set fire spread
model--it is not atmospherically coupled, and it fails to predict the mass fire
behavior found at the Creek Fire @stephensMassFireBehavior2022. It can be found
at: <https://github.com/lautenberger/elmfire>

@scanlonProgressionBehaviorCanoe2007 discusses the fuel models that best
describe the progression of the canoe fire from old growth to young growth stand
conditions, as well as on the ground, observed fire behavior.

## 2023-10-01

while looking up citations of @wilsonReexaminationRothermelFire1990 I came
across some interesting remote sensing papers:
@chowdhuryOperationalPerspectiveRemote2015 which gives an overview of potential
uses of remote sensing, and @maffeiCombiningMultispectralThermal2021 which
compares remotely sensed fire to fire danger mapping product. They define rate
of spread as circular area divded by total fire duration, which doesn't make
sense. 

## 2023-09-28

@cruzBilloRoadFire2007 demonstrates a post-fire case study and reconstruction.
The CFIS model can be
[downloaded](https://www.frames.gov/applied-fire-behavior/cfis) the application
comes with several articles pertaining to crown fire modeling. *I just realized
that CFIS is not the same as CFIM, the latter being used for predicting crown
fire ignition in ensemble models*

@scottComparisonCrownFire2006 compares NEXUS, Flammap, and CFIS models, and
comments on [[Abrupt changes in spread rate after crowning]]

@cruzAssessingCanopyFuel2003 discuss the determination of important canopy fuel
metrics.


## 2023-09-27

@justEffectsFuelLoading give some fuel loading values for Swanton Ranch, near
santa cruz.
@veirsCoastalRedwoodFire1985 has some interesting discussion, of historical
interest.
@cruzPredictingIgnitionCrown2006 Has the definition of the CFIM model, I would
like to try to code this in R.

## 2023-09-20

Have been reading @cruzAssessingCrownFire2010 and
@alexanderInterdependenciesFlameLength2011 for the last couple days. Trying to
determine how to use existing information to predict potential fire effects at
our site.

## 2023-09-17

Read Chap 8 of @werthSynthesisKnowledgeExtreme2011 Alexander and Cruz have much
to say about crown fire modeling. They tend towards the use of empirical models
and emphasize that current modeling uncertainty warrants making prominent use of
expert opinion.

## 2023-09-16

@forthoferLIHTFireHighresolution1D2022 led to reading
@ahmedLargeEddySimulations2022 who improved the model of Finney for 1d spread

## 2023-09-14

@ramirezNewApproachesFire2011 description of wildfire analyst.

@pereiraReviewGeneticAlgorithm2022 reviews Genetic algorithms, on which the
California used fire model Wildfire Analyst is based, as a method of calibrating
the Rothermel model based on observations. This paper lists the entire Rothermel
model in succinct form.

@jollyMechanisticLiveFuel2022 has a live fuel moisture model

@hillmanComparisonTLSUAS2021 Describing multiple strata of understorey fuels,
surface, near surface, elevated, and intermediate canopy.

@hillmanComparisonTLSUAS2021 Effectiveness of UAS or TLS lidar to characterize
crown fuels

@labenskiQuantifyingSurfaceFuels2023 is the most recent prediction of fuel from
lidar data. They summarize other findings and used their model for predict fire
behavior and perform parameter sensitivity analysis using FirebehavioR
(Rothermel). Found results most sensitive to shrub, likely because of fuel bed
depth.

@tanejaEffectFuelSpatial2021 finds scale of remote sensed fuel attributes has
significant impact on modeling results and a reduction in fire effects
(representing model failure) when fuel gaps are spatially resolved. Implication
for fuel reduction evaluation. Interesting discussion of the pitfalls of
physical models. 1D spread empirical model combined with an elliptical length to
breadth ratio. Because the emperical model was calibrated with plot level
averages, higher resolution data caused the model to break down.

@forthoferLIHTFireHighresolution1D2022 describes finneys 1d fire spread model
and @finneyDeepLearningHighResolution2022 describes its prediction with machine
learning. The approach is similar to that in @mellPhysicsbasedModelingFire2021
where fine scale fire spread is predicted with a computationally intensive model
and the results are used in a simpler model.

Followed
[link](https://firelab.org/project/deep-learning-high-resolution-wildfire-modeling)
provided by Joe. Discovered @viegasAdvancesForestFire2022 -- nearly 2,000 pages
of recent research.

Met with Peter, he likes machine learning. I couldn't explain succinctly the
problems with remotes sensing/machine learning techniques to answer my study
questions.

@benaliDecipheringImpactUncertainty2016 used modis fire perimeter to assess
uncertainty in fire spread model across fuel types, wind speed/direction, fuel
moisture, ignition point. Model error was not assessed.

## 2023-09-14

@mellPhysicsbasedModelingFire2021 explores the use of level-set model and
compares performance to physics based model in FDS. (coupled atmospheric fire
spread)

@crookstonPercentCanopyCover1999 gives the calculation of canopy cover from
individual tree crown area.

@jacksonValueInformationSensitivity2019 Describes method for identifying which
variables contribute the most to outcome uncertainty -- a mode of sensitivity
analysis.

@jollySevereFireDanger2019 this fire danger rating combines vegetation drying
and wind.

@matthewsDeadFuelMoisture2013 reviews models for fuel moisture, which otherwise
are just specified.

@albertBayesianApproachEstimation2022 has ideas about simplifying complex process
models.

@cookBayesianForecastingDisease2023 uses Bayesian modeling with no data, in
disease spread.

## 2023-09-13

I'm now getting into the world of ensemble modeling to quantify uncertainty
- @benaliFireSpreadPredictions2017
- @allaireNovelMethodPosteriori2021
- @finneyMethodEnsembleWildland2011


I feel like I've come full circle, back to papers I started reading in the
beginning by Cruz and Alexander:
- @cruzAssessingCrownFire2010 explains the various failure modes of current
  wildire prediction models.
- @cruzUncertaintyAssociatedModel2013 quantifies the uncertainty resulting from
  the errors above.

Maybe I'll do something similar to what @normanAssessingRisksMultiple2010 did.
It was the only fire behavior example cited in
@carrigerCausalBayesianNetworks2021. 

Cardil et al 2019 discuss methods of updating rate of spread models based on
observed rate of spread. This is not so helpful for me.

Distracted by a Bayesian model of fires spread that uses level set method and
whose math is far beyond me @yooBayesianSpatioTemporalLevel2022. I think it
works by calibrating on subsequently observed perimeters so is not immediately
useful for prediction.

@ritterVerticalHorizontalCrown2023 Tested the effect of horizontal and vertical
continuity (ladder fuels and clumps) on % canopy burned, including percent large
tree canopy burned. They found horizontal connectivity is important in addition
to vertical. Reinforced importance of CBD and midstory ladder fuels. These
findings are similar to @ritterRestorationFuelHazard2022.

@duffRevisitingWildlandFire2017 nice compilation of relevant fuel attributes and
critique of how they are collected. Advocate for universal collection protocols.
Not immediately useful.

@beggUncertaintyVsVariability2014 revealed to me that a uncertainty is just what
it claims to be. An individuals level of belief about something. I'm still
unclear about the role of variability in informing uncertainty, but I understand
that it is an observable quantity, whereas uncertainty is not.

## 2023-09-12

@brightPredictionForestCanopy2017 correlated lidar and landsat imagery with
canopy and surface fuels. Surface fuels were not as well predicted, but included
both direct and indirect relationships, that is, the best model included both
surface (below 1.37 m) and canopy (75 percentile) metrics. #unfinished

## 2023-09-11

read this paper:
[[annotated-bibliography#@hudakMappingForestStructure2016]]

a google search turned up @labenskiQuantifyingSurfaceFuels2023, a promising
review of lidar derived surface fuels in mixed forests.

found @nymanEcohydrologicalControlsMicroclimate2018 while on research rabbit,
who found subcanopy microclimate has strong influence on fuel moisture and
temperature, and thus, decomposition rates.

@mccarleyComparisonMultitemporalAirborne2022 compared FCCS and lidar for
estimating surface fuels and found that lidar captured variability not
detectable with FCCS. Selected a small set of lidar variables for predicting
total fuel loads with random forests algorithm. References include:
  - McCarley 2020 Estimating wildfire fuel consumption
  - @brightPredictionForestCanopy2017
  - Fekety 2015 Temporal transferability of lidar-based
  - Hudak 2012 Quantifying aboveground forest carbon

read this paper:
[[annotated-bibliography#@duffPredictingContinuousVariation2012]]

## 2023-09-10

**Lydersen 2015 Relating Fuel Loads**

- Strong relationship with Live and Dead BA
- Model explained around 1/4 of the variability- Some species specific effects:
  - Pine and white fir BA and canopy cover
- No significant of stem density. Emphasis on BA Suggests that large trees are
  more important
- Clearly defined variables and model tables
- Most predominate effects in best models were:
  - total canopy cover
  - total live AND dead BA
- variable importance factor revealed more species specific relationships not
  included in the best model
  - blk oak live BA on duff
- No 2-way interactions were significant

**Jackubowski et al 2013**

Tried many combinations of stack of lidar and RGB imagery to predict both fuel
models and fuel parameters using a number of different techniques including
regression and machine learning. Fuel model classification accuracy was not
great, around 45% for the Scott and Burgan 40, usig either lidar + MNF data or
PCA lidar+imagery data. Using more general classes (from anderson 16, SH TU TL)
they had around 75% accuracy using SMO (related to SVM). For estimating
individual fuel metrics, there were higher correlations with canopy and shrub
metrics. Their "best" results were distributed across dataset-algorith
combinations. SMO was often the best. Linear regression performed well with
"easy" variables like canopy height and basal area. They didn't even report
results for dead surface fuels, because of low accuracy.

I've been vaguely considering using a Dirichlet based distribution for jointly
modeling species distributions, but am not really sure how to do this. These two
references deal with latent Dirichlet allocation.

- Valle et al 2018 extending the latent Dirichlet allocation model to
  presence/absence data
- Valle et al. 2021 The latent Dirichlet allocation model with covariates

## 2023-09-08

- @keaneSpatialScalingWildland2012
  - need to incorporate variability into fuel maps
  - very complex/intensive sampling design
  - used firemon protocol
  - recommendations:
    - sampling fuel size classes that accurately capture meaningful
      distributions rather than those prescribed by fire models
    - similarly, sampling scale ("four static nested sampling frames") was not
      appropriate for all fuel classes
      - "logs should have been sampled at 5, 10, and 15 m distances"
    - Found no [[surface fuel correlations]] with stand structure or composition



## 2023-09-07

This morning I finished reading (listening to) @galeForestFireFuel2021, which
contains a great wealth of references, enough to probably keep me busy for the
next two years.

Sidetracked again! The rabbit hole consumes. I added these papers:

- @hudakNearestNeighborImputation2008 *I hadn't realized that Hudak was involved
  with this predicting stand characteristics from lidar*
- @hudakMappingForestStructure2016
- @chenStratabasedForestFuel2016 *Methods developed for
  @chenDevelopmentPredictiveModel2017, terrestrial lidar and point cloud
  classification*

and re-discovered these:

- @whiteModelDevelopmentApplication2017 *Canadian guide for foresters to augment
  inventories with lidar area-based approach*
- @maltamoPredictingTreeDiameter2014 *book chapter from the experts*


## 2023-09-06

I got sidetracked by two ideas:
- variability vs uncertainty
  - @beggUncertaintyVsVariability2014
  - <https://stats.stackexchange.com/q/551271/332632>
- and Gaussian process models vs K-nearest neighbors
  - @fathabadiComparisonBayesianKNearest2022 
    - this paper also used several metrics to quantify uncertainty including:
      - Prediction Interval Coverage Probability indicates less uncertainty
        (greater efficiency?) when closer to the nominal confidence level (e.g.
        95%)
      - Lower Mean Prediction Interval Widths and Average Relative Interval
        Lengths also indicate greater efficiency.

@collinsVariabilityVegetationSurface2016:
"Fine surface fuel loads in our study sites were positively associated with
canopy cover and proportion of shade-tolerant tree species. These are the same
variables that were connected to greater fine fuel loads in a long-fire
suppressed mixed-conifer forest in the central Sierra Nevada (Lydersen et al.,
2015), as well as in an old-growth Jeffrey pine-mixed-conifer forest in Baja
California, Mexico (Fry and Stephens, 2010)"

Used the following structure and composition variables:
- Total BA
- BA proportion by spp.
- total tree density
- tree density by spp.
- density by dbh class
  - 10 - 30.4 cm
  - 30.4 - 61
  - >61
- canopy cover
- shrub cover

Used the following site variables:
- Elevation
- slope (%)
- aspect (Parker (1982))
- Topographic relative moisture index
- Actual Evapotranspiration
- Annual climatic water deficit
- time since fire
- topographic position
  - valley bottom
  - gentle slope
  - steep slope
  - ridge

Used *Conditional Inference Trees*: @hothornCtreeConditionalInference, a recursive
partitioning technique that controls for overfitting and selection bias towards
covariates with many possibilities.


- @mutluMappingSurfaceFuel2008
  - References to look into:
    - Anderson et al. 2005 *Estimating forest canopy fuel parameters using lidar* 
    - Riano et al. 2003 *Modeling airborne laser scanning data...* 

  - Minimum Noise Fraction worked better than Principal Component Analysis when
  - using a combined RGB imagery and lidar predictors for supervised classification
  - of fuel models.

- @chenDevelopmentPredictiveModel2017 
  **fuel model inputs**:
  - Basal area correlated with fuel load (Agee et al 1973)
  - "forest fuel type", canopy density, soil type (Bresnehan 2003)
  - elevation -> temperature -> productivity
  - This study:
    - years since fire
    - litter depth  
    - canopy density (%)
    - elevation  
    - aspect
    - slope
    - forest type (damp/dry)

## 2023-09-05

Today I looked at @banerjeeBayesianModelingLarge2012 and
@babcockGeostatisticalEstimationForest2018 contemplating Bayesian spatial
models. Both of these deal with forest biomass.

## 2023-09-04


@keaneMappingWildlandFuels2001:
- Hardy et al. 2001, fuel models assigned to cover types described fuels based
  on stand structure
- Sandberg 2001, advances in fuel modeling
- fine scale maps essential for prescribed fire planning
- within stand fuel variability can equal landscape variability. They list many
  sources of variability. I wonder if these, known or unknown, could be
  incorporated into a fuel model: primary production, species longevity, a
  mortality model, a wind recruitment model, etc.
- stand history is the most important factor in fuel beds
- brown and Bevins (1986) few differences between cover and site due to stand
  histories
- fuel models do not reflect actual fuel loading as much as parameters that
  result in the expected fire behavior in a given fuel type
- creating a fuel model requires experience in both fire and fuels... Experience.
  This also makes the process subjective. Practitioners may disagree about which
  fuel model is correct. "more of an art than a science."
- fuel layers must be "mapped in parallel so they are congruent," this sounds
  like an argument for larger, more complex Bayesian models.
- Due to expense of data collection, we should try to achieve multiple
  objectives simultaneously. Resulting data should be useful for a variety of
  purposes: ecological, timber, fire, smoke, etc. Also, standardized
  methodologies could make data sets inter operable.
- categorizes strategies into direct, indirect, and biophysical modeling.
- advocates for "triplet approach"
  - biophysical settings
  - species composition
  - vertical stand structure
- vegetation classification considered one of the most important and difficult
  steps
- stands with same composition and structure and site conditions can have very
  different fuel models
- vegetation and stand structure are two most important ecosystem
  characteristics

## 2023-09-02

### Montagnes et al. 2021 - writing backwards

After doing your data analysis, when starting to write a manuscript, they
recommend a writing process as follows:
- take-home message
- results
- methods
- discussion and introduction
- abstract -> title

## 2023-09-01

I start reading @marcozziSensitivityLidarDerived2022 in order to further clarify
the question about [[#terrestiral lidar]] for Pascal. The introduction reminds
me that @skowronskiThreedimensionalCanopyFuel2011 used areal and terrestrial
based lidar to predict plot level Canopy Bulk Density, Canopy Fuel Weight, and
Canopy Bulk Density in 1-meter height bins, based on work by
@duveneckCharacterizingCanopyFuels2007, in which they destructively sampled the
target species and constructed allometric equations for mass of total foliage
and small branches. CBD and CBD_bin are then calculated using an [undocumented
5th order polynomial] for determining the cumulative distribution of mass over
tree crown length. This seems to be a gap in the literature, people always cite
@scottAssessingCrownFire2001, who in turn cite @sandoMethodEvaluatingCrown1972,
who used cones for tree crown shapes for the purpose of determining proportion
of mass in each height bin.

I would like to contact Matthew J. Duveneck to ask him about this.

Anyway, @skowronskiThreedimensionalCanopyFuel2011 found good agreement between
between ground-based lidar metrics and CFW, CBD, and CBD_bin. Of course
allometric equations to predict Canopy Fuel Weight, and weight by height
distributions are required first.

[undocumented 5th order polynomial]: http://www.umass.edu/nebarrensfuels/methods/index.html

## 2023-08-31

These first papers started with questioning that
[[surface fuel correlations]] This lead to several other papers, and
further questions about spatial variability.

- @collinsVariabilityVegetationSurface2016
  *Statistical relationship between surface fuels and forest structure*
- @ottmarUsingFinescaleFuel2012 *I only read the title, but it seems
  interesting*

Along the way, I ran into some fuel mapping papers:

- @mutluMappingSurfaceFuel2008

- @chenDevelopmentPredictiveModel2017 

- including this review: @galeForestFireFuel2021 

### terrestiral lidar

Then, I tried to answer to Pascals question about using
[[terrestrial-lidar-to-estimate-surface-fuel]].

- I briefly looked at @loudermilkLinkingComplexForest2012 they found continuity
  and heterogeneity to be the best predictors of fire behavior on small scales.

- @rowellUsingSimulated3D2016 *Compares lidar derived height model to a height
  model derived from a 3d plant model, the actual lidar height modeling was done
  for another paper for the RxCADRE experiments, listed next*

- @rowellDevelopmentValidationFuel2015 *Lidar fuel heights did not match field
  observed heights, suggests future work will need to translate heights into
  fuel metrics that are useful for fire modeling* 

- @rowellCouplingTerrestrialLaser2020  *lidar porosity, and surface area metrics
  were used to predict biomass because occupied volume was not well correlated
  with measured biomass. Validation depended on voxel sampling.*

- Eric Rowell has a doctoral thesis about this:
  @rowellVirtualizationFuelbedsBuilding2017 

