---
title: 2023 Climate-enhanced multispecies stock assessment for walleye pollock, Pacific cod, and arrowtooth flounder in the eastern Bering Sea
author: 
  - "Kirstin K. Holsman,Jim Ianelli, Kalei Shotwell, Steve Barbeaux," 
  - "Kerim Aydin, Grant Adams, Kelly Kearney"
email: kirstin.holsman@noaa.gov
institute: Alaska Fisheries Science Center, National Marine Fisheries Service, NOAA,
  7600 Sand Point Way N.E., Bld. 4, Seattle, Washington 98115
output:
  word_document:
    fig_caption: yes
    fig_width: 4
    keep_md: yes
  pdf_document:
    fig_caption: yes
    highlight: zenburn
    keep_tex: yes
    latex_engine: xelatex
    toc: true
  header-includes:
  - \usepackage{inputenc}
  - \usepackage{unicode-math}
  - \pagenumbering{gobble}
  html_document:
    df_print: kable
    fig_caption: yes
    theme: flatly
    toc: true
    toc_float: true
    toc_depth: 4
    number_sections: false
    collapsed: false
    smooth_scroll: false
---

![CEATTLE_icon](https://github.com/user-attachments/assets/156c97f2-0f64-4faf-9119-abe03d0b70d4){width=20%}






<!-- # 2023 Climate-enhanced multispecies Stock Assessment for walleye pollock, Pacific cod, and arrowtooth flounder in the South Eastern Bering Sea -->

<!-- ### Kirstin K. Holsman, Jim Ianelli, Kerim Aydin, Grant Adams, Kelly Kearney, Kalei Shotwell -->



![](Holsman_illustration_3spp.png){width="100%}

------------------------------------------------------------------------

*November 2023* | [kirstin.holsman\@noaa.gov](mailto:kirstin.holsman@noaa.gov){.email}
Alaska Fisheries Science Center, National Marine Fisheries Service, NOAA, 7600 Sand Point Way N.E., Seattle, Washington 98115  

**Suggested citation:** _Holsman, K. K., J. Ianelli,  K. Shotwell, S. Barbeaux, K. Aydin, G. Adams, K. Kearney, K. Shotwell (2023) Climate-enhanced multispecies stock assessment for walleye pollock, Pacific cod, and arrowtooth flounder in the eastern Bering Sea. In:  Ianelli, J. et al. 2023. Assessment of the eastern Bering Sea walleye pollock. North Pacific Fishery Management Council, Anchorage, AK._

\pagebreak

## 2023 BRP summary table (for reference only; not used for offical harvest recommendation)

*As estimated or recommended this year (2023) for:*

| Quantity|Walleye pollock || Pacific cod || Arrowtooth flounder ||
|:--------------------------------------------|-----------:|------------:|-----------:|-----------:|----------:|----------:|
||SSM | MSM |SSM | MSM |SSM | MSM |
|||  | |  ||  |
| 2023 M (age 1)|1.313| 1.195| 0.653| 0.594| 0.658| 0.468 |||||  |
| 2023 Average 3+ M|0.306| 0.306| 0.38| 0.38| 0.227| 0.227 || |  ||  |
| Projected (age 3+) $B_{2024}$ (t) |15,860,694| 16,265,727| 679,301| 686,562| 566,160| 569,909 |  ||  ||  |
| $SSB_{2023}$ (t)|6,790,160| 7,044,480| 157,340| 155,597| 429,700| 428,256 |  | |  ||  |
| \% change in $SSB$ (t) from 2022  | 10.3| 10.3| -9.2| -9.0| 0.1| 0.2 |  | |  ||  |
| Projected $SSB_{2024}$ (t)  |  6,239,390| 6,475,040| 156,408| 155,652| 374,227| 373,806 |  | |  ||  |
| Projected $SSB_{2025}$ (t)  |  5,828,060| 5,819,550| 128,478| 123,214| 351,317| 348,509 |  | |  ||  |
| \*Projected $SSB_{0,2100}$ (t)|                                                                    6,164,698| 6,504,694| 322,907| 372,244| 368,306| 426,212 |     |             |     |                     |     |
| \*Projected $SSB_{target,2100}$ (t)       |3,044,850| 3,136,376| 164,934| 169,131| 147,286| 170,536 |     |             |     |                     |     |
| \*\*Target 2100 $B/B_0$                   |                                                                                  0.494| 0.482| 0.511| 0.454| 0.4| 0.4 |     |             |     |                     |     |
| $F_{target,2100}$                         |  0.345| 0.547| 0.443| 0.481| 0.08| 0.086 |     |             |     |                     |     |
| $F_{ABC,2024}$                    |  0.134| 0.192| 0.498| 0.566| 0.033| 0.042 |     |             |     |                     |     |
| $\mathrm{ABC_{2024}}$             |        2,054,020| 2,965,510| 188,498| 205,756| 17,411| 21,741 |     |             |     |                     |     |
| $\mathrm{ABC_{2025}}$             |    1,853,370| 2,521,900| 156,934| 165,274| 16,533| 20,573 |     |             |     |                     |     |

> *\** $SSB_{0,2100}$ and $SSB_{target,2100}$ are based on climate-naive model projections of SSB at 2100 (equilibrium) given $F=0$ and $F=F_{target}$, respectively.\
> *\*\* Target ratios in 2100 are based on* $B/B_0$=0.4, given that $B/B_0 >$ 0.35 for all future yr.\
> *\*\*\* Projected* $SSB_{2024}$ (t) refers to SSB at the start of 2024 and Projected $SSB_{2025}$ (t) refers to SSB at the start of 2025 using $F_{ABC,2024}$ for 2024

<!-- #### As estimated or recommended in 2022 for:   -->

**2022 Values**


![](Figs/Table2022vals.jpg){width=100%}

\pagebreak

## Overview

This is a three species stock assessment for walleye pollock (*Gadus chalcogrammus*), Pacific cod (*Gadus macrocephalus*) and arrowtooth flounder (*Atheresthes stomias*), from the Eastern Bering Sea (EBS), Alaska updated from Holsman et al.(2016) and incorporating climate informed reference points as detailed in Holsman et al. (2020a,b). Results are presented from models estimated and projected without trophic interactions (single-species mode, SSM) and with trophic interactions (multispecies mode, MSM). The main features and settings for this multispecies model include:

-   Survey biomass and harvest indices from the NEBS and SEBS are included.
-   Predation natural mortality is age specific and annually varying (M2). Residual (non-predation) natural mortality (M1) is age specific but not-annually varying and differs slightly from current assessments for each species (see Table 3 below).
-   The multispecies mode uses the the residual mortality vectors of current single species assessment models for pollock, Pacific cod, and arrowtooth flounder except for the ages 1 and 2 mortality rates for pollock, which were adjusted downward to 0.01 and 0.30, respectively.
-   Pollock fishery composition data is age-based where as Pacific cod and arrowtooth fishery composition data is based on lengths.
-   Predator overlap index is set to 1 for all species (i.e., all prey are available to all predators).
-   Weights at age for pollock are based on values from the 2023 SAFE report; for Pacific cod and arrowtooth, they are calculated outside of the model using a temperature-dependent von Bertalanffy to fill in for missing years between 1979-2012 and assume 2012 weight at ages for 2013-2023. For projections, all three species use temperature-specific weights at age using the temperature-dependent von Bertalanffy for 1979-2012.
-   Acoustic trawl survey selectivity was set equal to the SAFE report model estimates. Fisheries selectivity and survey selectivity are age specific and constant over time.
-   Predator-prey suitability is age-specific and constant over time.
-   Arrowtooth flounder stock is treated as sexes combined (weight at age and proportion mature is calculated separately for males and females and combined using a mortality-based mean).
-   Maturity schedules are based on 2012 assessments and differ slightly from SAFE assessments.
-   Projections to derive ABC include a sequential method for determining universal climate-naive B0, and include the constraint that $\mathrm{SSB}_F>0.35\mathrm{SSB}_0$ for all years in the projection.
-   A moderately "climate-informed" approach is used to derive biological reference points through projecting the model forward with climate effects on weight at age and predation mortality but with a climate-naive Ricker stock recruitment curve (i.e., without environmental covariates).
-   Evaluation of $F_{ABC}$ for 2024 and 2025 is performed using an ensemble of warming scenarios.

### Key 2023 updates

-   Survey biomass and harvest from the NEBS and SEBS were updated through 2023.
-   Survey biomass and age or length composition were updated using data available from the NMFS bottom trawl survey and fishery observer database 2023.
-   Bottom temperature and other variables from the 2023 BERING10K operational hindcast using the 30 layer model (K20) was updated through 2023.
-   Residual natural mortality ($\mathrm{M1}_{ij}$) for ages 1-4 of all three species was set to the mean of the total natural mortality from the multispecies assessment for years 1979 - 2023. Natural mortality for older Pacific cod was set to 0.38 for to match the Pacific cod stock assessment (Thompson et al. 2018).
-   The newest set of ACLIM high resolution climate projections for the Bering sea (Hermann et al, 2021, Cheng et al. 2021) were used to drive projections and derive $F_{target}$ and ABC under climate effects. This includes a broader suite of scenarios from High carbon mitigation (ssp126) to low carbon mitigation scenarios (ssp585).
-   Climate effects on recruitment were included in the projection model suite for 2023, sensu Holsman et al. 2020.
-   More detailed information was included for temperature specific weight at age and climate-driven recruitment.
-   Estimated of annual biomass of key prey species eaten (NEBS and SEBS combined) were produced for species outside of the assessment (e.g., _"Opilio"_) based on model estimates of ration (kg eaten/pred) and abundance.

------------------------------------------------------------------------

### Response to Council, SSC or Plan Team comments

* Plan Team recommendations from Sept. 2023 included suggestions for more detail around climate-informed projections, indices, and recruitment. That section has been greatly expanded to include additional information (full methods to be publicaly available when the corresponding manucript is published)

* In  Sept. 2023, the Plan Team also recommended development of indices of predation effects on other species not included in the assessment (e.g. for use in ESPs or ESRs). Towards this we calculated the biomass of key prey species annually consumed by predators in the model. An Rdata file version of this output is also available and will be provided via the gitHub project page for the 2023 assessment available on the author's github page ([github.com/kholsman](https://github.com/kholsman)).

------------------------------------------------------------------------

<!-- \pagebreak -->

## Introduction

### Multispecies modeling

Multispecies statistical catch-at-age models (MSCAA) are an example of a class of Models with Intermediate Complexity for Ecosystem assessments (i.e., MICE; Plaganyi et al., 2014), which have particular utility in addressing both strategic and tactical EBFM questions (Hollowed et al. 2013; Fogarty 2014; Link and Browman 2014; Plaganyi et al., 2014, Holsman et al. 2020, Adams et al. 2022). MSCAA models may increase forecast accuracy, may be used to evaluate propagating effects of observation and process error on biomass estimates (e.g., Curti 2013; Ianelli et al., 2016), and can quantify climate and trophic interactions on species productivity. As such MSCAA models can address long recognized limitations of prevailing single species management, notably non-stationarity in mortality and biological reference points, and may help reduce risk of over-harvest, especially under climate change (Link 2010; Plaganyi et al., 2014; Fogarty 2014). Multispecies biological reference points (MBRPs) from MSCAA model are conditioned on the abundance of other species in the model (Collie and Gislason 2001; Plaganyi et al., 2014; Fogarty 2014), thus they may also have utility in setting harvest limits for multispecies fleets, evaluating population dynamics in marine reserves or non-fishing areas, and quantifying trade-offs that emerge among fisheries that impact multiple species in a food web (see reviews in Pikitch et al., 2004; Link 2010; Levin et al., 2013; Link and Browman 2014; Fogarty 2014).

Depending on their structure, MSCAA models can be used to evaluate climate- and fisheries-driven changes to trophodynamic processes, recruitment, and species abundance (Plaganyi et al., 2014). MSCAA models differ somewhat among systems and species, but most use abundance and diet data to estimate fishing mortality, recruitment, stock size, and predation mortality simultaneously for multiple species in a statistical framework. Similar to age structured single species stock assessment models widely used to set harvest limits, MSCAA models are based on a population dynamics model, the parameters of which are estimated using survey and fishery data and maximum likelihood methods (e.g., Jurado-Molina et al., 2005; Kinzey and Punt, 2009; Van Kirk et al., 2010; Kempf 2010; Curti et al., 2013; Tsehaye et al., 2014). Unlike most single-species models (but see Hollowed et al. 2000b; Spencer et al. 2016), MSCAA models additionally separate natural mortality into residual and annually varying predation mortality, and model the latter as a series of predator-prey functional responses. Thus, natural mortality rates for each species in MSCAA models depend on the abundance of predators in a given year and vary annually with changes in recruitment and harvest of each species in the model.

MSCAA models have specific utility in quantifying direct and indirect effects of fisheries harvest on species abundance and size distributions (see reviews in Hollowed et al., 2000a, 2013; Link 2010; Fogarty 2014; Link and Browman 2014; Plaganyi et al., 2014), which is important for EBFM and trade-off analyses of various management strategies under climate change. Rapidly shifting climate conditions are also of growing concern in fisheries management as changes in physical processes are known to influence individual growth, survival, and reproductive success of fish and shellfish (Hanson et al., 1997; Kitchell et al., 1977; Morita et al., 2010; Hollowed et al., 2013, Cheung et al., 2015, Holsman et al. 2020). Climate-driven changes in water temperature can directly impact metabolic costs, prey consumption, and somatic or gonadal tissue growth, with attendant indirect effects on survival, production, and sustainable harvest rates (e.g., Hanson et al., 1997; Morita et al., 2010, Cheung et al., 2015, Holsman and Aydin, 2015). Temperature-dependent predation, foraging, metabolic, and growth rates are common in more complex spatially-explicit food web or whole of ecosystem models such as GADGET (e.g., Howell and Bogstad 2010; Taylor et al., 2007), Atlantis (e.g., Fulton et al., 2011; Kaplan et al., 2012; 2013), and FEAST (Ortiz et al., 2016). Temperature functions for growth and predation can also be incorporated into MSCAA models, allowing this class of models to be used to evaluate interacting climate, trophodynamic, and fishery influences on recommended fishing mortality rates.

Numerous studies point to the importance of using multispecies models for EBFM (see review in Link 2010). Multispecies production models produced different estimates of abundances and harvest rates than single species models for Northeast US marine ecosystems (Gamble and Link, 2009; Tyrrell et al., 2011), and MSY of commercial groundfish stocks estimated from aggregated production models were different than the sum of MSY estimates from single-species assessments (Mueter and Megrey, 2006; Gaichas et al., 2012; Smith et al., 2015). Multispecies models have been used to demonstrate long-term increases in yield of Icelandic stocks of Atlantic cod (*Gadus morhua*) and reductions in capelin (*Mallotus villosus*) and Northern shrimp (*Pandalus borealis*) catch associated with short-term decreases in cod harvest (Danielsson et al., 1997). Kaplan et al. (2013) demonstrated the disproportionately large ecosystem impacts of applying the same Fx% harvest control rule approach to forage fish as is used for groundfish in the northeast Pacific, and separately, accounting for trophodynamics in a southern Benguela ecosystem resulted in higher carrying capacity for small pelagic species under fishing (versus no-fishing) scenarios (Smith et al., 2015).

Since natural mortality and recruitment rates in a MSCAA model are conditioned on harvest rates of predators in the model, an ongoing area of research is evaluating MSCAA model analogs to single-species biological reference points (see Moffitt et al., 2016), such as harvest rates that correspond to maximum yield (FMSY) or proxies thereof (e.g., Fx%). Other multispecies models have been used to derive and evaluate MBRPs, although these have largely focused on MSY (e.g., Kaplan et al., 2013; Smith et al., 2015). A notable exception is Collie and Gislason (2001), who evaluated a variety of MBRPs using a multispecies, virtual population analysis and found MBRPs to be sensitive to variation in natural mortality (much less so to variability in growth), and as such proposed that fishing mortality reference levels for prey species with high mortality be conditioned on the level of predation mortality. Building on this approach, Moffitt et al. (2016) demonstrated a projection approach for using multispecies models to derive a variety of MBRPs for EBFM. This provides a basis for the application of MSCAA models for increased use in tactical and strategic EBFM decision-making across a diversity of management frameworks worldwide.

### CEATTLE model

Here we present results from a three species climate-enhanced MSCAA model for the Bering Sea (hereafter CEATTLE, for Climate-Enhanced, Age-based model with Temperature-specific Trophic Linkages and Energetics) that includes temperature-dependent von Bertalanffy weight-at-age functions (VBGF; von Bertalanffy, 1938) and temperature-specific, bioenergetics-based predation interactions. The eastern Bering Sea (Alaska), is defined by large, climate-driven changes to trophodynamics and species productivity that can vary on annual and multi-annual timescales (see reviews in Aydin and Mueter 2007; Hunt et al., 2011; Stabeno et al., 2012; Baker et al., 2014). Accordingly, fisheries management in Alaska has a long history of using ecosystem information and multispecies models for strategic management advice (e.g., multispecies model-based indices, such as mean trophic level, are regularly reported in the annual Ecosystem Considerations chapter of Alaska Stock Assessment and Fishery Evaluation (SAFE) reports; see review in Livingston et al., 2011). Development of multiple MSCAA models in the region (Jurado-Molina et al., 2005; Kinzey and Punt , 2009; Van Kirk, 2010, Holsman et al. 2016, Adams et al. 2022) has advanced regional EBFM, facilitating use of estimates from MSCAA models in tactical single-species management advice. For instance, the CEATTLE multispecies model has been included as an appendix to the BSAI pollock assessment (Ianelli et al. 2019) since 2016. Similarly, Dorn et al. (2014) recently evaluated predation mortality estimates from a regional MSCAA model developed by Van Kirk (2010) to inform natural mortality for the Gulf of Alaska walleye pollock (*Gadus chalcogrammus*, hereafter pollock) stock assessment.

Climate-enhanced MSCAA like CEATTLE, have considerable utility in accounting for climate effects on harvest and are useful for species that exhibit strong trophic interactions (predator and prey species) or contrasting management or biological constraints that require simultaneous evaluation (Link 2010). In the eastern Bering Sea, pollock are both predators (adults) and prey for a variety of species including cannibalistic conspecifics (e.g., Boldt et al., 2012; Dunn and Matarese, 1987; Nishiyama et al., 1986). Variable climate conditions, particularly the spatial extent of winter sea ice, the timing of sea ice spring melt, and subsequent summer bottom temperatures, can deferentially promote survival of pollock and their predators and/or modulate predator and prey overlap in the region (e.g., Baily 1989; Zador et al., 2011; Boldt et. al 2012; Hunsicker et al. 2013; Baker and Hollowed 2014). Diet analyses suggest Pacific cod (*Gadus macrocephalus*), cannibalistic conspecifics, and arrowtooth flounder (*Atheresthes stomias*), among others, are important predators of pollock populations in the eastern Bering Sea (Livingston 1993; Aydin and Mueter 2007; Mueter et al., 2007). Climate driven changes to food webs, thermal experience, distribution, and growth collectively impact natural mortality for juveniles, especially juvenile pollock. Accounting for these multispecies interactions is increasingly important under climate-driven change (Holsman et al. 2019,2020, Karp et al. 2019).

## Methods

### Multispecies population dynamics

The CEATTLE model, is an environmentally-enhanced stock assessment model (sensu Link 2010), where temperature-specific algorithms predict size-at-age and predation mortality. CEATTLE is programmed in AD model builder (Fournier et al., 2012), and builds on earlier models that combine catch-at-age assessment models with multispecies virtual population analysis (MSVPA) in a statistical framework (i.e., Jurado-Molina et al., 2005). Abundance and biomass of each cohort is modeled using standard population dynamics equations, accounting for a plus age group (Table 1, Eqs. 1, 2). The initial age-structure is assumed to correspond to unfished equilibrium, and the numbers of each species $i$ at age $j$ in year 1 ($N_{0,ij}$) are treated as estimable parameters (Eq.1 ), such that:

Eq. 1 $$N_{ij,1} = \begin{array}{lrl}
R_{0,i}e^{\left(-j~\mathrm{M1}_{ij}\right)}N_{0,ij} & y=1 &~~~~~~~~ 1< j<A_i \\ 
R_{0,i}e^{\left(-j~\mathrm{M1}_{i,A_i}\right)}N_{0,i,A_i}/ \left(1-e^{\left(-\mathrm{M1}_{i,A_i}\right)}\right) & y=1 &~~~~~~~~ j\geq A_i 
\end{array}$$

The number of each species $i$, age $a$ each year $y$ is then:

Eq. 2 $$\begin{array}{lrl}
N_{i,j+1,y+1}=N_{i,j,y}e^{-Z_{ij,y}}  &~~~~~~~~  1\leq y \leq n_y &~~~~~~~~ 1\leq j< A_i-1 \\  
N_{i,A_i,y+1}=N_{i,A_i-1,y}e^{-Z_{i,A_i-1,y}}+N_{i,A_i,y}e^{-Z_{i,A_i,y}}  &~~~~~~~~ 1\leq y \leq n_y &~~~~~~~~ j\geq A_i 
\end{array}$$

Total mortality of each prey species $i$, age $j$ (or predator species $p$ age $a$) in each year $y$ is the sum of mortality due to predators in the model ($\mathrm{M2}_{ij,y}$), fishing mortality ($\mathrm{F}_{ij,y}$), and residual mortality ($\mathrm{M1}_{ij}$), Eq. T1.6). Predation mortality (Eq. T2.1) is based on the assumption that the annual age-specific ration of a predator is allocated to prey species of a given age according to predator selectivity (Table 2, Eq. T.2.2). Predator selectivity is based on the suitability function derived by Jurado-Molina et al. (2005) and fit to available data from 1981-2015, while annual ration is a function of temperature-specific allometric relationships between ration and fish weight based on bioenergetics models for each species (Eqs. T2.4 and T2.5; see Holsman et al. 2016, and Holsman and Aydin, 2015 for more detail).

The length-to-weight relationships, predator size and species diet preference, bioenergetics-based, temperature-specific predator rations, and maturity were based on previous studies (Tables 1 and 2; Table 5; Holsman et al. Holsman and Aydin, 2015, Holsman et al. 2016). Size-specific diet compositions for each species were assumed known based on diet data collected during the AFSC bottom trawl survey (i.e., diet data were not included in the objective function) and trophic patterns in survey and fishery-based diet data were used to calculate mean (across years and stations) predator-prey suitability (Eq. T2.2).

<!-- # ```{r fig1,fig.width=5,fig.height=4,echo=FALSE,message=FALSE,include=TRUE,fig.cap="Mean summer bottom temperature for the Eastern Bering Sea ($^o$C); blue and red represent temperatures below or above (respectively) 1 standard deviation of the 1979-2015 mean; dashed lines represent 95% confidence intervals; $*$ represent survey replicated temperature estimates from the Bering 10K regional ocean model."} -->

<!-- # ``` -->

![Mean summer bottom temperature (BT; $^o$C) for the eastern Bering Sea as observed on NMFS summer ground fish survey in the EBS (triangles). Circles represent the opertational Bering10K 30 layer high resolution oceanpgraphic model hindcast for bottom tempteratures used in this assessment (Kearney et al. 2020).](/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2023_Multispp_assessment/docs/MSM_Assessment/Figs/BT_plot.png){width="80%"}

### Temperature specific weight at age

Water temperature is known to directly impact growth through influencing metabolic and digestion rates, which often scale exponentially with body weight and temperature (see Hanson et al., 1997 for an overview). Thus we modified the generalized formulation of the von Bertalanffy growth function (VBGF; von Bertalanffy 1938; Pauly 1981; Temming 1994) to predict temperature-dependent growth by allowing the allometric scaling parameter $d$ to increase with temperature. Essington et al. (2010) and Holsman and Aydin (2015), and Holsman et al. (2016) describe the derivation and application of the VBGF towards bioenergetics modeling in great detail, so we do not repeat it here. Essentially, in this formulation $d$ represents the realized allometric slope of consumption, which integrates both the direct effect of temperature on consumption and indirect ecological interactions that scale with temperature and influence relative foraging rates (see Essington et al., 2010; Holsman and Aydin, 2015). We fit the VBGF to otolith-based length- and weight-at-age data ($n$ = 21,388, 14,362, and 772, for pollock, Pacific cod, and arrowtooth flounder, respectively) collected during AFSC Bering Sea surveys and analyzed at the AFSC such that:

Eq. 2 $~~~~~~W_{ij,y}=W_{\infty,iy} (1-e^{(-K_i (1-d_{i,y} )(j-t_{0,i} )) } )^{1/(1-d_{i,y} )} e^\varepsilon$, where $\varepsilon~N(0,\sigma_{d,i}^2 )$

where $t_{0,i}$ is the age at which $W_{ij,y} = 0$, $W_{\infty,iy}$ is the asymptotic mass which can vary by species $i$ and year $y$ (i.e., $W_{\infty,iy}=(H_i/K_i)^{1(1-d_{i,y}) }$), $H_i$ is the assimilation constant $K_i$ is the energy loss constant (Essington et al., 2010), and $\varepsilon$ is a normally and independently distributed random variable with mean 0 and variance $\sigma_{d, i}^2$. Essington et al. (2010) and Holsman and Aydin, (2015) statistically estimated the $d$, $K$ and $H$ parameters for various species to estimate consumption rates. In particular, Holsman and Aydin (2015) found that the $d$ parameter varied between species and regions in Alaska (USA). We further modified this approach to estimate $d$ annually for each year $y$ in the dataset, as a linear function of temperature $T_y$ such that:

Eq. 3 $~~~~~~d_{i,y}=e^{(\alpha_{d,i,y}+\alpha0_{d,i}+\beta_{d,i}T_y) }$

where $\alpha0_{d,i}$ and $\alpha_{d,i,y}$ represent the mean $d$ intercept and $\beta_{d,i}$ is the coefficient for the residual effect of temperature on the $d$ consumption parameter (see [https://github.com/kholsman](https://github.com/kholsman) for code and examples (available after publication of the revised analysis in 2024). We chose this formulation based on the empirical relationship between temperature and consumption, assuming that $d$ would capture the differential effects of temperature on growth, and that waste rates scale proportionally with weight but do not vary over time with diet or temperature (i.e. $K$ is constant but $d$ can vary with temperature). This formulation allows both the slope and asymptotic limit of growth to vary with temperature. Similar approaches, with slightly different modifications to the VBGF, including temperature and prey specific terms for $d$ and $K$, respectively, have been used elsewhere to evaluate climate impacts on fish growth (e.g., Cheung et al., 2015; Hamre, 2003).

![Example fit of the VonB with Temperature model (here for EBS pollock where bottom temperature (Temp) and Temp^2 are predictor variables.](Figs/vonB_plk_model_Temp.jpg){width="60%"}

**Table 1. Population dynamics equations for species** $i$ and age $j$ in each simulation year $y$. BT indicates the AFSC bottom trawl survey and EIT represents the echo-integrated acoustic-trawl survey. For all other parameter definitions see Table 3.

                                                                              
| Definition                       |        Equation|||
|:------------------|:--------------------------------------------|---------------:|-----:|
| Recruitment| $N_{i,1,y}=R_{i,y}=R_{0,i}e^{\tau_{i,y}}$|$\tau_{i,y}$ \~ $N(0,\sigma^2)$ |  T1.1 |
| Catch (numbers)| $C_{ij,y}=\frac{F_{ij,y}}{Z_{ij,y}}\left(1-e^{-Z_{ij,y}}\right)N_{ij,y}$||  T1.2 |
| Total yield (kg)                         | $Y_{i,y}=\sum_j^{A_i}\left(\frac{F_{ij,y}}{Z_{ij,y}}\left(1-e^{-Z_{ij,y}}\right)N_{ij,y}W_{ij,y}\right)$||  T1.3 |
| Biomass at age (kg)| $B_{ij,y}=N_{ij,y}W_{ij,y}$||  T1.4 |
| Spawning biomass at age (kg)| $SSB_{ij,y}=B_{ij,y}\rho_{ij}$||  T1.5 |
| Total mortality at age| $Z_{ij,y}=\mathrm{M1}_{ij}+\mathrm{M2}_{ij}+F_{ij}$||  T1.6 |
| Total mortality at age| $F_{ij,y}=F_{0,i}e^{\epsilon_{i,y}S^f_{ij} }$| $\epsilon_{i,y}\sim N\left(0,\sigma^2_{\mathrm{F},i}\right)$ |  T1.7 |
| Weight at age (kg)| $W_{ij,y}=W_{\infty,iy}\left( 1-e^{\left(-K_i\left(1-d_{i,y}\right)\left(j-t_{0,i} \right)\right)}\right)^{\frac{1}{1-d_i,y}}$|| T1.8a |
|| $d_{i,y}=e^{\left(\alpha_{d,i,y}+\alpha_{0,d,i}+\beta_{d,i}T_y\right)}$|| T1.8b |
|| $W_{\infty,iy}=\left(\frac{H_i}{K_i}\right)^{{1}/\left(1-d_{i,y}\right)}$|| T1.8c |
| Bottom trawl survey biomass (kg)| $\hat{\beta}^s_{i,y}=\sum_j^{A_i}\left(N_{ij,y}e^{-0.5Z_{ij,y}}W_{ij,y}S_{ij}^\mathrm{S}\right)$||  T1.9 |
| Acoustic survey biomass (kg)| $\hat{\beta}^{eit}_{y}=\sum_j^{A_i}\left(N_{1j,y}e^{-0.5Z_{1j,y}}W_{1j,y}S_{1j}^{eit}q^{eit}_{1,j}\right)$|(pollock only) | T1.10 |
| Fishery age composition| $\hat{O}^f_{ij,y}=\frac{C_{ij,y}}{\sum_jC_{ij,y}}$|| T1.11 |
| Bottom trawl age composition| $\hat{O}^s_{ij,y}=\frac{N_{ij,y}e^{0.5\left(-Z_{ij,y}\right)S_{ij}^\mathrm{S}}}{\sum_j{\left(N_{ij,y}e^{0.5\left(-Z_{ij,y}\right)S_{ij}^\mathrm{S}}\right)}}$ || T1.12 |
| Acoustic trawl age composition           | $\hat{O}^{eit}_{1j,y}=\frac{N_{1j,y}e^{-0.5Z_{1j,y}}S_{1j}^{eit}q^{eit}_{1,j}}{\sum_j{\left(N_{1j,y}e^{-0.5Z_{1j,y}}S_{1j}^{eit}q^{eit}_{1,j}\right)}}$       |        (pollock only) | T1.13 |
| Bottom trawl selectivity| $S_{ij}^\mathrm{s}=\frac{1}{1+e^{\left(-b_i^\mathrm{S} \cdotp j-a_i^\mathrm{S}\right)}}$|          | T1.14 |
| Fishery selectivity| $S_{ij}^f=e^{\eta_{ij}}~~~~~~~ j\leq A_{\eta,i}$|  $\eta_{ij}\sim N\left(0,\sigma^2_{f,i}\right)$ | T1.15 |
|| $S_{ij}^f=e^{\eta_{ij}A_{\eta,i}}~~~~~~~ j> A_{\eta,i}$||       |
| Proportion female| $\omega_{ij}=\frac{e^{-jM_{\mathrm{fem}}}}{e^{-jM_{\mathrm{fem}}}+e^{-jM_{\mathrm{male}}}}$|| T1.16 |
| Proportion of mature females| $\rho_{ij}=\omega_{ij}\phi_{ij}$|| T1.17 |
| Adjusted weight at age (kg) | $W_{ij,y}=W_{ij,y}^{\mathrm{fem}}\omega_{ij}+(1-\omega_{ij})W_{ij,y}^{\mathrm{male}}$|| T1.18 |
| Adjusted residual natural mortality (kg) | $\mathrm{M1}_{ij}=\mathrm{M1}_{ij}^{\mathrm{fem}}\omega_{ij}+(1-\omega_{ij})\mathrm{M1}_{ij,y}^{\mathrm{male}}$|| T1.19 |  

We used this approach to derive annual temperature-specific coefficients of $d$ for pollock and Pacific cod (combined sexes) and separately for male and female arrowtooth flounder (Table 3; Table 6). For arrowtooth flounder, we then used the age-specific proportions of mature females ($\rho_{ij}$) and males ($1-\rho_{ij}$) to derive the mean weight-at-age for both sexes combined (Eq. T1.18 and Table 5). Lastly, male and female natural mortality rates ($M_\mathrm{male}$ and $M_\mathrm{fem}$ , respectively) and age-specific maturity proportions ($\phi_{ij}$) from the 2012 stock assessments for eastern Bering Sea pollock (Ianelli et al., 2012), and Bering Sea and Aleutian Islands Pacific cod (Thompson and Lauth, 2012) and arrowtooth flounder (Spies et al., 2012), were used to derive estimates of the proportion of mature females at age ($\rho_{ij}$; Eq. T1.17).

<!-- |Predation mortality| $\mathrm{M2}_{ij,y}=\sum_{pa}\left( \frac{N_{pa,y}\delta_{pa,y}O_{pi,y}\bar{S}_{paij}}{\left( \sum_{ij}{O_{pi,y}\bar{S}_{paij}B_{ij,y}} \right)+B_p^{other}\left(1-\sum_{ij}{\left( O_{pi,y}\bar{S}_{paij}\right)} \right)} \right)$|T2.1| -->

**Table 2. Predation mortality (**$M2$) equations for predators $p$ of age $a$, and prey $i$ of age $j$.

| Definition          |        Equation||
|:-----------------------|:-----------------------------------------------------|:---------|
| Predation mortality          | $\mathrm{M2}_{ij,y}=\sum_{pa}\left( \frac{N_{pa,y}\delta_{pa,y}\bar{S}_{paij}}{\left( \sum_{ij}{\bar{S}_{paij}B_{ij,y}} \right)+B_p^{other}\left(1-\sum_{ij}{\left( \bar{S}_{paij}\right)} \right)} \right)$ | T2.1  |
| Predator-prey suitability| $\bar{S}_{paij}=\frac{1}{n_y}\sum_{y}\left(\frac{\frac{\bar{U}_{paij}}{B_{ij,y}}}{\sum_{ij}{\left(\frac{\bar{U}_{paij}}{B_{ij,y}}\right)+\frac{1-\sum_{ij}{\bar{U}_{paij}}}{B_p^{other}}}}\right)$           | T2.2  |
| Mean gravimetric diet proportion          | $\bar{U}_{paij}=\sum_{y}\frac{U_{paijy}}{n_y}$| T2.3  |
| Individual specific ration (kg yr$^{-1}$) | $\delta_{pa,y}=\hat{\varphi}_{pa,y}\alpha_{\delta}W_{pa,y}^{\left(1+\beta_{\delta}\right)}f\left(T_y\right)_p$| T2.3  |
| Temperature scaling consumption algorithm | $f\left( T_y \right)_p=V^Xe^{\left( X \left( 1-V \right)\right)}$| T2.5  |
|| $V=\left(T_p^{cm}-T_y\right)/\left(T_p^{cm}-T_p^{co}\right)$| T2.5a |
|| $X=\left(Z^2\left(1+\left(1+40/Y\right)^{0.5}\right)^2\right)/400$| T2.5b |
|| $Z=ln\left(Q_p^c\right)\left(T_p^{cm}-T_p^{co}\right)$| T2.5c |
|| $Y=ln\left(Q_p^c\right)\left(T_p^{cm}-T_p^{co}+2\right)$| T2.5d |

<!-- $\left( \sum_{i=1}^{n}{i} \right)$ -->

<!-- ![](Figs/Table2.png){ width=100% } -->

### Parameter estimation & data

The parameters of the model are either pre-specified or estimated by selecting parameters that minimize the log-likelihood function (Table 3) and include fishing mortality rates ($F_{ij,y}$), fishery and survey selectivity ($s_{ij}^\mathrm{f}$ and $s_{ij}^\mathrm{s}$ , respectively), initial (pre-harvest) abundance in year 1979 ($N_{0,ij}$), and annual recruitment ($R_{i,y}$), while the estimable parameter of the likelihood function is the catchability coefficient for the acoustic survey ($q_1^\mathrm{eit}$; Table 3; Table 4). We used summer bottom temperature indices from the Bering10K ROMSNPZ model (Fig. 1) to drive weight at age and bioenergetics sub-models. We fit the model to available survey and fishery data for the eastern Bering Sea including biomass estimates and age-composition data from the annual AFSC summer bottom trawl survey for the NEBS and SEBS sub-regions combined (Table 5), biomass and age-composition data from the AFSC Acoustic-trawl (AT) survey (pollock only), and the total fishery catch and fishery age-composition data collected by AFSC observers and analyzed at AFSC (NEBS and SEBS) (Hilborn and Walters, 1992; Quinn and Deriso, 1999). Penalties were imposed on the changes over age in fishery selectivity. Likelihood priors were applied to normalize the log of annual recruitment and the fisheries mortality deviations, as well as initial abundances (Table 5). Selectivity for the AT survey was set to previously reported values (Table 3; Honkalehto et al., 2011; Ianelli et al., 2012).

**Table 3. Parameter definition ($n$) is the number of parameters for estimated parameters only, value, data source, and type: (I) Input parameter (assigned); (M) model index; (E) Estimated parameter; (F) fixed parameter; (P) Derived quantity;(D) Data.**

|Parameter|Definition|Type|Value|Source|
|:---------|:---------------------------------------------|:---------|-----------------------:|:----|
|$y$ |Year |M|$[1,2,3\ldots n_y]$ |e|
|$p$ |Predator |M|$[1,2,3\ldots n_p]$ |e|
|$a$ |Predator age (years) |M|$[1,2,3\ldots A_p]$ |e|
|$i$ |Prey |M|$[1,2,3\ldots n_i ]$ |e|
|$j$ |Prey age (years) |M|$[1,2,3\ldots A_i]$ |e|
|$n_i$ |Number of prey species |I|3 |e|
|$n_p$ |Number of predator species |I|3 |e|
|$R_{0,i}$ |Mean Recruitment; $n=[1,1,1]$ |E| $\geq0$ |e|
|$\tau_{i,y}$ |Annual recruitment deviation; $n=[34,34,34]$ |E |number |e|
|$N_{0,ij}$ |Initial abundance; $n=[11,11,20]$ |E |$\geq0$ |e|
|$F_{0,i}$ |Mean fishing mortality; $n=[1,1,1]$ |E |$\geq0$ |e|
|$varepsilon_{i,y}$ |Annual fishing mort. deviation; $n=[34,11,20]$ |E |number |e|
|$\eta_{ij}$ |Fishery age selectivity coef. ; $n=[8,8,8]$ |E |number |e|
|$b_i^\mathrm{s}$ |Survey age selectivity slope; $n=[1,1,1]$ |E |number |e|
|$a_i^\mathrm{s}$ |Survey age selectivity limit ; $n=[1,1,1]$ |E |number |e|
|$d_{i,y}$ |VBGF allometric slope of consumption |P |$\geq0$ |e|
|$W_{\inf,iy}$ |VBGF max asymptotic weight (kg) |P |$>0$ |e|
|$\rho_{ij}$ |Proportion of mature females at age |P |$\in[0,1]$ |e|
|$\mathrm{M1}_{ij}$ |Residual natural mortality |F |$\geq0$ |e, h|
|$n_y$ |Number of estimation years |I | 45|e|
|$y_{0}$ |Start year |I |1979 |e|
|$\omega_{ij}$ |Female proportion of population |F |$\in[0,1]$ |c|
|$\phi_{ij}$ |Age-specific maturity proportions |F |$\in[0,1]$ |c|
|$C_{i,y}^*$ |Observed total yield (kg) |D |$\geq0$ |f|
|$O_{ij,y}^\mathrm{f}$ |Observed fishery age comp. |D |$\in[0,1]$ |f|
|$O_{ij,y}^\mathrm{s}$ |Observed BT age comp. |D |$\in[0,1]$ |b|
|$O_{1j,y}^\mathrm{eit}$ |Observed AT age comp. |D |$\in[0,1]$ |g|
|$\beta_{i,y}^\mathrm{s}$ |Observed BT survey biomass (kg) |D |number |b|
|$\beta_y^\mathrm{eit}$ |Observed AT survey biomass (kg) |D |number |g|
|$T_y$ |Bottom temperature ( ^oC) |D |number |b|
|$U_{paij,y}$ |Gravimetric proportion of prey in predator stomach |D |$\in[0,1]$ |b|
|$B_p^\mathrm{other}$ |Biomass of other prey (kg) |D |$0\geq$ |h|
|$S_{1j}^\mathrm{eit}$ |AT survey selectivity |F |$\in[0,1]$ |c| 

\pagebreak

**Table 3 (continued). Parameter, definition, species-specific value (Pollock; (Cod) Pacific cod; (ATF) Arrowtooth flounder both sexes; (M:) Arrowtooth flounder males; (F:) Arrowtooth flounder females), and type: (I) Input parameter (assigned); (M) model index; (E) Estimated parameter; (F) fixed parameter; (P) Derived quantity;(D) Data.**

|Parameter|Definition|Type|Pollock|Cod|ATF|Source|
|:---------|:---------------------------------------------|:---------|------:|------:|-----------:|:----|
|$A_i$|	Number of prey ages|I|12 |12 |21|	e|
|$A_p$|	Number of predator ages|I|12 |12 |21 |e|
|$\hat\varphi_p$ |Annual relative foraging rate (d yr$^{-1}$)|I|	|	|	|	d|
|$\alpha_{\delta}$|	Intercept of the allometric maximum consumption function (g g$^{-1}$ yr$^{-1}$)	|I|	0.119| 0.041|	0.125|	a|
|$\beta_{\delta}$|	Allometric slope of maximum consumption|I|	-0.460|	-0.122|	-0.245|	a|
|$T_p^{cm}$|	Consumption maximum physiological temperature ($^o$C)|I|	15.00|	21.00|	34.13|	a|
|$T_p^{co}$|	Consumption optimum physiological temperature ($^o$C)|I|	10.00|	13.70|	19.60|	a|
|$Q_p^c$|	Max consumption parameter|	I|	2.60|	2.41|	2.18|	a|
|$O_{pi,y}$|	Spatial predator-prey overlap index|	I|	1|	1|	1|	e|
|$\alpha0_{d,i}$|	Intercept for VBGF $d$ parameter	|F	|-0.817 |-0.375|	M:  -0.213|d|
|||||| F:  -0.340||	
|$\alpha_{d,i,y}$|	Annual intercept for VBGF $d$|F|	||  |||			d|
|$\beta_{d,i}$|Temperature covariate for VBGF $d$|F|	0.009	|0.004|M: -0.0057|d|
|||||| F: -0.0115||
|$K_i$|	VBGF energy loss (kg kg$^{-1}$ yr$^{-1}$)	|F|	0.22|	0.45|	M: 1.08|d| 
|||||| F:  0.38 ||
|$H_i$|	VBGF assimilation (kg kg$^{-d}$ yr$^{-1}$)	|F|	16.34|	9.30|	M: 5.19|d|
|||||| F:  5.90||
|$t_{0,i}$|	VBGF age when $W_{ij,y}$= 0 (years)|F|	0.53|	-0.16	|M: -1.00|d|
|||||| F:   -0.28	||
|$M_i^\mathrm{fem}$|	Female natural mortality|F|NA*|	0.37|	0.35|	c|
|$M_i^\mathrm{male}$|	Male natural mortality 	|F|	NA*|	0.37|	0.20|	c|  
> _* pollock age-specific M1 residual mortalities from the assessment were used (same values for male and females)._  
> _a. Holsman and Aydin 2015_  
> _b.	Alaska Fisheries Science Center eastern Bering Sea bottom trawl survey_  
> _c.	Stock assessments (Ianelli et al., 2012; Thompson and Lauth, 2012; Spies et al., 2012)_  
> _d.	Tables 5 & 6_  
> _e.	This assessment_  
> _f.	Fishery observer data_  
> _g.	Alaska Fisheries Science Center echo-integrated acoustic trawl survey_  
> _h.	Juarado Molina et al., 2005_

### Climate-informed BRPs

## Climate informed reference points

Following Holsman et al. (2020), we use a hybrid approach to derive climate-informed ABC. This includes a climate-naive target ($B_{target}$ (i.e.,a target conditioned on historical climate) and a climate-integrated model to derive climate informed references points (e.g., climate informed $F_{target}$ needed to achieve $x\%$ of the species-specific climate-naive unfished biomss, $B_0$). This approach avoids the non-intuitive outcome of increased (decreased) harvest rates on declining (increasing) populations that arises when using climate informed targets that may be lower (higher) than present day (see Holsman et al. 2020 and Szwualski et al. 2022 for more information on this approach and issue).

In order to derive ABC estimates the model was projected through the year 2100 to attain relative equilibrium under a climate-naive projection (i.e., where climate is held constant in the projections at the mean of historical conditions from 1979-2010; Holsman et al., 2020) without fishing (i.e.,$B_0$, simultaneously for pollock and Pacific cod, then for arrowtooth; Holsman et al. 2016). Using the approach of Holsman et al. (2016, 2020) and Moffitt et al. (2016), the model was then projected under fishing (with climate effects) to iteratively solve for the harvest rate ($F_{target}$, i.e., a climate informed rate) that results in an average of 40% of the climate-naive $B_0$ (i.e., $B_{40}$ = $B_{target}$, a climate-naive target) in the last 5 years of the projection period (2094-2099), with the constraint that spawning biomass under fishing is always greater than 35% unfished biomass during the projection year; $\mathrm{ABC_{2100}}$ is the catch in 2100 given $F_{target}$. $F_{target}$ was then applied to the model to derive $\mathrm{ABC}$ and $F_{ABC}$ for 2024 to 2025 projections (Holsman et al. 2020 a,b).

#### Climate driven recruitment

Following Holsman et al. (2020, submitted), projections use a recruitment model with environmental covariates from the 30 layer high resolution [Bering10K ROMSNPZ model hindcast](https://beringnpz.github.io/roms-bering-sea/B10K-dataset-docs/)(Kearney et al. 2020) and [Alaska Climate Integrated Modeling (ACLIM) project](https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-climate-integrated-modeling-project) projections under CMIP5 RCP45 and RCP85 and CMIP6 ssp126 and ssp585 (Hermann et al. 2021, Cheng et al. 2021, Hollowed et al. 2020). We used a combination of observed and modeled 2023 environmental conditions in the Bering Sea and estimates of SSB for each species in 2023 to predict 2024 age 1 recruitment. We modified the approach of Mueter et al. (2011) to estimate recruitment in a given year *y* as a function of spawning biomass and environmental covariates in the previous year *y-1*. This approach fits recruitment models with covariate effects on post-spawning survival to age 1 recruitment estimates from single- and multi- species models of CEATTLE, assuming either a linear recruitment model (i.e., no recruit per spawner effects; 'LM'), a linear model where spawning biomass is a predictor variable ('BLM'), a Ricker spawner-recruitment model and a beverton Holt spawner-recruitment model. Models with ROMSNPZ covariates pre-selected based on postulated relationships to spawning were included as predictors of annual variation in mean recruitment, as well as combination of indices (where covariation in predictors \< 0.5). This resulted in a set of 637 candidate models. We z-score scaled each covariate and covariates were fit to model estimates of recruitment and spawning stock biomass. We used Akaike information criterion to evaluate a set of possible candidate models with different covariate combinations, including the null model whereby no covariates were included. Recruitment models were fit using the 'futR()' package publicly available in 2024 [github.com/kholsman/futR](https://github.com/kholsman/futR) (see Holsman et al. submitted for more information).

#### Climate-informed ABC

Here we adopted the current over fishing limit (OFL) for Tier 3 acceptable biological catch ABC and MSY proxies for Bering Sea groundfish stocks; 40% of unfished biomass ($SSB_0$) as the proxy target biomass for the $B_{ABC}$, and 35% of $SSB_0$ as the proxy for $B_{MSY}$ (female spawning biomass corresponding to maximum sustainable yield, MSY, i.e., 35% of ; Punt et al., 2014; NPMFC, 2013; Clark et al., 1991; Brooks et al., 2010).

The species-specific, acceptable biological catch ($\mathrm{ABC}_{x,i,y}$) for each harvest scenario ($x$) was calculated as the fishery yield for each year *y* of the projection period [1, $n_y^{fut}$] given a constant fishing mortality rate for the projection period that satisfies each harvest scenario objective ($F_{\mathrm{ABC},x,i}^*$), such that:

Eq. 4 $~~~~\mathrm{ABC}_{x,i,y}=\sum_j^{A_i}((F_{\mathrm{ABC},x,i}^*s_{ij}^\mathrm{f}/Z_{x,ij,y})(1-e^{-Z_{x,ij,y}})N_{x,ij,y} W_{ij,y})$

where $Z_{x,ij,y}$ is the control-rule specific total annual mortality for species $i$ age $j$ in the set $[1, 2,\ldots A_i]$, $s_{ij}^\mathrm{f}$ is fishery age selectivity, and $N_{x,ij,y}$ and $W_{ij,y}$ are the annual species-specific abundance and weight-at-age for each projection year *y*. Using this approach, we found the species-specific fishing mortality rate ($F_{x,i}^*$) that results in mean female spawning biomass ($\overline{SSB}_{F,i}$) in the target projection period (i.e., last 5 years; 2046-2050) under fishing that is equal to the target proxy percentage (i.e., 40%) of mean unfished female spawning biomass ($\overline{SSB}_{0,i}$; Table 5). To find $F_{\mathrm{ABC},x,i}^*$, we iteratively project the model to find the $\overline{SSB}_{F,i}$ that corresponds to a given harvest rate $F_{x,i}^*$, adjusting $F_{x,i}^*$ downwards if $\overline{SSB}_{F,i}$ is below the target or upwards if $\overline{SSB}_{F,i}$ is above the target, until we achieve $\overline{SSB}_{F,i}$ near or at the proxy of 40% of $\overline{SSB}_{0,i}$. We ran this harvest scenario with the following variations (sensu Holsman et al. 2020a,b):

-   Project the model through the year 2100 to attain relative equilibrium under a climate-naive projection without fishing ($B_0$, simultaneously for pollock and Pacific cod, then for arrowtooth).

-   Using the approach of Holsman et al. (2016) and Moffitt et al. (2016), the model was then projected under fishing to iteratively solve for the harvest rate ($F_{target}$) that results in an average of 40% of unfished biomass in the last 5 years of the projection period (2094-2099), with the constraint that spawning biomass under fishing is always greater than 35% unfished biomass during the projection years.

-   $F_{target}$ was then applied to each climate informed model under each climate scenario to derive $\mathrm{ABC}_{target,2024}$ and $F_{ABC,2024}$ for 2024.

<!-- ![Climate informed ABC methodology using climate naive B0 and climate informed $F_{ABC,2024}$](/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2023_Multispp_assessment/docs/MSM_Assessment/Figs/FigS7.pdf){ width=100% } -->

## Results

### Model parametrization

The multispecies mode of the model achieved a slightly higher overall fit to the data (i.e., lower negative log-likelihood with the same number of estimated parameters for both models) for pollock and similar fits to the data for Pacific cod and arrowtooth. Both models fit annual total catch for all three species closely (0.996; Fig. 3). We observed similar fits to survey biomass and age composition data from the single-species (i.e.,$\mathrm{M2}_{ij,y}$ set to 0, hereafter single-species model) and multispecies modes of CEATTLE (Figs. 4,5, 18-23). Although both models predicted similar historical total and female spawning biomass, inclusion of trophic interactions in the multispecies model resulted in slightly higher estimates of total biomass for pollock (Fig. 5).

Including predation interactions in CEATTLE resulted in similar model fit to observations of survey age composition for pollock, with average annual Pearson correlation coefficient (i.e., $R^2$) values from CEATTLE model in multispecies mode of 0.86 versus single-species version of CEATTLE model values of 0.85 (Table 4). The single- and multispecies models performed similarly well for the annual Pacific cod and arrowtooth survey age composition data (0.72 for Pacific cod and 0.63 for arrowtooth, respectively). The single- and multispecies models fit the survey estimates of biomass with similar accuracy (single- and multispecies , $R^2$ respectively, of 0.49 and 0.48 for pollock, 0.35 for both models for Pacific cod, and 0.71 for arrowtooth; negative log-likelihood = 458.02, 767.78, 674.71 and 449.57, 773.23, 674.03 for the single- and multispecies models, respectively). Survey and fishery age selectivity curves were similar for single- and multispecies models for each species (Fig. 6).

**Table 4. Correlation coefficients for 1979-2023 survey biomass and age composition data and values estimated from the model run in single-species mode (SSM) and multispecies mode (MSM).**

| Table 4.|*SSM* |*MSM* |
|:---------------|----------:|----------:|
| *Total survey biomass*   |||
| Pollock|0.49 |0.48 |
| Pacific cod|0.35 |0.33 |
| Arrowtooth|0.71 |0.71 |
| *Survey age composition* |||
| Pollock| 0.86 |0.86 |
| Pacific cod| 0.7 | 0.7 |
| Arrowtooth| 0.49 |0.5 |


**Table 5. Log likelihood equations for fitting the assessment to data sources from the fishery, summer Bering Sea groundfish survey ('GBT') or Bering Sea acoustic trawl survey ('AT') for each species $i$ of age $j$ in year $y$ and where $v = 0.001$**.

||||
|:-----------------|:------------------------------------------|:------------------------------|
| **Description**   |**Equation** |**Data source** |
| GBT survey biomass   |$\sum_i{\sum_y{\frac{[\ln(\beta^s_{i,y}-\ln(\hat{\beta}^s_{i,y}))]]}{2\sigma^2_{s,i}}}}$| AFSC annual Bering Sea bottom trawl survey (1979–2023), except 1979, 1980, 1981, 2020 for all three species|
| GBT survey age composition|$-\sum_i{n_i}\sum_y{\sum_j{(O^s_{ij,y}+v)\ln(\hat{O}^s_{ij,y}+v)}}$| AFSC annual Bering Sea bottom trawl survey (1979–2023), except 1979, 1980, 1981, 2020 for pollock and Pacific cod, and 1979, 1980, 1981, 2020, 2023 for arrowtooth|
| AT survey biomass (pollock only)|$\sum_i{\sum_y{\frac{[\ln(\beta^{AT}_{i,y}-\ln(\hat{\beta}^{AT}_{i,y}))]]}{2\sigma^2_{AT}}}},\sigma^2_{AT} = 0.2$| Pollock acoustic trawl survey (1979–2023)|
| AT survey age composition  (pollock only)|$-\sum_i{n_i}\sum_y{\sum_j{(O^{AT}_{ij,y}+v)\ln(\hat{O}^{AT}_{ij,y}+v)}}, for ~ i =1$| Pollock acoustic trawl survey (1979–2023)|
| Total catch|$\sum_i{\sum_y{\frac{[\ln(C_{i,y}-\ln(\hat{C_{i,y}})]^2}{2\sigma^2_c}}},\sigma_c = 0.05$|Fishery observer data (1979–2023)|
| Fishery age composition|$-\sum_i{n_i}\sum_y{\sum_j{(O^f_{ij,y}+v)\ln(\hat{O}^f_{ij,y}+v)}}$|Fishery observer data (1979–2023), except 2023 for pollock and  1989, 2015, 2017, 2023 for arrowtooth|
| **Penalties**   |||
| Fishery selectivity|$$\sum_i{\sum_j^{A_{i-1}}}x[\ln(\frac{n^f_{ij}}{n^f_{ij+1}})-ln(\frac{n^f_{ij+1}}{n^f_{ij+2}})]^2,~x =\{\begin{array}{lrl}
20,if~~n^f_{ij}> n^f_{ij+1} \\
0,if~~n^f_{ij}\le n^f_{ij+1} \\
\end{array}$$||
| **Priors**   |||
|  |$\sum_i{\sum_y{(\tau_{i,y})^2}}$||
|  |$\sum_i{\sum_y{(N_{0,ij})^2}}$||
|  |$\sum_i{\sum_y{(\varepsilon_{i,y})^2}}$||

<!-- ![Table 5. Log likelihood equations for fitting the assessment to data sources.](Figs/LogLike_equations.png) -->

### Predation mortality

**Implications** We find evidence of continued declines in predation mortality of age 1 pollock, Pacific cod and arrowtooth flounder relative to recent high predation years (2014 - 2016). While warm temperatures continue to lead to high metabolic (and energetic) demand of predators, declines in total predator biomass, in particular Pacific cod, are contributing to an net decrease in total consumption (relative to 2016) and therefore reduced predation mortality in 2021-2023. This pattern indicates continued favorable top-down conditions for juvenile groundfish survival in 2022 through predator release due to declining biomass of groundfish.

Predation mortality has varied considerably with changes in thermal conditions and predator abundance over time (Figs. 7,8). While recent warm water temperatures (Fig. 1) continue to drive high individual predator demand for prey, declines in groundfish biomass combined with increased recruitment has resulted in net declines in estimates of predation mortality for juvenile groundfish in recent years. This pattern indicates continued favorable top-down conditions for juvenile groundfish survival in 2022 through 2023 via predation release. Total mortality ($\mathrm{M1}_{ij}$+$\mathrm{M2}_{ij,y}$) for all three species peaked in 2016 (Fig. 5). Between 1980 and 1993, relatively high natural mortality rates for pollock reflect patterns in combined annual demand for pollock prey by all three predators that was high in the mid 1980's (collectively 9.24 million t per year). The peak in predation mortality of age 1 pollock in 2016 corresponds to warmer than average conditions and higher than average energetic demand of predators combined with the maturation of the large 2010-2012 year classes of pollock and Pacific cod (collectively with arrowtooth 10.07 million t per year).

At 1.2 yr$^{-1}$, age 1 mortality estimated by the model was greatest for pollock and lower for Pacific cod and arrowtooth, with total age 1 natural mortality at around 0.65 and 0.66 yr$^{-1}$ for Pacific cod and arrowtooth, respectively. 2023 age 1 natural mortality across species is 11% to 39% lower than in 2016 and is near average for pollock (relative to the long-term mean) (Fig. 7). Similarly, Pacific cod and arrowtooth age 1 mortality are well below the long-term mean.

2023 natural mortality across species is 11% to 39% lower than in 2016 and is below average for pollock (relative to the long-term mean) (Fig. 7), while Pacific cod and arrowtooth mortality rates continue to decline and are well below the long-term mean. Total mortality ($\mathrm{M1}_{ij}$+$\mathrm{M2}_{ij,y}$) for all three species peaked in 2016 (Fig. 5).

Temporal patterns in natural mortality reflect annually varying changes in predation mortality that primarily impact age 1 fish (and to a lesser degree impact ages 2 and 3 fish in the model). Pollock are primarily consumed by older conspecifics, and pollock cannibalism accounts for 59% (on average) of total age 1 predation mortality, with the exception of the years 2006-2008 when predation by arrowtooth marginally exceeded cannibalism as the largest source of predation mortality of age 1 pollock (Fig. 8). The relative proportion of age 1 pollock consumed by older pollock increased in 2023 relative to previous years, while the relative proportion consumed by Pacific cod and arrowtooth declined.

Patterns in the total biomass of each species consumed by all three predators in the model (typically 1-3 yr old fish) exhibit divergent trends from predation mortality in 2023. Pollock and Pacific cod biomass consumed by all predators in the model is trending upward (indicating more pollock and Pacific cod were consumed this year than in previous years), while arrowtooth consumed is trending downward (Fig. 9). Combined annual predation demand (annual ration) of pollock, Pacific cod, and arrowtooth flounder in 2023 was 9.26 million tons, down slightly from the 9.99 million t annual average during the warm years and large maturing cohorts of 2014-2016. Walleye pollock represent approximately 77% of the model estimates of combined prey consumed with a long-term average of 5.73 million tons of pollock consumed annually by all three predators in the model. From 2015 - 2019, individual annual rations were above average for all three predator species, driven by anomalously warm water temperatures in the Bering Sea during during those years. However, cooler temperatures in 2023 relative to the recent warm years has resulted in annual rations at or below the long-term average (Fig. 10).

Diet analyses reveal that there are temporal shifts in the diets of juvenile and adult groundfish associated with marine heatwaves (MHW) in the region (see 2023 Ecosystem Status Report for more information). Changes in diet composition combined with temporal trends in ration (Fig. 10) indicates that Pacific cod consumption of pollock, Pacific cod conspecifics, and crab increased during the recent MHW (Figs. 11,12). Similarly, pollock consumption of zooplankton has also increased over the past decade (Fig. 12)

### Biomass

-   At 7 million tons, the 2023 EBS pollock spawning biomass from the multispecies model is above the long-term (1979-2015) average of 5.2 million tons and represents a 10% change from 2022 and 33% change from 2021 spawning biomass levels. Similarly, the trend in total biomass observed in the past few years has continued through 2023, with recent estimates placing the total 2023 biomass (22.4 million t) above the 1979-2015 average of 17.7 million tons. However it is important to note that because there was no Alaska Fisheries Science Center summer bottom trawl survey in 2020, estimates of, and differences relative to the 2020 biomass should be interpreted cautiously.

-   The 2023 EBS Pacific cod female spawning biomass has declined -9% since 2022 and -12% since 2021. 2023 estimates are approximately -4% below (slightly) the 1979-2015 average. Total biomass in the EBS has declined -38% since 2016, and at approximately 796 thousand tons, is 20% below the long-term 1979-2015 average of 1 million tons. These patterns are driven in part by continued low survey indices in 2022 and warm bottom temperatures that have induced northward redistribution of the Pacific cod stock (Spies et al. 2020, Stevenson et al. 2019).

-   Arrowtooth total and spawning biomass estimates are 41% and 62% greater than the long-term 1979-2015 average (respectively), and trends suggest relatively stable biomass since 2012.

-   The multispecies model estimates of a 10% and -9% change in spawning biomass (SSB) between 2022 and 2023 for pollock and Pacific cod ( respectively) agree with CEATTLE single species model patterns of decline ( 10% and -9%, respectively). Both models predict an increase (slightly) in spawning biomass for arrowtooth flounder relative to 2022.

### Recruitment

The multispecies version of CEATTLE compensates for elevated predation mortality on younger age classes by increasing estimates of recruitment. Thus, generally recruitment is higher in the multispecies model than in the status quo single-species models for all three species, especially those with high predation rates (i.e., pollock). Here, residual mortality ($M_1$) for each species is adjusted in the single species (CEATTLE) model to match the average (across years) multispecies model mortality for each age ($M_1+M_2$). Therefore recruitment and biomass between the models is comparable (Fig. 13), although notable annual trends and productivity reference points differ. Inclusion of trophic interactions (predation) in the model resulted in slightly different stock-recruitment curves (Figs. 14, 15), with stronger density dependence for pollock in the single-species model than in the multispecies model (where density dependence would be from non-predation interactions), and peak recruitment occurring at much higher spawning biomass levels in the multispecies model (i.e., \~3.75 million tons versus \~2.5 million tons for the single species model).

Although the magnitude varies between models, the overall directional change in annual recruitment estimates from year-to-year was generally the same for both models (i.e., both models increased or decreased recruitment in the same year; Fig. 13). Pollock recruitment from the single-species version of CEATTLE was slightly positively correlated with Pacific cod recruitment ($R^2$ = 0.46) and uncorrelated with arrowtooth recruitment ($R^2$ = 0.01). Correlations between pollock recruitment and Pacific cod or arrowtooth recruitment were similar between the single- and multispecies versions, although correlations were weaker in the multispecies model for Pacific cod ($R^2$ = 0.33).

Pollock age 1 recruitment estimates for 2023 are 180% above the 1979-2015 average and estimated recruitment has increased in 2023 relative to 2022 ( note that the most recent estimates have the highest uncertainty). Pacific cod age 1 recruitment in the EBS remains -12% lower than the 1979-2015 average, and is similar in 2023 to the record low recruitment estimated for 2017.Estimates of arrowtooth flounder age 1 recruitment increased relative to 2022 but is -12% below the 1979-2015 average.

We found some support for climate driven variation in recruitment with slightly higher support for Pacific cod as evidenced from a smaller set of top AICc selected models for that species (4 and 6, for single- and multi-species modes, respectively), while pollock had 14 and 11 top set of models (for single- and multi-species modes, respectively) and Arrowtooth single- and multi-species modes had 11 and 12, respectively (Table 5).

**Table 5. Top 3 AICc selected recruitment models for each species and mode.**


Table: <left>a) Single-species</left>

|model                                  |Type    |R2    |   aicc    |  deltaAIC  |  cumlAIC  |Species     |mode            |
|:--------------------------------------|:-------|:-----|:---------:|:----------:|:---------:|:-----------|:---------------|
|(Su)SST + (Sp)largeZoop                |Linear  |0.27  |  121.715  |   0.000    |   0.03    |W. Pollock  |Single-species  |
|(Su)SST + (Wi)NEwinds + (Sp)largeZoop  |Linear  |0.30  |  122.737  |   1.023    |   0.04    |W. Pollock  |Single-species  |
|(Su)SST + (Fa)NCaS + (Sp)largeZoop     |Linear  |0.35  |  123.003  |   1.288    |   0.06    |W. Pollock  |Single-species  |
|(Su)SST                                |Linear  |0.37  |  87.695   |   0.000    |   0.06    |P. Cod      |Single-species  |
|(Su)SST + (Fa)largeZoop                |Linear  |0.37  |  89.072   |   1.376    |   0.09    |P. Cod      |Single-species  |
|(Su)SST + (Su)BT                       |Linear  |0.38  |  89.426   |   1.731    |   0.12    |P. Cod      |Single-species  |
|(Sp)Cop                                |Ricker  |0.12  |  106.387  |   0.000    |   0.03    |Arrowtooth  |Single-species  |
|(Sp)EupS                               |Ricker  |0.13  |  106.405  |   0.018    |   0.05    |Arrowtooth  |Single-species  |
|(Sp)largeZoop                          |Ricker  |0.12  |  106.710  |   0.323    |   0.08    |Arrowtooth  |Single-species  |


Table: <left>b) Multi-species</left>

|model                                |Type    |R2    |   aicc    |  deltaAIC  |  cumlAIC  |Species     |mode           |
|:------------------------------------|:-------|:-----|:---------:|:----------:|:---------:|:-----------|:--------------|
|(Su)SST + (Sp)largeZoop              |Linear  |0.24  |  113.749  |   0.000    |   0.03    |W. Pollock  |Multi-species  |
|(Su)SST + (Fa)NCaS + (Sp)largeZoop   |Linear  |0.33  |  114.594  |   0.845    |   0.05    |W. Pollock  |Multi-species  |
|(Sp)uEast + (Su)SST + (Sp)largeZoop  |Linear  |0.27  |  115.025  |   1.275    |   0.07    |W. Pollock  |Multi-species  |
|(Su)SST                              |Linear  |0.34  |  84.948   |   0.000    |   0.05    |P. Cod      |Multi-species  |
|(Su)SST + (Su)BT                     |Linear  |0.36  |  86.271   |   1.323    |   0.08    |P. Cod      |Multi-species  |
|(Sp)SST                              |Linear  |0.34  |  86.381   |   1.432    |   0.10    |P. Cod      |Multi-species  |
|(Sp)EupS                             |Ricker  |0.10  |  105.944  |   0.000    |   0.02    |Arrowtooth  |Multi-species  |
|(Sp)Cop                              |Ricker  |0.09  |  106.058  |   0.114    |   0.04    |Arrowtooth  |Multi-species  |
|(Sp)largeZoop                        |Ricker  |0.10  |  106.126  |   0.182    |   0.06    |Arrowtooth  |Multi-species  |

Common predictor variables in the top selected recruitment models included summer sea surface temperature ('(Su)SST') and bottom temperature ('(Su)BT') as well as spring and fall large Zooplankton ('(Sp)largeZoop' and '(Fa)largeZoop', respectively; Fig. 15). This is consistent with previous studies that used a slightly different NPZ formulation (Holsman et al. 2020, Kearney et al. 2020), as well as field studies that have postulated the importance of lower trophic productivity for groundfish larval and juvenile survival in the Bering Sea (Duffy-Anderson et al. 2017). In general, recruitment was found to be inversely correlated with temperature and positively correlated with indices of ecosystem productivity (copepod and euphausiid abundance; Fig. 16).

### Fishing mortality

The single- and multispecies models estimate similar fishing mortality rates for pollock that have remained relatively stable at around 0.11 since the early 1980s (Fig. 17). Both models also estimate low and relatively steady fishing mortality rates for arrowtooth flounder (i.e., \~ 0.03). Fishing mortality for Pacific cod is generally higher than pollock or arrowtooth, and varies over time (0.27-0.49). Fishing mortality for Pacific cod declined substantially between 2012-2021 with indication of slight increases in 2022-2023  (Fig. 17).

### Climate-informed reference points

Projecting the CEATTLE model forward under mean recruitment produces trajectories of female spawning stock biomass that can be used to derive multispecies biological reference points and attendant fishing mortality rates (Holsman et al. 2016). Projections under the Ricker spawner-recruit model lead to some over-compensation recruitment dynamics for pollock in the first years of the projection (especially for single-species models; Fig. 24; sensu Botsford, 1986). However, a \>70 year projection period was sufficient to allow such dynamics to reach a relative equilibrium (Figs. 24,25).

In general, projected unfished and harvested female spawning stock biomass ($SSB_{0,iy}$ and $SSB_{target}$ , respectively) were similar for projections of the multi- and single-species models (Figs. 24-27) due to adjusted age 1 $M_1$ values for the single species model that match the mean (across years) age 1 $M_2$ from the multispecies model. Relative to 2023 levels, the model projects SSB of pollock will decline in 2024 (projected based on 2023 catch) followed by an decline in SSB in 2025 (projected with $F_{i,y}$ set to the climate naive $F_{target,i}$) for each species $i$. For Pacific cod the model projects a decline (slightly) in SSB in both 2024 and 2025.

<!-- The differential estimates of population productivity result in slight variation in forecasted recruitment for 2024, but in general there is agreement in the direction and relative magnitude between single and multispecies models in expected changes in recruitment. We used a combination of observed and modeled environmental conditions (Holsman et al. in prep) to forecast 2024 age 1 recruitment for each species. Correlation coefficients between recruitment model estimates and CEATTLE estimates of recruitment were highest for pollock, intermediate for Pacific cod, and lowest for arrowtooth. The model estimate likely decreasing in pollock and Pacific cod recruitment in 2024 due to poor environmental conditions in the Bering Sea in 2023, while the model forecasts increases in arrowtooth flounder recruitment (Fig. 20). -->

Near-term estimates of changes in spawning biomass were generated using the climate integrated model, where climate drives recruitment, growth, and predation mortality (in multispecies mode). For the multispecies model, ensemble projections using climate-enhanced recruitment models and projected future warming scenarios (including low carbon mitigation/ high warming, moderate mitigation and warming, high mitigation / low warming, as well as persistence scenarios and assuming 2023 catch for 2024 and $F_{ABC}$ for 2025) estimate a 95% chance that pollock SSB will remain between 89-93% of 2023 SSB in 2024 and will be between 82-84% of 2023 SSB levels in 2025. Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that Pacific cod SSB will continue to decline to between 96-102% of 2023 SSB in 2024 and between 78-82% of 2023 SSB levels in 2025. Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that arrowtooth SSB will be between 84 and 98% of 2023 SSB in 2024 and will be between 76 and 87% of 2023 SSB levels in 2025.

Similarly, for the single-species model, ensemble projections using climate-enhanced recruitment models and projected future warming scenarios (including low carbon mitigation/ high warming, moderate mitigation and warming, high mitigation / low warming, as well as persistence scenarios and assuming 2023 catch for 2024 and $F_{ABC}$ for 2025) estimate a 95% chance that pollock SSB will remain between 89-93% of 2023 SSB in 2024 and will be between 85-88% of 2023 SSB levels in 2025. Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that Pacific cod SSB will continue to decline to between 95-101% of 2023 SSB in 2024 and between 80-84% of 2023 SSB levels in 2025. Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that arrowtooth SSB will be between 84 and 98% of 2023 SSB in 2024 and will be between 76 and 87% of 2023 SSB levels in 2025.

### ABC and harvest recommendations

In order to derive ABC estimates the model was projected through the year 2100 to attain relative equilibrium under a climate-naive projection without fishing ($B_0$, simultaneously for pollock and Pacific cod, then for arrowtooth). Using the approach of Holsman et al. (2016, 2020) and Moffitt et al. (2016), the model was then projected under fishing to iteratively solve for the harvest rate ($F_{target}$) that results in an average of 40% of the climate-naive unfished biomass in the last 5 years of the projection period (2094-2099), with the constraint that spawning biomass under fishing is always greater than 35% unfished biomass during the projection years. $F_{target}$ was then applied to the model to derive $\mathrm{ABC}$ and $F_{ABC}$ for 2024 to 2025 projections (Holsman et al. 2020 a,b).

The hybrid approach (i.e., climate-naive target and climate-informed status and projections) method for estimating $F_{target}$ resulted in a proxy ABC harvest rate at equilibrium that corresponds to about 49% $SSB_0$ for pollock, 51% for Pacific cod, and 40% for arrowtooth flounder for single species models, and about 48%, 45%, and 40% $SSB_0$ for pollock, Pacific cod, and arrowtooth flounder using the multispecies model.

* Single and multispecies CEATTLE models project changes in 2024 recommended ABC for pollock over 2023 ABC (from last year's assessment) of -7% and 0%, respectively. 2025 ABC is -16% and -15% of 2023 ABC, respectively. 

* Single and multispecies CEATTLE models both project increases in 2024 recommended ABC for Pacific cod over 2023 ABC (from last year's assessment) of 12% and 12%, respectively. While, 2025 ABC is -7% and -10% of 2023 ABC, respectively. 

* Single and multispecies CEATTLE models both project a decline in 2024 recommended ABC for arrowtooth flounder 2023 ABC (from last year's assessment) of -23% and -20%, respectively. 2025 ABC is -27% and -24% of 2023 ABC, respectively.

## Climate-informed outlook

### Probability of near-term (+ 1-2 yr) biomass decline or increase

-   Relative to 2023 levels, the model projects SSB of pollock will decline in 2024 (projected based on 2023 catch) followed by a decline in SSB in 2025 (projected with $F_{ABC}$). For Pacific cod the model projects a decline (slightly) in SSB in both 2024 and 2025.

-   Ensemble projections using climate-enhanced recruitment models and projected future warming scenarios (including high (ssp126), moderate( RCP45), and low (ssp585) carbon mitigation scenarios, as well as a persistence scenario and assuming 2023 catch for 2024 and $F_{ABC}$ for 2025) estimate a 95% probability that pollock SSB will remain between 89-93% of 2023 SSB in 2024 and will be between 81-84% of 2023 SSB levels in 2025.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that Pacific cod SSB will continue to decline to between 96-102% of 2023 SSB in 2024 and between 78-82% of 2023 SSB levels in 2025.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that arrowtooth SSB will be between 84 and 98% of 2023 SSB in 2024 and will be between 76 and 86% of 2023 SSB levels in 2025.

### Low warming scenarios (SSP126): probability of long-term (2033, 2050, 2080) biomass decline or increase

- Trends in biomass and recruitment under high carbon mitigation (low warming; SSP126) scenarios are very similar to near-present day. *Note that projections assume no adaptation by the species, fishery, or fishery management.* See figures 22 and 23 for more information.

- Ensemble projections using climate-enhanced recruitment models and projected future warming scenarios and assuming $F_{ABC}$ for 2025 - 2100) estimate a 95% chance that pollock SSB will be between 59-63% of 2023 SSB in 2033, between 57-61% of 2023 SSB levels in 2050, and between 48-55% of 2023 SSB levels in 2080.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that Pacific cod SSB will be between 71-79% of 2023 SSB in 2033, between 73-79% of 2023 SSB levels in 2050, and between 62-69% of 2023 SSB levels in 2080.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that arrowtooth SSB will be between 62-74% of 2023 SSB in 2033, between 63-68% of 2023 SSB levels in 2050, and between 59-66% of 2023 SSB levels in 2080.

### High warming scenarios (SSP585): probability of long-term (2033, 2050, 2080) biomass decline or increase

- Trends in biomass and recruitment under low carbon mitigation (high warming; SSP585) scenarios are markedly different than historical or present day productivity. *Note that projections assume no adaptation by the species, fishery, or fishery management.*

- Ensemble projections using climate-enhanced recruitment models and projected future warming scenarios and assuming $F_{ABC}$ for 2025 - 2100) estimate a 95% chance that pollock SSB will be between 57 and 64% of 2023 SSB in 2033, between 50 and 55% of 2023 SSB levels in 2050, and between 29 and 34% of 2023 SSB levels in 2080.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that Pacific cod SSB will be between 65 and 75% of 2023 SSB in 2033, between 64 and 69% of 2023 SSB levels in 2050, and between 37 and 42% of 2023 SSB levels in 2080.

-   Ensemble projections using climate-enhanced recruitment models based on long-term projections estimate a 95% chance that arrowtooth SSB will be between 64 and 74% of 2023 SSB in 2033, between 58 and 61% of 2023 SSB levels in 2050, and between 40 and 43% of 2023 SSB levels in 2080.

## Discussion

### Application of MBRPs toward climate-resilient EBFM

Development of climate-informed multispecies biological reference points (MBRPs) from climate-enhanced models like CEATTLE is a critical step in managing for climate impacts on fisheries resources (Holsman et al. 2019; Karp et al. 2019; Link, 2010; Link and Browman, 2014). Projecting CEATTLE provides proxies for MBRPs that can readily inform current harvest advice to support Alaska fisheries management. Like previous authors, we found that ABC proxies differed from single-species CEATTLE model estimates (e.g., Gaichas et al., 2012) and are influenced by historical and future climate conditions (Holsman et al. 2020). Climate and trophic drivers can interact to affect MBRPs, but for prey species with high predation rates, trophic and management-driven changes may exceed direct effects of temperature on growth and predation in the near-term. Given this, MSCAA models can readily be used for tactical EBFM decisions under changing climate conditions, if, as suggested by Holsman et al. (2016, 2020) and others, harvest scenarios used for deriving MBRPs combined a minimum biomass threshold with yield targets to meet biodiversity and yield objectives (Worm et al., 2009; Gaichas et al., 2012). Biomass thresholds will require development of criteria for minimum limits in order represents a necessary advancement of the current approach.

### Short-term utility: potential application for climate informed single species assessments

This work demonstrates some alternative applications of climate informed multispecies trophic models within a management setting and there may be immediate relevance for current stock assessment models. For example, the estimated historical time series of natural mortality at age over time (M1 + M2) could be used directly within the assessment or used as priors in alternative assessment models with estimated annually varying natural mortality. Similarly, for the case of EBS pollock, the climate informed SSB and stock recruitment relationship may provide a basis for better estimates or prior distribution specification. It may be that by adding the time series of estimated total natural mortality at age that the estimated stock recruitment relationship may differ substantially given the relative differences in age 1 abundances. Further research on applying alternative stock recruitment relationships is needed as well, especially since the application of the Ricker curve has traditionally been justified due to cannibalistic nature of pollock, a situation that is partially accounted for in this application.

### Long-term utility: climate- and trophic-specific biological reference points

Long-term projections of climate conditions (ideally ensembles to capture future uncertainty) are needed to inform long-term climate-specific reference points (Holsman et al. 2019), while short-term forecasts (e.g., \<2 years) would also advance near-term understanding of harvest and productivity reference points, especially change in weight at age and survival within the 2 year harvest specification period (Karp et al. 2019). This assessment demonstrates the utility of using long-term projections to inform annual harvest reference points. Ongoing ROMSNPZ model validation is be useful for evaluating predictive performance and potential utility going forward. Incorporating additional species into the model, such as northern fur seals and Pacific halibut could help provide quantitative estimates of changes in juvenile pollock forage resources associated with different harvest rates of groundfish species in the EBS, as well as refine estimates of predation mortality for prey species in the model under changing conditions. Finally, ongoing incorporation of harvest scenarios into the model will add realism to projections both for assessment purposes and for research applications.

## Acknowledgments

Our work is the result of numerous collaborations with researchers at the University of Washington (UW), University of Alaska Fairbanks (UAF), and the NOAA Alaska Fisheries Science Center (AFSC) and Northwest Fisheries Science Center (NWFSC). We thank the Groundfish assessment, Age and Growth, Resource Ecology and Ecosystem Modeling, and Acoustics teams and programs at the AFSC, the ACLIM team, and the Bering10K modeling team for insight into this work. In particular, Ron Heintz (AFSC), Franz Mueter (UAF), and Elizabeth Siddon (UAF) supported an excellent discussion of the bioenergetics model sub-component of CEATTLE. We thank Grant Adams (UW), Grant Thompson (AFSC), I. Kaplan (NWFSC), and P. Sean McDonald (UW) for providing feedback on previous drafts. Drs. E. Moffitt and A. Punt contributed significantly to the development of multispecies harvest control rules used in this assessment. Support for the CEATTLE model came from the Alaska Integrated Ecosystem Assessment program (noaa.gov/iea), the NMFS Fisheries And The Environment (FATE) program, the Stock Assessment Analytical Methods program under award number 0002, and the North Pacific Research Board (publication number 547). This effort would not be possible without the help of numerous researchers and volunteers who contribute annually to the collection of biomass, demography, and diet information through Alaska Fishery Science Center surveys and the NOAA observer program, and the help of those who provide access to fishery-dependent and independent data through the Alaska Fisheries Science Center.

## References

Adams, G. D., K. K. Holsman, S. J. Barbeaux, M. W. Dorn, J. N. Ianelli, I. Spies, I. J. Stewart, A. E. Punt (2022) An ensemble approach to understand predation mortality for groundfish in the Gulf of Alaska. Fisheries Research (251) https://doi.org/10.1016/j.fishres.2022.106303.

Botsford, L. W., 1986. Effects of environmental forcing on age-structured populations: Northern California Dungeness crab (Cancer magister) as an example. Can. J. Fish. Aquat. Sci. 43, 2345-2352.

Brooks, E. N., Powers, J. E., and Cort??s, E., 2010. Analytical reference points for age-structured models: application to data-poor fisheries. ICES J. Mar. Sci., 67, 165175.

Caddy, J. F., Mahon, R., 1995. Reference points for fishery management. FAO Fisheries Technical Paper 347.

Cheung, W.W.L., Brodeur, R. D., Okey, T. A., Pauly, D., 2015. Projecting future changes in distributions of pelagic fish species of Northeast Pacific shelf seas. Prog. Oceanogr. 130, 1931.

Clark, W. G., 1991. Groundfish exploitation rates based on life history parameters. Can. J. Fish. Aquat. Sci. 48, 734750.

Collie, J. S., Gislason, H., 2001. Biological reference points for fish stocks in a multispecies context. Can. J. Fish. Aquat. Sci. 58, 2167-2176.

Coyle, K. O., Eisner L. B., Mueter F. J., Pinchuk A. I., Janout M. A.,Cieciel, K. D., Farley, E.V., Andrew, A. G., 2011. Climate change in the southeastern Bering Sea: impacts on pollock stocks and implications for the Oscillating Control Hypothesis. Fish. Ocean. 20(2), 139156.

Curti, K. I., Collie, J. S., Legault, C. M., and Link, J. S., 2013. Evaluating the performance of a multispecies statistical catch-at-age model. . Can. J. Fish. Aquat. Sci. 70, 470-484.

Danielsson, A., Stefansson, G., Baldursson, F. M., Thorarinsson K., 1997. Utilization of the Icelandic cod stock in a multispecies context. Mar. Res. Econ. 12(4), 329-344.

Dorn, M., Aydin, K., Jones, D., Palsson, W., Spalinger, K., 2014. Chapter 1: Assessment of the Walleye Pollock Stock in the Gulf of Alaska. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Gulf of Alaska Region, Alaska Fisheries Science Center, National Marine Fisheries Service, Anchorage, AK, p 53170.

Duffy-Anderson JT, Stabeno PJ, Siddon EC, Andrews AG, Cooper DW, Eisner LB, et al. (2017) Return of warm conditions in the southeastern Bering Sea: Phytoplankton - Fish. PLoS ONE 12(6): e0178955. <https://doi.org/10.1371/journal.pone.0178955>

Dunn, J. R. Matarese, A. C., 1987. A review of the early life history of Northeast Pacific gadoid fishes. Fish. Res. 5, 165-184.

Gaichas, S., Gamble, R., Fogarty, M., Beno??t, H., Essington, T., Fu, C., Koen-Alonso, M., Link, J., 2012. Assembly rules for aggregate-species production models: simulations in support of management strategy evaluation. Mar. Eco. Prog. Ser. 459, 275292.

Gamble R. J. and Link, J. S., 2009. Analyzing the tradeoffs among ecological and fishing effects on an example fish community: a multispecies (fisheries) production model. Ecol. Model. 220, 2570-2582.

Gamble, R. J. and Link, J., 2012. Using an aggregate production simulation model with ecological interactions to explore effects of fishing and climate on a fish community. Mar. Eco. Prog. Ser. 459, 259274, 2012 doi: 10.3354/meps09745

Gislason, H. 1999. Single and multispecies reference points for Baltic fish stocks. ICES J. Mar. Sci. 56, 571-583.

Gouhier, T. C., Guichard, F., Gonzalez, A., 2010. Synchrony and Stability of Food Webs in Metacommunities. Am. Nat., 175 (2), E16-E34

Essington, T., Kitchell J., Walters, C., 2001. The von Bertalanffy growth function, bioenergetics, and the consumption rates of fish. Can. J. Fish. Aquat. Sci. 58, 21292138.

Fogarty, M. J. 2014. The art of ecosystem-based fishery management. Can. J. Fish. Aquat. Sci. 71, 479-490.

Fogarty, M.J., Overholtz, W. J., Link, J. S., 2012. Aggregate surplus production models for demersal fishery resources of the Gulf of Main. Mar. Ecol. Prog. Ser. 459, 247-258.

Fournier, D. A., Skaug, H. J., Ancheta, J., Ianelli, J., Magnusson, A., Maunder, M. N., Nielsen, A., Sibert, J., 2012. AD Model Builder: using automatic differentiation for statistical inference of highly parameterized complex nonlinear models. Optimization Methods and Software, 27, 233-249.

Fulton, E. A., Link, J. S., Kaplan, I. C., Savina-Rolland, M., Johnson, P., Ainsworth, C., Horne, P., Gorton, R., Gamble, R.J., Smith, A.D.M., Smith, D.C., 2011. Lessons in modelling and management of marine ecosystems: the Atlantis experience. Fish Fish. 12, 171188.

Hanson, P., Johnson, T. Schindler, D., Kitchell, J., 1997. Fish Bioenergetics 3.0. Madison, WI: University of Wisconsin Sea Grant Institute.

Hamre, J. 2003. Capelin and herring as key species for the yield of north-east Arctic cod. Results from multispecies model runs. Sci. Mar. 67 (Suppl 1), 315-323.

Hermann et al., 2021 A.J. Hermann, K. Kearney, W. Cheng, D. Pilcher, K. Aydin, K.K. Holsman, et al. Coupled modes of projected regional change in the Bering Sea from a dynamically downscaling model under CMIP6 forcing Deep-Sea Res. II (2021), 10.1016/j.dsr2.2021.104974 194 104974.

Hollowed, K. K. Holsman, A. C. Haynie, A. J. Hermann, A. E. Punt, K. Y. Aydin, J. N. Ianelli, S. Kasperski, W. Cheng, A. Faig, K. Kearney, J. C. P. Reum, P. D. Spencer, I. Spies, W. J. Stockhausen, C. S. Szuwalski, G. Whitehouse, and T. K. Wilderbuer. Integrated modeling to evaluate climate change impacts on coupled social-ecological systems in Alaska. Frontiers in Marine Science, 6(January):1--18, 2020. DOI: 10.3389/fmars.2019.00775.

Hilborn, R. and Walters, C. J., 1992. Quantitative Fisheries Stock Assessment: Choice, Dynamics and Uncertainty. Chapman and Hall, New York. 570 p.

Hollowed, A. B., Bax, N., Beamish, R., Collie, J., Fogarty, M., Livingston, P., Pope, J., Rice, J. C., 2000a. Are multispecies models an improvement on single-species models for measuring fishing impacts on marine ecosystems? ICES J. Mar. Sci., 57, 707719. <doi:10.1006/jmsc.2000.0734>.

Hollowed, A. B., Ianelli, J. N., and Livingston, P. A., 2000b. Including predation mortality in stock assessments: a case study for Gulf of Alaska walleye Pollock. ICES J. Mar. Sci., 57, 279293.

Hollowed, A. B., Curchitser, E. N., Stock, C. A., Zhang, C. 2013. Trade-offs associated with different modeling approaches for assessment of fish and shellfish responses to climate change. Climatic Change 119, 111129 DOI 10.1007/Table 6584-012-0641-z

Holsman, K. K., Ianelli, J., Aydin, K., Punt, A. E., Moffitt , E. A. (2016). Comparative biological reference points estimated from temperature-specific multispecies and single species stock assessment models. Deep Sea Res. II. <doi:10.1016/j.dsr2.2015.08.001>.

Holsman, K. K. and Aydin, K. 2015. Comparative methods for evaluating climate change impacts on the foraging ecology of Alaskan groundfish. Mar. Ecol. Prog. Ser. DOI 10.3354/mep102

Holsman, KK, EL Hazen, A Haynie, S Gourguet, A Hollowed, S Bograd, JF Samhouri, K Aydin. 2019. Toward climate-resiliency in fisheries management. ICES Journal of Marine Science. 10.1093/icesjms/fsz031

Holsman, K.K., A Haynie, A Hollowed et al. 2020a. Climate-informed multispecies assessment model methods for determining biological references points and Acceptable Biological Catch., 24 September 2020, PROTOCOL (Version 1) available at Protocol Exchange [+<https://doi.org/10.21203/rs.3.pex-1084/v1+>].

Holsman, K.K., A. Haynie, A. Hollowed, J. Reum, K. Aydin, A. Hermann, W. Cheng, A. Faig, J. Ianelli, K. Kearney, A. Punt. 2020b. Ecosystem-based fisheries management forestalls climate-driven collapse. Nature Communications. <DOI:10.1038/s41467-020-18300-3>.

Honkalehto, T., Ressler, P.H., Towler, R.H., Wilson, C.D., 2011.Using acoustic data from fishing vessels to estimate walleye pollock (Theragra chalcogramma) abundance in the eastern Bering Sea. 2011. Can. J. Fish. Aquat. Sci. 68, 12311242

Howell, D., Bogstad, B. 2010. A combined Gadget/FLR model for management strategy evaluations of the Barents Sea fisheries. ICES J. Mar. Sci, 67, 000000.

Hunsicker, M. E., Ciannelli, L., Bailey, K. M., Zador, S., Stige, L. C., 2013. Climate and demography dictate the strength of predator-prey overlap in a subarctic marine ecosystem. PloS one 8(6), e66025. <doi:10.1371/journal.pone.0066025>.

Hunt G. L. Jr, Coyle K. O., Eisner L., Farley E. V., Heintz R., Mueter, F., Napp, J. M., Overland, J. E., Ressler, P. H., Salo, S., Stabeno, P. J., 2011. Climate impacts on eastern Bering Sea foodwebs: A synthesis of new data and an assessment of the Oscillating Control Hypothesis. ICES J. Mar. Sci. 68(6), 12301243.

Ianelli, J. N., Honkalehto T., Barbeaux S., Kotwicki S., 2014. Chapter 1: Assessment of the walleye pollock stock in the Eastern Bering Sea. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Bering Sea/Aleutian Islands Regions, Alaska Fisheries Science Center, National Marine Fisheries Service, Anchorage, AK, p 55156.

Ianelli, J. N., Barbeaux, S., Honkalehto, T., Kotwicki, S., Aydin, K., and Williamson, N. 2012. Assessment of Alaska Pollock Stock in the eastern Bering Sea. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Bering Sea/Aleutian Islands Regions, pp. 31124.

Ianelli, J. N., et al. 2019. Assessment of Alaska Pollock Stock in the eastern Bering Sea. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Bering Sea/Aleutian Islands Regions.

Ianelli, J. N., Holsman, K. K. Punt, A. E., Aydin, K. 2016. Multi-model inference for incorporating trophic and climate uncertainty into stock assessments. Deep Sea Res. II

Jurado-Molina, J., Livingston, P. A., Ianelli, J. N., 2005. Incorporating predation interactions in a statistical catch-at-age model for a predator-prey system in the eastern Bering Sea. Can. J. Fish. Aquat. Sci. 62, 1865-1873.

Kaplan, I. C. , P. J. Horne, P. S. Levin., 2012. Screening California Current fishery manamgent scenarios using the Atlantis end-to-end ecosystem model. Prog. Oceanogr. 102, 5-18.

Kaplan, I.C., Brown, C.J., Fulton, E.A., Gray, I.A., Field, J.C., Smith, A.D.M., 2013. Impacts of depleting forage species in the California Current. Environ. Cons. 40, 380393.

Karp, M ,JO Peterson, PD Lynch, RB Griffis, C Adams, B Arnold, L Barnett, Y deReynier, J DiCosimo, K Fenske, S Gaichas, A Hollowed, K Holsman, + 13. 2019. Accounting for Shifting Distributions and Changing Productivity in the Development of Scientific Advice for Fishery Management. ICES Journal of Marine Science fsz048, <https://doi.org/10.1093/icesjms/fsz048>

Kearney, K, A. Hermann, W. Cheng, I. Ortiz, and K. Aydin. A coupled pelagic benthic-sympagic biogeochemical model for the Bering Sea: documentation and validation of the BESTNPZ model (v2019.08.23) within a high resolution regional ocean model. Geoscientific Model Development, 13 (2):597--650, 2020. DOI: 10.5194/gmd13-597-2020.

Kinzey, D. Punt, A. E., 2009. Multispecies and single-species age-structured models of fish population dynamics: Comparing parameter estimates. Nat. Res. Mod. 22, 67-104.

Kitchell, J. F., Stewart, D. J. and Weininger, D., 1977. Applications of a bioenergetics model to yellow perch (Perca flavenscens) and walleye (Stizostedion vitreum vitreum). J. Fish. Res. Board Can. 34, 1922-1935.

Levin, P. S., Kelble, C. R., Shuford, R., Ainsworth, C., deReynier, Y., Dunsmore, R., Fogarty, M. J., Holsman, K., Howell, E., Monaco, M., Oakes, S., Werner, F., 2013. Guidance for implementation of integrated ecosystem assessments: a US perspective. ICES J. Mar. Sci, <doi:10.1093/icesjms/fst112>.

Link J. S., 2010. Ecosystem-based fisheries management: confronting tradeoffs, Cambridge University Press, Cambridge.

Link, J. S., Browman, H. I., 2014. Integrating what? Levels of marine ecosystem-based assessment and management. ICES J. Mar. Sci, 71, 11701173

Livingston, P. A., Aydin, K., Bolt, J. L., Hollowed , A. B., Napp, J. M., 2011. Alaskan marine fisheries management: advances and linkages to ecosystem research. In A Belgrano and W Fowler (eds.), Ecosystem-Based Management for Marine Fisheries: An Evolving Perspective. Cambridge University Press, pp 113-152.

Livingston, P., 1993. Importance of predation by groundfish, marine mammals and birds on walleye pollock Theragra chalcogramma and Pacific herring Clupea pallasi in the eastern Bering Sea. Mar. Ecol. Prog. Ser. 102(3), 205215.

Moffitt, E., Punt, A. E., Holsman, K. K., Aydin, K. Y., Ianelli, J. N., Ortiz, I., 2016. Moving towards Ecosystem Based Fisheries Management: options for parameterizing multispecies harvest control rules. Deep Sea Res. II.

Morita, K., Fukuwaka, M. A., Tanimata, N. and Yamamura, O., 2010. Size-dependent thermal preferences in a pelagic fish. Oikos 119, 1265-1272.

Murawski, S., Matlock G., 2006. Ecosystem science capabilities required to support NOAAs mission in the year 2020. NOAA Technical Memorandum, NMFS-F/SPO-74, Silver Spring, MD.

Mueter, F. J. Megrey, B. A., 2006. Using multispecies surplus production models to estimate ecosystemlevel maximum sustainable yields. Fish. Res. 81, 189201.

Mueter, F. J., Boldt, J. L., Megrey, B. A., Peterman, R. M., 2007. Recruitment and survival of Northeast Pacific Ocean fish stocks: temporal trends, covariation, and regime shifts. Can. J. Fish. Aquat. Sci. 64(6), 911-927.

Mueter, F. J., Bond, N. A., Ianelli, J. N., & Hollowed, A. B. (2011). Expected declines in recruitment of walleye pollock ( Theragra chalcogramma ) in the eastern Bering Sea under future climate change.

Nishiyama, T., Hirano, K., and Haryu, T., 1986. The early life history and feeding habits of larval walleye pollock Theragra chalcogramma (Pallas) in the southeast Bering Sea. Int. North Pac. Fish. Comm. Bull. 45, 177227.

North Pacific Fishery Management Council (NPFMC). 2013. Fishery Management plan for groundfish of the Bering Sea and Aleutian Islands management area. North Pacific Fishery Management Council, Anchorage, AK.

Ortiz, I, K. Aydin, A. J. Hermann, G. Gibson. 2016. Climate to fisheries: Exploring processes in the eastern Bering Sea based on a 40 year hindcast. Deep Sea Res. II.

Pauly, D., 1981. The relationship between gill surface area and growth performance in fish: a generalization of von Bertalanffys theory of growth. Meeresforschung 28, 251-282.

Plaganyi, ??. E., Punt, A.E., Hillary, R., Morello, E.B., Th??baud, O., Hutton, T., Pillans, R.D., Thorson, J.T., Fulton, E. A., Smith, A. D. M., Smith, F., Bayliss, P., Haywood, M., Lyne, V., Rothlisberg, P.C., 2014. Multispecies fisheries management and conservation: tactical applications using models of intermediate complexity. Fish Fish. 15, 1-22.

Pikitch E. K., Santora C., Babcock E. A., Bakun A., Bonfi, R., Conover, D. O., Dayton, P., Doukakis, P., Fluharty, D., Heneman, B., Houde, E. D., Link, J., Livingston, P. A., Mangel, M., McAllister, M. K., Pope, J., Sainsbury, K. J., 2004. Ecosystem-based fishery management. Science 305, 346347

Punt, A.E., Smith, A.D.M., Smith, D.C., Tuck, G., Klaer, N., 2014. Selecting relative abundance proxies for BMSY and BMEY. ICES J. Mar. Sci. 71, 469-483.

Quinn, T. J., II, Deriso, R. B., 1999. Quantitative Fish Dynamics. Oxford University Press, New York.

Ricker, W. E. (1954) Stock and Recruitment Journal of the Fisheries Research Board of Canada, 11(5): 559623. <doi:10.1139/f54-039>

Siddon E. C., Kristiansen T., Mueter F. J., Holsman K. K., Heintz R. A., Farley, E. V., 2013. Spatial Match-Mismatch between Juvenile Fish and Prey Provides a Mechanism for Recruitment Variability across Contrasting Climate Conditions in the Eastern Bering Sea. PLoS ONE 8(12), e84526. <doi:10.1371/journal.pone.0084526>

Smith, M. D., Fulton, E. A., and Day, R.W. 2015. An investigation into fisheries interaction effects using Atlantis. ICES J. Mar. Sci, 72(1), 275283. <doi:10.1093/icesjms/fsu114>

Spencer, PD, KK Holsman, S Zador, NA Bond, FJ Mueter, AB Hollowed1, and JN Ianelli. (2016). Modelling spatially dependent predation mortality of eastern Bering Sea walleye pollock, and its implications for stock dynamics under future climate scenarios. ICES Journal of Marine Science; <doi:10.1093/icesjms/fsw040>

Spies, I. Wilderbuer, T. K., Nichol, D. G. and Aydin, K., 2012. Chapter 6. Arrowtooth Flounder. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Bering Sea/Aleutian Islands Regions, pp. 31124.

Spies, I. et al. Genetic evidence of a northward range expansion in the eastern Bering Sea stock of Pacific cod. Evol. Appl. 13, 362--375 (2020).

Stabeno, P.J., Farley, E.V., Jr., Kachel, N.B., Moor, S., Mordy, C.W., Napp, J.M., Overland, J.E., Pinchuk, A.I., Sigler, M.F., 2012. A comparison of the physics of the northern and southern shelves of the eastern Bering Sea and some implications for the ecosystem. Deep Sea Res. II 65-70, 14-30.

Stevenson, D. E. & Lauth, R. R. Bottom trawl surveys in the northern Bering Sea indicate recent shifts in the distribution of marine species. Polar Biol. 42, 407--421 (2019).

Taylor, L. , Begley, J., Kupca1, V. Stefansson, G., 2007. A simple implementation of the statistical modelling framework Gadget for cod in Icelandic waters. Afr. J. Mar. Sci., 29(2), 223245

Temming, A., 1994. Food conversion efficiency and the von Bertalanffy growth function. Part II and conclusion: extension of the new model to the generalized von Bertalanffy growth function. NAGA The ICLARM Quarterly, 17(4), 41-45.

Tsehaye, I., Jones, M. I., Bence, J. R., Brenden, T. O., Madenjian, C. P., Warner, D. M., 2014. A multispecies statistical age-structured model to assess predator-prey balance: application to an intensively managed Lake Michigan pelagic fish community. Can. J. Fish. Aquat. Sci. 71, 627-644.

Thompson, G. G., Lauth, R. R., 2012. Chapter 2: Assessment of the Pacific Cod Stock in the Eastern Bering Sea and Aleutian Islands Area. In Stock Assessment and Fishery Evaluation Report for the Groundfish Resources of the Bering Sea/Aleutian Islands Regions, pp. 31124.

Tyrrell M. C., Link J. S., Moustahfid H., 2011. The importance of including predation in some fish population models: implications for biological reference points. Fish. Res. 108, 1-8.

Van Kirk, K. F., Quinn II, T. J., Collie, J. S., 2010. A multispecies age-structured assessment model for the Gulf of Alaska. Can. J. Fish. Aquat. Sci. 67, 1135-1148.

von Bertalanffy, L., 1938. A quantitative theory of organic growth. Hum. Biol. 10: 181-213.

Worm B., Hilborn R., Baum J. K., Branch T. A. Collie, J. S., Costello, C., Fogarty, M. J., Fulton, E. A., Hutchings, J. A., Jennings, S., Jensen, O. P., Lotze, H. K., Mace, P. M., McClanahan, T. R., Minto, C., Palumbi, S. R., Parma, A. M., Ricard, D. , Rosenberg, A. A., Watson, R., Zeller, D., 2009. Rebuilding global fisheries. Science 325, 578585

Zador S., Aydin K., Cope J., 2011. Fine-scale analysis of arrowtooth flounder Atherestes stomias catch rates reveals spatial trends in abundance. Mar. Ecol. Prog. Ser. 438, 229-239

\pagebreak

## Figures & Tables


**Table 6. Proportion mature ($\rho_{ij}$) and residual natural mortality ($\mathrm{M1}_{ij}$) for each species $i$ and age $j$ in the single-species ($SSM$) or multi-species model ($MSM$) for wallleye pollock (plk), Pacific cod (pcod), and Arrowtooth flounder (atf).**

|Age|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|
|:---------------|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|
|$\rho_{ij}$|||||||||||||||||||||
|plk|0.00|0.01|0.29|0.64|0.84|0.90|0.95|0.96|0.97|1.00|1.00|1.00||||||||| 
|pcod|0.00|0.02|0.06|0.14|0.30|0.53|0.75|0.89|0.95|0.98|0.99|1.00|||||||||
|atf|0.00|0.00|0.01|0.02|0.06|0.16|0.34|0.59|0.80|0.92|0.97|0.99|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|
|SSM $\mathrm{M1}_{ij}$|||||||||||||||||||||
|plk|0.90|0.45|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|||||||||
|pcod|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|||||||||
|atf|0.27|0.26|0.26|0.25|0.25|0.24|0.24|0.23|0.23|0.23|0.22|0.22|0.22|0.22|0.21|0.21|0.21|0.21|0.21|0.21|0.21|
|MSM $\mathrm{M1}_{ij}$|
|plk|0.01|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|0.30|||||||||
|pcod|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|0.34|||||||||
|atf|0.27|0.26|0.26|0.25|0.25|0.24|0.24|0.23|0.23|0.23|0.22|0.22|0.22|0.22|0.21|0.21|0.21|0.21|0.21|0.21|0.21|


\pagebreak

![Total observed catch (circles) and model estimates of annual catch (lines) for single- and multispecies models (note that single species lines may not be visible as they overlap with multispecies estimates).](Figs/R_figures/catch_biomass.jpg){width="75%"}

![Single- (solid) and multispecies (dashed) retrospective model estimates of total biomass, female spawning, and measured and estimated bottom-trawl survey biomass (right hand columns). Filled circles represent mean observed groundfish survey biomass and standard errors of the mean (error bars).](Figs/R_figures/plot_allBiomass.jpg){width="100%"}

![Single-species and multispecies fishery (first row) or survey selectivity (second row). Total suitability (across all prey species) for each predator age (third row).](Figs/R_figures/plot_suit2.jpg){width="90%"}

![Annual variation in total mortality ($\mathrm{M1}_{ij}$ +$\mathrm{M2}_{ij,y}$) for age 1 pollock (as prey; top), age 1 Pacific cod (as prey; middle), and age 1 arrowtooth flounder (as prey; bottom) from the single-species models (dashed), and the multispecies models with temperature (points). Updated from Holsman et al. 2016. Solid lines are a 10 y (symmetric) loess polynomial smoother indicating trends in age 1 mortality over time.](Figs/R_figures/Mortality2.jpg){width="60%"}

![Proportion of total predation mortality for age 1 pollock from pollock, Pacific cod, and arrowtooth flounder predators across years.](Figs/R_figures/plot_propM2_1.jpg){width="90%"}

<!-- ![a) Combined total predator ration (all three predators combined) over time grouped by predator. b) Total prey consumed by all three predators combined (note the log scale). c) Pollock predation mortality (M2 ; age 1 only) consumed by each predator species.](/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2023_Multispp_assessment/runs/Nov_final/assmnt_2023_0/R_figures/Eaten2.pdf){width=75%} -->

![Multispecies estimates of prey species biomass consumed by all predators in the model (points) a) total biomass of walleye pollock consumed by predators annually b) total biomass of Pacific cod consumed by predators annually, c) total biomass of arrowtooth flounder consumed by predators annually. Gray lines indicate 1979-2023 mean estimates for each species; dashed lines represent 1 standard deviation of the mean. Solid lines are a 10 y (symmetric) loess polynomial smoother indicating trends in biomass consumed over time.](Figs/R_figures/biomass_eaten_index.jpg){width="65%"}

<!-- ![Multispecies estimates of prey species biomass consumed by all predators in the model a) total biomass of walleye pollock consumed by predators annually b) total biomass of Pacific cod consumed by predators annually, c) total biomass of arrowtooth flounder consumed by predators annually. Gray lines indicate 1979-2023 mean estimates for each species. ](Figs/Holsman_Fig2.jpg){ width=75% } -->

<!-- ![Multispecies estimates of annual ration (kg consumed per individual per year) for adult (age 4 + ) predators: a) pollock, b) Pacific cod, and c) arrowtooth flounder. Gray lines indicate 1979 -2023 mean estimates and 1 SD for each species; orange line is a 10 y (symmetric) loess polynomial smoother indicating trends in ration over time.](Figs/Holsman_Fig4.jpg){ width=75% } -->

![Multispecies estimates of annual ration (kg consumed per individual per year) for adult (age 4 + ) predators: a) pollock, b) Pacific cod, and c) arrowtooth flounder. Gray lines indicate 1979 -2023 mean estimates and 1 SD for each species; orange line is a 10 y (symmetric) loess polynomial smoother indicating trends in ration over time.](Figs/R_figures/annual_ration_index.jpg){width="65%"}

![Average summer diet composition (empirically biomass weighted by survey CPUE) for juveniles (left column; < 45 cm) and adults (right column) of each predator (rows).](Figs/dietByWt_JA.png){width="100%"} 

![Patterns in prey categories consumed (panels) by each predator in the model (different colors) based on empirical annual mean diets (dashed lines; prey >50% digested removed) or averaged diets expanded by CEATTLE model estimates of annual ration (solid lines). Note that empirical diet lines where removed from arrowtooth and octopus panels; small values ~1 also removed for visual clarity).](Figs/Prey_eaten_annually.jpg){width="90%"}

![Annual single- and multispecies CEATTLE model estimates of recruitment (age 1) for pollock (top), Pacific cod (middle), and arrowtooth flounder (lower). Error bars represent 95% CI around mean estimates.](Figs/R_figures/recruitment.jpg){width="100%"}

![Spawningstock biomass and recruitment for the single-species model. Color scale text indicates cohort years summer bottom temperature (from the Bering10K ROMSNPZ model) for the EBS in $^oC$. Lines represent climate-naive Ricker stock recruit curves.](Figs/RSPLOT_0.jpg){width="75%"}

![Spawningstock biomass and recruitment for the single-species model. Color scale text indicates cohort years summer bottom temperature (from the Bering10K ROMSNPZ model) for the EBS in $^oC$. Lines represent climate-naive Ricker stock recruit curves.](Figs/RSPLOT_2.jpg){width="75%"}

![Canidate recruitment covariates from the Bering10K ROMSNPZ model provided through the ACLIM2 project.](Figs/assessment_vars.jpg){width="100%"}

![Recruitment patterns over time. Recruiment estimated from the 2023 assessment multispecies model (green dashed lines; top row) for each species (columns), and fits based on a climate-naive Ricker RS model ('mnRs_Rec'), top AIC selected climate-driven model for each species ("TopAIC_Rec"), and climate-driven model with the highest $R^2$ fit ('TopR2_Rec'). In the bottom panel the combined covariates for the top and AIC models are plotted for comparison ('TopAICcov' and 'TopR2cov').](Figs/Rec_fits_2.jpg){alt="Recruitment covariate fits for the multispecies model updated for 2023" width="100%"}

![Timeseries of single- and multispecies (solid and dashed lines, respectively) CEATTLE model estimates of fishing mortality rate for eastern Being Sea walleye pollock, Pacific cod, and arrowtooth flounder. Note that the single- and multispecies lines for pollock and arrowtooth flounder overlap.](Figs/R_figures/plot_F.jpg){width="90%"}

![Survey age compostitions for walleye pollock. Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/surv_age_comp_by_yr_1v2.jpg){width="100%"}

![Survey length compostitions for Pacific cod Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/surv_age_comp_by_yr_2v2.jpg){width="100%"}

![Survey length compostitions for arrowtooth flounder Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/surv_age_comp_by_yr_3v2.jpg){width="100%"}

![Fishery age compostitions for walleye pollock. Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/fsh_age_comp_by_yr_1v2.jpg){width="100%"}

![Fishery age compostitions for Pacific cod. Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/fsh_age_comp_by_yr_2v2.jpg){width="100%"}

![Fishery age compostitions for arrowtooth flounder. Bars represent observed values, black and gray points represent single- and multispecies fits to the data, respectively.](Figs/R_figures/fsh_age_comp_by_yr_3v2.jpg){width="100%"} 

![Single-species CEATTLE model projections of unfished ($B_0$; dashed lines) and fished spawning stock biomass at the harvest rate corresponding to the ABC proxy ($B_{40\%}$;solid lines) for the climate naive scenario ("mnhind"), high carbon mitigation (ssp126) and low carbon mitigation (ssp585) scenarios. $B_0$ from the climate naive model (left panel) was used to determine the target biomass $B_{40\%}$ each species (rows). The lines in the two carbon mitigation scenarios represent different (earth system model) realizations which differentially impact recruitment and weight at age in the model. Harvest is set at $F_{target}$ for each species.Projections assume no adaptation by the species, fishery, or fishery management. See Holsman et al. 2020 for more information and climate informed projections.](Figs/ssb_plot0.png){width="100%"}

![Multispecies CEATTLE model projections of unfished ($B_0$; dashed lines) and fished spawning stock biomass at the harvest rate corresponding to the ABC proxy ($B_{40\%}$;solid lines) for the climate naive scenario ("mnhind"), high carbon mitigation (ssp126) and low carbon mitigation (ssp585) scenarios. $B_0$ from the climate naive model (left panel) was used to determine the target biomass $B_{40\%}$ each species (rows). The lines in the two carbon mitigation scenarios represent different (earth system model) realizations which differentially impact recruitment, weight at age, and predation in the model. Harvest is set at $F_{target}$ for each species. Projections assume no adaptation by the species, fishery, or fishery management. See Holsman et al. 2020 for more information and climate informed projections.](Figs/ssb_plot2.png){width="100%"}

![As in Fig. 24, but with closer detail at 2023. Spawning stock biomass for each species at the start of each model year (multispecies). Vertical line represents end of 2023 start of 2024](Figs/ssb_plot_zoom2.png){width="100%"}

![As in Fig. 25, but with closer detail at 2023. Spawning stock biomass for each species at the start of each model year (multispecies). Vertical line represents end of 2023 start of 2024](Figs/ssb_plot_zoom2.png){width="100%"}

\pagebreak

![Table 6. Temperature-dependent Von Bertalanffy parameter (parm) estimates, standard deviation in parameter estimates (stdev), and confidence intervals (CI) from Holsman and Aydin, 2015.](Figs/vonBtable.jpg){width="65%"}



<!-- ![Climate-enhanced recruitment (log) fits and 2024 forecasted recruitment given 2023 SSB and environmental conditions.](/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2023_Multispp_assessment/docs/MSM_Assessment/Figs/projectedRec.pdf){width=100%} -->

<!-- \listoftables -->

<!-- \listoffigures -->

<!-- \tableofcontents -->
