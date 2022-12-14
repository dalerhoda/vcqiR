# vcqiR

Vaccination Coverage Quality Indicators (VCQI) 

This is the repository for the R implementation of VCQI, an open-source set of R programs meant to analyze data from vaccination coverage surveys. VCQI conducts analyses that are consistent with recent guidance from the World Health Organization (WHO) in the [2018 Vaccination Coverage Cluster Survey Reference Manual](https://www.who.int/immunization/documents/who_ivb_18.09/en/). VCQI documentation can be found in the Documentation directory in this repository. A [VCQI User's Group](http://www.technet-21.org/en/network/groups/293-vcqi) is hosted on the [TechNet-21](http://www.technet-21.org/en/) website. The user's group includes a forum for discussion and links to tutorial materials. Users are welcome to submit issues to this repository, but the main thread for discussing VCQI development plans will be in the user's group forum. Links to VCQI resources may also be found at [this website](http:/www.biostatglobal.com/VCQI_RESOURCES.html).

To install this package, ensure you have R version 4.2.1 or later, and then run the following commands: 

``` r
if (!requireNamespace("pak")){install.packages("pak")}

pak::pkg_install("BiostatGlobalConsulting/vcqiR")
```

The R version of VCQI is being developed in stages. There are modules and indicators in the [Stata version](https://github.com/BiostatGlobalConsulting/vcqi-stata-public) that do not yet appear in the R version. As time and funding allow, our intention is to implement all of VCQI’s capabilities in R, too. 

At this time, the R version of VCQI analyzes data from routine immunization (RI) surveys only, and calculates the following indicators: 
- RI_COVG_01 – crude coverage
- RI_COVG_02 – valid coverage
- RI_QUAL_07B – what would valid coverage be if every child had received every dose that was due at every one of their documented vaccination visits
- RI_QUAL_08 – percentage of documented vaccination visits that include 1+ missed opportunities for simultaneous vaccination (MOSV)
- RI_QUAL_09 – percentage of children with 1+ missed opportunities for simultaneous vaccination (MOSV)
- RI_VCTC_01 – coverage and timeliness charts (the R version does not currently show HBR availability or % fully vaccinated or % not vaccinated, but includes all other VCTC features)
