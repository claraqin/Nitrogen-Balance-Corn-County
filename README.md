Create Validation Data from CDL & NEI & NASS
================
Kenneth Qin
2017-06-22

Background
----------

This is a part of the EDF Sustainable Agriculture program's pilot project for the remote sensing of excess nitrogen on cultivated lands.

The main barrier to successfully deploying a satellite-based remote sensing system for targeted fertilizer conservation programs is the lack of sufficient ground-truthed field-scale fertilizer application data. In theory, if we could receive annual reports of yield, crop type, and nitrogen (N) fertilizer application rate from each field in the U.S., we could develop a crude N-balance prediction system that operates at the field-scale. However, this data is elusive in part because of the legitimate concern for farmers' privacy rights. A recent study from Iowa State University reported that "30% of farmers agreed that government use of remote sensing and other tools to identify issues on private land is an invasion of privacy" (Arbuckle, 2013).

In this pilot project, **we do not attempt to predict field-scale attributes using satellite remote sensing**. Instead, we attempt to predict the mean and heterogeneity of **N-balance at the county scale**.

County-scale information on total N fertilizer usage was made available in the EPA's 2011 National Emissions Inventory (NEI) report. When combined with the Cropland Data Layer's pixel counts of each land cover type for each county, we can identify those counties where the vast majority (i.e. &gt; 95%) of all cultivated area consists of corn/soybean, and make the assumption that all N fertilizer within the county was applied to corn acreage. Through this process we can arrive at a **mean fertilizer application rate for corn** for a large subset of counties.

County-level yield data can be retrieved from the USDA National Agricultural Statistics Service (NASS) via Quick Stats.

**County-level N-balance for corn** can then be calculated through the following equation:

*N*.*b**a**l**a**n**c**e* = *N*.*i**n**p**u**t* − *N*.*o**u**t**p**u**t* = *N*.*i**n**p**u**t* − *C**r**o**p*.*P**r**o**d**u**c**t**i**o**n* \* *N*.*r**e**m**o**v**a**l*.*r**a**t**e*

This simplified N-balance equation is sufficient for our purpose of generating county-level validation data, though we recognize that we are making the following assumptions:

-   Corn stover / stubble is not removed from the field.
-   The grain has the standard moisture content of 15.5%.
-   The grain has a constant N concentration of 1.4%.

*See <http://www.ipni.net/article/IPNI-3296> for details on the grain N calculation.*

Once N-balance has been calculated for each county, we will have produced a dataset for use in validating an *a priori* model that converts vegetation indices collected through satellite remote sensing into county-level N-balance predictions.

Analysis
--------

Define constants, load libraries, and import data.

``` r
# Constants

SQ_METER_PER_PIXEL <- 900
SQ_METER_PER_ACRE <- 4046.86
ACRES_PER_SQ_MILE <- 640

# Libraries

library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.3.3

``` r
# Data imports

#setwd("C:/Users/kqin/Documents/Remote Sensing/Remote Sensing of N/Validation Data/CDL")

fips <- read.csv("fips_codes.csv")

CDL2016 <- read.csv("CDL_Cnty_Pixel_2016_30m.csv")

CDL.names <- read.csv("CDL_class_names.csv")
cultivated.codes <- CDL.names$CODE[which(CDL.names$CULTIVATED)]
cultivated.codes.str <- str_pad(cultivated.codes, 3, side="left", "0")
cultivated.cats <- paste("Category_", cultivated.codes.str, sep="")

Corn2011 <- read.csv("corn_county_production_2011.csv")
```

### Part 1: Find counties where (nearly) all fertilizer went into corn

Merge county pixel-count data with county FIPS-code data.

``` r
fips$Fips <- fips$STATEFP*1000 + fips$COUNTYFP
CDL2016.m <- merge(fips, CDL2016, by="Fips")
```

Convert pixel-counts into sq. miles and acres.

``` r
CDL2016.sqm <- CDL2016.m
CDL2016.sqm[1:nrow(CDL2016.sqm),8:ncol(CDL2016.sqm)] <- 
  CDL2016.sqm[1:nrow(CDL2016.sqm),8:ncol(CDL2016.sqm)] * SQ_METER_PER_PIXEL

CDL2016.ac <- CDL2016.sqm
CDL2016.ac[1:nrow(CDL2016.ac),8:ncol(CDL2016.ac)] <- 
  CDL2016.ac[1:nrow(CDL2016.ac),8:ncol(CDL2016.ac)] / SQ_METER_PER_ACRE
```

Calculate total area and total cultivated area in each county.

``` r
CDL2016.ac$Total_ac <- rowSums(select(CDL2016.ac, Category_001:Category_254))
CDL2016.ac$Total_sqmi <- CDL2016.ac$Total_ac/ACRES_PER_SQ_MILE

CDL2016.ac$Total_cult_ac <- rowSums(select(CDL2016.ac, one_of(cultivated.cats)))
```

    ## Warning in one_of(cultivated.cats): Unknown variables: `Category_210`,
    ## `Category_230`, `Category_231`, `Category_233`, `Category_234`,
    ## `Category_235`, `Category_239`

``` r
CDL2016.ac$Total_cult_sqmi <- CDL2016.ac$Total_cult_ac/ACRES_PER_SQ_MILE

# Ensure that cultivated area is less than total area
prop.cultivated <- CDL2016.ac$Total_cult_ac / CDL2016.ac$Total_ac
hist(prop.cultivated, main="Hist. of proportion of area cultivated")
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

Calculate proportion of corn+soybean out of total CULTIVATED AREA.

``` r
CDL2016.ac.select <- select(CDL2016.ac, -(Category_001:Category_254))

CDL2016.ac.select %>%
  mutate(prop_corn = Corn_all / Total_cult_ac,
         prop_soybean = Soybeans_all / Total_cult_ac,
         prop_cornsoybean = (Corn_all + Soybeans_all) / Total_cult_ac) ->
  CDL2016.ac.select

hist(CDL2016.ac.select$prop_cornsoybean, main="Hist. of proportion of cultivated area\nconsisting of corn or soy")
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Identify counties with over 95% of cultivated acres consisting of corn or soy.

First, what proportion of counties have over 95% of their cultivated acreage consist of corn or soy?

``` r
# Answer: For 95% threshold, 18% of counties (N=568).
#         For 90% threshold, 26% of counties (N=806).
mean(CDL2016.ac.select$prop_cornsoybean > 0.95, na.rm=TRUE)
```

    ## [1] 0.1829308

``` r
# Record these counties' fips codes
ind.95.cornsoy <- which(CDL2016.ac.select$prop_cornsoybean > 0.95)
cornsoy.counties.fips <- CDL2016.ac.select$Fips[ind.95.cornsoy]
```

Where are these counties?

``` r
# Where are these counties?
CDL2016.ac.select[ind.95.cornsoy,] %>%
  group_by(STATE) %>%
  summarise(state_n = n()) %>%
  arrange(desc(state_n)) ->
  CDL.95.statecount
CDL.95.statecount
```

    ## # A tibble: 22 × 2
    ##     STATE state_n
    ##    <fctr>   <int>
    ## 1      IL      97
    ## 2      IA      76
    ## 3      KY      70
    ## 4      MO      70
    ## 5      IN      66
    ## 6      TN      49
    ## 7      NE      34
    ## 8      MN      19
    ## 9      OH      17
    ## 10     MS      16
    ## # ... with 12 more rows

What else do these counties grow?

``` r
# What are the other crops in these counties?
CDL2016.ac.select[ind.95.cornsoy,] %>%
  select(Corn_all:Lettuce_all) %>%
  summarise_each(funs(sum)) ->
  temp
ggplot(melt(temp), aes(x=variable, y=value)) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  ggtitle("Crop Acreage Distributions within High-Corn/Soy Counties")
```

    ## No id variables; using all as measure variables

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

Aside from corn and soybean, these counties also grow a substantial amount of winter wheat. Winter wheat removes almost twice as much N as corn, which may bias the corn N-balance downwards, but this bias will probably be balanced out by the remainder of crops taking less N than corn. Besides, these crops only make up to 5% of total acreage in these counties.

### Part 2: Extract N fertilizer data from NEI 2011 for this subset of counties

### Part 3: Calculate N-balance for this subset of counties

First create standard FIPS code within Corn data.

``` r
Corn2011$Fips <- Corn2011$State.ANSI*1000 + Corn2011$County.ANSI
```

Reshape Corn data and calculate total corn production (in bushels) for each county.

``` r
Corn2011.w <- dcast(Corn2011, Fips + State + County ~ Data.Item, value.var="Value", fun.aggregate = sum)

names(Corn2011.w)[4] <- "Prod.bu"
```

Assuming IPNI nutrient removal rate of **0.67 lbs. N / bu**, calculate county-scale N-balance (lbs. N / ac).

``` r
Nbalance <- merge(CDL2016.ac.select, Corn2011.w)
Nbalance.cornsoy <- filter(Nbalance, Fips %in% cornsoy.counties.fips)

dim(Nbalance.cornsoy)
```

    ## [1] 472  27

Where are these counties?

``` r
Nbalance.cornsoy %>%
  group_by(STATE) %>%
  summarise(state_n = n()) %>%
  arrange(desc(state_n)) ->
  Nbalance.statecount
Nbalance.statecount
```

    ## # A tibble: 17 × 2
    ##     STATE state_n
    ##    <fctr>   <int>
    ## 1      IL      94
    ## 2      IA      76
    ## 3      IN      60
    ## 4      MO      59
    ## 5      KY      54
    ## 6      NE      34
    ## 7      TN      31
    ## 8      MN      19
    ## 9      OH      16
    ## 10     VA      11
    ## 11     KS       5
    ## 12     NC       5
    ## 13     MS       3
    ## 14     AL       2
    ## 15     AR       1
    ## 16     MD       1
    ## 17     SD       1
