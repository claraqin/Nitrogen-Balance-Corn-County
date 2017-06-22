# Cropland Data Layer County Crop Estimates

SQ_METER_PER_PIXEL <- 900
SQ_METER_PER_ACRE <- 4046.86
ACRES_PER_SQ_MILE <- 640

library(dplyr)
library(stringr)

# setwd("C:/Users/vtostado/Dropbox/EDF Sustainable Ag Data/CDL/County_Pixel_Count")
setwd("~/Dropbox/EDF Sustainable Ag Data/CDL/County_Pixel_Count")

category.names <- read.csv("class_names.csv")
cultivated.codes <- category.names$CODE[which(category.names$CULTIVATED)]
cultivated.codes.str <- str_pad(cultivated.codes, 3, side="left", "0")
cultivated.cats <- paste("Category_", cultivated.codes.str, sep="")

fips <- read.csv("fips_codes.csv")
fips$Fips <- fips$STATEFP*1000 + fips$COUNTYFP

CDL2016 <- read.csv("CDL_Cnty_Pixel_2016_30m.csv")

CDL2016.m <- merge(fips, CDL2016, by="Fips")

CDL2016.sqm <- CDL2016.m
CDL2016.sqm[1:nrow(CDL2016.sqm),8:ncol(CDL2016.sqm)] <- 
  CDL2016.sqm[1:nrow(CDL2016.sqm),8:ncol(CDL2016.sqm)] * SQ_METER_PER_PIXEL

CDL2016.ac <- CDL2016.sqm
CDL2016.ac[1:nrow(CDL2016.ac),8:ncol(CDL2016.ac)] <- 
  CDL2016.ac[1:nrow(CDL2016.ac),8:ncol(CDL2016.ac)] / SQ_METER_PER_ACRE

write.csv(CDL2016.ac, "CDL_Cnty_Acre_2016.csv", row.names=FALSE)


# Okay to use this instead of other table with calculate acreage?

# original files already had calculated acreage, though to less precision
CDL2016.acs <- read.csv("CDL_Cnty_Acres_2016.csv")
# Compare my calculated acreage with their calculated acreage
rm.ind1 <- which(!(CDL2016.ac$Fips %in% CDL2016.acs$Fips))
resid <- as.matrix(CDL2016.ac[-rm.ind1,8:ncol(CDL2016.ac)]) - 
  as.matrix(CDL2016.acs[,3:ncol(CDL2016.acs)])
plot(log(as.matrix(CDL2016.ac[-rm.ind1,8:ncol(CDL2016.ac)])) ~
       log(as.matrix(CDL2016.acs[,3:ncol(CDL2016.acs)])))
abline(a=0,b=1,col='red')
# Looks fine, of course a little less accurate for lower-pixel counties
rm(CDL2016.acs)


# Calculate total area in each county
# Calculate total cultivated area in each county

CDL2016.ac$Total_ac <- rowSums(select(CDL2016.ac, Category_001:Category_254))
CDL2016.ac$Total_sqmi <- CDL2016.ac$Total_ac/ACRES_PER_SQ_MILE

CDL2016.ac$Total_cult_ac <- rowSums(select(CDL2016.ac, one_of(cultivated.cats)))
CDL2016.ac$Total_cult_sqmi <- CDL2016.ac$Total_cult_ac/ACRES_PER_SQ_MILE

hist(CDL2016.ac$Total_cult_ac / CDL2016.ac$Total_ac, main="Hist of % acres cultivated")

CDL2016.ac.select <- select(CDL2016.ac, -(Category_001:Category_254))


# Calculate proportion of corn, soybean, and corn+soybean
# out of total CULTIVATED AREA

CDL2016.ac.select %>%
  mutate(prop_corn = Corn_all / Total_cult_ac,
         prop_soybean = Soybeans_all / Total_cult_ac,
         prop_cornsoybean = (Corn_all + Soybeans_all) / Total_cult_ac) ->
  CDL2016.ac.select

hist(CDL2016.ac.select$prop_corn)
hist(CDL2016.ac.select$prop_cornsoybean)

# What proportion of counties have over 95% of their cultivated acreage
# as corn and soy? 
# Answer: For 95% threshold, 18% of counties (N=568).
#         For 90% threshold, 26% of counties (N=806).
mean(is.na(CDL2016.ac.select$prop_cornsoybean)) # only 0.1% are NA
mean(CDL2016.ac.select$prop_cornsoybean > 0.95, na.rm=TRUE)
mean(CDL2016.ac.select$prop_cornsoybean > 0.9, na.rm=TRUE)

# Record these counties' fips codes
ind.95.cornsoy <- which(CDL2016.ac.select$prop_cornsoybean > 0.95)
cornsoy.counties <- CDL2016.ac.select$Fips[ind.95.cornsoy]

# Where are these counties?
CDL2016.ac.select[ind.95.cornsoy,] %>%
  group_by(STATE) %>%
  summarise(state_n = n()) %>%
  arrange(state_n) ->
  CDL.95.statecount

# What are the other crops in these counties?
plot(colSums(CDL2016.ac.select[ind.95.cornsoy,8:17]))
names(CDL2016.ac.select)[8:17]
# Mostly winter wheat, which takes almost twice as much N
# but it'll probably be balanced out by the remainder of crops taking less N
# and besides, these crops only make up to 5% of total acreage here


