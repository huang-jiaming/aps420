install.packages("tidyverse")
install.packages('abind')

library(abind)
library(data360r)
library(tidyverse)
library(reshape2)
library(rworldmap)
library(ggpmisc)
library(RColorBrewer)
library(gganimate)
library(gridExtra)
library(dplyr)

GDP_percapita <- get_data360(site = "tc",
                             indicator_id = 944,
                             dataset_id = NULL,
                             country_iso3 = NULL,
                             timeframes = c(2019), #comma for list, colon for range 
                             output_type = "wide")

country_data <- get_metadata360(site = "tc",
                                metadata_type = "countries")

merged <- merge(GDP_percapita, country_data,
                by.x = "Country ISO3", by.y = "iso3")

names(merged)[names(merged) == '2019'] <- 'gdp'

ggplot(merged, aes(x = gdp, y = geo.lat)) + 
  geom_point(aes(colour = incomeLevel)) +
  xlab("GDP per Capita") +
  ylab("Latitude") +
  ggtitle("Latitude versus GDP per Capita in 2019")

model_2019 = lm(gdp~abs(geo.lat), data=merged)

summary(model_2019)

merged %>%
  count(incomeLevel)

stem_avail <- get_data360(site = "tc",
                         indicator_id = 607,
                         dataset_id = NULL,
                         country_iso3 = NULL,
                         period = c(2017:2018), 
                         output_type = "long")

cp <- brewer.pal(9,'Greens')
mapz <- joinCountryData2Map(stem_avail, joinCode = "ISO3", nameJoinColumn = "Country ISO3", nameCountryColumn = "Country Name")
mapParams <- mapCountryData(mapz, nameColumnToPlot="Observation", 
                            addLegend=TRUE, mapTitle="Availability of Scientists and Engineers", mapRegion="World", 
                            catMethod="quantiles", numCats=9, colourPalette=cp, 
                            borderCol="black")

cp <- brewer.pal(9,'Greens')
mapz <- joinCountryData2Map(GDP_percapita, joinCode = "ISO3", nameJoinColumn = "Country ISO3", nameCountryColumn = "Country Name")
mapParams <- mapCountryData(mapz, nameColumnToPlot="2019", 
                            addLegend=TRUE, mapTitle="GDP per capita", mapRegion="World", 
                            catMethod="quantiles", numCats=9, colourPalette=cp, 
                            borderCol="black")

GDP_percapita <- get_data360(site = "tc",
                             indicator_id = 944,
                             dataset_id = NULL,
                             country_iso3 = NULL,
                             timeframes = c(2018), #comma for list, colon for range 
                             output_type = "wide")

stem_avail_gdp <- merge(GDP_percapita, stem_avail,
                by.x = "Country ISO3", by.y = "Country ISO3")

stem_avail_gdp <- stem_avail_gdp[stem_avail_gdp$Period == '2017-2018',]

names(stem_avail_gdp)[names(stem_avail_gdp) == '2018'] <- 'gdp'

ggplot(stem_avail_gdp, aes(x = gdp, y = Observation)) + 
  geom_point(aes(colour = gdp)) +
  xlab("GDP per Capita") +
  ylab("Availability of Scientists and Engineers") +
  ggtitle("Availability of Scientists and Engineers versus GDP per Capita in 2017-2018")

model_stem = lm(gdp~Observation, data=stem_avail_gdp)

summary(model_stem)

gender_equal <- get_data360(site = "tc",
                             indicator_id = 41933,
                             dataset_id = NULL,
                             country_iso3 = NULL,
                             #timeframes = c(2019), #comma for list, colon for range 
                             output_type = "wide")

gender_equal_clean <- gender_equal[, c(1, 2, 3, 4, 49)]

GDP_growth <- get_data360(site = "tc",
                            indicator_id = 940,
                            dataset_id = NULL,
                            country_iso3 = NULL,
                            #timeframes = c(2019), #comma for list, colon for range 
                            output_type = "wide")

GDP_growth_clean <- GDP_growth[, c(1, 2, 3, 4, 63)]

gender_growth <- merge(GDP_growth_clean, gender_equal_clean,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(gender_growth)[names(gender_growth) == '2019.x'] <- 'gdp'
names(gender_growth)[names(gender_growth) == '2019.y'] <- 'gender'

ggplot(gender_growth, aes(x = gdp, y = gender)) + 
  geom_point(aes(colour = gdp)) +
  xlab("GDP Growth") +
  ylab("Gender Equality Index") +
  ggtitle("Gender Equality Index versus GDP Growth in 2019")

model_gender = lm(gdp~gender, data=gender_growth)

summary(model_gender)

GDP_percapita_7 <- get_data360(site = "tc",
                             indicator_id = 944,
                             dataset_id = NULL,
                             country_iso3 = NULL,
                             timeframes = c(2019), #comma for list, colon for range 
                             output_type = "wide")

gender_capita <- merge(GDP_percapita_7, gender_equal_clean,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(gender_capita)[names(gender_capita) == '2019.x'] <- 'gdp'
names(gender_capita)[names(gender_capita) == '2019.y'] <- 'gender'

ggplot(gender_capita, aes(x = gdp, y = gender)) + 
  geom_point(aes(colour = gdp)) +
  xlab("GDP per Capita") +
  ylab("Gender Equality Index") +
  ggtitle("Gender Equality Index versus GDP per Capita in 2019")

model_gender_capita = lm(gdp~gender, data=gender_capita)

summary(model_gender_capita)

gender_equal$growth <- (gender_equal$`2019` - gender_equal$`2014`)

gender_equal_growth <- gender_equal[, c(1, 2, 3, 4, 50)]

GII <- get_data360(site = "tc",
                                   indicator_id = 40712,
                                   dataset_id = NULL,
                                   country_iso3 = NULL,
                                   #timeframes = c(2019), #comma for list, colon for range 
                                   output_type = "wide")

GII_2015 <- GII[, c(1, 2, 3, 4, 7)]
GII_2020 <- GII[, c(1, 2, 3, 4, 12)]

political_stability <- get_data360(site = "tc",
                            indicator_id = 40266,
                            dataset_id = NULL,
                            country_iso3 = NULL,
                            #timeframes = c(2019), #comma for list, colon for range 
                            output_type = "wide")

merge_political <- merge(GII_2015, political_stability,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_political)[names(merge_political) == '2015.x'] <- 'GII'
names(merge_political)[names(merge_political) == '2015.y'] <- 'political'

ggplot(merge_political, aes(x = GII, y = political)) + 
  geom_point(aes(colour = GII)) +
  xlab("GII Index") +
  ylab("Political Stability") +
  ggtitle("Political Stability versus GII Index in 2015")

model_political = lm(GII~political, data=merge_political)

summary(model_political)

institutions <- get_data360(site = "tc",
                                   indicator_id = 40262,
                                   dataset_id = NULL,
                                   country_iso3 = NULL,
                                   #timeframes = c(2019), #comma for list, colon for range 
                                   output_type = "wide")

institutions_clean <- institutions[, c(1, 2, 3, 4, 12)]

merge_institutions <- merge(GII_2020, institutions_clean,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_institutions)[names(merge_institutions) == '2020.x'] <- 'GII'
names(merge_institutions)[names(merge_institutions) == '2020.y'] <- 'institutions'

ggplot(merge_institutions, aes(x = GII, y = institutions)) + 
  geom_point(aes(colour = GII)) +
  xlab("GII Index") +
  ylab("Institutions") +
  ggtitle("Institutions versus GII Index in 2020")

model_institutions = lm(GII~institutions, data=merge_institutions)

summary(model_institutions)

credit <- get_data360(site = "tc",
                                   indicator_id = 40418,
                                   dataset_id = NULL,
                                   country_iso3 = NULL,
                                   #timeframes = c(2019), #comma for list, colon for range 
                                   output_type = "wide")

credit_clean <- credit[, c(1, 2, 3, 4, 12)]

merge_credit <- merge(GII_2020, credit_clean,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_credit)[names(merge_credit) == '2020.x'] <- 'GII'
names(merge_credit)[names(merge_credit) == '2020.y'] <- 'credit'

ggplot(merge_credit, aes(x = GII, y = credit)) + 
  geom_point(aes(colour = GII)) +
  xlab("GII Index") +
  ylab("Credit") +
  ggtitle("Ease of Getting Credit versus GII Index in 2020")

model_credit = lm(GII~credit, data=merge_credit)

summary(model_credit)

entrepreneurship <- get_data360(site = "tc",
                                   indicator_id = 3111,
                                   dataset_id = NULL,
                                   country_iso3 = NULL,
                                   #timeframes = c(2019), #comma for list, colon for range 
                                   output_type = "wide")

entrepreneurship_clean <- entrepreneurship[, c(1, 2, 3, 4, 22)]

merge_entrepreneurship <- merge(GII_2020, entrepreneurship_clean,
                       by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_entrepreneurship)[names(merge_entrepreneurship) == '2020.x'] <- 'GII'
names(merge_entrepreneurship)[names(merge_entrepreneurship) == '2020.y'] <- 'entrepreneurship'

ggplot(merge_entrepreneurship, aes(x = GII, y = entrepreneurship)) + 
  geom_point(aes(colour = GII)) +
  xlab("GII Index") +
  ylab("Entrepreneurship") +
  ggtitle("Entrepreneurship as a Desirable Career Choice versus GII Index in 2020")

model_entrepreneurship = lm(GII~entrepreneurship, data=merge_entrepreneurship)

summary(model_entrepreneurship)

institutions_clean_GDP <- institutions[, c(1, 2, 3, 4, 10)]

merge_institutions_GDP <- merge(GDP_percapita, institutions_clean_GDP,
                            by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_institutions_GDP)[names(merge_institutions_GDP) == '2018.x'] <- 'gdp'
names(merge_institutions_GDP)[names(merge_institutions_GDP) == '2018.y'] <- 'institutions'

ggplot(merge_institutions_GDP, aes(x = gdp, y = institutions)) + 
  geom_point(aes(colour = gdp)) +
  xlab("GDP per Capita") +
  ylab("Institutions") +
  ggtitle("Institutions versus GDP per Capita in 2018")

model_institutions_GDP = lm(gdp~institutions, data=merge_institutions_GDP)

summary(model_institutions_GDP)

merge_GII_GDP <- merge(GDP_percapita, GII,
                                by.x = "Country ISO3", by.y = "Country ISO3")

names(merge_GII_GDP)[names(merge_GII_GDP) == '2018.x'] <- 'gdp'
names(merge_GII_GDP)[names(merge_GII_GDP) == '2018.y'] <- 'gii'

ggplot(merge_GII_GDP, aes(x = gdp, y = gii)) + 
  geom_point(aes(colour = gdp)) +
  xlab("GDP per Capita") +
  ylab("GII Index") +
  ggtitle("GII Index versus GDP per Capita in 2018")

model_GII_GDP = lm(gdp~gii, data=merge_GII_GDP)

summary(model_GII_GDP)

life_expectancy_male <- get_data360(site = "tc",
                               indicator_id = 837,
                               dataset_id = NULL,
                               country_iso3 = NULL,
                               #timeframes = c(1960:2018), #comma for list, colon for range 
                               output_type = "wide")

life_expectancy_female <- get_data360(site = "tc",
                                    indicator_id = 836,
                                    dataset_id = NULL,
                                    country_iso3 = NULL,
                                    #timeframes = c(1960:2018), #comma for list, colon for range 
                                    output_type = "wide")

life_expectancy_male <- life_expectancy_male[,-c(1:4) ]
life_expectancy_female <- life_expectancy_female[,-c(1:4) ]

life_expectancy_average <- (life_expectancy_male+life_expectancy_female)/2
life_expectancy_average = cbind(life_expectancy_average, Max_Difference = 0)
life_expectancy_average = cbind(life_expectancy_average, Max = 0)
life_expectancy_average = cbind(life_expectancy_average, Min_Difference = 0)
life_expectancy_average = cbind(life_expectancy_average, Min = 0)

life_expectancy_average <- na.omit(life_expectancy_average)

life_expectancy_average <- data.matrix(life_expectancy_average, rownames.force = NA)
for(i in 1:187) {
  life_expectancy_average[i, 61] <- max(diff(life_expectancy_average[i,1:60]))
  life_expectancy_average[i, 62] <- which.max(diff(life_expectancy_average[i,1:60])) + 2
  life_expectancy_average[i, 63] <- min(diff(life_expectancy_average[i,1:60]))
  life_expectancy_average[i, 64] <- which.min(diff(life_expectancy_average[i,1:60]))+ 2
}

life_expectancy_average <- as.data.frame(life_expectancy_average)

life_expectancy_average <- cbind(a = 0, life_expectancy_average)
life_expectancy_average <- cbind(b = 0, life_expectancy_average)

life_expectancy_male <- get_data360(site = "tc",
                                    indicator_id = 837,
                                    dataset_id = NULL,
                                    country_iso3 = NULL,
                                    #timeframes = c(1960:2018), #comma for list, colon for range 
                                    output_type = "wide")

life_expectancy_male <- na.omit(life_expectancy_male)

life_expectancy_average$b = life_expectancy_male$`Country ISO3`
life_expectancy_average$a = life_expectancy_male$`Country Name`
names(life_expectancy_average)[names(life_expectancy_average) == 'b'] <- 'Country ISO3'
names(life_expectancy_average)[names(life_expectancy_average) == 'a'] <- 'Country Name'

life_expectancy_melt <- life_expectancy_average[,-c(1:2) ]
life_expectancy_melt <- life_expectancy_melt[,-c(61:64) ]
life_expectancy_melt <- as.data.frame(t(as.matrix(life_expectancy_melt)))
life_expectancy_melt <- life_expectancy_melt[c(92,146)]
life_expectancy_melt$Year <- 1960:2019

life_expectancy_melt <- data.frame(x = life_expectancy_melt$Year,                           
                          y = c(life_expectancy_melt$V92, life_expectancy_melt$V146),
                          Country = c(rep("Cambodia", nrow(life_expectancy_melt)),
                                    rep("Rwanda", nrow(life_expectancy_melt))))

ggplot(life_expectancy_melt, aes(x, y, col = Country)) +
  geom_line() +
  xlab("Year") +
  ylab("Life Expectancy at Birth") +
  ggtitle("Life Expectancy at Birth in Rwanda and Cambodia Between 1960 and 2019")

life_expectancy_by_region <- merge(life_expectancy_average, country_data, by.x = 'Country ISO3', by.y = 'iso3')
life_expectancy_by_region <- life_expectancy_by_region[,-c(63:70) ]
life_expectancy_by_region <- life_expectancy_by_region[,-c(64:68) ]
life_expectancy_by_region <- life_expectancy_by_region[,-c(1:2) ]

life_expectancy_by_region[nrow(life_expectancy_by_region) + 1,] = c(0)

for(i in 1:60) {
  life_expectancy_by_region[188, i] <-mean(life_expectancy_by_region[1:187, i])
}

life_expectancy_by_region[188,61] = 'World'
life_expectancy_by_region <- na.omit(life_expectancy_by_region)
life_expectancy_by_region<-life_expectancy_by_region[(life_expectancy_by_region$adminRegion =="World" | life_expectancy_by_region$adminRegion == "SSA"),]

life_expectancy_by_region[nrow(life_expectancy_by_region) + 1,] = c(0)

for(i in 1:60) {
  life_expectancy_by_region[48, i] <-mean(life_expectancy_by_region[1:47, i])
}

life_expectancy_by_region[48,61] = 'SSA'
life_expectancy_by_region <- life_expectancy_by_region[-c(1:46),] 

life_expectancy_melt <- as.data.frame(t(as.matrix(life_expectancy_by_region)))
life_expectancy_melt$Year <- 1960:2020
life_expectancy_melt[61,1] = 'World'
life_expectancy_melt[61,2] = 'SSA'
names(life_expectancy_melt)[names(life_expectancy_melt) == '188'] <- 'World'
names(life_expectancy_melt)[names(life_expectancy_melt) == '48'] <- 'SSA'
life_expectancy_melt <- life_expectancy_melt[-c(61),] 

life_expectancy_melt <- data.frame(x = life_expectancy_melt$Year,                           
                                   y = c(life_expectancy_melt$SSA, life_expectancy_melt$World),
                                   Country = c(rep("Africa", nrow(life_expectancy_melt)),
                                               rep("World", nrow(life_expectancy_melt))))

life_expectancy_melt$y <- as.numeric(life_expectancy_melt$y)


ggplot(life_expectancy_melt, aes(x, y, col = Country)) +
  geom_line() +
  xlab("Year") +
  ylab("Life Expectancy at Birth") +
  ggtitle("Life Expectancy at Birth in Sub-Saharan Africa and the World Between 1960 and 2019")

life_expectancy_average<-life_expectancy_average[(life_expectancy_average$'Country ISO3' =="BGD" | 
                                                    life_expectancy_average$'Country ISO3' =="KOR" |
                                                    life_expectancy_average$'Country ISO3' =="MYS"),]

life_expectancy_average <- life_expectancy_average[,-c(1:2,63:66) ]

life_expectancy_average <- as.data.frame(t(as.matrix(life_expectancy_average)))
names(life_expectancy_average)[names(life_expectancy_average) == '16'] <- 'BGD'
names(life_expectancy_average)[names(life_expectancy_average) == '94'] <- 'KOR'
names(life_expectancy_average)[names(life_expectancy_average) == '121'] <- 'MYS'

life_expectancy_average$Year <- 1960:2019

life_expectancy_average <- data.frame(x = life_expectancy_average$Year,                           
                                   y = c(life_expectancy_average$BGD, life_expectancy_average$KOR, life_expectancy_average$MYS),
                                   Country = c(rep("Bangladesh", nrow(life_expectancy_average)),
                                               rep("Korea", nrow(life_expectancy_average)),
                                               rep("Malaysia", nrow(life_expectancy_average))))

life_expectancy_average$y <- as.numeric(life_expectancy_average$y)


ggplot(life_expectancy_average, aes(x, y, col = Country)) +
  geom_line() +
  xlab("Year") +
  ylab("Life Expectancy at Birth") +
  ggtitle("Life Expectancy at Birth in Bangladesh, Korea, and Malaysia Between 1960 and 2019")

GDP_percapita_4 <- get_data360(site = "tc",
                               indicator_id = 944,
                               dataset_id = NULL,
                               country_iso3 = c('BGD', 'KOR', 'MYS'),
                               timeframes = c(1960:2019), #comma for list, colon for range 
                               output_type = "wide")

GDP_percapita_4 <- GDP_percapita_4[,-c(1:4) ]

GDP_percapita_4 <- as.data.frame(t(as.matrix(GDP_percapita_4)))
names(GDP_percapita_4)[names(GDP_percapita_4) == 'V1'] <- 'BGD'
names(GDP_percapita_4)[names(GDP_percapita_4) == 'V2'] <- 'KOR'
names(GDP_percapita_4)[names(GDP_percapita_4) == 'V3'] <- 'MYS'

GDP_percapita_4$Year <- 1960:2019

GDP_percapita_4 <- data.frame(x = GDP_percapita_4$Year,                           
                                      y = c(GDP_percapita_4$BGD, GDP_percapita_4$KOR, GDP_percapita_4$MYS),
                                      Country = c(rep("Bangladesh", nrow(GDP_percapita_4)),
                                                  rep("Korea", nrow(GDP_percapita_4)),
                                                  rep("Malaysia", nrow(GDP_percapita_4))))

GDP_percapita_4$y <- as.numeric(GDP_percapita_4$y)


ggplot(GDP_percapita_4, aes(x, y, col = Country)) +
  geom_line() +
  xlab("Year") +
  ylab("GDP per Capita") +
  ggtitle("GDP per Capita in Bangladesh, Korea, and Malaysia Between 1960 and 2019")

internet <- get_data360(site = "tc",
                        indicator_id = 591,
                        dataset_id = NULL,
                        country_iso3 = NULL,
                        #timeframes = c(2017), #comma for list, colon for range 
                        output_type = "wide")

#fetch 2017-2018 data (most recent)
internet_clean <- internet[, c(1, 2, 3, 4, 15)]

landlocked = read.csv("landlocked_dataset.csv")  # read csv file 

names(internet_clean)[names(internet_clean) == 'Country ISO3'] <- 'iso3'

landlocked_internet = merge(x=internet_clean,y=landlocked,by="iso3",all=TRUE)

landlocked_internet <- landlocked_internet[, -c(4,6,7)]

landlocked_internet <- na.omit(landlocked_internet)

landlocked_internet_group <- landlocked_internet %>% 
  group_by(landlocked)

ggplot(landlocked_internet_group, aes(y= `2017-2018`)) +
  geom_boxplot(aes(landlocked)) +
  xlab("LLDC or Non-LLDC") +
  ylab("Fixed Broadband Internet Subscriptions") +
  ggtitle("Fixed Broadband Internet Subscriptions in LLDCs and Non-LLDCs Between 2017 and 2018")

internet <- get_data360(site = "tc",
                        indicator_id = 591,
                        dataset_id = NULL,
                        country_iso3 = NULL,
                        #timeframes = c(2017), #comma for list, colon for range 
                        output_type = "wide")

landlocked = read.csv("landlocked_dataset.csv")  # read csv file 

names(internet)[names(internet) == 'Country ISO3'] <- 'iso3'

landlocked_internet = merge(x=internet,y=landlocked,by="iso3",all=TRUE)

landlocked_internet <- landlocked_internet[, -c(4,6,7)]

landlocked_internet <- na.omit(landlocked_internet)
landlocked_internet <- as.data.frame((as.matrix(landlocked_internet)))

landlocked_internet[nrow(landlocked_internet) + 1,] = c(0)
landlocked_internet[nrow(landlocked_internet) + 1,] = c(0)

count_landlocked = 0
count_other = 0
sum_landlocked = 0
sum_other = 0

for(i in 4:12) {
  for(j in 1:102) {
    if(landlocked_internet[j,15] == 'FALSE') {
      sum_other = sum_other + as.numeric(landlocked_internet[j,i])
      count_other = count_other + 1
    }
    else {
      sum_landlocked = sum_landlocked + as.numeric(landlocked_internet[j,i])
      count_landlocked = count_landlocked + 1
    }
  }
  landlocked_internet[103, i] = sum_landlocked/count_landlocked
  landlocked_internet[104, i] = sum_other/count_other
  sum_landlocked = 0
  count_landlocked = 0
  sum_other = 0
  count_other = 0
}

landlocked_internet[103,3] <- 'Landlocked'
landlocked_internet[104,3] <- 'Non-Landlocked'

landlocked_internet <- landlocked_internet[,-c(1:2,13:15)]
landlocked_internet <- landlocked_internet[-c(1:102),]

landlocked_internet <- as.data.frame(t(as.matrix(landlocked_internet)))
landlocked_internet$Year <- 2008:2017
landlocked_internet[1,3] <- 2007
names(landlocked_internet)[names(landlocked_internet) == '103'] <- 'Landlocked'
names(landlocked_internet)[names(landlocked_internet) == '104'] <- 'Non-Landlocked'
landlocked_internet <- landlocked_internet[-c(1),] 

landlocked_internet <- data.frame(x = landlocked_internet$Year,                           
                                   y = c(landlocked_internet$'Landlocked', landlocked_internet$'Non-Landlocked'),
                                   Region = c(rep("Landlocked", nrow(landlocked_internet)),
                                               rep("Non-Landlocked", nrow(landlocked_internet))))

landlocked_internet$y <- as.numeric(landlocked_internet$y)


ggplot(landlocked_internet, aes(x, y, col = Region)) +
  geom_line() +
  xlab("Year") +
  ylab("Fixed Broadband Internet Subscriptions") +
  ggtitle("Fixed Broadband Internet Subscriptions in LLDC's and Non-LLDC's Between 2007 and 2018")

