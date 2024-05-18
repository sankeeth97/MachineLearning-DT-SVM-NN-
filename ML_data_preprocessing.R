#Data Loading
ico_original_data <- read.csv("LUBS5990M_courseworkData_202223.csv",
                              stringsAsFactors = FALSE, encoding = 'UTF-8')

ico_data <- read.csv("LUBS5990M_courseworkData_202223.csv", 
                     stringsAsFactors = FALSE, encoding = 'UTF-8')

#columns with blank values
#priceUSD, countryRegion, teamSize, platform
#rm(ico_data_imputed)
str(ico_data)


#removing rows with empty platforms values
#ico_data <- subset(ico_data, platform != " ")


#converting dates to no of days
ico_data$startDate <- as.Date(ico_data$startDate, format = "%d/%m/%Y")
ico_data$endDate <- as.Date(ico_data$endDate, format = "%d/%m/%Y")

#No of days in between
library('dplyr')
library('mice')


ico_data$days <- difftime(ico_data$endDate, ico_data$startDate, units = "days")

ico_data$daysinBtwn <- as.numeric(ico_data$days)
ico_data$daysinBtwn <- abs(ico_data$daysinBtwn) # there are some cases where the start date is higher than the end date
# since anyways we are gonna remove the dates column no changes were done to it.

ico_data <- ico_data %>% relocate(daysinBtwn, .before = teamSize)
ico_data <- ico_data %>% relocate(days, .before = teamSize)
ico_data$days <- NULL

write.csv(ico_data, "ico_data.csv", row.names = FALSE)

#Finding Missing Values
md.pattern(ico_data)

sum(is.na(ico_data)) #-->334
sum(is.na(ico_data$priceUSD))# --> 180
sum(is.na(ico_data$teamSize))# --> 154
sum(is.na(ico_data$countryRegion))
#sum(is.na(ico_data$endDate))


#Changing categorical data to numerical value

#grouping countries under continents
# I first thought of grouping countries by creating 3-4 catogeries based on GDP/capita like the
#first category with top 50 GDP/capita and second with second best and so on, but on observing
#even the countries with high value of GDP/capita has a mixed proportion of success , so I believe 
# there won't be a useful benefit in grouping them in that way.


unique_countries <- data.frame(countries = table(ico_original_data$countryRegion))

rm(unique_countries)

# México & Curaçao are with latin words in between which should be translated to english 
#for extracting the right continents from the given set of countries. (R works in English)

ico_data$countryRegion <- iconv(ico_data$countryRegion, to = "ASCII//TRANSLIT")


install.packages("countrycode")
library('countrycode')

ico_data$continentRegion <- countrycode(ico_data$countryRegion, "country.name", "continent")

ico_data <- ico_data %>% relocate(continentRegion, .before = startDate)

write.csv(ico_data, "ico_data.csv", row.names = FALSE)

#validating the proportion of success for each continents

prop.table(table(ico_data$success))

americas_table <- filter(ico_data, continentRegion == 'Americas') %>% select(success)
table(americas_table$success)
prop.table(table(americas_table$success))

europe_table <- filter(ico_data, continentRegion == 'Europe') %>% select(success)
table(europe_table$success)
prop.table(table(europe_table$success))


africa_table <- filter(ico_data, continentRegion == 'Africa') %>% select(success)
table(africa_table$success)
prop.table(table(africa_table$success))

asia_table <- filter(ico_data, continentRegion == 'Asia') %>% select(success)
table(asia_table$success)
prop.table(table(asia_table$success))

oceania_table <- filter(ico_data, continentRegion == 'Oceania') %>% select(success)
table(oceania_table$success)
prop.table(table(oceania_table$success))

na_table <- filter(ico_data, continentRegion == 'unknown') %>% select(success)
table(na_table$success)
prop.table(table(na_table$success))

rm(americas_table)
rm(europe_table)
rm(africa_table)
rm(asia_table)
rm(oceania_table)
rm(na_table)

#Converting continent column to numerical column

ico_data$continenetAmericas <- ifelse(ico_data$continentRegion == 'Americas', 1, 0)
ico_data$continenetAfrica <- ifelse(ico_data$continentRegion == 'Africa', 1, 0)
ico_data$continenetAsia <- ifelse(ico_data$continentRegion == 'Asia', 1, 0)
ico_data$continenetEurope <- ifelse(ico_data$continentRegion == 'Europe', 1, 0)
ico_data$continenetOceania <- ifelse(ico_data$continentRegion == 'Oceania', 1, 0)

ico_data$continenetAmericas[is.na(ico_data$continenetAmericas)] <- 0
ico_data$continenetAfrica[is.na(ico_data$continenetAfrica)] <- 0
ico_data$continenetAsia[is.na(ico_data$continenetAsia)] <- 0
ico_data$continenetEurope[is.na(ico_data$continenetEurope)] <- 0
ico_data$continenetOceania[is.na(ico_data$continenetOceania)] <- 0

ico_data <- ico_data %>% relocate(continenetAmericas, .before = startDate)
ico_data <- ico_data %>% relocate(continenetAfrica, .before = startDate)
ico_data <- ico_data %>% relocate(continenetAsia, .before = startDate)
ico_data <- ico_data %>% relocate(continenetEurope, .before = startDate)
ico_data <- ico_data %>% relocate(continenetOceania, .before = startDate)

ico_data$continentRegion[is.na(ico_data$continentRegion)] <- "unknown"





write.csv(ico_data, "ico_data.csv", row.names = FALSE)

#Converting platform to numerical values

library('NLP')
library(tm)

unique_platforms <- data.frame(platforms = unique(ico_data$platform))

ico_data$platform <- iconv(ico_data$platform, to = "ASCII//TRANSLIT")

ico_data <- ico_data %>%
  mutate(platform = recode(platform," " = "others"))

ico_data$platform <- tolower(ico_data$platform)

#ico_data$platform <- stripWhitespace(ico_data$platform)

ico_data$platform <- trimws(ico_data$platform)

#toSpace <- function(x) gsub("[^[:graph:]]", "", x)
#ico_data$platform<- toSpace(ico_data$platform)

# To remove any non-alpha characters before that appears before the text
#removeNumPunct <- function(x) gsub("(?<=^|\\s)[^[:alpha:]]+", "", x, perl = TRUE)
#ico_data$platform <- removeNumPunct(ico_data$platform)

#To remove particular words

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"btc"="bitcoin"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"?komodo"="komodo"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"ethererum"="ethereum"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"ethereum, waves"="ethereum"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"etherum"="ethereum"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"erc20"="ethereum"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"vechainthor vip180"="vechain"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"stellar protocol"="stellar"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"x11 blockchain"="x11"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"eth"="ethereum"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"pos,pow"="pospow"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"pow/pos"="pospow"))

ico_data <- ico_data %>%
  mutate(platform = recode(platform,"pos + pow"="pospow"))


platforms <- table(ico_data$platform)
platforms <- data.frame(platforms)
write.csv(platforms, "platforms.csv", row.names = FALSE)
sum(platforms$Freq)
rm(platforms)
rm(unique_platforms)

# Big list
imp_platform_list <- c('ethereum', 'separate blockchain', 'stellar', 'waves', 'bitcoin', 'eos', 'nem', 'neo', 
                       'scrypt', 'tron', 'x11')

ico_data$pltfrmEthereum <- ifelse(ico_data$platform == 'ethereum', 1, 0)
ico_data$pltfrmSprtblkchn <- ifelse(ico_data$platform == 'separate blockchain', 1, 0)
ico_data$pltfrmStellar <- ifelse(ico_data$platform == 'stellar', 1, 0)
ico_data$pltfrmWaves <- ifelse(ico_data$platform == 'waves', 1, 0)
ico_data$pltfrmBtcn <- ifelse(ico_data$platform == 'bitcoin', 1, 0)
ico_data$pltfrmEos <- ifelse(ico_data$platform == 'eos', 1, 0)
ico_data$pltfrmNem <- ifelse(ico_data$platform == 'nem', 1, 0)
ico_data$pltfrmNeo <- ifelse(ico_data$platform == 'neo', 1, 0)
ico_data$pltfrmScrpt <- ifelse(ico_data$platform == 'scrypt', 1, 0)
ico_data$pltfrmTrn <- ifelse(ico_data$platform == 'tron', 1, 0)
ico_data$pltfrmX11 <- ifelse(ico_data$platform == 'x11', 1, 0)


# small list
imp_platform_list <- c('ethereum', 'separateblockchain', 'stellar', 'waves')

ico_data$pltfrmEthereum <- ifelse(ico_data$platform == 'ethereum', 1, 0)
ico_data$pltfrmSprtblkchn <- ifelse(ico_data$platform == 'separateblockchain', 1, 0)
ico_data$pltfrmStellar <- ifelse(ico_data$platform == 'stellar', 1, 0)
ico_data$pltfrmWaves <- ifelse(ico_data$platform == 'waves', 1, 0)


ico_data$pltfrmOthers <- 1

for (i in 1:nrow(ico_data)) {
  if (ico_data$platform[i] %in% imp_platform_list) {
    ico_data$pltfrmOthers[i] <- 0
  } 
}


ico_data <- ico_data %>% relocate(pltfrmEthereum, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmSprtblkchn, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmStellar, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmWaves, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmOthers, .before = coinNum)

#big List
ico_data <- ico_data %>% relocate(pltfrmEthereum, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmSprtblkchn, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmStellar, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmWaves, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmBtcn, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmEos, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmNem, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmNeo, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmScrpt, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmTrn, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmX11, .before = coinNum)
ico_data <- ico_data %>% relocate(pltfrmOthers, .before = coinNum)



for (i in 1:nrow(ico_data)) {
  if (ico_data$pltfrmOthers[i] == 1) {
    ico_data$platform[i] <- 'others'
  } 
}

write.csv(ico_data, "ico_data.csv", row.names = FALSE)

ethereum_table <- filter(ico_data, platform == 'ethereum') %>% select(success)
table(ethereum_table$success)
prop.table(table(ethereum_table$success))

separateblockchain_table <- filter(ico_data, platform == 'separate blockchain') %>% select(success)
table(asia_table$success)
prop.table(table(separateblockchain_table$success))

stellar_table <- filter(ico_data, platform == 'stellar') %>% select(success)
table(oceania_table$success)
prop.table(table(stellar_table$success))

waves_table <- filter(ico_data, platform == 'waves') %>% select(success)
table(na_table$success)
prop.table(table(waves_table$success))

others_table <- filter(ico_data, platform == 'others') %>% select(success)
table(others_table$success)
prop.table(table(others_table$success))

x11_table <- filter(ico_data, platform == 'x11') %>% select(success)
table(x11_table$success)
prop.table(table(x11_table$success))

rm(ethereum_table)
rm(separateblockchain_table)
rm(stellar_table)
rm(waves_table)
rm(others_table)
rm(platforms)

#Remove unwanted columns
ico_data$ID <- NULL
ico_data$countryRegion <- NULL
ico_data$startDate <- NULL
ico_data$endDate <- NULL
ico_data$brandSlogan <- NULL

ico_data$continentRegion <- NULL
ico_data$platform <- NULL

#imputation for missing columns

ico_data$success_n <- NULL
ico_data$success_n <- ifelse(ico_data$success == 'Y', 1, 0)

ico_data <- ico_data %>% relocate(success_n, .before = brandSlogan)

ico_data_imputed <- ico_data

library("corrgram")
str(ico_data_imputed)
corrgram(ico_data)
cor(ico_data1)

#rm(ico_data1)

#mean, S.D & Histogram of 'ABV' & 'EBC' columns.
mean(ico_data$priceUSD, na.rm = TRUE)
sd(ico_data$priceUSD, na.rm = TRUE)
hist(ico_data$priceUSD)

mean(ico_data$teamSize, na.rm = TRUE)
sd(ico_data$teamSize, na.rm = TRUE)
summary(ico_data$teamSize)
hist(ico_data$teamSize)

#simple imputation
#ico_data3 <- ico_data
ico_data$priceUSD[is.na(ico_data$priceUSD)] <- median(ico_data$priceUSD, na.rm = TRUE)
ico_data$teamSize[is.na(ico_data$teamSize)] <- mean(ico_data$teamSize, na.rm = TRUE)

md.pattern(ico_data)

hist(ico_data$priceUSD, main = "Histogram of priceUSD", xlab = "Price (USD)")
hist(ico_data$teamSize, main = "Histogram of teamSize", xlab = "Team Size")
hist(ico_original_data$teamSize, main = "Histogram of teamSize", xlab = "Team Size")

hist(ico_data$coinNum, main = "Histogram of coinNum", xlab = "No of coins")

hist(ico_data$daysinBtwn, main = "Duration distribution (days)", xlab = "Duration")

hist(ico_data$distributedPercentage, main = "Histogram of distributedPercentage",
     xlab = "Distributed Percentage (%)")
?hist

#----------------------Bar Plot for categorical data-----------------------------------------------
Platform_freq <- table(ico_data$platform)
Platform_freq <- sort(Platform_freq, decreasing = TRUE)
barplot(Platform_freq, col = "lightblue", main = "Platform Distribution",
        xlab = "Platforms", ylab = "Frequency", space = 1)

ContinentRegion_freq <- table(ico_data$continentRegion)
ContinentRegion_freq <- sort(ContinentRegion_freq, decreasing = TRUE)
barplot(ContinentRegion_freq, col = "lightblue", main = "Continent Distribution",
        xlab = "Continents", ylab = "Frequency")
?barplot
#---------------------------------------------------------------------------------------------------

#--------------------------------------Scatter plot for categorical data--------------------------

plot(ico_data$rating, ico_data$hasVideo, 
     main = "Scatterplot Example", xlab = "Rating", ylab = "hasVideo")

plot(ico_data$rating, ico_data$success_n, xlim = c(1, 5.0), ylim = c(0, 1),
     main = "Rating Vs Success of ICO", xlab = "Rating", ylab = "Success")

?plot
# Add gridlines (optional)
grid()

# Add a regression line (optional)
abline(lm(ico_data$success ~ ico_data$hasVideo))

# Add a legend (optional)
legend("topright", legend = "Data", pch = 1)

#--------------------------------------------------------------------------------------------------

mean(ico_data$priceUSD)
mean(ico_data$teamSize)
mean(ico_data$distributedPercentage)

md.pattern(ico_original_data, rotate.names = TRUE)
?md.pattern

sd(ico_data$priceUSD)
sd(ico_data$teamSize)

write.csv(ico_data3, "ico_data.csv", row.names = FALSE)

# To Numerically identify outliers in columns & remove them-------------------------------------------
boxplot(ico_data$priceUSD, main = "Boxplot of priceUSD", ylab = "Price (USD)")
boxplot(ico_data$teamSize, main = "Boxplot of teamSize", ylab = "team Size")

boxplot(ico_data$coinNum, main = "Boxplot of coinNum", ylab = "No of Coins")

boxplot(ico_data$distributedPercentage, main = "Boxplot of distributedPercentage",
        ylab = "Distributed Percentage (%)")


IQR(ico_data$priceUSD, na.rm = TRUE)

boxplot(ico_data$teamSize, ylim=c(0, 100))
boxplot(ico_data$daysinBtwn, main = "Duration distribution (days)")

boxplot(ico_data$coinNum, ylim=c(0,5.000e+09))

#------ Significance of Outliers----------------------------------------------------------------
install.packages("outliers")
library(outliers)

grubbs.test(ico_data$priceUSD)
grubbs.test(ico_data$daysinBtwn)
grubbs.test(ico_data$teamSize)
grubbs.test(ico_data$distributedPercentage)
grubbs.test(ico_data$coinNum)


ico_data <- ico_data
str(ico_data)

ico_data <- subset(ico_data, priceUSD <= 1000)
ico_data <- subset(ico_data, distributedPercentage <= 100)
ico_data <- subset(ico_data, coinNum <= 1.2e+13)
ico_data <- subset(ico_data, daysinBtwn <= 1000)


summary(ico_data1$priceUSD)
summary(ico_data$priceUSD)

summary(ico_data1$teamSize)
summary(ico_data$teamSize)

summary(ico_data1$distributedPercentage)
summary(ico_data$distributedPercentage)

summary(ico_data$daysinBtwn)

summary(ico_data$coinNum)

write.csv(ico_data, "ico_data.csv", row.names = FALSE)

# Finding an interaction variable

# Step 1 --> Should convert categorical data to factors
str(ico_data)

ico_data$success <- factor(ico_data$success, levels = c('Y', 'N'), labels = c("Y", "N"))
ico_data$success <- factor(ico_data$success, levels = c('N', 'Y'), labels = c('0', '1'))
ico_data$hasVideo <- factor(ico_data$hasVideo, levels = c('0', '1'))
ico_data$continenetAmericas <- factor(ico_data$continenetAmericas, levels = c('0', '1'))
ico_data$continenetAfrica <- factor(ico_data$continenetAfrica, levels = c('0', '1'))
ico_data$continenetAsia <- factor(ico_data$continenetAsia, levels = c('0', '1'))
ico_data$continenetEurope <- factor(ico_data$continenetEurope, levels = c('0', '1'))
ico_data$continenetOceania <- factor(ico_data$continenetOceania, levels = c('0', '1'))
ico_data$hasGithub <- factor(ico_data$hasGithub, levels = c('0', '1'))
ico_data$hasReddit <- factor(ico_data$hasReddit, levels = c('0', '1'))
ico_data$minInvestment <- factor(ico_data$minInvestment, levels = c('0', '1'))
ico_data$pltfrmEthereum <- factor(ico_data$pltfrmEthereum, levels = c('0', '1'))
ico_data$pltfrmSprtblkchn <- factor(ico_data$pltfrmSprtblkchn, levels = c('0', '1'))
ico_data$pltfrmStellar <- factor(ico_data$pltfrmStellar, levels = c('0', '1'))
ico_data$pltfrmWaves <- factor(ico_data$pltfrmWaves, levels = c('0', '1'))
ico_data$pltfrmBtcn <- factor(ico_data$pltfrmBtcn, levels = c('0', '1'))
ico_data$pltfrmEos <- factor(ico_data$pltfrmEos, levels = c('0', '1'))
ico_data$pltfrmNem <- factor(ico_data$pltfrmNem, levels = c('0', '1'))
ico_data$pltfrmNeo <- factor(ico_data$pltfrmNeo, levels = c('0', '1'))
ico_data$pltfrmScrpt <- factor(ico_data$pltfrmScrpt, levels = c('0', '1'))
ico_data$pltfrmTrn <- factor(ico_data$pltfrmTrn, levels = c('0', '1'))
ico_data$pltfrmX11 <- factor(ico_data$pltfrmX11, levels = c('0', '1'))
ico_data$pltfrmOthers <- factor(ico_data$pltfrmOthers, levels = c('0', '1'))

library(ggplot2)
install.packages('graphics')
library(graphics)

#------------------------Interaction plot--------------------------------------------------------
interaction.plot(
  x.factor = ico_data$success,
  trace.factor = ico_data$hasVideo,
  response = ico_data$rating
)

?interaction.plot
#Interaction plot cannot be used for this case as our 'response' variable the dependent variable is categorical
#this technique is useful only when the response variable is numerical and independent variables
# are categorical
#---------------------------------------------------------------------------------------------------


ggplot(ico_data, aes(x = hasVideo, fill = hasReddit)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ success)

#--------hasVideo
ggplot(ico_data, aes(x = hasVideo)) +
  geom_bar(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)

#-----------hasReddit
ggplot(ico_data, aes(x = hasReddit)) +
  geom_bar(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)

#--------------hasGithub
ggplot(ico_data, aes(x = hasGithub)) +
  geom_bar(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)

#---------------minInvestment
ggplot(ico_data, aes(x = minInvestment)) +
  geom_bar(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)


ggplot(ico_data, aes(x = minInvestment)) +
  geom_bar(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)

ggplot(ico_data, aes(x = rating)) +
  hist(position = "dodge",  col= 'lightblue') +
  facet_grid(. ~ success)




hist(ico_data$rating, main = 'Histogram of Rating', xlab = 'Rating')

?ggplot
?geom_bar

ggplot(ico_data, aes(x = hasVideo, y = success, fill = hasReddit)) +
  geom_point(position = position_dodge(0.9), shape = 21, size = 3) +
  labs(x = "hasVideo", y = "success") +
  scale_fill_discrete(name = "hasReddit") +
  theme_bw()


anova_model <- anova(success ~., data = ico_data)
summary(anova)

md.pattern(ico_data)
str(ico_data)
ico_data$success_n <- NULL

#--------------- to find the interaction between numerical variables using logistic regression------


library(corrgram)
corrgram(ico_data)

ico_cor_data <- select(ico_data, rating, priceUSD, daysinBtwn, teamSize, coinNum, distributedPercentage)

cor(ico_cor_data)

#-----------------Logistic Regression to test significance----------------------------------------

library(dplyr)

ico_data1 <- ico_data

ico_data1$success <- NULL
ico_data1$success <- ifelse(ico_data1$success == 'Y', 1, 0)

ico_data1 <- ico_data1 %>% relocate(success, .before = hasVideo)

ico_data1$pltfrmOthers <- NULL

ico_data1$continentRegion <- NULL
ico_data1$platform <- NULL

str(ico_data1)

model <- glm(success ~.,
             data = ico_data1, family = binomial)

model <- lm(success ~.,
             data = ico_train)

model <- glm(success ~.,
             data = ico_train, family = binomial)


model <- glm(success ~ daysinBtwn*teamSize,
             data = ico_data, family = binomial)

model <- glm(success ~ daysinBtwn + teamSize + priceUSD + coinNum + distributedPercentage + rating,
             data = ico_data, family = binomial)

summary(model)
anova(model)

ico_pred <- predict(model, newdata = ico_test, type = 'response')

predicted_classes <- ifelse(ico_pred >= 0.5, 'Y', 'N')



predicted_classes <- factor(predicted_classes, levels = c('Y', 'N'), 
                               labels = c("Y", "N"))
ico_test$success <- ifelse(ico_test$success_n == 0, 'N', 'Y')

ico_test$success <- factor(ico_test$success, levels = c('Y', 'N'), 
                            labels = c("Y", "N"))

CrossTable(ico_test$success, predicted_classes, 
           prop.chisq = TRUE, prop.c = TRUE, prop.r = TRUE,
           dnn = c('actual success', 'predicted success'))
?CrossTable

CrossTable(ico_data$success, ico_data$continentRegion, chisq = TRUE)
CrossTable(ico_data$success, ico_data$platform, chisq = TRUE)

?t.test

t.test(ico_data1$success, ico_data1$rating)
t.test(ico_data1$success, ico_data1$priceUSD)
t.test(ico_data1$success, ico_data1$daysinBtwn)
t.test(ico_data1$success, ico_data1$teamSize)
t.test(ico_data1$success, ico_data1$coinNum)
t.test(ico_data1$success, ico_data1$minInvestment)
t.test(ico_data1$success, ico_data1$hasGithub)
t.test(ico_data1$success, ico_data1$hasVideo)
t.test(ico_data1$success, ico_data1$hasReddit)
t.test(ico_data1$success, ico_data1$distributedPercentage)


CrossTable

confusionMatrix(predicted_classes,ico_test$success,
                positive = "Y")

rm(ico_pred)
ico_test
ico_train

ico_pred

ico_test$success_n <- NULL

summary(model)

corrgram(ico_data)
#-------------------------------------------Feature Selection-----------------------------------------

ico_data_select <- ico_data[, c("success", "rating", "continenetAmericas", "continenetAfrica",
                                "continenetAsia", "continenetEurope", "daysinBtwn", "teamSize", "pltfrmStellar")]

#-------------------------------------------------------------------------------------------------------
interaction_term <- interaction(ico_data$hasVideo, ico_data$hasGithub)
interaction_term
?interaction

anova(model)

anova(success_n ~., data = ico_data1)
aov(success_n ~., data = ico_data1)



?anova
?aov
?glm
library(corrgram)
cor(ico_data$rating, ico_data$teamSize)
heatmap(corr)

interaction.plot(model, pred = success, modx = hasReddit)

?interaction.plot
?interaction



# Text Mining-------------------------------------------------------------------------------------------

library('NLP')
library(tm)
# build a corpus, and specify the source to be character vectors 

ico_data$brandSlogan <- iconv(ico_data$brandSlogan, to = "ASCII//TRANSLIT")


myCorpus_raw <- Corpus(VectorSource(ico_data$brandSlogan))
myCorpus <- myCorpus_raw

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
myCorpus<- tm_map(myCorpus,toSpace,"[^[:graph:]]")

inspect(myCorpus[[2]])

inspect(myCorpus_raw[[2]])

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
stopwords()
myCorpus <- tm_map(myCorpus, removeWords, stopwords()) # remove stop words
myCorpus <- tm_map(myCorpus, removePunctuation) # remove punctuation
myCorpus <- tm_map(myCorpus, stripWhitespace) # remove whitespace

library("SnowballC")
myCorpus <- tm_map(myCorpus, stemDocument)


#Build Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))

tdm$v
nrow(tdm) # number of words
ncol(tdm) # number of slogans

# inspect frequent words
# lowfreq means the lower frequency bound
freq_thre = 50
(freq.terms <- findFreqTerms(tdm, lowfreq = freq_thre))

# calculate the word frenquency 
term.freq <- rowSums(as.matrix(tdm))
term.freq
# only keep the frequencies of words(terms) appeared more then freq_thre times
term.freq <- subset(term.freq, term.freq >= freq_thre)
# select the words(terms) appeared more then freq_thre times, according to selected term.freq
df <- data.frame(term = names(term.freq), freq = term.freq)
library('NLP')
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#-------------------------Wordcloud--------------------------------

library('wordcloud')
wordcloud(myCorpus, min.freq = 40, random.order = FALSE)
#---------------------------------------------------------------------------------------------------

###### Task: Sentiment Analysis------------------------------------------------------------------
library(usethis)
library(devtools)
library(RCurl)
library(rjson)
library(plyr)
library(sentiment)

ico_data$brandSlogan <- iconv(ico_data$brandSlogan, to = "ASCII//TRANSLIT")


ico_data$brandSlogan <- tolower(ico_data$brandSlogan)

ico_data$brandSlogan <- stripWhitespace(ico_data$brandSlogan)

ico_data$brandSlogan <- trimws(ico_data$brandSlogan)


sentiments <- sentiment(ico_data$brandSlogan)
table(sentiments$polarity)


# 2) using the emotion lexicon
#install.packages('syuzhet')
library(syuzhet)
# use the cleaned text for emotion analysis 
# since get_nrc_sentiment cannot deal with non-graphic data 
tweet_clean <- data.frame(text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)
tweet_clean
# each row: a document (a tweet); each column: an emotion
emotion_matrix <- get_nrc_sentiment(tweet_clean$text)
tweet_clean$text[2]
emotion_matrix[1,]

# Matrix Transpose
# so each row will be an emotion and each column will be a tweet
td <- data.frame((emotion_matrix))
td_plt <- data.frame(t(emotion_matrix))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td_plt))
td_new

#Transformation and cleaning
names(td_new)[1] <- "count"
td_new
td_new <- cbind("sentiment" = rownames(td_new), td_new)
td_new
rownames(td_new) <- NULL
td_new

df[1:2,]

# emotion Visualisation
library("ggplot2")
qplot(sentiment, data=td_new, weight=count, geom="bar",
      fill=sentiment)+ggtitle("Slogan emotion and sentiment")

ico_data1 <- cbind(ico_data3, td)

ico_data1$brandSlogan <- NULL
ico_data1$continentRegion <- NULL
ico_data1$platform <- NULL
ico_data1$success_n <- NULL

ico_data1$success_n <- ifelse(ico_data1$success == 'Y', 1, 0)

str(ico_data1)


corrgram(ico_data1)

model <- glm(success ~ ., data = ico_data1, family = binomial())
summary(model)

anova <- aov(success ~ ., data = ico_data1)
summary(anova)

write.csv(ico_data1, "ico_data.csv", row.names = FALSE)

?glm
