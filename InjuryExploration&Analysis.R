library(ggplot2)
library(dplyr)

data <- read.csv("Injury_Data.csv")

#the size of the original data set
dim(data) # 203  19

summary(data)

##########################################################################################################
# ---------- GOOD FINDING ----------- CLOSE TO SIGNIFICANT ---------
# Test whether the means of the male and female Intensity scores differ
male <- na.omit(data %>% filter(sex == "male") %>% select("Intensity_Score") )
female <-  na.omit(data %>% filter(sex == "female") %>% select("Intensity_Score"))
#genderIS <- as.data.frame(na.omit( cbind(as.character(data$sex), as.numeric(data$Intensity_Score))))

ggplot() + 
  geom_density(aes(x = male$Intensity_Score), alpha = 0.2, fill="blue")+
  geom_density(aes(x = female$Intensity_Score), alpha = 0.2, fill = "red") +
  geom_vline(aes(xintercept = mean(male$Intensity_Score)), color = "blue") + 
  geom_vline(aes(xintercept = mean(female$Intensity_Score)), color = "red") +
  labs(title = "Densities of Male and Female Intensity Scores", x = "Intensity Score", y = "Density")

# H0: maleavg = femaleavg    H1: malavg < femaleavg
sd(male$Intensity_Score)
sd(female$Intensity_Score)
t.test(male, female, alternative = "less", mu = 0, var.equal = TRUE)

##########################################################################################################
# FINDING #1
subset <- na.omit(data %>% filter(injury_type != "" ) %>% select("Age", "injury_type"))
ggplot(subset, aes(x = subset$Age, group = subset$injury_type, color = subset$injury_type, fill = subset$injury_type)) +
  geom_density(alpha = 0.1)
m <- aggregate(subset, list(subset$injury_type), mean)[,1:2]
z <- (m[,2] -mean(subset$Age))^2/(sd(subset$Age)/sqrt(length(subset$Age)))
results <- rbind(z, c(pnorm(z[1], lower.tail = FALSE) + pnorm(-z[1], lower.tail = TRUE), pnorm(-z[2], lower.tail=TRUE) + pnorm(z[2], lower.tail = FALSE), pnorm(-z[3], lower.tail = TRUE), pnorm(z[4], lower.tail = FALSE), pnorm(z[5], lower.tail = TRUE)))

subset <- na.omit(data %>% filter(injury_type == c("sport", "other") ) %>% select("Age", "injury_type"))
subsetData <- na.omit(data %>% filter(injury_type != "" ) %>% select("Age", "injury_type"))
ggplot() +
  geom_density(aes(x = subset$Age, group = subset$injury_type, color = subset$injury_type, fill = subset$injury_type), alpha = 0.1) +
  geom_density(aes(x = subsetData$Age), color = "green", fill = "green", alpha = 0.1) +
  geom_vline(aes(xintercept = mean( (subset %>% filter(injury_type == "other"))$Age)), color = "red") +
  geom_vline(aes(xintercept = mean( (subset %>% filter(injury_type == "sport"))$Age)), color = "blue") +
  geom_vline(aes(xintercept = mean(subsetData$Age)), color = "green")+
  labs(x = "Age", y="Density", title="Distribution Density of Average Age for Injuries")

##########################################################################################################
subset <- na.omit(data %>% filter(injury_type != "" ) %>% select("Injury_Duration", "injury_type"))
ggplot(subset, aes(x = subset$Injury_Duration, group = subset$injury_type, color = subset$injury_type, fill = subset$injury_type)) +
  geom_density(alpha = 0.1)  
(m <- aggregate(subset, list(subset$injury_type), mean)[,1:2])
z <- (m[,2] -mean(subset$Injury_Duration))^2/(sd(subset$Injury_Duration)/sqrt(length(subset$Injury_Duration)))
# ---------- GOOD FINDING ----------- SIGNIFICANT ---------
#H0: mu_other > mu_pop      H1: mu_other <= mu_pop   index[3,]
pnorm(z[3], lower.tail = FALSE)

##########################################################################################################
# Finding #3
subset <- na.omit(data %>% select(Intensity_Score, Rating2))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating2)) 
lmodel <- lm(Intensity_Score ~ Rating2, subset)
summary(lmodel)
ggplot()+
  geom_point(aes(x = subset$Rating2, y = subset$Intensity_Score)) +
  geom_line(aes(x = subset$Rating2, y = lmodel$fitted.values), color = "red") +
  labs(x = "Rating2", y="Intensity Score", title = "Predicting Intensity Score Using Linear Regression")

##########################################################################################################
# ----------------- SIGNIFICANT P-Value ----------------
# Finding #4
subset <- na.omit(data %>% filter(Care.Site != "") %>% filter(Xray != "") %>% select("Care.Site", "Xray"))
tbl <-  table(subset$Care.Site, subset$Xray)[2:4,2:3]
exp <- (rowSums(tbl) %*% t(colSums(tbl)))/sum(tbl)
(d <- sum((tbl - exp)^2/exp))
pchisq(d, 2, lower.tail = FALSE)
chisq.test(tbl)


##########################################################################################################
# Finding #2
subset <- na.omit(data %>% filter(injury_type != "" ) %>% select("Age", "injury_type"))
s <- aggregate(subset, list(subset$injury_type), sd)[,1:2]
l <- aggregate(subset, list(subset$injury_type), length)[,1:2]
chisq2 <- ((l[,2] - 1)*s[,2]^2)/sd(subset$Age)^2
chisq2 <-  rbind(chisq2, c(pchisq(chisq2[1], l[1,2], lower.tail = TRUE), pchisq(chisq2[2], l[2,2], lower.tail = TRUE), pchisq(chisq2[3], l[3,2], lower.tail = FALSE), pchisq(chisq2[4], l[4,2], lower.tail = TRUE), pchisq(chisq2[5], l[5,2], lower.tail = TRUE)))
chisq2 <- rbind(qchisq(0.95, l[,2]), chisq2)


##########################################################################################################
# ---------- BAD FINDING -----------
male <- na.omit(data %>% filter(sex == "male") %>% select("Injury_Duration") )
female <-  na.omit(data %>% filter(sex == "female") %>% select("Injury_Duration"))
#genderIS <- as.data.frame(na.omit( cbind(as.character(data$sex), as.numeric(data$Intensity_Score))))

ggplot() + 
  geom_density(aes(x = male$Injury_Duration), alpha = 0.2, fill="blue")+
  geom_density(aes(x = female$Injury_Duration), alpha = 0.2, fill = "red") +
  geom_vline(aes(xintercept = mean(male$Injury_Duration)), color = "blue") + 
  geom_vline(aes(xintercept = mean(female$Injury_Duration)), color = "red") +
  labs(title = "Densities of Male and Female Intensity Scores", x = "Intensity Score", y = "Density")

# H0: maleavg = femaleavg    H1: malavg < femaleavg
sd(male$Injury_Duration)
sd(female$Injury_Duration)
t.test(male, female, alternative = "less", mu = 0, var.equal = TRUE)

##########################################################################################################
# ---------- BAD FINDING -----------
male <- na.omit(data %>% filter(sex == "male") %>% select("Xray", "Injury_Duration") )
male <- male[-which(male$Xray == ""),]
female <-  na.omit(data %>% filter(sex == "female") %>% select("Xray", "Injury_Duration"))
female <- female[-which(female$Xray == ""),]

ggplot() + 
  geom_density(aes(x = male$Injury_Duration, color = male$Xray), alpha = 0.2) +
ggplot()+
  geom_density(aes(x = female$Injury_Duration, color = female$Xray), alpha = 0.2) 

# > temp <- male %>% filter(Xray == "No")
#> mean(temp$Injury_Duration)
#[1] 7.428444
#> temp <- male %>% filter(Xray == "Yes")
#> mean(temp$Injury_Duration)
#[1] 5.984615
# > temp <- female %>% filter(Xray == "Yes")
#> mean(temp$Injury_Duration)
#[1] 9.875
#> temp <- female %>% filter(Xray == "No")
#> mean(temp$Injury_Duration)
#[1] 7.646364

sd(male$Injury_Duration)
sd(female$Injury_Duration)
t.test(male, female, alternative = "less", mu = 0, var.equal = TRUE)
z.test(male$Injury_Duration, female$Injury_Duration, alternative = "two.sided", sigma.x = sd(male$Injury_Duration), sigma.y=sd(female$Injury_Duration))

##########################################################################################################
female <- na.omit(data %>% filter(sex == "female") %>% select(Injury_Duration, Intensity_Score))
t.test(female$Injury_Duration, female$Intensity_Score, paired = TRUE, var.equal = FALSE, alternative = "two.sided")

male <- na.omit(data %>% filter(sex == "male") %>% select(Injury_Duration, Intensity_Score))
t.test(male$Injury_Duration, male$Intensity_Score, paired = TRUE, var.equal = FALSE, alternative = "two.sided")



##########################################################################################################
subset <- na.omit(data %>% filter(injury_type != "" ) %>% select("Age", "injury_type"))
ggplot(subset, aes(x = subset$Age, group = subset$injury_type, color = subset$injury_type, fill = subset$injury_type)) +
  geom_density(alpha = 0.1)  
m <- aggregate(subset, list(subset$injury_type), mean)[,1:2]
z <- (m[,2] -mean(subset$Age))^2/(sd(subset$Age)/sqrt(length(subset$Age)))
# ---------- NEED TO DOUBLE CHECK THIS FORMULATION ---------
#H0: mu_other < mu_pop      H1: mu_other > mu_pop    index[3,]
pnorm(z[3], lower.tail = TRUE)
# ---------- GOOD FINDING ----------- SIGNIFICANT ---------
#H0: mu_sport > mu_pop      H1: mu_sport > mu_pop    index[4,]
pnorm(z[4], lower.tail = FALSE)

##########################################################################################################
subset <- na.omit(data %>% filter(injury_type != "" ) %>% select("Injury_Duration", "injury_type"))
ggplot(subset, aes(x = subset$Injury_Duration, group = subset$injury_type, color = subset$injury_type, fill = subset$injury_type)) +
  geom_density(alpha = 0.1)  
(m <- aggregate(subset, list(subset$injury_type), mean)[,1:2])
z <- (m[,2] -mean(subset$Injury_Duration))^2/(sd(subset$Injury_Duration)/sqrt(length(subset$Injury_Duration)))
# ---------- GOOD FINDING ----------- SIGNIFICANT ---------
#H0: mu_other > mu_pop      H1: mu_other <= mu_pop   index[3,]
pnorm(z[3], lower.tail = FALSE)
# ---------- NEED TO DOUBLE CHECK THIS FORMULATION ---------
#H0: mu_sport < mu_pop      H1: mu_sport >= mu_pop   index[4,]
pnorm(z[4])

##########################################################################################################
subset <- na.omit(data %>% select(Injury_Duration, Intensity_Score))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Intensity_Score))

subset <- na.omit(data %>% select(Injury_Duration, Rating1))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating1))

subset <- na.omit(data %>% select(Injury_Duration, Rating2))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating2))

subset <- na.omit(data %>% select(Injury_Duration, Rating3))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating3))

subset <- na.omit(data %>% select(Injury_Duration, Rating4))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating4))
chisq.test(subset$Injury_Duration, subset$Rating4)

subset <- na.omit(data %>% select(Injury_Duration, Rating5))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Injury_Duration, Rating6))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Injury_Duration, Rating8))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Injury_Duration, Rating9))
cor(as.matrix(subset$Injury_Duration), as.matrix(subset$Rating))




subset <- na.omit(data %>% select(Intensity_Score, Rating1))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating1))

subset <- na.omit(data %>% select(Intensity_Score, Rating2))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating2)) #-0.78
lmodel <- lm(Intensity_Score ~ Rating2, subset)
summary(lmodel)
ggplot()+
  geom_point(aes(x = subset$Rating2, y = subset$Intensity_Score)) +
  geom_line(aes(x = subset$Rating2, y = lmodel$fitted.values), color = "red") +
  labs(x = "Rating2", y="Intensity Score", title = "Predicting Intensity Score Using Linear Regression")

subset <- na.omit(data %>% select(Intensity_Score, Rating3))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating3))

subset <- na.omit(data %>% select(Intensity_Score, Rating4))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating4))

subset <- na.omit(data %>% select(Intensity_Score, Rating5))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Intensity_Score, Rating6))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Intensity_Score, Rating8))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Intensity_Score, Rating9))
cor(as.matrix(subset$Intensity_Score), as.matrix(subset$Rating))



subset <- na.omit(data %>% select(Rating1, Rating2))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating2)) #0.69

subset <- na.omit(data %>% select(Rating1, Rating3))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating3))

subset <- na.omit(data %>% select(Rating1, Rating4))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating4))

subset <- na.omit(data %>% select(Rating1, Rating5))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Rating1, Rating6))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Rating1, Rating8))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating1, Rating9))
cor(as.matrix(subset$Rating1), as.matrix(subset$Rating9))




subset <- na.omit(data %>% select(Rating2, Rating3))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating3))

subset <- na.omit(data %>% select(Rating2, Rating4))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating4))

subset <- na.omit(data %>% select(Rating2, Rating5))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Rating2, Rating6))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Rating2, Rating8))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating2, Rating9))
cor(as.matrix(subset$Rating2), as.matrix(subset$Rating9))



subset <- na.omit(data %>% select(Rating3, Rating4))
cor(as.matrix(subset$Rating3), as.matrix(subset$Rating4))

subset <- na.omit(data %>% select(Rating3, Rating5))
cor(as.matrix(subset$Rating3), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Rating3, Rating6))
cor(as.matrix(subset$Rating3), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Rating3, Rating8))
cor(as.matrix(subset$Rating3), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating3, Rating9))
cor(as.matrix(subset$Rating3), as.matrix(subset$Rating9))



subset <- na.omit(data %>% select(Rating4, Rating5))
cor(as.matrix(subset$Rating4), as.matrix(subset$Rating5))

subset <- na.omit(data %>% select(Rating4, Rating6))
cor(as.matrix(subset$Rating4), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Rating4, Rating8))
cor(as.matrix(subset$Rating4), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating4, Rating9))
cor(as.matrix(subset$Rating4), as.matrix(subset$Rating9))



subset <- na.omit(data %>% select(Rating5, Rating6))
cor(as.matrix(subset$Rating5), as.matrix(subset$Rating6))

subset <- na.omit(data %>% select(Rating5, Rating8))
cor(as.matrix(subset$Rating5), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating5, Rating9))
cor(as.matrix(subset$Rating5), as.matrix(subset$Rating9))



subset <- na.omit(data %>% select(Rating6, Rating8))
cor(as.matrix(subset$Rating6), as.matrix(subset$Rating8))

subset <- na.omit(data %>% select(Rating6, Rating9))
cor(as.matrix(subset$Rating6), as.matrix(subset$Rating9))



subset <- na.omit(data %>% select(Rating8, Rating9))
cor(as.matrix(subset$Rating8), as.matrix(subset$Rating9)) #0.608

##########################################################################################################
subset <- na.omit(data %>% select("Injury_Duration", "Rating8"))
lnmodel <- lm(Rating8 ~ Injury_Duration, subset)
summary(lnmodel)
ggplot(subset, aes(x = subset$Injury_Duration, y = subset$Rating8)) +
  geom_point()

subset <- na.omit(data %>% select("Age", "Rating1"))
lnmodel <- lm(Rating1 ~ Age, subset)
summary(lnmodel)
ggplot() +
  geom_point(subset, aes(x = subset$Age, y =subset$Rating1)) 



##########################################################################################################
subset <- na.omit(data %>% filter(Care.Site != "") %>% filter(injury_type != "") %>% select("Care.Site", "injury_type"))
tbl <- table(subset$Care.Site, subset$injury_type)[2:4,2:6]
exp <- (rowSums(tbl) %*% t(colSums(tbl)))/sum(tbl)
sum((tbl - exp)^2/exp)
#r = 3    c=5     then df = 8   X2_0.95,8= 15.507    X2_0.99,8 = 20.09
chisq.test(tbl)

# ----------------- SIGNIFICANT P-Value ----------------
subset <- na.omit(data %>% filter(Care.Site != "") %>% filter(Xray != "") %>% select("Care.Site", "Xray"))
tbl <-  table(subset$Care.Site, subset$Xray)[2:4,2:3]
chisq.test(tbl) 

subset <- na.omit(data %>% filter(Xray != "") %>% filter(injury_type != "") %>% select("Xray", "injury_type"))
tbl <- table(subset$Xray, subset$injury_type)[2:3,2:6]
exp <- (rowSums(tbl) %*% t(colSums(tbl)))/sum(tbl)
(d <- sum((tbl - exp)^2/exp))
chisq.test(tbl)
#r = 1   c= 5   df=4    X2_0.95,4=9.488  therefore accept H0

# Not significant p = 0.104
subset <- na.omit(data %>% filter(Xray != "") %>% select("Xray", "sex"))
tbl <- table(subset$Xray, subset$sex)[2:3,1:2]
chisq.test(tbl)

# p = 0.9177
subset <- na.omit(data %>% filter(sex != "") %>% filter(race != "") %>% select("sex", "race"))
tbl <- table(subset$sex, subset$race)
chisq.test(tbl)

# ------------ significant finding but not quite sure how to interpret ----------------
subset <- na.omit(data %>% select("Dazed", "Hospital_Admit"))
tbl <- table(subset$Dazed, subset$Hospital_Admit)[2:3, 2:3]
chisq.test(tbl)

##########################################################################################################
tempData <- na.omit(cbind.data.frame(as.factor(data$injury_type), as.factor(data$Injury_Duration)))

ggplot(tempData)+
  geom_bar(aes(tempData[,1] , tempData[,2]))

