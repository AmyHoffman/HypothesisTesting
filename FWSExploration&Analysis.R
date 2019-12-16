library(ggplot2)
library(cowplot)
library(dplyr)

# https://studentaid.ed.gov/sa/about/data-center/student/title-iv
data <- read.csv("FederalWorkStudy.csv")
data <- na.omit(data)
summary(data)

mean(data$FWS...Federal.Award)
mean(data$Recipients2)
mean(data$FWS.Disbursements)


(subsetSd <- aggregate(data[,2:4], list(data$School.Type), sd))
(subsetCount <- aggregate(data[,2], list(data$School.Type), length))
(total <- c("Total", as.numeric(mean(data$FWS...Federal.Award)), as.numeric(mean(data$Recipients2)), as.numeric(mean(data$FWS.Disbursements)) ))

data$Diff <- data$FWS...Federal.Award - data$FWS.Disbursements

private <- data %>% filter(data$School.Type == "Private/Non-Profit")
prop <- data %>% filter(data$School.Type == "Proprietary")
public <- data %>% filter(data$School.Type == "Public")
##########################################################################################################
(subsetMean <- aggregate(data[,2:4], list(data$School.Type), mean))
mean(data$FWS...Federal.Award)
(z <- (subsetMean[,2] - mean(data$FWS...Federal.Award))^2 / (sd(data$FWS...Federal.Award)/sqrt(length(data$FWS...Federal.Award))))
rbind(z, c(pnorm(z[1], lower.tail = FALSE), pnorm(-z[2], lower.tail = TRUE), pnorm(z[3], lower.tail = FALSE)))

mean(data$FWS.Disbursements)
(z <- (subsetMean[,3] - mean(data$FWS.Disbursements))^2 / (sd(data$FWS.Disbursements)/sqrt(length(data$FWS.Disbursements))))
rbind(z, c(pnorm(z[1], lower.tail = FALSE), pnorm(-z[2], lower.tail = TRUE), pnorm(z[3], lower.tail = FALSE)))

p1 <- ggplot() +
  geom_density(aes(x = data$FWS...Federal.Award, group = data$School.Type, color = data$School.Type, fill = data$School.Type), alpha = 0.1)+
  scale_x_continuous(limits = c(0, 1.5e+06)) +
  labs(x = "FWS Federal Award", y = "Density", title = "Densities of FWS Federal Award by School Type")
p2 <-  ggplot() + 
  geom_density(aes(x = data$FWS.Disbursements, group=data$School.Type, color = data$School.Type, fill =data$School.Type), alpha = 0.1)+
  scale_x_continuous(limits = c(0, 1.5e+06))+
  labs(x = "FWS Disbursement", y = "Density", title = "Densities of FWS Disbursements by School Type")
plot_grid(p1, p2, nrow = 2)
##########################################################################################################

# H0: mu_private recp > mu_total recp      H1: mu_private recp <= mu_total recp
z <- (mean(data$Recipients2) - as.numeric(total[3]))^2/(sd(data$Recipients2)/sqrt(length(data$Recipients2)))
pnorm(z, lower.tail = FALSE)

# -------------- SIGNIFICANT --------------------
# H0: sig_private = sig_public    H1: sig_private != sig_public
# explicitly show defaults
var.test(private$Recipients2, public$Recipients2, ratio = 1, alternative= "two.sided", conf.level = 0.95)
# H0: mu_private = mu_public    H1: mu_private != mu_public
# from above we know var are equal
t.test(private$Recipients2, public$Recipients2, alternative = "greater", paired = FALSE, var.equal=TRUE)

# -------------- NOT SIGNIFICANT --------------------
# H0: sig_private = sig_public    H1: sig_private != sig_public
# explicitly show defaults
var.test(private$FWS.Disbursements, public$FWS.Disbursements, ratio = 1, alternative= "two.sided", conf.level = 0.95)
# H0: mu_private = mu_public    H1: mu_private != mu_public
# from above we know var are not equal
t.test(private$FWS.Disbursements, public$FWS.Disbursements, alternative = "two.sided", paired = FALSE, var.equal=FALSE)


# -------------- NOT SIGNIFICANT --------------------
# H0: sig_private = sig_public    H1: sig_private != sig_public
# explicitly show defaults
var.test(private$FWS...Federal.Award, public$FWS...Federal.Award, ratio = 1, alternative= "two.sided", conf.level = 0.95)
# H0: mu_private = mu_public    H1: mu_private != mu_public
# from above we know var are not equal
t.test(private$FWS...Federal.Award, public$FWS...Federal.Award, alternative = "two.sided", paired = FALSE, var.equal=FALSE)

##########################################################################################################
cor(data$FWS...Federal.Award, data$Recipients2) # Related
cov(data$FWS...Federal.Award, data$Recipients2) # Not Indep
plot(data$FWS...Federal.Award, data$Recipients2)

cor(data$FWS...Federal.Award, data$FWS.Disbursements)
cov(data$FWS...Federal.Award, data$FWS.Disbursements)
plot(data$FWS...Federal.Award, data$FWS.Disbursements)

cor(data$Recipients2, data$FWS.Disbursements)
cov(data$Recipients2, data$FWS.Disbursements)
plot(data$Recipients2, data$FWS.Disbursements)

##########################################################################################################
greater <- na.omit(data %>% filter(data$Diff >= 0) %>% select("Diff", "School.Type"))
ggplot() +
  geom_density(aes(x = as.numeric(greater$Diff), group = greater$School.Type, color = greater$School.Type, fill = greater$School.Type), alpha = 0.1)

aggregate(greater[,1], list(greater$School.Type), mean)
aggregate(greater[,1], list(greater$School.Type), sd)

less <- na.omit(data %>% filter(data$Diff < 0) %>% select("Diff", "School.Type"))
ggplot() +
  geom_density(aes(x = as.numeric(less$Diff), group = less$School.Type, color = less$School.Type, fill = less$School.Type), alpha = 0.1)

aggregate(less[,1], list(less$School.Type), mean)
aggregate(less[,1], list(less$School.Type), sd)


ggplot() +
  geom_density(aes(x = as.numeric(data$Diff), group = data$School.Type, color = data$School.Type, fill = data$School.Type), alpha = 0.1)
(m <- aggregate(data$Diff, list(data$School.Type), mean))
(s <- aggregate(data$Diff, list(data$School.Type), sd))
var.test(private$Diff, public$Diff, ratio = 1, alterntiave = "two.sided")
t.test(private$Diff, public$Diff, alternative = "less", paired = FALSE, var.equal=FALSE)

ggplot() +
  geom_density(aes(x = as.numeric(data$Diff), group = data$School.Type, color = data$School.Type, fill = data$School.Type), alpha = 0.1) +
  geom_density(aes(x = as.numeric(data$Diff)), color = "purple", fill = "purple", alpha = 0.1)
(z <- (m[,2] - mean(data$Diff))^2 / (sd(data$Diff)/sqrt(length(data$Diff))))
rbind(z, c(pnorm(-z[1], lower.tail = TRUE), pnorm(z[2], lower.tail = FALSE), pnorm(-z[3], lower.tail = TRUE)))

mean(data$Diff)
aggregate(data$Diff, list(data$School.Type), sd)


##########################################################################################################
p1 <- ggplot(private) +
  geom_density(aes(x = FWS.Disbursements), color = 'red', fill = "red", alpha = 0.1) +
  geom_density(aes(x = FWS...Federal.Award), color = 'blue', fill = "red", alpha = 0.1) +
  geom_vline(aes(xintercept = mean(FWS.Disbursements)), color = "red") +
  geom_vline(aes(xintercept = mean(FWS...Federal.Award)), color = "blue")+
  scale_x_continuous(limits = c(0, 1.5e+06)) +
  labs(x = "Dollars", y = "Density", title = "FWS Award (Blue) and Disbursements (Red) for Private Institutions")
p2 <- ggplot(public) +
  geom_density(aes(x = FWS.Disbursements), color = 'red', fill = "red", alpha = 0.1) +
  geom_density(aes(x = FWS...Federal.Award), color = 'blue', fill = "red", alpha = 0.1) +
  geom_vline(aes(xintercept = mean(FWS.Disbursements)), color = "red") +
  geom_vline(aes(xintercept = mean(FWS...Federal.Award)), color = "blue")+
  scale_x_continuous(limits = c(0, 1.5e+06)) +
  labs(x = "Dollars", y = "Density", title = "FWS Award (Blue) and Disbursements (Red) for Public Institutions") 
plot_grid(p1, p2, nrow = 2)

var.test(private$FWS...Federal.Award, private$FWS.Disbursements, ratio = 1, alternative = "two.sided")
t.test(private$FWS...Federal.Award, private$FWS.Disbursements, alternative = "less", paired = FALSE, var.equal = FALSE)

var.test(public$FWS...Federal.Award, private$FWS.Disbursements, ratio = 1, alterntaive = "two.sided")
t.test(public$FWS...Federal.Award, public$FWS.Disbursements, alterntiave = "less", paired = FALSE, var.equal = FALSE)
##########################################################################################################
