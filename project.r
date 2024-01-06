###
###  NMST570:  	Statistical Methods in Psychometrics
###
###  Final project
###
###  Michaela Cichrova, Kristina Sakmarova
###
### ==============================================
rm(list = ls())


# loading of required packages
library(psych)
library(lavaan)
library(stringr)
library(ggplot2)
library(corrplot)
library(ShinyItemAnalysis)
library(ggdendro)
library(nFactors)
library(dplyr)
library(xtable)
library(reshape2)
library(psychometric)
library(betafunctions)
library(WrightMap)
library(eRm) 
library(plyr) 
library(WrightMap)

#-----------------------------------------------------------------
# Plot settings
#-----------------------------------------------------------------

theme_fig <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_blank(),
      legend.background = element_blank()
    )
}


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Data
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- read.delim("~/MFF/Psychometrie/data.csv", header=TRUE)
#data <- read.delim("C:/Users/kika/Desktop/UK/Psychometria/data.csv", header=TRUE)

summary(data[,-c(1:126)])

colnames(data)

data$country <- as.factor(data$country)
data$married <- as.factor(data$married)
data$education <- as.factor(data$education)
data$religion <- as.factor(data$religion)
data$voted <- as.factor(data$voted)
data$race <- as.factor(data$race)
data$orientation<-as.factor(data$orientation)
data$hand <- as.factor(data$hand)
data$engnat<- as.factor(data$engnat)


sd(data$age)

proportions(summary(data$gender))*100
proportions(summary(data$education))*100
proportions(summary(data$married))*100
proportions(summary(data$engnat))*100


dim(data[which(data$age>100),])

data$age[which(data$age>100)]<-NA

sd(data$age,na.rm=TRUE)
summary(data$age,na.rm=TRUE)

answers <- data[, grepl("A" , names(data))]

shinydata<-cbind(answers,data[,c("VCL6","VCL9","VCL12","age","gender")])
shinydata$total <- rowSums(answers)
shinydata <- shinydata %>%
  mutate(criterion = as.numeric(VCL6==0 & VCL9== 0 & VCL12==0))
shinydata<-shinydata[, !(colnames(shinydata) %in% c("VCL6","VCL9","VCL12"))]
data$total<- shinydata$total

summary(shinydata)

summary(as.factor(shinydata$criterion))


shinydata<-subset(shinydata,gender %in% c(1,2))
shinydata$gender<-shinydata$gender-1


#datasets fof ShinyItemAnalysis
write.csv(shinydata$criterion, "C:/Users/kika/Desktop/UK/Psychometria/criterion.csv", row.names=FALSE)
write.csv(shinydata$total, "C:/Users/kika/Desktop/UK/Psychometria/total.csv", row.names=FALSE)
write.csv(shinydata[,c(1:42)], "C:/Users/kika/Desktop/UK/Psychometria/answers.csv", row.names=FALSE)
write.csv(shinydata$gender, "C:/Users/kika/Desktop/UK/Psychometria/gender.csv", row.names=FALSE)


dim(shinydata)
head(shinydata)


summary(shinydata$total)


ggplot(data = shinydata, aes(total)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    col = "black",
    fill = "gold"
  ) +
  stat_function(fun = dnorm, colour = "red", linewidth = 0.8, args = list(mean = mean(shinydata$total), sd = sd(shinydata$total))) +
  xlab("Total score") + ylab("Density") + theme_fig()


ggplot(data.frame(score = shinydata$total ,
                  group = factor(rep(c("Total"), each = nrow(shinydata)), 
                                 levels = "Total")),
       aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  #geom_point(size = 2, color = "black", position = position_jitter(width = 0.1))  +
  ylab("Total score") + xlab("") +
  theme_fig() +
  theme(legend.position = "none")

TAB<-NULL
for (i in 1:42) {
  tab <-round(proportions(table(shinydata[,i])),2)
  TAB <-cbind(TAB,tab)
}

colnames(TAB)<-paste("Q",1:42,sep="")
TAB

# Z-score
(shinydata$zscore <- as.vector(scale(shinydata$total)))

shinydata$zscore[1:10]

# T-score
(shinydata$tscore <- 10 * shinydata$zscore + 50)
#

shinydata$tscore[1:10] 

# success rate
(shinydata$success_rate <- 100 * (shinydata$total / max(shinydata$total)))

shinydata$success_rate[1:10]

(shinydata$percentiles <- round(100 * ecdf(shinydata$total)(shinydata$total))) # percentiles

shinydata$percentiles[1:10]

plot(ecdf(shinydata$total), xlab = "Total score", ylab = "Percentile",main="Empirical distribution function")
ecdf(shinydata$total)(shinydata$total)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Test validity
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#criterion validity check- boxplot celkoveho skore podla kriteria
ggplot(shinydata, aes(x = criterion, y = total, fill = factor(criterion))) + 
  geom_boxplot()+
  labs(fill="Criterion")


cor(shinydata$criterion,shinydata$total,method="pearson")  # nedava zmysel pre binarnu premennu


#discriminant validity
cor(data$urban,data$total) # 0.0215306

ggplot(data, aes(x = factor(urban), y = total, fill = factor(urban))) + 
  geom_boxplot()+
  labs(fill="Criterion")


cor(data$total,as.numeric(data$religion)) #-0.02478027

ggplot(data, aes(x = factor(religion), y = total, fill = factor(religion))) + 
  geom_boxplot()+
  labs(fill="Criterion")


cor(data$total,as.numeric(data$engnat)) #-0.03187548

ggplot(data, aes(x = factor(engnat), y = total, fill = factor(engnat))) + 
  geom_boxplot()+
  labs(fill="Criterion")


cor(data$total,as.numeric(data$race)) #0.02273909

ggplot(data, aes(x = factor(race), y = total, fill = factor(race))) + 
  geom_boxplot()+
  labs(fill="Criterion")


cor(data$total,as.numeric(data$familysize)) #-0.02821805





polychoric(x=answers[,1:42])

(corans <- polychoric(x =answers[,1:42])$rho)

lavCor(answers[,1:42], ordered = names(answers[,1:42]), se = "standard", output = "est") 

plot_corr(Data = answers[,1:42], cor = "polychoric")

#cluster analysis

hc <- hclust(d = as.dist(1 - corans), method = "ward.D2")
ggdendrogram(data = hc)

plot_corr(Data = HCI[, 1:20], cor = "polychoric", clust_method = "ward.D2")



#Factor analysis

#function to calculate number of factors
calculate_factors <- function(n_items) {
  if (n_items > 0) {
    k <- 1  #k pocet faktorov
    d <- 1
    while (d > 0) {
      d <- 1 / 2 * (n_items - k) ^ 2 - 1 / 2 * (n_items + k)
      if (d >= 0) {
        k <- k + 1
      }
      else  {
        break
      }
    }
    max_fact <-
      k - 1  #navysili sme pocet faktorov, ale d uz je zaporne, tak treba krok spat
    return(max_fact)
  }
  else{
    print("n_items musi byt prirodzene cislo!")
    return()
  }
}

nitems <- 42
(max_factors <- calculate_factors(nitems)) 


do_fa<-function(data,nfactors){
  fa <- list()
  for(i in 1:nfactors) {
    fa[[i]] <-  factanal(x = data, factors = i,rotation ="varimax")
  }
  return(fa)
}


EFA<-do_fa(answers,9)

print_fa<-function(fa,digits,cutoff){
  if (missing(digits)){digits=2}
  if (missing(cutoff)){cutoff=0}
  print(fa, digits=digits, cutoff=cutoff, sort=TRUE)
}

print_fa(EFA)

plot_scree <- function(data) {
  ev <- eigen(cor(data))
  ap <-
    parallel(
      subject = nrow(data),
      var = ncol(data),
      rep = 100,
      cent = .05
    )
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  plotnScree(nS)
}

plot_scree(answers[,1:42])

rbPal <- colorRampPalette(c('red', 'blue'))

#zvoleny pocet parametrov pre FA, vizualizacia factor loagings
choose_fa <- 4

#loadings z konkretnej FA
loadings <- data.frame(EFA[[choose_fa]]$loadings[, 1:choose_fa])

plot_bars_FA <- function(choose_fa, loadings,type) {
  for (i in 1:choose_fa) {
    colF <- paste("colF", i, sep = "")
    nam <- paste("nam", i, sep = "")
    loadings[, colF] <-
      rbPal(20)[as.numeric(cut(loadings[, i], breaks = seq(-1, 1, length = 20)))]
    loadings[, nam] <-
      paste(row.names(loadings), round(loadings[, i], digits = 3), sep = "\n")
    barplot(
      abs(loadings[, i]),
      col = loadings[, colF],
      ylim = c(0, 1),
      ylab = paste("Factor ", i, sep = ""),
      names.arg = loadings[, nam],
      cex.names = 0.8,main=paste(choose_fa, " factors",sep="")
    )
  }
  
}

plot_bars_FA(choose_fa, loadings)



model3 <-
  "F1=~ Q3A+Q5A+Q10A+Q13A+Q16A+Q17A+Q21A+Q24A+Q26A+Q31A+Q34A+Q37A+Q38A+Q42A #deppresion
   F2=~ Q1A+Q6A+Q8A+Q9A+Q11A+Q12A+Q14A+Q18A+Q22A+Q27A+Q29A+Q32A+Q33A+Q35A+Q39A+Q40A  #stress
   F3=~ Q2A+Q4A+Q7A+Q15A+Q19A+Q20A+Q23A+Q25A+Q28A+Q30A+Q36A+Q41A"  #anxiety


fit_cfa<-cfa(model3,answers[,1:42])
summary(fit_cfa,fit.measures=TRUE, standardized=TRUE)


psych::lavaan.diagram(fit_cfa)


semPlot::semPaths(fit_cfa, what = "stdest", rotation = 4)



### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Test reliability
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

answers <- data[, grepl("A" , names(data))]

# Crohnbach's alpha
round(psychometric::alpha(answers), 4)
# 0.9699

# CI for Crohnbach's alpha
a <- psychometric::alpha(answers)
round(psychometric::alpha.CI(a, N= nrow(answers), k = ncol(answers), level = 0.95), 4)
# (0.9695, 0.9703)


# Other (than the Crohnbach's alpha) reliability measures 

# Split-half coefficient
# first-second split-half coefficient
df1 <- answers[, 1:21]
df2 <- answers[, 22:42]
cor_x <- cor(rowSums(df1), rowSums(df2))
split_half <- 2 * cor_x / (1+cor_x)
round(split_half, 4)
# 0.9774
# even-odd split-half coefficient
df_even <- answers[, seq(2, ncol(answers), by = 2)]
df_odd <- answers[, seq(1, ncol(answers), by = 2)]
cor_x_even_odd <- cor(rowSums(df_even), rowSums(df_odd))
split_half_even_odd <- 2 * cor_x_even_odd / (1 + cor_x_even_odd)
round(split_half_even_odd, 4)
# 0.9754

# One-way ANOVA

# long formaat
answers_long <- answers[,c(1:42)]
answers_long$ID <- 1:nrow(answers_long)
answers_long <- melt(answers_long, id.vars = "ID")

# estimate
n <- dim(answers)[1]
m <- 42
mean_answers <- tapply(answers_long$value, answers_long$ID, mean)
mean_overall <- mean(answers_long$value)
(SSTotal <- sum((answers_long$value - mean_overall)^2) )
(SSP <- m*sum((mean_answers - mean_overall)^2))
(SSe <- SSTotal - SSP)
(MSP <- SSP / (n - 1))
(MSe <- SSe / (n*m - n))
rel <- ((MSP - MSe)/m) / ((MSP - MSe) / m + MSe/m)
round(rel, 4)
# 0.9665

# omega
# additional- McDonald`s Omega
# 2 decimal places
omega_result <- omega(answers, nfactors=3, digits = 6)
# 4 decimal places
round(mdo(answers, fit = FALSE), 4)

# Spearman-Brown formula
# doubling the items
# k (factor in the B-S formula, 2)
k1 <- (42+42)/42
r1 <- 0.97
(r2 <- k1*r1 / (1+ (k-1)*r1))
# 0.9847716

# number of items needed to get 0.9
# k_2 is now number of items removed from the test
r1_2 <- 0.97
r2_2 <- 0.9
(k_2 <- ( r1_2 * (1 - r2_2) ) / (r2_2 * (1 - r1_2) )) 
# 3.592593
# rounded it is 4 items difference, but we take
# 3 items to get over the bound 0.9
# 39 items needed

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Item analysis
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# dif. and discrimination indices
calculate_item_indices <- function(data) {
  indices <- matrix(NA, ncol = 5, nrow = ncol(data))
  colnames(indices) <- c("difficulty", "RIR", "RIT", "ULI", "alphaDrop")
  rownames(indices) <- colnames(data)
  
  total_score <- rowSums(data, na.rm = TRUE)
  alpha_results <- psych::alpha(data)
  alpha_drops <- alpha_results$alpha.drop$raw_alpha
  
  for (i in 1:ncol(data)) {
    item <- data[, i]
    
    # difficulty
    indices[i, "difficulty"] <- mean(item, na.rm = TRUE)
    
    # RIT and RIR
    rest <- total_score - item
    indices[i, "RIT"] <- cor(item, total_score, use = "complete.obs")
    indices[i, "RIR"] <- cor(item, rest, use = "complete.obs")
    
    # ULI
    sorted_scores <- sort(total_score, decreasing = TRUE)
    upper_threshold <- sorted_scores[ceiling(length(sorted_scores) / 3)]
    lower_threshold <- sorted_scores[ceiling(2 * length(sorted_scores) / 3)]
    
    upper_group <- item[total_score >= upper_threshold]
    lower_group <- item[total_score <= lower_threshold]
    indices[i, "ULI"] <- (mean(upper_group, na.rm = TRUE) - mean(lower_group, na.rm = TRUE))/4
    
    # alphaDrop
    indices[i, "alphaDrop"] <- alpha_drops[i]
  }
    indices_df <- as.data.frame(indices)
  
  data.frame(indices_df)
}

# printing the latex table 
latex_table <- calculate_item_indices(answers[,c(1:42)])
# formatting
first_half <- latex_table[1:21, ]
second_half <- latex_table[22:42, ]
first_half$Question <- rownames(first_half)
first_half <- first_half[, c("Question", setdiff(names(first_half), "Question"))]
second_half$Question <- rownames(second_half)
second_half <- second_half[, c("Question", setdiff(names(second_half), "Question"))]
combined_table <- cbind(first_half, second_half)
print(xtable(combined_table), include.rownames = FALSE)


# empirical ICCs (generalizationfor nominal items)
# item 23
plotDistractorAnalysis(Data = answers, item = 23, num.group = 4)
# item 34
plotDistractorAnalysis(Data = answers, item = 34, num.group = 4)


# regression model
# binarized dataset
answers <- answers[,c(1:42)]
answer_bin <- answers[,c(1:42)]

transform_value <- function(x) {
  ifelse(x <= 2, 0, 1)  # Convert values 1, 2 to 1 and values 3, 4 to 2
}
answer_bin <- answer_bin %>% dplyr::mutate(across(everything(), transform_value))

answer_bin$Total <- rowSums(answer_bin)
# z-scores
z_scores <- scale(answer_bin$Total)
# item 23
fit2PL <- glm(answer_bin[,23] ~ z_scores, family = binomial)
IRTpars13 <- c(coef(fit2PL)[2], -coef(fit2PL)[1] / coef(fit2PL)[2])
IRTpars13 <- setNames(IRTpars13, c("a", "b"))
# a and b parameters
round(IRTpars13, 2)

# item 34
fit2PL <- glm(answer_bin[,34] ~ z_scores, family = binomial)
IRTpars13 <- c(coef(fit2PL)[2], -coef(fit2PL)[1] / coef(fit2PL)[2])
IRTpars13 <- setNames(IRTpars13, c("a", "b"))
# a and b parameters
round(IRTpars13, 2)



### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Item response theory models 
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# cumulative hazard model for polytomous multidimensional items

load("mGRM_file.RData")
#mGRM <- mirt(answers, model = 3, itemtype = "graded")
#save(mGRM, file = "mGRM_file.RData")
# model coefficients
coef(mGRM, simplify = TRUE)
xtable(coef(mGRM, simplify = TRUE)$items)
plot(mGRM, which.items = c(1, 25), type = "trace")
mGRM
itemplot(mGRM, item = "Q1A")


# Rasch-type model
depression <- answers[, c("Q3A", "Q5A", "Q10A", "Q13A", "Q16A", "Q17A", 
                          "Q21A", "Q24A", "Q26A", "Q31A", "Q34A", "Q37A", 
                          "Q38A", "Q42A")]
depression_b <- as.data.frame(lapply(depression, function(x) ifelse(x %in% c(1, 2), 0, 1)))
#anxiety_b <- as.data.frame(lapply(anxiety, function(x) ifelse(x %in% c(1, 2), 0, 1)))
#stress_b <- as.data.frame(lapply(stress, function(x) ifelse(x %in% c(1, 2), 0, 1)))

# for depression
# Subtracting 1 from observations so that the lowest category is 0 (required for eRm)
depression_balanced <- depression-1

PC_model <- PCM(depression_balanced)
summary(PC_model)

# Wright map
plotPImap(PC_model)

# ICCs
eRm::plotICC(PC_model,  ask = FALSE)

# For item 3
plotICC(PC_model, item.subset = 2, xlab = "depression", ylab = "Probability", main = "ICC for item Q5", ask = FALSE) 

# For item 5
plotICC(PC_model, item.subset = 2, xlab = "depression", ylab = "Probability", main = "ICC for item Q5", ask = FALSE) 
itemplot(PC_model, item = 1)

# IICs
plotINFO(PC_model, type = "item", xlab = "depression z-score", mainI = "Item Information Curve")
plotINFO(PC_model, type = "test", xlab = "depression z-score", mainT = "Test Information Curve")

# item parameters table
xtable(summary(PC_model))

# threshold parameters
print(thresholds(PC_model))
# their standard errors
print(thresholds(PC_model)$se.thresh)

# most informative items
theta_values <- c(-1, 0, 1)

# Calculate item information at the specified theta values
(info_list <- item_info(PC_model, theta = theta_values))
# item6, item6, item13 


# first respondent
person_params <- person.parameter(PC_model)
first_respondent_ability <- round(person_params$thetapar$NAgroup1[1], 2)
first_respondent_se <- round(person_params$se.theta$NAgroup1[1], 2)

# Print the ability estimate and standard error for the first respondent
ci_lower <- round(first_respondent_ability - 1.96 * first_respondent_se, 2)
ci_upper <- round(first_respondent_ability + 1.96 * first_respondent_se, 2)

# Print the confidence interval
print(paste("Ability Estimate:", first_respondent_ability)) #1.14
print(paste("Standard Error:", first_respondent_se)) #0.32
print(paste("95% Confidence Interval: (", ci_lower, ",", ci_upper, ")")) #( 0.51 , 1.77 )


# Relationship Between IRT Ability Estimate and Standardized Total Scores
depression_balanced$total_score <- rowSums(depression_balanced)
depression_balanced$standardized_total_score <- scale(depression_balanced$total_score)
person_params <- person.parameter(PC_model)
plot(depression_balanced$standardized_total_score, person_params$theta.table$`Person Parameter`, 
     xlab = "Standardized Total Score", 
     ylab = "IRT Ability Estimate", 
     main = "Relationship Between IRT Ability Estimate and Standardized Total Scores")

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
###   Differential item functioning
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# for each latent trait separately
depression <- answers[, c("Q3A", "Q5A", "Q10A", "Q13A", "Q16A", "Q17A", 
                          "Q21A", "Q24A", "Q26A", "Q31A", "Q34A", "Q37A", 
                          "Q38A", "Q42A")]
depression$gender <- data$gender 
depression <- subset(depression, gender %in% c("1", "2"))
depression$gender <- as.factor(depression$gender )
stress <- answers[, c("Q1A", "Q6A", "Q8A", "Q11A", "Q12A", "Q14A", "Q18A", 
                      "Q22A", "Q27A", "Q29A", "Q32A", "Q33A", "Q35A", 
                      "Q39A")]
stress$gender <- data$gender 
stress <- subset(stress, gender %in% c("1", "2"))
stress$gender <- as.factor(stress$gender )

anxiety <- answers[, c("Q2A", "Q4A", "Q7A", "Q9A", "Q15A", "Q19A", 
                       "Q20A", "Q23A", "Q25A", "Q28A", "Q30A", "Q36A", "Q40A", 
                       "Q41A")]
anxiety$gender <- data$gender 
anxiety <- subset(anxiety, gender %in% c("1", "2"))
anxiety$gender <- as.factor(anxiety$gender )

# testing both DIF effects with adjacent category logit model
model1 <- difORD(subset(depression, select = -gender), depression$gender, focal.name = 2, model = "cumulative", alpha= 0.01)
model2 <- difORD(subset(anxiety, select = -gender), depression$gender, focal.name = 1, model = "cumulative", alpha = 0.01)
model3 <- difORD(subset(stress, select = -gender), depression$gender, focal.name = 1, model = "cumulative", alpha = 0.01)
model1
model2
model3


#depression
llM0 <- model1$llM0
llM1 <- model1$llM1
ll_0 <- model1$llM0 
R2_McFadden0 <- 1 - (llM0 / ll_0)
R2_McFadden1 <- 1 - (llM1 / ll_0)
Delta_R2_McFadden <- R2_McFadden1 - R2_McFadden0
max(Delta_R2_McFadden) #0.003154476

#anxiety
llM0 <- model2$llM0
llM1 <- model2$llM1
ll_0 <- model2$llM0 
R2_McFadden0 <- 1 - (llM0 / ll_0)
R2_McFadden1 <- 1 - (llM1 / ll_0)
Delta_R2_McFadden <- R2_McFadden1 - R2_McFadden0
max(Delta_R2_McFadden) #0.002330008

#stress
llM0 <- model3$llM0
llM1 <- model3$llM1
ll_0 <- model3$llM0 
R2_McFadden0 <- 1 - (llM0 / ll_0)
R2_McFadden1 <- 1 - (llM1 / ll_0)
Delta_R2_McFadden <- R2_McFadden1 - R2_McFadden0
max(Delta_R2_McFadden) #0.006364947

# random subset of 500 items
n <- 500
set.seed(12345) 
depression_subset <- depression[sample(nrow(depression), n), ]
set.seed(12345) 
anxiety_subset <- anxiety[sample(nrow(depression), n), ]
set.seed(12345) 
stress_subset <- stress[sample(nrow(depression), n), ]
model1_subset <- difORD(subset(depression_subset, select = -gender), depression_subset$gender, focal.name = 2, model = "cumulative", alpha= 0.01)
model2_subset <- difORD(subset(anxiety_subset, select = -gender), anxiety_subset$gender, focal.name = 1, model = "cumulative", alpha = 0.01)
model3_subset <- difORD(subset(stress_subset, select = -gender), stress_subset$gender, focal.name = 1, model = "cumulative", alpha = 0.01)
model1_subset
model2_subset
model3_subset

llM0 <- model1_subset$llM0
llM1 <- model1_subset$llM1
ll_0 <- model1_subset$llM0 
R2_McFadden0 <- 1 - (llM0 / ll_0)
R2_McFadden1 <- 1 - (llM1 / ll_0)
(Delta_R2_McFadden <- R2_McFadden1 - R2_McFadden0)
max(Delta_R2_McFadden) # 0.02

llM0 <- model3_subset$llM0
llM1 <- model3_subset$llM1
ll_0 <- model3_subset$llM0 
R2_McFadden0 <- 1 - (llM0 / ll_0)
R2_McFadden1 <- 1 - (llM1 / ll_0)
(Delta_R2_McFadden <- R2_McFadden1 - R2_McFadden0)
max(Delta_R2_McFadden) #0.01

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Discussion
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

