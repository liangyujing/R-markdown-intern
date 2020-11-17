
---
title             : "Data analysis in RStudio: Comparative communication. Study 1: Initial appraisal of implicit and explicit differences. Extension: The role of group membership"
shorttitle        : "Statistics VI Assigment"

author: 
  - name          : "Yujing Liang"
    affiliation   : "1"
    corresponding : yes    # Define only one corresponding author
    address       : "Tiensestraat 102, 3000 Leuven"
    email         : "yujing.liang@student.kuleuven.com"

affiliation:
  - id            : "1"
    institution   : "KU Leuven"

keywords          : "keywords"
wordcount         : "X"

bibliography      : ["r-references.bib"]

floatsintext      : no
figurelist        : no
tablelist         : no
footnotelist      : no
linenumbers       : no
mask              : no
draft             : no

documentclass     : "apa6"
classoption       : "man"
output            : papaja::apa6_pdf
---


```{r analysis-set, include = FALSE, echo = FALSE}
rm(list=ls())
# Set your working dir as the current dir
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis")
dev = "png"
# Read data
library("haven")
my_data_original_1a <- read_sav("Study1_ready_yujing_1a_short.sav")
my_data_original_1b <- read_sav("Study1_ready_yujing_1b_short.sav")
#summary(my_data_original_1a)

# loading libraries used for analysis
library("papaja")
library("pwr")
library("MASS")
library("psych")
library("ggpubr")
library("lsmeans")
library("multcompView")
library("ggpubr")
library("sjstats")
library("car")
library("ggplot2")
library("fancycut")
library("numform")
library("ez")
library("multcompView")
library("lsmeans")
```

```{r Truth_gender, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_G_T1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","TruthPosGender","TruthNegGender"))

# Truth
#Main effect of valence, prediction: positive > negative 
Ttest_GT<- t.test(my_data_extracted_G_T1$TruthPosGender, my_data_extracted_G_T1$TruthNegGender, paired=TRUE, alternative = "two.sided")
sum_Ttest_GT<- summary(Ttest_GT)

#Stack
library(reshape2)
my_data_gender_T1 <- melt(my_data_extracted_G_T1, id.vars=1:4)
my_data_gender_T1
#valence
my_data_gender_T1$valence <- ifelse(my_data_gender_T1$variable=="TruthPosGender", "1","2")
# Get descriptives
library(psych) 
Descriptives_GT_TTEST<-describeBy(my_data_gender_T1, 
           group = my_data_gender_T1$valence)


# Truth: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_G_T2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","TruthInPos","TruthOutPos", "TruthInNeg", "TruthOutNeg"))

#Stack
library(reshape2)
my_data_gender_T <- melt(my_data_extracted_G_T2, id.vars=1:4)
my_data_gender_T

##membership
my_data_gender_T$membership <- ifelse(my_data_gender_T$variable=="TruthInPos", "1","2")
my_data_gender_T$membership[my_data_gender_T$variable=="TruthOutPos"] <- 2
my_data_gender_T$membership[my_data_gender_T$variable=="TruthInNeg"] <- 1
my_data_gender_T$membership[my_data_gender_T$variable=="TruthOutNeg"] <- 2
#valence
my_data_gender_T$valence <- ifelse(my_data_gender_T$variable=="TruthInPos", "1","2")
my_data_gender_T$valence[my_data_gender_T$variable=="TruthOutPos"] <- 1
my_data_gender_T$valence[my_data_gender_T$variable=="TruthInNeg"] <- 2
my_data_gender_T$valence[my_data_gender_T$variable=="TruthOutNeg"] <- 2

# Factor
my_data_gender_T$membership<- factor(my_data_gender_T$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_T$valence<- factor(my_data_gender_T$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_T$ID<- factor(my_data_gender_T$ID)

# Truth: Group membership x Valence Interaction 
Interaction_GT  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_gender_T)
sum_Interaction_GT <- summary(Interaction_GT)

#Interaction_GT <- ezANOVA(my_data_gender_T, # specify data frame
 #                    dv = value, # specify dependent variable 
  #                   wid = ID, # specify the subject variable
   #                  within = .(valence, membership), # specify within-subject variables
    #                 detailed = TRUE) # get a detailed table that includes SS
##table(my_data_gender_T$valence, my_data_gender_T$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_GT)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_GT<-emmeans(Interaction_GT, ~valence|membership)
Contrast1_GT <- pairs(Simple.Effects.By.membership_GT,adjust='none')
sum_Contrast1_GT<-summary(Contrast1_GT)

#???Contrast 2:by valence
Simple.Effects.By.valence_GT<-emmeans(Interaction_GT, ~membership|valence)
Contrast2_GT <- pairs(Simple.Effects.By.valence_GT,adjust='none')
sum_Contrast2_GT<-summary(Contrast2_GT)

##3 way
#Interaction_GT3_C <- aov(value~Consistency*valence*membership,data=my_data_gender_T)
Interaction_GT3_C  <- aov(value ~ Consistency*valence*membership+
                            Error(ID/(valence*membership)),
                            data=my_data_gender_T)
sum_Interaction_GT3_C<-summary(Interaction_GT3_C)

#Interaction_GT3_F <- aov(value~Format*valence*membership+Error(ID / (valence*membership)),data=my_data_gender_T)
Interaction_GT3_F  <- aov(value ~ Format*valence*membership+
                            Error(ID/(Format*valence*membership)),
                            data=my_data_gender_T)
sum_Interaction_GT3_F<-summary(Interaction_GT3_F)

##faxian
GT.sub_Con_1 <- subset(my_data_gender_T, Consistency == 1)
GT.sub_Con_2 <- subset(my_data_gender_T, Consistency == 2)
Interaction_GT3_Con_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_1)
sum_Interaction_GT3_Con_1<-summary(Interaction_GT3_Con_1)

Interaction_GT3_Con_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_2)
sum_Interaction_GT3_Con_2<-summary(Interaction_GT3_Con_2)

GT.sub_For_1 <- subset(my_data_gender_T, Format == 1)
GT.sub_For_2 <- subset(my_data_gender_T, Format == 2)
Interaction_GT3_For_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_1)
sum_Interaction_GT3_For_1<-summary(Interaction_GT3_For_1)
Interaction_GT3_For_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_2)
sum_Interaction_GT3_For_2<-summary(Interaction_GT3_For_2)

#visualize
ggplot(data = my_data_gender_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_gender_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Interaction effect of valence and membership on the truth of gender-related claims."}
interaction.plot(x.factor = my_data_gender_T$valence, 
                 trace.factor = my_data_gender_T$membership, 
                 response = my_data_gender_T$value, fun = mean, 
                 trace.label = "Membership",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))
```

```{r Truth_age, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_A_T1 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","TruthPosAge","TruthNegAge"))

# Truth
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Ttest_AT<- t.test(my_data_extracted_A_T1$TruthPosAge, my_data_extracted_A_T1$TruthNegAge, paired=TRUE, alternative = "two.sided")
sum_Ttest_AT<- summary(Ttest_AT)

#Stack
library(reshape2)
my_data_age_T1 <- melt(my_data_extracted_A_T1, id.vars=1:4)
my_data_age_T1
#valence
my_data_age_T1$valence <- ifelse(my_data_age_T1$variable=="TruthPosAge", "1","2")
# Get descriptives
library(psych) 
Descriptives_AT_TTEST<-describeBy(my_data_age_T1, 
           group = my_data_age_T1$valence)

# Truth: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_T2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","TruthInPos","TruthOutPos", "TruthInNeg", "TruthOutNeg"))
#Stack
library(reshape2)
my_data_age_T <- melt(my_data_extracted_A_T2, id.vars=1:4)
my_data_age_T

##membership
my_data_age_T$membership <- ifelse(my_data_age_T$variable=="TruthInPos", "1","2")
my_data_age_T$membership[my_data_age_T$variable=="TruthOutPos"] <- 2
my_data_age_T$membership[my_data_age_T$variable=="TruthInNeg"] <- 1
my_data_age_T$membership[my_data_age_T$variable=="TruthOutNeg"] <- 2
#valence
my_data_age_T$valence <- ifelse(my_data_age_T$variable=="TruthInPos", "1","2")
my_data_age_T$valence[my_data_age_T$variable=="TruthOutPos"] <- 1
my_data_age_T$valence[my_data_age_T$variable=="TruthInNeg"] <- 2
my_data_age_T$valence[my_data_age_T$variable=="TruthOutNeg"] <- 2

# Factor
my_data_age_T$membership<- factor(my_data_age_T$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_T$valence<- factor(my_data_age_T$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_T$ID<- factor(my_data_age_T$ID)

# Truth: Group membership x Valence Interaction 
Interaction_AT  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_age_T)
sum_Interaction_AT <- summary(Interaction_AT)

#print(model.tables(Interaction_AT,"means"),digits=3)
##table(my_data_age_T$valence, my_data_age_T$membership)
# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AT)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_AT<-emmeans(Interaction_AT, ~valence|membership)
Contrast1_AT <- pairs(Simple.Effects.By.membership_AT,adjust='paired')
sum_Contrast1_AT<-summary(Contrast1_AT)

#???Contrast 2:by valence
Simple.Effects.By.valence_AT<-emmeans(Interaction_AT, ~membership|valence)
Contrast2_AT <- pairs(Simple.Effects.By.valence_AT,adjust='paired')
sum_Contrast2_AT<-summary(Contrast2_AT)

##3 way
Interaction_AT3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_age_T)
sum_Interaction_AT3_C<-summary(Interaction_AT3_C)

Interaction_AT3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_age_T)
sum_Interaction_AT3_F<-summary(Interaction_AT3_F)

##faxian
GT.sub_Con_1 <- subset(my_data_age_T, Consistency == 1)
GT.sub_Con_2 <- subset(my_data_age_T, Consistency == 2)
Interaction_AT3_Con_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_1)
sum_Interaction_AT3_Con_1<-summary(Interaction_AT3_Con_1)

Interaction_AT3_Con_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_2)
sum_Interaction_AT3_Con_2<-summary(Interaction_AT3_Con_2)

GT.sub_For_1 <- subset(my_data_age_T, Format == 1)
GT.sub_For_2 <- subset(my_data_age_T, Format == 2)
Interaction_AT3_For_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_1)
sum_Interaction_AT3_For_1<-summary(Interaction_AT3_For_1)
Interaction_AT3_For_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_2)
sum_Interaction_AT3_For_2<-summary(Interaction_AT3_For_2)

#visualize
ggplot(data = my_data_age_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))

ggplot(data = my_data_age_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm', aes(group=membership))
```

```{r,  fig.width=6, fig.height=4, fig.cap="Interaction effect of valence and membership on the truth of age-related claims."}
interaction.plot(x.factor = my_data_age_T$valence, 
                 trace.factor= my_data_age_T$membership, 
                 response = my_data_age_T$value, fun = mean, 
                 trace.label = "Membership",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))
```

```{r Acc_gender, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_G_A1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","AccPosGender","AccNegGender"))

Ttest_GA<- t.test(my_data_extracted_G_A1$AccPosGender, my_data_extracted_G_A1$AccNegGender, paired=TRUE, alternative = "two.sided")
sum_Ttest_GA<- summary(Ttest_GA)


#Stack
library(reshape2)
my_data_gender_A1 <- melt(my_data_extracted_G_A1, id.vars=1:4)
my_data_gender_A1

#valence
my_data_gender_A1$valence <- ifelse(my_data_gender_A1$variable=="AccPosGender", "1","2")

# Get descriptives
library(psych) 
Descriptives_GA_TTEST<-describeBy(my_data_gender_A1, 
           group = my_data_gender_A1$valence)


# Acc: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_G_A2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","AccInPos","AccOutPos", "AccInNeg", "AccOutNeg"))
#Stack
library(reshape2)
my_data_gender_A <- melt(my_data_extracted_G_A2, id.vars=1:4)
my_data_gender_A

##membership
my_data_gender_A$membership <- ifelse(my_data_gender_A$variable=="AccInPos", "1","2")
my_data_gender_A$membership[my_data_gender_A$variable=="AccOutPos"] <- 2
my_data_gender_A$membership[my_data_gender_A$variable=="AccInNeg"] <- 1
my_data_gender_A$membership[my_data_gender_A$variable=="AccOutNeg"] <- 2
#valence
my_data_gender_A$valence <- ifelse(my_data_gender_A$variable=="AccInPos", "1","2")
my_data_gender_A$valence[my_data_gender_A$variable=="AccOutPos"] <- 1
my_data_gender_A$valence[my_data_gender_A$variable=="AccInNeg"] <- 2
my_data_gender_A$valence[my_data_gender_A$variable=="AccOutNeg"] <- 2

# Factor
my_data_gender_A$membership<- factor(my_data_gender_A$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_A$valence<- factor(my_data_gender_A$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_A$ID<- factor(my_data_gender_A$ID)

# Acc: Group membership x Valence Interaction 
#Interaction_GA  <- aov(value ~ valence*membership,data=my_data_gender_A)
Interaction_GA  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_gender_A)
sum_Interaction_GA <- summary(Interaction_GA)
##table(my_data_gender_A$valence, my_data_gender_A$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_GA)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_GA<-emmeans(Interaction_GA, ~valence|membership)
Contrast1_GA <- pairs(Simple.Effects.By.membership_GA,adjust='none')
sum_Contrast1_GA<-summary(Contrast1_GA)

#???Contrast 2:by valence
Simple.Effects.By.valence_GA<-emmeans(Interaction_GA, ~membership|valence)
Contrast2_GA <- pairs(Simple.Effects.By.valence_GA,adjust='none')
sum_Contrast2_GA<-summary(Contrast2_GA)

##3 way
Interaction_GA3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_gender_A)
sum_Interaction_GA3_C<-summary(Interaction_GA3_C)

Interaction_GA3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_gender_A)
sum_Interaction_GA3_F<-summary(Interaction_GA3_F)

##faxian
GT.sub_Con_1 <- subset(my_data_gender_A, Consistency == 1)
GT.sub_Con_2 <- subset(my_data_gender_A, Consistency == 2)
Interaction_GA3_Con_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_1)
sum_Interaction_GA3_Con_1<-summary(Interaction_GA3_Con_1)
Interaction_GA3_Con_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_Con_2)
sum_Interaction_GA3_Con_2<-summary(Interaction_GA3_Con_2)

GT.sub_For_1 <- subset(my_data_gender_A, Format == 1)
GT.sub_For_2 <- subset(my_data_gender_A, Format == 2)
Interaction_GA3_For_1<- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_1)
sum_Interaction_GA3_For_1<-summary(Interaction_GA3_For_1)

Interaction_GA3_For_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=GT.sub_For_2)
sum_Interaction_GA3_For_2<-summary(Interaction_GA3_For_2)

#visualize
ggplot(data = my_data_gender_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_gender_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r,  fig.width=6, fig.height=4, fig.cap="Interaction effect of valence and membership on the acceptability of gender-related claims."}
interaction.plot(x.factor = my_data_gender_A$valence, trace.factor = my_data_gender_A$membership, 
                 response = my_data_gender_A$value, fun = mean, 
                 trace.label = "Membership",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
```

```{r Acc_age, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_A_A1 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","AccPosAge","AccNegAge"))

Ttest_AA<- t.test(my_data_extracted_A_A1$AccPosAge, my_data_extracted_A_A1$AccNegAge, paired=TRUE, alternative = "two.sided")
sum_Ttest_AA<- summary(Ttest_AA)

#Stack
library(reshape2)
my_data_age_A1 <- melt(my_data_extracted_A_A1, id.vars=1:4)
my_data_age_A1

#valence
my_data_age_A1$valence <- ifelse(my_data_age_A1$variable=="AccPosAge", "1","2")

# Get descriptives
library(psych) 
Descriptives_AA_TTEST<-describeBy(my_data_age_A1, 
           group = my_data_age_A1$valence)

# Acc: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_A2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","AccInPos","AccOutPos", "AccInNeg", "AccOutNeg"))
#Stack
library(reshape2)
my_data_age_A <- melt(my_data_extracted_A_A2, id.vars=1:4)
my_data_age_A

##membership
my_data_age_A$membership <- ifelse(my_data_age_A$variable=="AccInPos", "1","2")
my_data_age_A$membership[my_data_age_A$variable=="AccOutPos"] <- 2
my_data_age_A$membership[my_data_age_A$variable=="AccInNeg"] <- 1
my_data_age_A$membership[my_data_age_A$variable=="AccOutNeg"] <- 2
#valence
my_data_age_A$valence <- ifelse(my_data_age_A$variable=="AccInPos", "1","2")
my_data_age_A$valence[my_data_age_A$variable=="AccOutPos"] <- 1
my_data_age_A$valence[my_data_age_A$variable=="AccInNeg"] <- 2
my_data_age_A$valence[my_data_age_A$variable=="AccOutNeg"] <- 2

# Factor
my_data_age_A$membership<- factor(my_data_age_A$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_A$valence<- factor(my_data_age_A$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_A$ID<- factor(my_data_age_A$ID)

# Acc: Group membership x Valence Interaction 
#Interaction_AA  <- aov(value ~ valence*membership,data=my_data_age_A)
Interaction_AA  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_age_A)
sum_Interaction_AA <- summary(Interaction_AA)
##table(my_data_age_A$valence, my_data_age_A$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AA)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_AA<-emmeans(Interaction_AA, ~valence|membership)
Contrast1_AA <- pairs(Simple.Effects.By.membership_AA,adjust='none')
sum_Contrast1_AA<-summary(Contrast1_AA)

#???Contrast 2:by valence
Simple.Effects.By.valence_AA<-emmeans(Interaction_AA, ~membership|valence)
Contrast2_AA <- pairs(Simple.Effects.By.valence_AA,adjust='none')
sum_Contrast2_AA<-summary(Contrast2_AA)

##3 way
Interaction_AA3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_age_A)
sum_Interaction_AA3_C<-summary(Interaction_AA3_C)

Interaction_AA3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_age_A)
sum_Interaction_AA3_F<-summary(Interaction_AA3_F)

##faxian
AA.sub_Con_1 <- subset(my_data_age_A, Consistency == 1)
AA.sub_Con_2 <- subset(my_data_age_A, Consistency == 2)
Interaction_AA3_Con_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=AA.sub_Con_1)
sum_Interaction_AA3_Con_1<-summary(Interaction_AA3_Con_1)
Interaction_AA3_Con_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=AA.sub_Con_2)
sum_Interaction_AA3_Con_2<-summary(Interaction_AA3_Con_2)

AA.sub_For_1 <- subset(my_data_age_A, Format == 1)
AA.sub_For_2 <- subset(my_data_age_A, Format == 2)
Interaction_AA3_For_1 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=AA.sub_For_1)
sum_Interaction_AA3_For_1<-summary(Interaction_AA3_For_1)
Interaction_AA3_For_2 <- aov(value~valence*membership+
                             Error(ID/(valence*membership)),
                             data=AA.sub_For_2)
sum_Interaction_AA3_For_2<-summary(Interaction_AA3_For_2)

#visualize
ggplot(data = my_data_age_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))

ggplot(data = my_data_age_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r,  fig.width=6, fig.height=4, fig.cap="Interaction effect of valence and membership on the acceptability of age-related claims."}
interaction.plot(x.factor = my_data_age_A$valence, trace.factor = my_data_age_A$membership,
                 trace.lab = "Membership",
                 response = my_data_age_A$value, fun = mean, 
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Membership", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
```

```{r Posi_gender, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_G_P1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","PosiIn","PosiOut"))
Ttest_GP<- t.test(my_data_extracted_G_P1$PosiIn, my_data_extracted_G_P1$PosiOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_GP<- summary(Ttest_GP)

#Stack
library(reshape2)
my_data_gender_P1 <- melt(my_data_extracted_G_P1, id.vars=1:4)
my_data_gender_P1
#valence
my_data_gender_P1$membership <- ifelse(my_data_gender_P1$variable=="PosiIn", "1","2")
# Get descriptives
library(psych) 
Descriptives_GP_TTEST<-describeBy(my_data_gender_P1, 
           group = my_data_gender_P1$membership)


# Posi: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_G_P2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","PosiInPos","PosiOutPos", "PosiInNeg", "PosiOutNeg"))

#Stack
library(reshape2)
my_data_gender_P <- melt(my_data_extracted_G_P2, id.vars=1:4)
my_data_gender_P

##membership
my_data_gender_P$membership <- ifelse(my_data_gender_P$variable=="PosiInPos", "1","2")
my_data_gender_P$membership[my_data_gender_P$variable=="PosiOutPos"] <- 2
my_data_gender_P$membership[my_data_gender_P$variable=="PosiInNeg"] <- 1
my_data_gender_P$membership[my_data_gender_P$variable=="PosiOutNeg"] <- 2
#valence
my_data_gender_P$valence <- ifelse(my_data_gender_P$variable=="PosiInPos", "1","2")
my_data_gender_P$valence[my_data_gender_P$variable=="PosiOutPos"] <- 1
my_data_gender_P$valence[my_data_gender_P$variable=="PosiInNeg"] <- 2
my_data_gender_P$valence[my_data_gender_P$variable=="PosiOutNeg"] <- 2

# Factor
my_data_gender_P$membership<- factor(my_data_gender_P$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_P$valence<- factor(my_data_gender_P$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_P$ID<- factor(my_data_gender_P$ID)

# Posi: Group membership x Valence Interaction 
Interaction_GP  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_gender_P)
sum_Interaction_GP <- summary(Interaction_GP)
##table(my_data_gender_P$valence, my_data_gender_P$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_GP)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_GP<-emmeans(Interaction_GP, ~valence|membership)
Contrast1_GP <- pairs(Simple.Effects.By.membership_GP,adjust='none')
sum_Contrast1_GP<-summary(Contrast1_GP)


#???Contrast 2:by valence
Simple.Effects.By.valence_GP<-emmeans(Interaction_GP, ~membership|valence)
Contrast2_GP <- pairs(Simple.Effects.By.valence_GP,adjust='none')
sum_Contrast2_GP<-summary(Contrast2_GP)

##3 way
Interaction_GP3_C <- aov(value~Consistency*valence*membership+
                           Error(ID/(valence*membership)),
                           data=my_data_gender_P)
sum_Interaction_GP3_C<-summary(Interaction_GP3_C)

Interaction_GP3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_gender_P)
sum_Interaction_GP3_F<-summary(Interaction_GP3_F)

##faxian
GP.sub_Con_1 <- subset(my_data_gender_P, Consistency == 1)
GP.sub_Con_2 <- subset(my_data_gender_P, Consistency == 2)
Interaction_GP3_Con_1 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=GP.sub_Con_1)
sum_Interaction_GP3_Con_1<-summary(Interaction_GP3_Con_1)
Interaction_GP3_Con_2 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=GP.sub_Con_2)
sum_Interaction_GP3_Con_2<-summary(Interaction_GP3_Con_2)

GP.sub_For_1 <- subset(my_data_gender_P, Format == 1)
GP.sub_For_2 <- subset(my_data_gender_P, Format == 2)
Interaction_GP3_For_1 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=GP.sub_For_1)
sum_Interaction_GP3_For_1<-summary(Interaction_GP3_For_1)
Interaction_GP3_For_2 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=GP.sub_For_2)
sum_Interaction_GP3_For_2<-summary(Interaction_GP3_For_2)

#visualize
ggplot(data = my_data_gender_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_gender_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r Posi_age, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_A_P1 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","PosiIn","PosiOut"))

Ttest_AP<- t.test(my_data_extracted_A_P1$PosiIn, my_data_extracted_A_P1$PosiOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_AP<- summary(Ttest_AP)

#Stack
library(reshape2)
my_data_age_P1 <- melt(my_data_extracted_A_P1, id.vars=1:4)
my_data_age_P1

#valence
my_data_age_P1$membership <- ifelse(my_data_age_P1$variable=="PosiIn", "1","2")

# Get descriptives
library(psych) 
Descriptives_AP_TTEST<-describeBy(my_data_age_P1, 
           group = my_data_age_P1$membership)

# Posi: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_P2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","PosiInPos","PosiOutPos", "PosiInNeg", "PosiOutNeg"))
#Stack
library(reshape2)
my_data_age_P <- melt(my_data_extracted_A_P2, id.vars=1:4)
my_data_age_P

##membership
my_data_age_P$membership <- ifelse(my_data_age_P$variable=="PosiInPos", "1","2")
my_data_age_P$membership[my_data_age_P$variable=="PosiOutPos"] <- 2
my_data_age_P$membership[my_data_age_P$variable=="PosiInNeg"] <- 1
my_data_age_P$membership[my_data_age_P$variable=="PosiOutNeg"] <- 2
#valence
my_data_age_P$valence <- ifelse(my_data_age_P$variable=="PosiInPos", "1","2")
my_data_age_P$valence[my_data_age_P$variable=="PosiOutPos"] <- 1
my_data_age_P$valence[my_data_age_P$variable=="PosiInNeg"] <- 2
my_data_age_P$valence[my_data_age_P$variable=="PosiOutNeg"] <- 2

# Factor
my_data_age_P$membership<- factor(my_data_age_P$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_P$valence<- factor(my_data_age_P$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_P$ID<- factor(my_data_age_P$ID)

# Posi: Group membership x Valence Interaction 
Interaction_AP  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_age_P)
sum_Interaction_AP <- summary(Interaction_AP)
##table(my_data_age_P$valence, my_data_age_P$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AP)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_AP<-emmeans(Interaction_AP, ~valence|membership)
Contrast1_AP <- pairs(Simple.Effects.By.membership_AP,adjust='none')
sum_Contrast1_AP<-summary(Contrast1_AP)

#???Contrast 2:by valence
Simple.Effects.By.valence_AP<-emmeans(Interaction_AP, ~membership|valence) 
Contrast2_AP <- pairs(Simple.Effects.By.valence_AP,adjust='none')
sum_Contrast2_AP<-summary(Contrast2_AP)

##3 way
Interaction_AP3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_age_P)
sum_Interaction_AP3_C<-summary(Interaction_AP3_C)

Interaction_AP3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                         data=my_data_age_P)
sum_Interaction_AP3_F<-summary(Interaction_AP3_F)

##faxian
AP.sub_Con_1 <- subset(my_data_age_P, Consistency == 1)
AP.sub_Con_2 <- subset(my_data_age_P, Consistency == 2)
Interaction_AP3_Con_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AP.sub_Con_1)
sum_Interaction_AP3_Con_1<-summary(Interaction_AP3_Con_1)
Interaction_AP3_Con_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AP.sub_Con_2)
sum_Interaction_AP3_Con_2<-summary(Interaction_AP3_Con_2)

AP.sub_For_1 <- subset(my_data_age_P, Format == 1)
AP.sub_For_2 <- subset(my_data_age_P, Format == 2)
Interaction_AP3_For_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AP.sub_For_1)
sum_Interaction_AP3_For_1<-summary(Interaction_AP3_For_1)
Interaction_AP3_For_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AP.sub_For_2)
sum_Interaction_AP3_For_2<-summary(Interaction_AP3_For_2)

#visualize
ggplot(data = my_data_age_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_age_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r Fam_gender, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_G_F1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","FamIn","FamOut"))

Ttest_GF<- t.test(my_data_extracted_G_F1$FamIn, my_data_extracted_G_F1$FamOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_GF<- summary(Ttest_GF)

#Stack
library(reshape2)
my_data_gender_F1 <- melt(my_data_extracted_G_F1, id.vars=1:4)
my_data_gender_F1

#valence
my_data_gender_F1$membership<- ifelse(my_data_gender_F1$variable=="FamIn", "1","2")


# Get descriptives
library(psych) 
Descriptives_GF_TTEST<-describeBy(my_data_gender_F1, 
           group = my_data_gender_F1$membership)


# Fam: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_G_F2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","FamInPos","FamOutPos", "FamInNeg", "FamOutNeg"))

#Stack
library(reshape2)
my_data_gender_F <- melt(my_data_extracted_G_F2, id.vars=1:4)
my_data_gender_F

##membership
my_data_gender_F$membership <- ifelse(my_data_gender_F$variable=="FamInPos", "1","2")
my_data_gender_F$membership[my_data_gender_F$variable=="FamOutPos"] <- 2
my_data_gender_F$membership[my_data_gender_F$variable=="FamInNeg"] <- 1
my_data_gender_F$membership[my_data_gender_F$variable=="FamOutNeg"] <- 2
#valence
my_data_gender_F$valence <- ifelse(my_data_gender_F$variable=="FamInPos", "1","2")
my_data_gender_F$valence[my_data_gender_F$variable=="FamOutPos"] <- 1
my_data_gender_F$valence[my_data_gender_F$variable=="FamInNeg"] <- 2
my_data_gender_F$valence[my_data_gender_F$variable=="FamOutNeg"] <- 2

# Factor
my_data_gender_F$membership<- factor(my_data_gender_F$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_F$valence<- factor(my_data_gender_F$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_F$ID<- factor(my_data_gender_F$ID)

# Fam: Group membership x Valence Interaction 
Interaction_GF  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_gender_F)
sum_Interaction_GF <- summary(Interaction_GF)
##table(my_data_gender_F$valence, my_data_gender_F$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_GF)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_GF<-emmeans(Interaction_GF, ~valence|membership)
Contrast1_GF <- pairs(Simple.Effects.By.membership_GF,adjust='none')
sum_Contrast1_GF<-summary(Contrast1_GF)


#???Contrast 2:by valence
Simple.Effects.By.valence_GF<-emmeans(Interaction_GF, ~membership|valence)
Contrast2_GF <- pairs(Simple.Effects.By.valence_GF,adjust='none')
sum_Contrast2_GF<-summary(Contrast2_GF)

##3 way
Interaction_GF3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_gender_F)
sum_Interaction_GF3_C<-summary(Interaction_GF3_C)

Interaction_GF3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_gender_F)
sum_Interaction_GF3_F<-summary(Interaction_GF3_F)

##faxian
GF.sub_Con_1 <- subset(my_data_gender_F, Consistency == 1)
GF.sub_Con_2 <- subset(my_data_gender_F, Consistency == 2)
Interaction_GF3_Con_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),data=GF.sub_Con_1)
sum_Interaction_GF3_Con_1<-summary(Interaction_GF3_Con_1)
Interaction_GF3_Con_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),data=GF.sub_Con_2)
sum_Interaction_GF3_Con_2<-summary(Interaction_GF3_Con_2)

GF.sub_For_1 <- subset(my_data_gender_F, Format == 1)
GF.sub_For_2 <- subset(my_data_gender_F, Format == 2)
Interaction_GF3_For_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GF.sub_For_1)
sum_Interaction_GF3_For_1<-summary(Interaction_GF3_For_1)
Interaction_GF3_For_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GF.sub_For_2)
sum_Interaction_GF3_For_2<-summary(Interaction_GF3_For_2)

#visualize
ggplot(data = my_data_gender_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_gender_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r Fam_age, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_A_F1 <- subset(my_data_original_1b, select = c("ID", "gender","Consistency","Format","FamIn","FamOut"))

Ttest_AF<- t.test(my_data_extracted_A_F1$FamIn, my_data_extracted_A_F1$FamOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_AF<- summary(Ttest_AF)

#Stack
library(reshape2)
my_data_age_F1 <- melt(my_data_extracted_A_F1, id.vars=1:4)
my_data_age_F1

#valence
my_data_age_F1$membership<- ifelse(my_data_age_F1$variable=="FamIn", "1","2")


# Get descriptives
library(psych) 
Descriptives_AF_TTEST<-describeBy(my_data_age_F1, 
           group = my_data_age_F1$membership)

# Fam: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_F2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","FamInPos","FamOutPos", "FamInNeg", "FamOutNeg"))

#Stack
library(reshape2)
my_data_age_F <- melt(my_data_extracted_A_F2, id.vars=1:4)
my_data_age_F

##membership
my_data_age_F$membership <- ifelse(my_data_age_F$variable=="FamInPos", "1","2")
my_data_age_F$membership[my_data_age_F$variable=="FamOutPos"] <- 2
my_data_age_F$membership[my_data_age_F$variable=="FamInNeg"] <- 1
my_data_age_F$membership[my_data_age_F$variable=="FamOutNeg"] <- 2
#valence
my_data_age_F$valence <- ifelse(my_data_age_F$variable=="FamInPos", "1","2")
my_data_age_F$valence[my_data_age_F$variable=="FamOutPos"] <- 1
my_data_age_F$valence[my_data_age_F$variable=="FamInNeg"] <- 2
my_data_age_F$valence[my_data_age_F$variable=="FamOutNeg"] <- 2

# Factor
my_data_age_F$membership<- factor(my_data_age_F$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_F$valence<- factor(my_data_age_F$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_F$ID<- factor(my_data_age_F$ID)

# Fam: Group membership x Valence Interaction 
Interaction_AF  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_age_F)
sum_Interaction_AF <- summary(Interaction_AF)
##table(my_data_age_F$valence, my_data_age_F$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AF)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_AF<-emmeans(Interaction_AF, ~valence|membership)
Contrast1_AF <- pairs(Simple.Effects.By.membership_AF,adjust='none')
sum_Contrast1_AF<-summary(Contrast1_AF)

#???Contrast 2:by valence
Simple.Effects.By.valence_AF<-emmeans(Interaction_AF, ~membership|valence)
Contrast2_AF <- pairs(Simple.Effects.By.valence_AF,adjust='none')
sum_Contrast2_AF<-summary(Contrast2_AF)

##3 way
Interaction_AF3_C <- aov(value~Consistency*valence*membership+
                           Error(ID/(valence*membership)),
                           data=my_data_age_F)
sum_Interaction_AF3_C<-summary(Interaction_AF3_C)

Interaction_AF3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_age_F)
sum_Interaction_AF3_F<-summary(Interaction_AF3_F)

##faxian
AF.sub_Con_1 <- subset(my_data_age_F, Consistency == 1)
AF.sub_Con_2 <- subset(my_data_age_F, Consistency == 2)
Interaction_AF3_Con_1 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=AF.sub_Con_1)
sum_Interaction_AF3_Con_1<-summary(Interaction_AF3_Con_1)

Interaction_AF3_Con_2 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=AF.sub_Con_2)
sum_Interaction_AF3_Con_2<-summary(Interaction_AF3_Con_2)

AF.sub_For_1 <- subset(my_data_age_F, Format == 1)
AF.sub_For_2 <- subset(my_data_age_F, Format == 2)
Interaction_AF3_For_1 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=AF.sub_For_1)
sum_Interaction_AF3_For_1<-summary(Interaction_AF3_For_1)
Interaction_AF3_For_2 <- aov(value~valence*membership+
                           Error(ID/(valence*membership)),
                           data=AF.sub_For_2)
sum_Interaction_AF3_For_2<-summary(Interaction_AF3_For_2)

#visualize
ggplot(data = my_data_age_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_age_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r Ster_gender, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_G_S1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","SterIn","SterOut"))

Ttest_GS<- t.test(my_data_extracted_G_S1$SterIn, my_data_extracted_G_S1$SterOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_GS<- summary(Ttest_GS)

#Stack
library(reshape2)
my_data_gender_S1 <- melt(my_data_extracted_G_S1, id.vars=1:4)
my_data_gender_S1
#valence
my_data_gender_S1$membership<- ifelse(my_data_gender_S1$variable=="SterIn","1","2")

# Get descriptives
library(psych) 
Descriptives_GS_TTEST<-describeBy(my_data_gender_S1, 
           group = my_data_gender_S1$membership)


# Ster: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_G_S2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","SterInPos","SterOutPos", "SterInNeg", "SterOutNeg"))
#Stack
library(reshape2)
my_data_gender_S <- melt(my_data_extracted_G_S2, id.vars=1:4)
my_data_gender_S

##membership
my_data_gender_S$membership <- ifelse(my_data_gender_S$variable=="SterInPos", "1","2")
my_data_gender_S$membership[my_data_gender_S$variable=="SterOutPos"] <- 2
my_data_gender_S$membership[my_data_gender_S$variable=="SterInNeg"] <- 1
my_data_gender_S$membership[my_data_gender_S$variable=="SterOutNeg"] <- 2
#valence
my_data_gender_S$valence <- ifelse(my_data_gender_S$variable=="SterInPos", "1","2")
my_data_gender_S$valence[my_data_gender_S$variable=="SterOutPos"] <- 1
my_data_gender_S$valence[my_data_gender_S$variable=="SterInNeg"] <- 2
my_data_gender_S$valence[my_data_gender_S$variable=="SterOutNeg"] <- 2

# Factor
my_data_gender_S$membership<- factor(my_data_gender_S$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_S$valence<- factor(my_data_gender_S$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_S$ID<- factor(my_data_gender_S$ID)

# Ster: Group membership x Valence Interaction 
Interaction_GS  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_gender_S)
sum_Interaction_GS <- summary(Interaction_GS)
##table(my_data_gender_S$valence, my_data_gender_S$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_GS)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_GS<-emmeans(Interaction_GS, ~valence|membership)
Contrast1_GS <- pairs(Simple.Effects.By.membership_GS,adjust='none')
sum_Contrast1_GS<-summary(Contrast1_GS)

#???Contrast 2:by valence
Simple.Effects.By.valence_GS<-emmeans(Interaction_GS, ~membership|valence)
Contrast2_GS <- pairs(Simple.Effects.By.valence_GS,adjust='none')
sum_Contrast2_GS<-summary(Contrast2_GS)

##3 way
Interaction_GS3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID/(valence*membership)),
                           data=my_data_gender_S)
sum_Interaction_GS3_C<-summary(Interaction_GS3_C)

Interaction_GS3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_gender_F)
sum_Interaction_GS3_F<-summary(Interaction_GS3_F)

##faxian
GS.sub_Con_1 <- subset(my_data_gender_S, Consistency == 1)
GS.sub_Con_2 <- subset(my_data_gender_S, Consistency == 2)
Interaction_GS3_Con_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GS.sub_Con_1)
sum_Interaction_GS3_Con_1<-summary(Interaction_GS3_Con_1)
Interaction_GS3_Con_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GS.sub_Con_2)
sum_Interaction_GS3_Con_2<-summary(Interaction_GS3_Con_2)

GS.sub_For_1 <- subset(my_data_gender_S, Format == 1)
GS.sub_For_2 <- subset(my_data_gender_S, Format == 2)
Interaction_GS3_For_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GS.sub_For_1)
sum_Interaction_GS3_For_1<-summary(Interaction_GS3_For_1)
Interaction_GS3_For_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=GS.sub_For_2)
sum_Interaction_GS3_For_2<-summary(Interaction_GS3_For_2)

#visualize
ggplot(data = my_data_gender_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_gender_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r Ster_age, include = FALSE, result="axis"}
# Extract columns
my_data_extracted_A_S1 <- subset(my_data_original_1b, select = c("ID", "gender","Consistency","Format","SterIn","SterOut"))

Ttest_AS<- t.test(my_data_extracted_A_S1$SterIn, my_data_extracted_A_S1$SterOut, paired=TRUE, alternative = "two.sided")
sum_Ttest_AS<- summary(Ttest_AS)

#Stack
library(reshape2)
my_data_age_S1 <- melt(my_data_extracted_A_S1, id.vars=1:4)
my_data_age_S1

#valence
my_data_age_S1$membership<- ifelse(my_data_age_S1$variable=="SterIn", "1","2")


# Get descriptives
library(psych) 
Descriptives_AS_TTEST<-describeBy(my_data_age_S1, 
           group = my_data_age_S1$membership)


# Ster: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_S2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","SterInPos","SterOutPos", "SterInNeg", "SterOutNeg"))
#Stack
library(reshape2)
my_data_age_S <- melt(my_data_extracted_A_S2, id.vars=1:4)
my_data_age_S

##membership
my_data_age_S$membership <- ifelse(my_data_age_S$variable=="SterInPos", "1","2")
my_data_age_S$membership[my_data_age_S$variable=="SterOutPos"] <- 2
my_data_age_S$membership[my_data_age_S$variable=="SterInNeg"] <- 1
my_data_age_S$membership[my_data_age_S$variable=="SterOutNeg"] <- 2
#valence
my_data_age_S$valence <- ifelse(my_data_age_S$variable=="SterInPos", "1","2")
my_data_age_S$valence[my_data_age_S$variable=="SterOutPos"] <- 1
my_data_age_S$valence[my_data_age_S$variable=="SterInNeg"] <- 2
my_data_age_S$valence[my_data_age_S$variable=="SterOutNeg"] <- 2

# Factor
my_data_age_S$membership<- factor(my_data_age_S$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_S$valence<- factor(my_data_age_S$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_S$ID<- factor(my_data_age_S$ID)

# Ster: Group membership x Valence Interaction 
Interaction_AS  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                         data=my_data_age_S)
sum_Interaction_AS <- summary(Interaction_AS)
##table(my_data_age_S$valence, my_data_age_S$membership)

# Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AS)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership_AS<-emmeans(Interaction_AS, ~valence|membership)
Contrast1_AS <- pairs(Simple.Effects.By.membership_AS,adjust='none')
sum_Contrast1_AS<-summary(Contrast1_AS)


#???Contrast 2:by valence
Simple.Effects.By.valence_AS<-emmeans(Interaction_AS, ~membership|valence)
Contrast2_AS <- pairs(Simple.Effects.By.valence_AS,adjust='none')
sum_Contrast2_AS<-summary(Contrast2_AS)

##3 way
Interaction_AS3_C <- aov(value~Consistency*valence*membership+                        
                           Error(ID/(valence*membership)),
                           data=my_data_age_S)
sum_Interaction_AS3_C<-summary(Interaction_AS3_C)

Interaction_AS3_F <- aov(value~Format*valence*membership+
                           Error(ID / (valence*membership)),
                           data=my_data_age_S)
sum_Interaction_AS3_F<-summary(Interaction_AS3_F)

##faxian
AS.sub_Con_1 <- subset(my_data_age_S, Consistency == 1)
AS.sub_Con_2 <- subset(my_data_age_S, Consistency == 2)
Interaction_AS3_Con_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AS.sub_Con_1)
sum_Interaction_AS3_Con_1<-summary(Interaction_AS3_Con_1)
Interaction_AS3_Con_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AS.sub_Con_2)
sum_Interaction_AS3_Con_2<-summary(Interaction_AS3_Con_2)

AS.sub_For_1 <- subset(my_data_age_S, Format == 1)
AS.sub_For_2 <- subset(my_data_age_S, Format == 2)
Interaction_AS3_For_1 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AS.sub_For_1)
sum_Interaction_AS3_For_1<-summary(Interaction_AS3_For_1)
Interaction_AS3_For_2 <- aov(value~valence*membership+
                           Error(ID / (valence*membership)),
                           data=AS.sub_For_2)
sum_Interaction_AS3_For_2<-summary(Interaction_AS3_For_2)

#visualize
ggplot(data = my_data_age_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
ggplot(data = my_data_age_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the truth of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the truth of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the truth of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the truth of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_T, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the acceptabilityof gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the acceptabilityof age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the acceptabilityof gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the acceptabilityof age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_A, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the familiarity of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the familiarity of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the familiarity of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the familiarity of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_F, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the stereotypicality of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the stereotypicality of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the stereotypicality of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the stereotypicality of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the positivity of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between consistency, valence and membership on the positivity of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```


```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the positivity of gender-related claims."}
library(ggplot2) 
ggplot(data = my_data_gender_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  ylim(0,7)+
  geom_smooth(method='lm',aes(group=membership))
```

```{r, fig.width=6, fig.height=4, fig.cap="Three way interaction between format, valence and membership on the positivity of age-related claims."}
library(ggplot2) 
ggplot(data = my_data_age_P, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Format) +
  geom_jitter() + 
  geom_smooth(method='lm',aes(group=membership))
```

\newpage

# Data analysis

**Dataset.** I conducted the analysis using the data sets Study1_ready_yujing_1a_short.sav and Study1_ready_yujing_1b_short.sav.

**Gender groups.** The cleaned dataset comprised of 128 male subjects and 129 female subjects.

**Age groups.** Our subjects comprised of 73 younger people, 108 middle-aged people and 71 older people. Among them, 4 younger subjects and 21 older subjects identified themselves as middle-aged. None of the subjects identified with the 'wrong' age group (younger participants identifying with older people, or older participants identifying with younger people). 

## Analysis plan

### Main effect and interaction effect on the judgement of truth
According to the pre-analysis plan that we registered, first, a linear regression will be performed on data sets my_data_gender_T and my_data_age_T, which involves testing the main effect of valence on the judgments of truth. Then a two way ANOVA will be carried out to test the interaction effect between group membership and valence. Further, two planned contrasts of the interaction will be tested. 

### Main effect and interaction effect on the judgement of social acceptability
Accordingly, a linear regression on data sets my_data_gender_A and my_data_age_A involves testing the main effect of valence on the judgments of acceptability. Then, a two way ANOVA will be carried out to test the interaction effect between group membership and valence. Further 2 planned contrasts of the interaction will be tested.

### Exploratory analysis
In the exploratory analysis, the regression analysis will be performed on related subdatasets. The  analysis involves testing the main effect of group membership and the interaction effect between group membership and valence on the perceived familiarity, stereotypicality and positivity. Further, a linear regression will be carried out to test if consistency and format of the claims affect how group membership and valence affect various dependent variables.

# Preregisterd analyses
## Results of Judgments of truth
### Analyses for Experiment 1a (Gender-related claims)
A significant main effect of valence (positive, negative) on the judgments of truth 
 ($t [`r Ttest_GT[["parameter"]][["df"]]`]$ = `r (Ttest_GT[["statistic"]])` , _p_ = `r f_num(Ttest_GT[["p.value"]], digits= 3)`) was found, with positively valenced claims (_M_ = `r Descriptives_GT_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_GT_TTEST[[1]][["sd"]][6]`) receiving significantly higher scores on truth than negative claims (_M_ = `r Descriptives_GT_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_GT_TTEST[[2]][["sd"]][6]`).
 
Additionally a marginally significant interaction was found between valence and group membership (ingroup, outgroup) on the judgments of truth ($F [`r sum_Interaction_GT[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

Planned contrasts showed that subjects believed positively valenced claims was significantly truer than negative valenced ones when the claims are targeted at their ingroup ($t [`r sum_Contrast1_GT$df[1]`]$ = `r (sum_Contrast1_GT$t.ratio[1])` , _p_ = `r f_num(sum_Contrast1_GT$p.value[1], digits= 3)`), but there was no difference between valences when the claims are targeted at their outgroup ($t [`r sum_Contrast1_GT$df[2]`]$ = `r (sum_Contrast1_GT$t.ratio[2])` , _p_ = `r f_num(sum_Contrast1_GT$p.value[2], digits= 3)`). Moreover, no differences were found between ingroupers and outgroupers on the the judgement of truth in the positive condition  ($t [`r sum_Contrast2_GT$df[1]`]$ = `r (sum_Contrast2_GT$t.ratio[1])` , _p_ = `r f_num(sum_Contrast2_GT$p.value[1], digits= 3)`) , and negative condition  ($t [`r sum_Contrast2_GT$df[2]`]$ = `r (sum_Contrast2_GT$t.ratio[2])` , _p_ = `r f_num(sum_Contrast2_GT$p.value[2], digits= 3)`).

### Analyses for Experiment 1b (Age-related claims)
A significant main effect of valence (positive, negative) on the judgments of truth ($t [`r Ttest_AT[["parameter"]][["df"]]`]$ = `r (Ttest_AT[["statistic"]])` , _p_ = `r f_num(Ttest_AT[["p.value"]], digits= 3)`) was found, with positively valenced claims (_M_ = `r Descriptives_AT_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AT_TTEST[[1]][["sd"]][6]`) receiving significantly higher scores on truth than negatively valenced ones (_M_ = `r Descriptives_AT_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AT_TTEST[[2]][["sd"]][6]`).

No significant interaction was found between valence and group membership (ingroup, outgroup, middle-aged) on the judgments of truth ($F [`r sum_Interaction_AT[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AT[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AT[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

Planned contrasts showed that there are no significant difference in truth between ingroupers and outgroupers, both in positive condition ($t [`r sum_Contrast2_AT$df[1]`]$ = `r (sum_Contrast2_AT$t.ratio[1])` , _p_ = `r f_num(sum_Contrast2_AT$p.value[1], digits= 3)`) and negative condition ($t [`r sum_Contrast2_AT$df[2]`]$ = `r (sum_Contrast2_AT$t.ratio[2])` , _p_ = `r f_num(sum_Contrast2_AT$p.value[2], digits= 3)`). Moreover, compared to negative ones, subjects perceive positive claims as significantly truer, both when they are targeted at their ingroup ($t [`r sum_Contrast1_AT$df[1]`]$ = `r (sum_Contrast1_AT$t.ratio[1])` , _p_ = `r f_num(sum_Contrast1_AT$p.value[1], digits= 3)`) and outgroup ($t [`r sum_Contrast1_AT$df[2]`]$ = `r (sum_Contrast1_AT$t.ratio[2])` , _p_ = `r f_num(sum_Contrast1_AT$p.value[2], digits= 3)`).

## Results of Judgments of acceptability
### Analyses for Experiment 1a (Gender-related claims)
A significant main effect of valence (positive, negative) on the judgments of acceptability ($t [`r Ttest_GA[["parameter"]][["df"]]`]$ = `r (Ttest_GA[["statistic"]])` , _p_ = `r f_num(Ttest_GA[["p.value"]], digits= 3)`) was found, with positively valenced claims (_M_ = `r Descriptives_GA_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_GA_TTEST[[1]][["sd"]][6]`) receiving significantly higher scores on acceptability than negatively valenced claims (_M_ = `r Descriptives_GA_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_GA_TTEST[[2]][["sd"]][6]`).

Additionally a marginally significant interaction was found between valence and group membership (ingroup, outgroup) on the judgments of acceptability ($F [`r sum_Interaction_GA[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GA[[4]][[1]][["Df"]][2] `]$ = `r (sum_Interaction_GA[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

Planned contrasts showed that when the claims are negative, subjects believe the outgroup-targeted claims are marginally significantly more acceptable than ingroup-targeted ones ($t [`r sum_Contrast2_GA$df[2]`]$ = `r (sum_Contrast2_GA$t.ratio[2])` , _p_ = `r f_num(sum_Contrast2_GA$p.value[2], digits= 3)`). However, when the content of claims is positive, there is no significant difference in the acceptability between ingroupers and outgroupers  ($t [`r sum_Contrast2_GA$df[1]`]$ = `r (sum_Contrast2_GA$t.ratio[1])` , _p_ = `r f_num(sum_Contrast2_GA$p.value[1], digits= 3)`).

### Analyses for Experiment 1b (Age-related claims)
A significant main effect of valence (positive, negative) on the judgments of acceptability ($t [`r Ttest_AA[["parameter"]][["df"]]`]$ = `r (Ttest_AA[["statistic"]])` , _p_ = `r f_num(Ttest_AA[["p.value"]], digits= 3)`) was found, with positively valenced claims (_M_ = `r Descriptives_AA_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AA_TTEST[[1]][["sd"]][6]`) receiving significantly higher score on acceptability than negatively valenced ones (_M_ = `r Descriptives_AA_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AA_TTEST[[2]][["sd"]][6]`).

Additionally no significant interaction was found between valence and group membership (ingroup, outgroup) on the judgments of acceptability ($F [`r sum_Interaction_AA[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AA[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AA[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

Planned contrasts showed that there are no significant difference in acceptability between ingroupers and outgroupers, both in positive condition ($t [`r sum_Contrast2_AA$df[1]`]$ = `r (sum_Contrast2_AA$t.ratio[1])` , _p_ = `r f_num(sum_Contrast2_AA$p.value[1], digits= 3)`) and negative condition ($t [`r sum_Contrast2_AA$df[2]`]$ = `r (sum_Contrast2_AA$t.ratio[2])` , _p_ = `r f_num(sum_Contrast2_AA$p.value[2], digits= 3)`).

# Results of exploratory analysis
## Judgments of truth
### Analyses for Experiment 1a (Gender-related claims)
The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant ($F [`r sum_Interaction_GT3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GT3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GT3_C[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GT3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

ster: interaction:($F [`r sum_Interaction_GT3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_GT3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_GT3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_GT3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)


im: interaction:($F [`r sum_Interaction_GT3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_GT3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_GT3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_GT3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

When the claims are stereotypical, there is no significant main effect of valence on the judgments of truth ($F [`r sum_Interaction_GT3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`). However, when presented claims are counter-stereotypical, there is a significant main effect of valence ($F [`r sum_Interaction_GT3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`).

Additionally, when the claims are stereotypical, the interaction effect between valence and membership is marginally significant ($F [`r sum_Interaction_GT3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). However, when presented claims are counter-stereotypical, the interaction effect is not significant ($F [`r sum_Interaction_GT3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`).

The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_GT3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GT3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GT3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GT3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

When the claims are implicit, there is a significant main effect of valence on the judgments of truth ($t [`r sum_Interaction_GT3_For_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`). However, when presented claims are explicit, there is no significant effect of valence ($t [`r sum_Interaction_GT3_For_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`).

ex: interaction: ($F [`r sum_Interaction_GT3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 
im: interaction: ($F [`r sum_Interaction_GT3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GT3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

im: valence: ($F [`r sum_Interaction_GT3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`). 
ex: valence: ($F [`r sum_Interaction_GT3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GT3_For_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_GT3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GT3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`). 

### Analyses for Experiment 1b (Age-related claims)
The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant ($F [`r sum_Interaction_AT3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AT3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AT3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_AT3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

ster: interaction:($F [`r sum_Interaction_AT3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AT3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AT3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_AT3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AT3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AT3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_AT3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AT3_Con_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AT3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_AT3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AT3_Con_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AT3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)



When the claims are stereotypical, there is a significant main effect of valence ($t [`r sum_Interaction_AT3_Con_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`) and a marginally significant main effect of membership on the judgments of truth ($t [`r sum_Interaction_AT3_Con_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_Con_1[[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AT3_Con_1[[1]][["Pr(>F)"]][2], digits= 3)`).

When presented claims are counter-stereotypical, there is also a significant main effect of valence ($t [`r sum_Interaction_AT3_Con_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`), but no significant effect of membership ($t [`r sum_Interaction_AT3_Con_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_Con_2[[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AT3_Con_2[[1]][["Pr(>F)"]][2], digits= 3)`).

The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_AT3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AT3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AT3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AT3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

There is a significant main effect of valence on the judgments of truth both in implicit condition ($t [`r sum_Interaction_AT3_For_1[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`), and explicit condition ($t [`r sum_Interaction_AT3_For_2[[4]][[1]][["Pr(>F)"]][1]`]$ = `r (sum_Interaction_AT3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AT3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`).



## Judgments of acceptability
### Analyses for Experiment 1a (Gender-related claims)
The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant ($F [`r sum_Interaction_GA3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GA3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GA3_C[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GA3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

There is a significant main effect of valence on the judgments of acceptability both in stereotypical condition ($t [`r sum_Interaction_GA3_Con_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GA3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`), and counter stereotypical condition ($t [`r sum_Interaction_GA3_Con_2[[2]][[1]][["Df"]][1]`]$ = `r (sum_Interaction_GA3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`).

ster: interaction:($F [`r sum_Interaction_GA3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GA3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GA3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_GA3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GA3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GA3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)


The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_GA3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GA3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GA3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GA3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

There is a significant main effect of valence on the judgments of acceptability in implicit condition ($t [`r sum_Interaction_GA3_For_1[[2]][[1]][["Df"]][1]`]$ = `r (sum_Interaction_GA3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`), but not in explicit condition ($t [`r sum_Interaction_GA3_For_2[[2]][[1]][["Df"]][1]`]$ = `r (sum_Interaction_GA3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`).
Additionally, there appears marginally significant interaction effect between valence and membership only in explicit condition 

im:interaction:($t [`r sum_Interaction_GA3_For_1[[4]][[1]][["Df"]][1]`]$ = `r (sum_Interaction_GA3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`).

ex: interaction: ($t [`r sum_Interaction_GA3_For_2[[4]][[1]][["Df"]][1]`]$ = `r (sum_Interaction_GA3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GA3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`).


### Analyses for Experiment 1b (Age-related claims)
The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant ($F [`r sum_Interaction_AA3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AA3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_AA3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 
ster: interaction:($F [`r sum_Interaction_AA3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AA3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_AA3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AA3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_AA3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_AA3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_AA3_Con_1[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_1[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_Con_1[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_1[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_AA3_Con_2[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_Con_2[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_Con_2[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_Con_2[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)


There is a significant main effect of valence on the judgments of acceptability both in stereotypical condition , and counter stereotypical condition.

The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_AA3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AA3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AA3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

im: interaction:($F [`r sum_Interaction_AA3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AA3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_AA3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AA3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_AA3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_AA3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

im:membership: ($F [`r sum_Interaction_AA3_For_1[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_1[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_For_1[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_1[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: membership:($F [`r sum_Interaction_AA3_For_2[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AA3_For_2[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AA3_For_2[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AA3_For_2[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)

There is a significant main effect of valence on the judgments of acceptability both in implicit condition , and explicit condition .


## Judgments of familiarity
### Analyses for Experiment 1a (Gender-related claims)
There is no significant main effect of membership (ingroup, outgroup) on the judgments of familarity ($t [`r Ttest_GF[["parameter"]][["df"]]`]$ = `r (Ttest_GF[["statistic"]])` , _p_ = `r f_num(Ttest_GF[["p.value"]], digits= 3)`), with outgroup-targeted cliams (_M_ = `r Descriptives_GF_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_GF_TTEST[[2]][["sd"]][6]`) receiving slightly higher scores on familarity than those targeted at ingroup (_M_ = `r Descriptives_GF_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_GF_TTEST[[1]][["sd"]][6]`)

Additionally there no significant interaction was found between valence (positive, negative) and group membership on the judgments of familarity ($F [`r sum_Interaction_GF[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GF[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GF[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is also not significant ($F [`r sum_Interaction_GF3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GF3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_GF3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). There is a marginally significant main effect of consistency on the judgement of familarity ($F [`r sum_Interaction_GF3_C[[1]][["Df"]][1] `, `r sum_Interaction_GF3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_C[[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_C[[1]][["Pr(>F)"]][1], digits= 3)`).

The three-way interaction between format (implicit, explicit), valence and group membership is also not significant ($F [`r sum_Interaction_GF3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GF3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GF3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). There is a marginally significant main effect of format on the judgement of familarity ($F [`r sum_Interaction_GF3_F[[1]][["Df"]][1] `, `r sum_Interaction_GF3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_F[[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_F[[1]][["Pr(>F)"]][1], digits= 3)`).

im: interaction:($F [`r sum_Interaction_GF3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GF3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_GF3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GF3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_GF3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GF3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_GF3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GF3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

im:membership: ($F [`r sum_Interaction_GF3_For_1[[1]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_1[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_For_1[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_1[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: membership:($F [`r sum_Interaction_GF3_For_2[[1]][[1]][["Df"]][1] `, `r sum_Interaction_GF3_For_2[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GF3_For_2[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GF3_For_2[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)

### Analyses for Experiment 1b (Age-related claims)
There is no significant main effect of membership (ingroup, outgroup) on the judgments of familarity ($t [`r Ttest_AF[["parameter"]][["df"]]`]$ = `r (Ttest_AF[["statistic"]])` , _p_ = `r f_num(Ttest_AF[["p.value"]], digits= 3)`), with outgroup-targeted cliams (_M_ = `r Descriptives_AF_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AF_TTEST[[2]][["sd"]][6]`) receiving slightly higher scores on familarity than those targeted at ingroup (_M_ = `r Descriptives_AF_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AF_TTEST[[1]][["sd"]][6]`)

Additionally there no significant interaction was found between valence (positive, negative) and group membership on the judgments of familarity ($F [`r sum_Interaction_AF[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AF[[1]][["Df"]][4]`]$ = `r (sum_Interaction_AF[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF[[1]][["Pr(>F)"]][3], digits= 3)`). 

A significant three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership ($F [`r sum_Interaction_AF3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AF3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_AF3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`) was found. There is a significant main effect of consistency on the judgement of familarity ($F [`r sum_Interaction_AF3_C[[1]][["Df"]][1] `, `r sum_Interaction_AF3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_C[[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_C[[1]][["Pr(>F)"]][1], digits= 3)`).

The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_AF3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AF3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AF3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). There is a significant main effect of format on the judgement of familarity ($F [`r sum_Interaction_AF3_F[[1]][["Df"]][1] `, `r sum_Interaction_AF3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_F[[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_F[[1]][["Pr(>F)"]][1], digits= 3)`).

ster: interaction:($F [`r sum_Interaction_AF3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AF3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_AF3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AF3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_AF3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_AF3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_AF3_Con_1[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_1[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_Con_1[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_1[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_AF3_Con_2[[1]][[1]][["Df"]][1] `, `r sum_Interaction_AF3_Con_2[[1]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AF3_Con_2[[1]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AF3_Con_2[[1]][[1]][["Pr(>F)"]][1], digits= 3)`)


## Judgments of stereotypicality
### Analyses for Experiment 1a (Gender-related claims)
There was no significant main effect of membership (ingroup, outgroup) on the judgments of stereotypicality ($t [`r Ttest_GS[["parameter"]][["df"]]`]$ = `r (Ttest_GS[["statistic"]])` , _p_ = `r f_num(Ttest_GS[["p.value"]], digits= 3)`), with ingroup-targeted cliams  (_M_ = `r Descriptives_AS_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AS_TTEST[[1]][["sd"]][6]`) receiving slightly higher scores on stereotypicality than those targeted at outgoup (_M_ = `r Descriptives_AS_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AS_TTEST[[2]][["sd"]][6]`).

Additionally no significant interaction was found between valence (positive, negative) and group membership on the judgments of stereotypicality ($F [`r sum_Interaction_GS[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GS[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is also not significant ($F [`r sum_Interaction_GS3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GS3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GS3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_GS3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). The three-way interaction between format (implicit, explicit), valence and group membership is also not significant ($F [`r sum_Interaction_GS3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GS3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GS3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GS3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`).

IM: interaction:($F [`r sum_Interaction_GS3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
EX: interaction:($F [`r sum_Interaction_GS3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

IM: valence:($F [`r sum_Interaction_GS3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
EX: valence:($F [`r sum_Interaction_GS3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

IM:membership: ($F [`r sum_Interaction_GS3_For_1[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_1[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_1[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)
EX: membership:($F [`r sum_Interaction_GS3_For_2[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_For_2[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_For_2[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: interaction:($F [`r sum_Interaction_GS3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_GS3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_GS3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_GS3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_GS3_Con_1[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_1[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_1[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_GS3_Con_2[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GS3_Con_2[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GS3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GS3_Con_2[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)


### Analyses for Experiment 1b (Age-related claims)
There was no significant main effect of membership (ingroup, outgroup) on the judgments of stereotypicality ($t [`r Ttest_AS[["parameter"]][["df"]]`]$ = `r (Ttest_AS[["statistic"]])` , _p_ = `r f_num(Ttest_AS[["p.value"]], digits= 3)`), with ingroup-targeted cliams (_M_ = `r Descriptives_AS_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AS_TTEST[[1]][["sd"]][6]`) receiving slightly higher scores on stereotypicality than those targeted at outgoup (_M_ = `r Descriptives_AS_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AS_TTEST[[2]][["sd"]][6]`).

Additionally no significant interaction was found between valence (positive, negative) and group membership on the judgments of stereotypicality ($F [`r sum_Interaction_AS[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AS[[1]][["Df"]][4]`]$ = `r (sum_Interaction_AS[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS[[1]][["Pr(>F)"]][3], digits= 3)`). 

There is a significant three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership ($F [`r sum_Interaction_AS3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AS3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_AS3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

Additionally, when presented claims are  stereotypical, the interaction effect is marginally significant. However, when the claims are counter stereotypical, the interaction effect between valence and membership is not significant 

The three-way interaction between format (implicit, explicit), valence and group membership is not significant ($F [`r sum_Interaction_AS3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AS3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AS3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`).

ster: interaction:($F [`r sum_Interaction_AS3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_AS3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_AS3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_AS3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_AS3_Con_1[[3]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_1[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_1[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_AS3_Con_2[[3]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_Con_2[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_Con_2[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)


im: interaction:($F [`r sum_Interaction_AS3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_AS3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_AS3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_AS3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AS3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

im:membership: ($F [`r sum_Interaction_AS3_For_1[[3]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_1[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_1[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: membership:($F [`r sum_Interaction_AS3_For_2[[3]][[1]][["Df"]][1] `, `r sum_Interaction_AS3_For_2[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AS3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AS3_For_2[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)


## Judgments of positivity

### Analyses for Experiment 1a (Gender-related claims)
There was no significant main effect of membership (ingroup, outgroup) on the judgments of positivity ($t [`r Ttest_GP[["parameter"]][["df"]]`]$ = `r (Ttest_GP[["statistic"]])` , _p_ = `r f_num(Ttest_GP[["p.value"]], digits= 3)`), with outgroup-targeted cliams (_M_ = `r Descriptives_GP_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_GP_TTEST[[2]][["sd"]][6]`) receiving slightly higher scores on positivity than those targeted at ingroup (_M_ = `r Descriptives_GP_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_GP_TTEST[[1]][["sd"]][6]`)

Additionally there is a significant interaction was found between valence (positive, negative) and group membership on the judgments of positivity ($F [`r sum_Interaction_GP[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GP[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP[[4]][[1]][["Pr(>F)"]][1], digits= 3)`). 

Post-hoc comparisons showed subjects believed that positively valenced claims were significantly more positive than negatively valenced ones both when the they are targeted at ingroup ($t [`r sum_Contrast1_GP$df[1]`]$ = `r (sum_Contrast1_GP$t.ratio[1])` , _p_ = `r f_num(sum_Contrast1_GP$p.value[1], digits= 3)`) and outgroup ($t [`r sum_Contrast1_GP$df[2]`]$ = `r (sum_Contrast1_GP$t.ratio[2])` , _p_ = `r f_num(sum_Contrast1_GP$p.value[2], digits= 3)`). Moreover, when the claims are negative, outgroupers rated significantly higher on positivity compared to ingroupers ($t [`r sum_Contrast2_GP$df[2]`]$ = `r (sum_Contrast2_GP$t.ratio[2])` , _p_ = `r f_num(sum_Contrast2_GP$p.value[2], digits= 3)`). Yet, no significant differences was found between ingroupers and outgroupers on the the judgement of positivity when the content of claims are positive ($t [`r sum_Contrast2_GP$df[1]`]$ = `r (sum_Contrast2_GP$t.ratio[1])` , _p_ = `r f_num(sum_Contrast2_GP$p.value[1], digits= 3)`).

The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant ($F [`r sum_Interaction_GP3_C[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GP3_C[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GP3_C[[1]][["F value"]][7])`, _p_ = `r f_num(sum_Interaction_GP3_C[[4]][[1]][["Pr(>F)"]][2], digits= 3)`). 

There is a significant main effect of valence on the judgments of positivity both in stereotypical condition , and counter stereotypical condition .

The three-way interaction between format (implicit, explicit), valence and group membership is also not significant ($F [`r sum_Interaction_GP3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_GP3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_GP3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_GP3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`).

There is a significant main effect of valence on the judgments of positivity both in implicit condition , and explicit condition .

Additionally, when the claims are explicit, the interaction effect between valence and membership is marginally significant . However, when presented claims are implicit, the interaction effect is not significant .

ster: interaction:($F [`r sum_Interaction_GP3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_GP3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_GP3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_GP3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_GP3_Con_1[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_1[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_1[[3]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_1[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_GP3_Con_2[[3]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_Con_2[[3]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_Con_2[[3]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_Con_2[[3]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: interaction:($F [`r sum_Interaction_GP3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_GP3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_GP3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_For_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_GP3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_GP3_For_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_GP3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_GP3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

### Analyses for Experiment 1b (Age-related claims)
There is no significant main effect of membership (ingroup, outgroup) on the judgments of positivity ($t [`r Ttest_AP[["parameter"]][["df"]]`]$ = `r (Ttest_AP[["statistic"]])` , _p_ = `r f_num(Ttest_AP[["p.value"]], digits= 3)`), with ingroup-targeted cliams (_M_ = `r Descriptives_AP_TTEST[[1]][["mean"]][6]`, _SD_ = `r Descriptives_AP_TTEST[[1]][["sd"]][6]`) receiving slightly higher scores on positivity than those targeted at outgroup (_M_ = `r Descriptives_AP_TTEST[[2]][["mean"]][6]`, _SD_ = `r Descriptives_AP_TTEST[[2]][["sd"]][6]`).

Additionally no significant interaction was found between valence (positive, negative) and group membership on the judgments of positivity . 

Post-hoc comparisons showed subjects believed that positively valenced claims were significantly more positive than negatively valenced ones both when the they are targeted at ingroup ($t [`r sum_Contrast1_AP$df[1]`]$ = `r (sum_Contrast1_AP$t.ratio[1])` , _p_ = `r f_num(sum_Contrast1_AP$p.value[1], digits= 3)`) and outgroup ($t [`r sum_Contrast1_AP$df[2]`]$ = `r (sum_Contrast1_AP$t.ratio[2])` , _p_ = `r f_num(sum_Contrast1_AP$p.value[2], digits= 3)`).

The three-way interaction between consistency (stereotypical, counter-stereotypical), valence and group membership is not significant . 

There is a significant main effect of valence on the judgments of positivity both in stereotypical condition , and counter stereotypical condition.

The three-way interaction between format (implicit, explicit), valence and group membership is also not significant ($F [`r sum_Interaction_AP3_F[[4]][[1]][["Df"]][2] `, `r sum_Interaction_AP3_F[[4]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AP3_F[[4]][[1]][["F value"]][2])`, _p_ = `r f_num(sum_Interaction_AP3_F[[4]][[1]][["Pr(>F)"]][2], digits= 3)`).

There is a significant main effect of valence on the judgments of positivity both in implicit condition , and explicit condition .

ster: interaction:($F [`r sum_Interaction_AP3_Con_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_Con_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: interaction:($F [`r sum_Interaction_AP3_Con_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_Con_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster: valence:($F [`r sum_Interaction_AP3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: valence:($F [`r sum_Interaction_AP3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

ster:membership: ($F [`r sum_Interaction_AP3_Con_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AP3_Con_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
counter: membership:($F [`r sum_Interaction_AP3_Con_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_Con_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AP3_Con_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_Con_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: interaction:($F [`r sum_Interaction_AP3_For_1[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_1[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_For_1[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_1[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: interaction:($F [`r sum_Interaction_AP3_For_2[[4]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_2[[4]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_For_2[[4]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_2[[4]][[1]][["Pr(>F)"]][1], digits= 3)`)

im: valence:($F [`r sum_Interaction_AP3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_1[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: valence:($F [`r sum_Interaction_AP3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_2[[2]][[1]][["Df"]][2]`]$ = `r (sum_Interaction_AP3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

im:membership: ($F [`r sum_Interaction_AP3_For_1[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_1[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AP3_For_1[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_1[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)
ex: membership:($F [`r sum_Interaction_AP3_For_2[[2]][[1]][["Df"]][1] `, `r sum_Interaction_AP3_For_2[[2]][[1]][["Df"]][3]`]$ = `r (sum_Interaction_AP3_For_2[[2]][[1]][["F value"]][1])`, _p_ = `r f_num(sum_Interaction_AP3_For_2[[2]][[1]][["Pr(>F)"]][1], digits= 3)`)

\begingroup
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}

<div id = "refs"></div>
\endgroup




