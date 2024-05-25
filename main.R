library('ordinal')
library('tidyr')
library('tidyverse')
library('lattice')
library('gridExtra')
library('lme4')
library('lmerTest')
library('performance')
library('emmeans')
library('ggeffects')
library('ggplot2')

source('./helper_functions.R')

data = read.csv('Drugs_Questionnaire.csv')
data$age = as.factor(data$age)
data$education = as.factor(data$education)
data$drug_use = as.factor(data$drug_use)
data$`cannabis.` = as.factor(data$`cannabis.`)
data$`mdma.` = as.factor(data$`mdma.`)
data$`lsd.` = as.factor(data$`lsd.`)
data$`salvia.` = as.factor(data$`salvia.`)
data$`mushroom.` = as.factor(data$`mushroom.`)

data = subset(data, drug_use != 'Never')

View(data)

################################################################################
#                           Drug experience Analysis
################################################################################

drug_data = gather_drug_data(data)

drug_data = mutate(drug_data, never = ifelse(drug_use == "Never", 1, 0))
drug_data$never = as.factor(drug_data$never)

View(drug_data)
#drug_data$response = as.factor(drug_data$response)


# 'Never' drug_use analysis
p1 = ggplot(drug_data, aes(x = model, y = response, fill=drug)) + 
  geom_boxplot() +
  xlab("Model") +
  ylab("Survey scores") +
  ggtitle("Drug Realism Survey scores by Model type for various drugs")
p2 = ggplot(drug_data[drug_data$drug_use != 'Never',], aes(x = model, y = response, fill=drug)) + 
  geom_boxplot() +
  xlab("Model") +
  ylab("Survey scores") +
  ggtitle("Drug Realism Survey scores by Model type for various drugs")
grid.arrange(p1, p2,nrow = 1)

never_model = lmer(response ~ never + (1|drug) + (1|ID), data = drug_data)
summary(never_model)
confint(never_model)

ggplot(drug_data, aes(x = response, color=drug_use)) + geom_density() + xlab("Markov Model Responses") +
  ylab('Density')



# View model density
ggplot(drug_data, aes(x = response, color=model)) + geom_density() + xlab("Model Responses") +
  ylab('Density')

ggplot(drug_data, aes(x = model, y = response, fill=drug)) + 
  geom_boxplot() +
  xlab("Model") +
  ylab("Perceived Realisticness") +
  ggtitle("Perceived Realisticness Survey scores by Model type for various drugs")


# Fit models - Overall model (All drugs)
all_drugs_model = lmer(response ~ model + (1|drug) + (1|ID), data = drug_data)
summary(all_drugs_model)

all_drugs_model_slopes = lmer(response ~ model + (1+model|drug) + (1|ID), data = drug_data)
summary(all_drugs_model_slopes)

performance(all_drugs_model)
performance::check_model(all_drugs_model)
effectsize::effectsize(all_drugs_model)
ranef(all_drugs_model)
confint(all_drugs_model)
emmeans(all_drugs_model, specs = 'model')

performance(all_drugs_model_slopes)
performance::check_model(all_drugs_model_slopes)
effectsize::effectsize(all_drugs_model_slopes)
ranef(all_drugs_model_slopes)



predict_data = as.data.frame(ggpredict(all_drugs_model, terms = c("model",'drug'), type = "random", interval = "confidence"))
predict_data

ggplot(predict_data, aes(x = x, y = predicted, group = group)) + 
  geom_line(aes(color = group, group = group), linewidth=1) +
  xlab("Model") +
  ylab("Survey scores") +
  ggtitle("Predicted Drug Realism Survey scores by Model type for various drugs") +
  labs(fill = "Polarity", color='Polarity')



drug_mixed_model = lmer(response ~ model + (1|ID), data = drug_data[drug_data$drug=='mushroom',])
summary(drug_mixed_model)
effectsize::effectsize(drug_mixed_model)

p <- c(7.00e-07, 0.0292, 0.198, 0.499, 1)
p.adjust(p, method = 'hochberg', n = length(p))



################################################################################
#                      Sentence Coherence Analysis
################################################################################

coherent_data1 = gather(data[c(21:22,27)], "model", "response", 1:2, na.rm=T)
coherent_data1$model[coherent_data1$model == 'coherent_markov1'] <- 'markov'
coherent_data1$model[coherent_data1$model == 'coherent_mistral1'] <- 'mistral'

coherent_data2 = gather(data[c(23:24,27)], "model", "response", 1:2, na.rm=T)
coherent_data2$model[coherent_data2$model == 'coherent_markov2'] <- 'markov'
coherent_data2$model[coherent_data2$model == 'coherent_mistral2'] <- 'mistral'

coherent_data3 = gather(data[c(25:26,27)], "model", "response", 1:2, na.rm=T)
coherent_data3$model[coherent_data3$model == 'coherent_markov3'] <- 'markov'
coherent_data3$model[coherent_data3$model == 'coherent_mistral3'] <- 'mistral'

coherent_data = rbind(coherent_data1, coherent_data2, coherent_data3)

coherent_model = lmer(response ~ model + (1|ID), data = coherent_data)
summary(coherent_model)
effectsize::effectsize(coherent_model)
confint(coherent_model)


coherent_markov = gather(data[c(21, 23, 25)], "model", "response", 1:2, na.rm=T)
coherent_mistral = gather(data[c(22, 24, 26)], "model", "response", 1:2, na.rm=T)
p1 <- ggplot(coherent_markov, aes(x = response)) + geom_density(alpha = 0.5) + xlab("Markov Model Responses") +
  ylab('Density') +
  ggtitle("Sentence coherence scores for Markov model")
p2 <- ggplot(coherent_mistral, aes(x = response)) + geom_density(alpha = 0.5) + xlab("Mistral Model Responses") +
  ylab('Density') +
  ggtitle("Sentence coherence scores for Mistral model")
grid.arrange(p1, p2,nrow = 1)





