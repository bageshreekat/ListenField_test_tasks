library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)

pheno <- read.csv('D:/Personal/Job_related/Listenfield/Task_data/phenos.csv')
pheno

cor(pheno$trait1, pheno$trait2) # 0.2273717
plot(pheno$trait1, pheno$trait2)

hist(pheno$trait1)
hist(pheno$trait2)

## regression equation
traits.lm <- lm(trait2 ~ trait1, data = pheno)
summary(traits.lm)$r.squared  #0.0516

## scatterplot
cor_trait1_2 <- ggplot(data = pheno, aes(x = trait1, y = trait2 )) +
  geom_point(color = "black", size=1.2) +
  stat_smooth(method = "lm", col = "#C42126", size=1, se= FALSE) +
  labs(y = "Trait2", x= "Trait1") +
  theme_bw() +
  theme(axis.text = element_text(size=12, colour = "black"), 
        axis.text.y = element_text(angle = 90), 
        axis.title = element_text(size=12, face = "bold"))
cor_trait1_2

ggsave("D:/Personal/Job_related/Listenfield/Scipts/Output/traits12_scatterplot.png", 
       cor_trait1_2, height = 4, width = 5)

###############################################
## dividing data into quartiles

res<-quantile(pheno$trait1, probs = c(0,0.25,0.5,0.75,1))
res
#df with quantiles
pheno_quartile <- pheno %>%
  mutate(quantile = ntile(trait1, 5))
pheno_quartile

#splitting df in quantiles
splitted_quantiles_traits <- split(pheno_quartile, with(pheno_quartile, interaction(quantile)), drop = TRUE)
trait1_25 <- splitted_quantiles_traits$`1`
mean(trait1_25$trait1)
mean(trait1_25$trait2)
cor(trait1_25$trait2, trait1_25$trait1)
plot(trait1_25$trait2, trait1_25$trait1)

mean(splitted_quantiles_traits$`2`$trait1)
mean(splitted_quantiles_traits$`2`$trait2)
cor(splitted_quantiles_traits$`2`$trait1, splitted_quantiles_traits$`2`$trait2)

mean(splitted_quantiles_traits$`3`$trait1)
mean(splitted_quantiles_traits$`3`$trait2)
cor(splitted_quantiles_traits$`3`$trait1, splitted_quantiles_traits$`3`$trait2)

mean(splitted_quantiles_traits$`4`$trait1)
mean(splitted_quantiles_traits$`4`$trait2)
cor(splitted_quantiles_traits$`4`$trait1, splitted_quantiles_traits$`4`$trait2)
plot(splitted_quantiles_traits$`4`$trait1, splitted_quantiles_traits$`4`$trait2)

mean(splitted_quantiles_traits$`5`$trait1)
mean(splitted_quantiles_traits$`5`$trait2)
cor(splitted_quantiles_traits$`5`$trait1, splitted_quantiles_traits$`5`$trait2)

#####################################################
