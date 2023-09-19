library(openxlsx)
library(dplyr)

pheno <- read.csv('D:/Personal/Job_related/Listenfield/Task_data/phenos.csv')
pheno
is.data.frame(pheno)

#understanding charactersitics
summary(pheno)

plot(pheno$plot)
table(pheno$ind)
table(pheno$year)
table(pheno$plot)
table(pheno$pathogen)
table(pheno$trait3)

hist(pheno$trait2)
boxplot(pheno$trait2)
summary(sorted_pheno)

table(pheno[pheno$pathogen == 'TRUE', 'pathogen'])

### Arranging df in descending order
## highest potential individuals
sorted_pheno <- pheno[order(pheno$trait2,decreasing=TRUE),]
sorted_pheno

## grouping in categories
# with presence and absence of pathogens
pheno_groupsFT <- pheno %>% mutate(group =
                                     case_when(pathogen == 'FALSE' ~ "Pathogen\nAbsent",
                                               pathogen == 'TRUE'  ~ "Pathogen\npresent")
)

table(pheno_groupsFT$group)
dim(pheno_groupsFT)

# with affected and not affected categories based on trait3
pheno_affected_groups <- pheno %>% mutate(group =
                     case_when(pathogen == 'TRUE' & trait3 == 0 ~ "Not\nAffected",
                               pathogen == 'TRUE' & trait3 == 1 ~ "Affected")
)

pheno_affected_groups <- filter(pheno_affected_groups, group != "NA")

## Identifying potential individuals from group not affected
unaffected_group <- filter(pheno_affected_groups, group == "Not\nAffected")
dim(unaffected_group)
unaffected_group[order(unaffected_group$trait2,decreasing=TRUE),]
table(unaffected_group$ind)

table(pheno_affected_groups$group)
dim(pheno_affected_groups)

###plotting boxplot
library(ggplot2)
library(viridis)

plot_df <- rbind(pheno_groupsFT, pheno_affected_groups)
dim(plot_df)

trait_boxplot <- ggplot() + 
  geom_boxplot(data=plot_df, aes(x=group, y=trait2, fill=group)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  labs(y = "Trait2", x= "Group") +
  theme_bw() +
  theme(legend.position = "None",
        axis.text = element_text(face = "bold", size = 12, colour = "black"),
        axis.title = element_text(face = "bold", size = 12, colour = "black"))
 
trait_boxplot

ggsave("D:/Personal/Job_related/Listenfield/Scipts/Output/boxplot_trait2_task2.1.1.png", 
       trait_boxplot, height = 5, width = 6)
###############################################################################
################### Initial calculations (rough work) #########################
#3 groups will be there to identify the most advantageous individuals for cross breeding.
#  When pathogen are not there
# when pathogen are there and not affected
# when pathogens are there and affected

pathF_3F <- filter(pheno_modified, group == "NA") #mean 18.012
summary(pathF_3F$trait2) 
table(pathF_3F$ind)
table(pathF_3F$pathogen)
boxplot.stats(pathF_3F$trait2)$out

pathT_3F <- filter(pheno_modified, group == "Not affected") #mean 16.12
summary(pathT_3F$trait2)
table(pathT_3F$pathogen)
boxplot.stats(pathT_3F$trait2)$out

pathT_3T <- filter(pheno_modified, group == "Affected") #mean 15.627
summary(pathT_3T$trait2)
table(pathT_3T$pathogen)
boxplot.stats(pathT_3T$trait2)$out
##################################

df_pathT <- filter(pheno, pathogen == 'TRUE')
mean(df_pathT$trait2) #15.72475
boxplot(df_pathT$trait2)
boxplot.stats(df_pathT$trait2)$out

df_pathF <- filter(pheno, pathogen == 'FALSE')
mean(df_pathF$trait2) #18.01232
boxplot(df_pathF$trait2)
boxplot.stats(df_pathF$trait2)$out
dim(df_pathF)
outlier_row <- filter(pheno, trait2 == 7.515127)

# Remove outliers
df_pathF_woo <- df_pathF[!(df_pathF$trait2 %in% boxplot.stats(df_pathF$trait2)$out), ]
dim(df_pathF_woo)
mean(df_pathF_woo$trait2) # 18.01815
