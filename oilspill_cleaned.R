setwd("C:/Users/User/Documents/R")
library(data.table)
library(reshape2)
library(ggplot2)
library(corrplot)
library(car)
library(caTools)
library(rpart)
library(rpart.plot) 
library(dplyr)
library(nnet)
library(caret)

oilspill <- fread("oilspill_cleaned.csv")
set.seed(2004)
options(scipen = 999, digits = 10)

str(oilspill)
sum(is.na(oilspill)) #230NA will be handled later
summary(oilspill)

oilspill$`Cause Category` <- factor(oilspill$`Cause Category`)
oilspill$`Cause Subcategory` <- factor(oilspill$`Cause Subcategory`)

#exploratory analysis
#corrosion is the 2nd most common cause for oil spill
ggplot(data = oilspill, aes(x = `Cause Category`, fill = `Cause Category`), group = `Cause Category`) + geom_bar() +  
  labs(x = "Cause", y = "Count", title = "Number of oil spill incidents by cause") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_legend(title = "Cause Category"))

#2nd most expensive cause
ggplot(data = oilspill, aes(x = factor(`Cause Category`), y = `All Costs`, fill = factor(`Cause Category`))) +
  geom_bar(stat = "identity") +
  labs(x = "Cause Category", y = "Total Costs", title = "Total Costs by cause") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Cause Category")) 

#all costs over the year
sum_costs_by_year_overall <- aggregate(cbind(`Property Damage Costs`, `Lost Commodity Costs`, 
                                             `Public/Private Property Damage Costs`, `Emergency Response Costs`, `Environmental Remediation Costs`, `Other Costs`) ~ `Accident Year`, 
                                       data = oilspill, sum)
melted_data_overall <- melt(sum_costs_by_year_overall, id.vars = "Accident Year")
ggplot(melted_data_overall, aes(x = as.factor(`Accident Year`), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Costs Over the Years",
       x = "Year",
       y = "All Costs") +
  theme_minimal()

#corrosion insight
oilspill2 <- oilspill[`Cause Category`=="CORROSION",]
summary(oilspill2)

#avg cost per incident caused by corrosion is $667780 - a huge number
sum(oilspill2$`All Costs`) / nrow(oilspill2) #667779.8598
nrow(oilspill2[`Net Loss (Barrels)` != 0]) / nrow(oilspill2) * 100 #49.66216
sum(oilspill2$`Net Loss (Barrels)`) / nrow(oilspill2[`Net Loss (Barrels)` !=0]) #136.5194
sum(oilspill2$Shutdown_hours) #96712.97
sum(oilspill2$Shutdown_hours) / nrow(oilspill2[Shutdown_hours != 0]) #312.987
nrow(oilspill2[Shutdown_hours != 0]) / nrow(oilspill2) * 100 #52.19595

oilspill2[which.max(oilspill2$`All Costs`), ] #142931884
oilspill2[which.max(oilspill2$`Net Loss (Barrels)`), ] #8000
nrow(oilspill2[oilspill2$`Barrel loss (%)` == 100]) #122 accidents lost 100% oil
oilspill2[which.max(oilspill2$Shutdown_hours), ] #12475.13

#Top most expensive costs involve: emergency response costs, environmental costs and property damage costs
a <- round(sum(oilspill2$`Property Damage Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`)*100,2)
b <- round(sum(oilspill2$`Lost Commodity Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
c <- round(sum(oilspill2$`Public/Private Property Damage Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
d <- round(sum(oilspill2$`Emergency Response Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
e <- round(sum(oilspill2$`Environmental Remediation Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
f <- round(sum(oilspill2$`Other Costs`, na.rm = TRUE) / sum(oilspill2$`All Costs`) * 100,2)
#Pie chart
values <- c(a, b, c, d, e, f)
a + b + c + d + e + f #check if it's = 100
labels <- c("Property Damage Costs", "Lost Commodity Costs", "Public/Private Property Damage Costs", "Emergency Response Costs", "Environmental Remediation Costs", "Other Costs")
pie_colors <- c("pink", "yellow", "green", "lightblue", "red","orange")
pie <- data.frame(labels, values)
ggplot(pie, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +  # Create a polar coordinate system
  coord_polar("y", start=0) +
  theme_void() +   # Customize the theme if needed
  labs(title = "Breakdown of costs for incidents caused by corrosion (%)") + 
  geom_text(data = pie, aes(label = values), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Set2")

sum_costs_by_year_corrosion <- aggregate(cbind(`Property Damage Costs`, `Lost Commodity Costs`,
                                               `Public/Private Property Damage Costs`, `Emergency Response Costs`, `Environmental Remediation Costs`, `Other Costs`) ~ `Accident Year`, 
                                         data = oilspill2, sum)

#stacked bar chart
melted_data_corrosion <- melt(sum_costs_by_year_corrosion, id.vars = "Accident Year")
ggplot(melted_data_corrosion, aes(x = as.factor(`Accident Year`), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Costs (Corrosion) Over the Years",
       x = "Year",
       y = "All Costs") +
  theme_minimal()

#internal corrosion contributes more to incidents because it is harder to detect than external corrosion
ggplot(data = oilspill2, aes(x = factor(`Cause Subcategory`), fill = factor(`Cause Subcategory`), group = factor(`Cause Subcategory`))) + geom_bar() +  
  labs(x = "Cause", y = "Count", title = "Number of oil spill incidents by cause") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_legend(title = "Cause Category"))

#costs of internal corrosion over the years
internal_corrosion_data <- subset(oilspill, `Cause Category` == "CORROSION" & `Cause Subcategory` == "INTERNAL")
sum_costs_by_year_internal_corrosion <- aggregate(cbind(`Property Damage Costs`, `Lost Commodity Costs`, 
                                                        `Public/Private Property Damage Costs`, `Emergency Response Costs`, `Environmental Remediation Costs`, `Other Costs`) ~ `Accident Year`, 
                                                  data = internal_corrosion_data, sum)
melted_data_internal_corrosion <- melt(sum_costs_by_year_internal_corrosion, id.vars = "Accident Year")
ggplot(melted_data_internal_corrosion, aes(x = as.factor(`Accident Year`), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Costs (Internal Corrosion) Over the Years",
       x = "Year",
       y = "All Costs") +
  theme_minimal()


#build model to predict pipeline shutdown
oilspill3 <- oilspill[,c("Pipeline Location", "Pipeline Type", "Liquid Type", "Liquid Ignition", 
                         "Liquid Explosion", "Pipeline Shutdown")]

oilspill3 <- oilspill3 %>% filter_at(vars("Pipeline Shutdown"), all_vars(!is.na(.))) 
oilspill3 <- oilspill3 %>% filter_at(vars("Pipeline Type"), all_vars(!is.na(.))) 
sum(is.na(oilspill3))


oilspill3$`Pipeline Shutdown` <- factor(oilspill3$`Pipeline Shutdown`)
oilspill3$`Pipeline Location` <- factor(oilspill3$`Pipeline Location`)
oilspill3$`Pipeline Type` <- factor(oilspill3$`Pipeline Type`)
oilspill3$`Liquid Type` <- factor(oilspill3$`Liquid Type`)
oilspill3$`Liquid Ignition` <- factor(oilspill3$`Liquid Ignition`) 
oilspill3$`Liquid Explosion` <- factor(oilspill3$`Liquid Explosion`)

str(oilspill3)
#Pipeline location only has 1 level 
nrow(oilspill3[`Pipeline Location` == "ONSHORE",]) #problem here is pipeline location is 100% onshore 

train <- sample.split(Y = oilspill3$`Pipeline Shutdown`, SplitRatio = 0.7)
trainset <- subset(oilspill3, train==T)
testset <- subset(oilspill3, train==F)

reg = glm(`Pipeline Shutdown` ~ `Pipeline Type` + `Liquid Type` + `Liquid Ignition`, 
          family = binomial, data = trainset)
summary(reg)

prob = predict(reg, type = 'response')
classifier = ifelse(prob>0.5, "YES", "NO")
table(classifier, trainset$`Pipeline Shutdown`)
mean(classifier == trainset$`Pipeline Shutdown`)*100 #59.52115813 

test_prob = predict(reg, newdata=testset, type="response")
test_classifier = ifelse(test_prob>0.5,"YES","NO")
table(test_classifier, testset$`Pipeline Shutdown`)
mean(test_classifier == testset$`Pipeline Shutdown`)*100 #61.2987013


test_classifier = as.factor(test_classifier)
confusionMatrix(test_classifier,testset$`Pipeline Shutdown`)
log_model_acc = mean(test_classifier == testset$`Pipeline Shutdown`) * 100
log_model_acc


m <- rpart(`Pipeline Shutdown` ~ ., data = trainset, method = 'class',
            control = rpart.control(minsplit = 20, cp = 0))
rpart.plot(m, nn= T, main = "Maximal Tree for Pipeline shutdown")
print(m)
printcp(m)
plotcp(m, main = "Subtrees for Pipeline shutdown")
cp1 <- 0.025
m2 <- prune(m, cp = cp1)
printcp(m2)
rpart.plot(m2, nn= T, main = "Optimal Tree for Pipeline shutdown")
m2$variable.importance

cart.predict <- predict(m2, newdata = trainset, type = "class")
results <- data.frame(trainset, cart.predict)
mean(cart.predict == results$Pipeline.Shutdown) #0.596325167

cart.predict.test <- predict(m2, newdata = testset, type = "class")
results <- data.frame(testset, cart.predict.test)
mean(cart.predict.test == results$Pipeline.Shutdown) #0.612987013

