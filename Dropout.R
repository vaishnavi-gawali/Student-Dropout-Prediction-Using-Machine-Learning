#loading dataset into R
library(readxl)
School<-read_excel("Dataset_schooling_with_categories_and_codes.xlsx")
View(School)



#checking how many rows and columns are present in the data
nrow(School)
ncol(School)
head(School)



# checking names of columns
names(School)



#checking if there are null values
sum(is.na(School))



#checking class of variables
sapply(School, class)



#summary statistics of data
summary(School)



#remove non numeric columns
School_1<- School[,-c(2,4,7,9,11,13,15,17,19,21,23,25,27,29,31,33,36,52)]
View(School_1)


#getting basic statistics of the numerical variables
data.frame(mean=sapply(School_1, mean),
           sd= sapply(School_1, sd),
           median= sapply(School_1, median),
           max= sapply(School_1, max),
           min= sapply(School_1, min))



#creating basic plots and checking their distribution
hist(School$`Age at enrollment`, main = "Age at Enrollment", xlab = "Age at Enrollment", ylab = "Frequency")
hist(School$GDP, main = "Histogram of GDP", xlab = "GDP", ylab = "Frequency", breaks = 10)
hist(School$`Inflation rate`, main = "Histogram of Inflation Rate", xlab = "Inflation Rate", ylab = "Frequency", breaks = 5)
hist(School$`Unemployment rate`)



?hist
boxplot(School$`Age at enrollment`)
boxplot(School$GDP)
boxplot(School$`Inflation rate`)




#checking skewness
install.packages("moments")
library(moments)



skewness(School$`Age at enrollment`)
skewness(School$GDP)
skewness(School$`Inflation rate`)



#plotting distribution of various categories
# count of students by dropout vs enrolled vs graduate
table(School$Target)
library(ggplot2)
ggplot(School, aes(x=School$Target, fill=School$Target)) +
  geom_bar()



ggplot(School, aes(x=School$`Gender Categories`, fill= School$`Gender Categories`)) +
  geom_bar()



School_gender_target <- table(School$`Gender Categories`, School$Target)
School_gender_target
barplot(School_gender_target, legend.text = TRUE)



ggplot(School,
       aes(x = School$`Gender Categories`,
           fill = School$Target)) +
  geom_bar(position = "stack")



ggplot(School,
       aes(x = School$`Gender Categories`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")



ggplot(School,
       aes(x = School$`Gender Categories`,
           fill = School$Target)) +
  geom_bar(position = position_dodge(preserve = "single"))



ggplot(School,
       aes(x = School$`Course categories`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")



ggplot(School,
       aes(x = School$`Course categories`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggplot(School,
       aes(x = School$`Daytime_evening_attendance categories`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))



ggplot(School,
       aes(x = School$Nationality,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggplot(School,
       aes(x = School$`Age at enrollment`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




ggplot(School,
       aes(x = School$GDP,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggplot(School,
       aes(x = School$`Inflation rate`,
           fill = School$Target)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#correlation matrix
#need to remove categorical codes columns
School_2<- School_1[,-c(1,2,4:17,19)]
View(School_2)
cor(School_2)
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(School_2), hc.order = TRUE, type = "upper", lab = TRUE)
# correlation matrix shows that following variables are strongly correlated
# curricular units 2nd sem approved and curricular units 1st sem approved
# curricular units 1st sem grade and curricular units 2nd sem grade
# curricular units 1st sem evaluations and curricular units 2nd sem evaluations
# curricular units 1st sem enrolled and curricular units 2nd sem enrolled

School_3<- School[,-c(2,4,7,9,11,13,15,17,19,21,23,25,27,29,31,33,36)]
Sch = School_3
View(Sch)
Sch$Target <- factor(School_3$Target, levels = c("Graduate", "Dropout", "Enrolled"))
Sch$Target <- as.numeric(Sch$Target)-1
View(Sch)

Sch_filtered <- Sch[Sch$Target != 2, ]

Model2<- Sch_filtered[,-c(1,3,13,22,24,25,27,28)]
Model2<- Model2[,-c(20,21,22,23,25,26)]
View(Model2)

set.seed(10)
train_index_2 <- sample(nrow(Model2), (nrow(Model2) * 0.7), replace = FALSE)
train_S2 <- Model2[train_index_2, ]
test_S2 <- Model2[-train_index_2, ]

install.packages("pROC")
library(pROC)
install.packages("caret")
library(caret)


logit.reg2 <- glm(train_S2$Target ~ . , data = train_S2, family = "binomial")



summary(logit.reg2)
predictions_logit.reg_train2 <- predict(logit.reg2, train_S2, type = "response")
confusionMatrix(as.factor(ifelse(predictions_logit.reg_train2>0.5,1,0)), as.factor(train_S2$Target))



predictions_logit.reg_val2 <- predict(logit.reg2, test_S2, type = "response")
confusionMatrix(as.factor(ifelse(predictions_logit.reg_val2>0.5,1,0)), as.factor(test_S2$Target))




r_logi2 <- roc(test_S2$Target,predictions_logit.reg_val2, auc = TRUE)
plot.roc(r_logi2)
auc(r_logi2)
