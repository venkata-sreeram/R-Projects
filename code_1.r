## load packages
install.packages("data.table")
install.packages("dplyr")

library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining

library(caret)      # used for modeling

install.packages("corrplot")
library(corrplot)   # used for making correlation plot
install.packages("xgboost")
library(xgboost)    # used for building XGBoost model


## read datasets
train = read.table(file="D:/CARRER/My_Course/CaseStudies/Train_UWu5bXk.csv",header=T,sep=",")
test = read.table(file="D:/CARRER/My_Course/CaseStudies/Test_u94Q5KV.csv",header=T,sep=",")

#submission = fread("SampleSubmission_TmnO39y.csv")

## train data column names
names(train)

## test data column names
names(test)

## structure of train data
str(train)

## structure of test data
str(test)

#-------------------------------------------------------------------------------------------------------------------------

test$Item_Outlet_Sales = NA ## add Item_Outlet_Sales to test data
str(test)

combi = rbind(train, test) # combining train and test datasets
str(combi)
summary(combi$Item_Outlet_Sales)
dim(combi)
#-------------------------------------------------------------------------------------------------------------------------


## EDA - Univariate
#  Independent Variables (numeric variables)
library(ggplot2)    # used for ploting 
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")

library(cowplot)    # used for combining multiple plots 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package
#-------------------------------------------------------------------------------------------------------------------------

# Independent Variables (categorical variables)
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
#-------------------------------------------------------------------------------------------------------------------------

#Now let's check the other categorical variables.

# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)

plot_grid(p4, second_row, ncol = 1)
#-------------------------------------------------------------------------------------------------------------------------

#We'll also check the remaining categorical variables.


# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7, p8, ncol = 2)
#-------------------------------------------------------------------------------------------------------------------------

