# -
#
# Copyright 2012 Dave Langer
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#  	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 



#
# This R source code file corresponds to video 1 of the YouTube series
# "Introduction to Data Science with R" located at the following URL:
#     http://www.youtube.com/watch?v=32o0DnuRjfg
#



# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets 
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.Survived)

# A bit about R data types (e.g., factors)
str(tomatoXL)

tomatoXL$Survived <- as.factor(tomatoXL$Survived)
tomatoXL$Pclass <- as.factor(tomatoXL$Pclass)


# Take a look at gross survival rates  总的生存情况
table(tomatoXL$Survived)


# Distribution across classes  舱位等级情况
table(tomatoXL$Pclass)


# Load up ggplot2 package to use for visualizations 安装ggplot2包
library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate假设——富人的存活率更高
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$Name))


# How many unique names are there across both train & test?在TRAIN和TEST中有多少个唯一的名字?
length(unique(as.character(tomatoXL$Name)))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector由上可得出有两个重复的名字，获取重复的名称并将它们存储为一个向量
dup.names <- as.character(tomatoXL[which(duplicated(as.character(tomatoXL$Name))), "Name"])


# Next, take a look at the records in the combined data set接下来，查看组合数据集中的记录
tomatoXL[which(tomatoXL$Name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?区分小姐先生
library(stringr)


# Any correlation with other variables (e.g., sibsp)?Miss与其他变量(如sibsp)是否相关
misses <- tomatoXL[which(str_detect(tomatoXL$Name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age假设——姓名头衔与年龄有关
mrses <- tomatoXL[which(str_detect(tomatoXL$Name, "Mrs.")), ]
mrses[1:5,]


# Check out males to see if pattern continues检查男性，看看这种模式是否会继续
males <- tomatoXL[which(tomatoXL$Sex == "male"), ]
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(tomatoXL)) {
  titles <- c(titles, extractTitle(tomatoXL[i,"Name"]))
}
tomatoXL$title <- as.factor(titles)


# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(tomatoXL[1:891,], aes(x = title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# 自己试验，下三行等于35行
library(readxl)
excel_sheets('hebing.xlsx')
tomatoXL<-read_excel('hebing.xlsx')




# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set查看整个数据集的年龄提取
summary(tomatoXL$Age)
summary(tomatoXL[1:891,"Age"])


# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
# 为了更全面，我们来看看按性别、pclass和年龄划分的存活率
ggplot(tomatoXL[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children验证男孩
boys <- tomatoXL[which(tomatoXL$title == "Master."),]
summary(boys$Age)

# We know that "Miss." is more complicated, let's examine further进一步研究miss
misses <- tomatoXL[which(tomatoXL$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later女性和儿童的存活率不同，需要进一步研究
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the sibsp variable, summarize the variable是否有亲属列
summary(tomatoXL$SibSp)

# Can we treat as a factor?
length(unique(tomatoXL$SibSp))

tomatoXL$SibSp <- as.factor(tomatoXL$SibSp)

# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title我们相信头衔具有预测性。通过sibsp、pclass和title可视化生存策略
ggplot(tomatoXL[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the parch vaiable as a factor and visualize把仓位的可能性作为一个因素，想象一下
tomatoXL$Parch <- as.factor(tomatoXL$Parch)
ggplot(tomatoXL[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")



# Let's try some feature engineering. What about creating a family size feature?
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
tomatoXL$family.size <- as.factor(temp.SibSp + temp.Parch + 1)

# Visualize it to see if it is predictive家庭大小与生存情况
ggplot(tomatoXL[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


###新添加
