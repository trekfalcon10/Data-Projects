#Install titanic dataset
install.packages("titanic")

#Load titanic, ggplot2 and dplyr libraries
library(titanic)
library(ggplot2)
library(dplyr)

#Initialize titanic dataset variable, selecting only appropriate columns
titanic <- select(titanic_train, Survived, Pclass, Sex, Age)

#Make Sex column a factor
titanic$Sex <- factor(titanic$Sex)

#Omit "NA" rows
titanic <- na.omit(titanic)

# 1 - Check the structure of titanic
str(titanic)

# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge") + facet_grid(. ~ Survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = Pclass, col = Sex, y = Age)) + 
  geom_point(position = posn.jd, size = 3, alpha = 0.5) + facet_grid(. ~ Survived)
