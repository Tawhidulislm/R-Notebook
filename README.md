# R-Notebook

---
title: "Political Explanation Homework Assignment 2"
subtitle: "GV900"
output: 
  pdf_document: default
  html_document: default
  word_document: default
date: "2022-12-15"
---

**1. Preparing R packages**

```{r}

library(ggplot2)       # for graphics
library(gmodels)       # for contingency table with percentages
library(Hmisc)         # for data analysis and high-level graphics
library(stargazer)     # for Regression table
library(effects)       # for effect
library(gridExtra)     # For grid.arrange

```

**2. Loading "Titanic" data set**

```{r}

td<-read.csv("F:/titanic.csv",header=TRUE)
attach(td)

```

**3. Total number of passengers in the dataset**

```{r}

length(td$name)

```

**# The result of the command expresses that there are 1309 passengers in the dataset.**

**4. Frequency table of the survived variable**

```{r}

table<-table(td$survived)
View(table)

```

**5. Percentage of the survived passengers**

```{r}
# 'prop.table' for the proportion of each cell

prop.table(table)*100

```

**# About 38.19%  passengers have survived.**

   
**6. Frequency table of pclass**

```{r}

table(td$pclass)

```

**7. In our study, passenger socioeconomic class is an independent variable and passenger survival is a dependent variable.**

**8. Creating a two-way frequency table**

```{r}

CrossTable( td$survived, td$pclass, prop.r = TRUE, prop.t = FALSE, prop.chisq = FALSE, chisq = TRUE) 

```

**9.**

**(a) The survival percentage among the 1nd class passengers is 40%.**
**(b) The survival percentage among the 2nd class passengers is 24%.**
**(c) the survival percentage among the 3rd class passengers is 34%.**


**10. According to the sample's pattern, the paternity of the survivors varies depending on the passenger's class. As a result, there is a connection between surviving and pclass.**


**11. (l), (n), (e), (p).**

**12. Exploring the relationship between the female variable and fare**

```{r}

t.test(td$fare ~ td$female, data=td, var.equal=FALSE, na.rm=TRUE)

```
    
**13. We can rule out the null hypothesis that there is no difference in ticket fares between the groups of male and female with a 99.9% confidence interval because the test statistic yields a p-value that is less than 0.001.**
**The average ticket fare for the female demographic is higher (46.19668).**
**As a result, tickets for female passengers are typically more expensive than those for male passengers.**


**14. Graphical relationship between age and fare using the ggplot function.**

```{r}

ggplot(data = td,aes(x=age,y=fare))+geom_point()

```

**15. Exploring the relationship between age and fare.**

```{r}
x=as.matrix(td[c("age","fare")])
rcorr(x,type = "pearson")

```

**16. At a 99.9% confidence interval, the computed correlation coefficient between age and fare is positive (0.18) and statistically significant. Supporting our hypothesis that elderly passengers typically purchase tickets at a higher price.**

**17. Regression table using the stargazer function**

```{r}
t1=lm(fare~age)
stargazer(t1,type="text")

```   

**18. Creating a plot that illustrates the estimated effect of age on fare**

```{r}

eff.t1 <- effect(term = "age", mod = t1)
plot(eff.t1)

```

**19.**

**It is clear from the model that the regression coefficient of age is 0.692 (p.005). Age and fare have a favorable association in the plot.**


**20. Graphical relationship between age and fare for female and male passengers separately**

```{r}
td.f<-td[td$female=="Female",]
td.m<-td[td$female=="Male",]
par(mfrow=c(1,2))
ggplot(td.f,aes(x=age,y=fare))+geom_point()
ggplot(td.m,aes(x=age,y=fare))+geom_point()

```

**21. Regress fare on age and female**

```{r}

fit.td<-lm(fare~age+female,data=td)
stargazer(fit.td,type="text")

```

**22. (d), (g), (i), (i)**

**23. Based on the second regression model creating a plot **

```{r}

eff.fit.td<- effect(term = "female", mod = fit.td)
plot(eff.fit.td)

```

**24. Plotting one that has two panels**

```{r}
f<-lm(fare~age,data=td.f)
m<-lm(fare~age,data=td.m)
fit.td.f<- effect(term = "age", mod = f)
fit.td.m<- effect(term = "age", mod = m)
plot(fit.td.f)+plot(fit.td.m)

```



#  update R into latest version using installr and updateR() command
install.packages("installr")
library(installr)
update(r)

# edit with multiple-selections in RStudio: Press Ctrl + Alt + Shift + M


# Create a, b, c, d variables
a <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df

# Name the data frame
names(df) <- c('ID', 'items', 'store', 'price')
df

# Print the structure
str(df)

## Select row 1 in column 2
df[1,2]

## Select Rows 1 to 2
df[1:2,]

## Select Columns 1
df[,1]

## Select Rows 1 to 3 and columns 3 to 4
df[1:3, 3:4]

# Slice with columns name
df[, c('ID', 'store')]

# Create a new vector
quantity <- c(10, 35, 40, 5)

# Add `quantity` to the `df` data frame
df$quantity <- quantity
df


# Select the column ID
df$ID

# Select price above 5
subset(df, subset = price > 5)


# Vector with numeric from 1 up to 5
vect  <- 1:5

# A 2x 5 matrix
mat  <- matrix(1:9, ncol = 5)
dim(mat)

# select the 10th row of the built-in R data set EuStockMarkets
df <- EuStockMarkets[1:10,]


# Construct list with these vec, mat, and df:
my_list <- list(vect, mat, df)
my_list


# Print second element of the list
my_list[[2]]


# Built-in Data Frame
PATH <-'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/prison.csv'
df <- read.csv(PATH)[1:5]
head(df, 5)

# Structure of the data
str(df)


# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find Mean.
result.mean <- mean(x)
print(result.mean)

df
# Create a vector.
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find Mean.
result.mean <-  mean(x,trim = 0.3)
print(result.mean)

a <- c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b <- c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
mean.test(a,b)


x <- 0:10
x
print(2*x+1)

x1= seq(1,21,2)
x1
x2= seq(50, 11, by=-3)
x2

A= matrix(1:16,ncol = 4, byrow = T)
A


det (A)
solve(A)


student <- read.table("B:/Assignments/R, SAS Assignment/student.txt", header=T)
student

attach(student)
mean(Pulse,na.rm=T)
mean(student$Pulse[student$Age<20],na.rm = T)
mean(student$Age[student$W.Hnd=="Right"],na.rm =T)
plot(Pulse,log(Age-10))

par(mfrow=c(1,2))
x<-Age
hist(x,main = "Histogram for Age",xlab="Age",freq = F,col="green",las=2)
n1=rnorm(length(x),mean(x),sd(x))
lines(density(n1,adjust = 1.9),col="red")

x<-Age
par(mfrow=c(1,2))
hist(x^2,main = "Histogram for Age^2", xlab="Age^2",freq = F,col="blue")
n2=rnorm(length(x^2),mean(x^2),sd(x^2))
lines(density(n2,adjust = 1.9),col="red")

hist(Age,freq=F,col="green")
hist(Age^2,freq=F,col="red")

table=table(student$Smoke,student$Sex)
table
chisq.test(table)

model=lm(iq$PIQ~iq$Brain+iq$Height+iq$Weight)
summary(model)
hist((model$residual+32.74)/19.79,freq=F,xlab="sresid")

mydat <- sample(1:4,1000,rep=TRUE, prob=c(.2,.3,.2,.3))
table(mydat)

set.seed(1)
rnorm(4)

install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
library(mlbench)
data(Sonar)
data(mtcars)

