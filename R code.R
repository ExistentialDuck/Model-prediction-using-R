setwd("C:/Users/Stephen Hanna/Documents/Classes/Data analysis")
#Sets working directory to find the excel sheet with our values
MyData <-read.csv("Group_26.csv",header=T)
#Assigns excel sheet values to a data frame in R
MyData[1]<- NULL
#Removes first column since it is only composed of index values
shapiro.test(MyData[,1])
#Tests for normality
#Since the p value is below .1, we reject the null hypothesis and conclude that the data is not normal
fit <- lm(DV_Y~(E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^3,data=MyData)
#Makes a linear model for the data, but includes all pairwise interactions. 
bc <- boxcox(fit,lambda = seq(-6,6,0.1))
#Conducts BoxCox analysis to find lambda value for transformation
bc$x[which.max(bc$y)]
#Determines corresponding lambda value for the peak of the boxcox curve
MyData[,1] = (MyData[,1])^3.1
#Conducts boxcox transformation on dependent variable values using lambda value
shapiro.test(MyData[,1])
#Conducts another normality test for the dependent variable
data:  MyData[, 1]
#Since p is greater than .01 now, the dependent variable has been transformed into a normal distribution
x <- model.matrix(fit,MyData[,-1])[,-1]
#Creates matrix with all independent variables and their associated values
y = as.matrix(MyData[,1])
#Creates vector of transformed dependent variable
cvfit = cv.glmnet(x, y)
#Creates a Lasso model
coef(cvfit, s = "lambda.1se")
#Shows coefficients for all variables, two way interactions, and three way interactions. All non-significant variables or interactions have a coefficient of zero.
fit <- lm(DV_Y~E1 + E1:G2 + E1:G11 + E2:G2 + E5:G2 + G9:G13 + E1:G8:G9 + E1:G8:G11 + E5:G2:G15,data=MyData)
#Creates linear model using variables or interactions with non zero coefficients from Lasso analysis
summary(fit)
#Creates table with relevant values from previous linear model
fit <- lm(DV_Y~E1+E2:G2+E5:G2+G9:G13,data=MyData)
#Creates linear model using only variables or interactions that had p values significantly below .01 from last linear model
summary(fit)
#Creates table with relevant values from previous linear model
plot(fit)
#Plots several graphs related to the model. I believe the most important two are shown below.
Anova <- aov(DV_Y~E1+E2:G2+E5:G2+G9:G13,data=MyData)
#Assigns anova table of the model to a variable
summary(Anova)
