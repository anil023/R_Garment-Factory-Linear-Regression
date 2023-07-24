##Setup-----

# Clear the Environment, Console and Plots respectively from memory
rm(list = ls())
cat("\014")
#dev.off() #clear any plots
# Save R file and the data set in the same directory
# Set working directory to where the R file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



##Load Packages-------

set.seed(1)
library(ggplot2) #ggplot, geom_bar
library(ggpubr) #ggdoghnutchart
library(moments) #skewness
library(extrafont) #more font types for plots
library(graphics) # R graphics
library(grDevices) # R graphics devices for color and fonts
library(corrplot) #correlation plot
library(car) #ScatterPlot Matrix,Applied Regression and Data sets
#library(carData)
library(leaps) #Model selection
library(olsrr) #Cooks Distance Plot
#library(DMwR2) #Knn Imputation

#Base Packages
library(rgl)
library(base)
#library(datasets)
library(methods)
library(stats)
library(utils)



##Data and Variable Assignment----------

#converted blank spaces to NA while importing
garwork <- read.csv('garments_worker_productivity.csv', na.strings=c(""," "))

summary(is.na(garwork))
#knnImputation(garwork ,k = 10, scale = TRUE, meth = "weighAvg")
#Observe that only 'wip' variable has NA values and the count of NA is about 42% of the variable data.
#Since we can't confirm whether the NA cells can be considered as having value 0, we won't change the values.
#Imputing is not feasible as the count/percentage of NA is high and functions are coercing back to NA.
#Hence it is recommended to remove the variable 'wip' from the data set.

#Removing the 'date' variable from the analysis, reason - not exploring a Time Series Analysis with the data
garwork <- subset(garwork, select = -c(date,wip) )

str(garwork) #check variable types etc
#Notice the categorical variables doesn't have categorical data type in the data set, lets change that.
garwork$quarter <- as.factor(garwork$quarter)
garwork$department <- as.factor(garwork$department)
garwork$day <- as.factor(garwork$day)
garwork$team <- as.factor(garwork$team)
garwork$no_of_style_change <- as.factor(garwork$no_of_style_change)
str(garwork) #running again for verification

head(garwork)
summary(garwork)
#Note:
#over_time,incentive, idle_time and idle_men has a very high Max value, also the Mean and Median are widely spread - check the plots for skewness
#actual_productivity ranges from 0.2 to 1.1
Y <- garwork$actual_productivity



##Explore Categorical Variables------

# Create a scatter plot matrix of your response and the five predictor variables
scatterplotMatrix(~Y+quarter+department+day+team+no_of_style_change,garwork,smooth=FALSE)
#we don't observe any correlation except with no_of_style_change

#GGPlot Theme for similar visual plots
#Define theme_ggpbar() function
theme_ggpbar <- function(){ 
  font <- "Arial"   #font family
  theme_grey() %+replace%    #replace elements we want to change
    theme(
      #text elements
      plot.title = element_text(family = "Arial",colour="#616161",face = "bold",size=13, hjust = 0.5),
      plot.subtitle = element_text(family = "Arial",colour="#616161",size=10, hjust = 0.5),
      axis.title = element_text(family = "Arial",colour="#616161",face = "bold",size=10),
      axis.text = element_text(family = "Arial",colour="#616161",size=8)
    )
}

#quarter
ggplot(garwork,aes(x=reorder(quarter, quarter, function(x)-length(x)))) +
  geom_bar(fill='red') +  
  labs(x='Quarter:1 to 5', y='Count', title = "Frequency Distribution: Quarter", subtitle = "") +
  geom_text(stat='count', aes(label=..count..), vjust=2) + 
  theme_ggpbar() +
  theme(legend.title = element_text(family = "Arial",colour="#616161",face = "bold",size=10),
        legend.text = element_text(family = "Arial",colour="#616161",face = "bold",size=8))
#generally we observe lesser number of observations recorded towards the end of the month
#notice that the Quarter 4 in a month seems to have more data points than Quarter 3

#department
ggplot(data=data.frame(Count = summary(garwork$department), Department = levels(garwork$department)), aes(x=Department, y= Count,fill = Department))+
  geom_bar(stat="identity") +
  geom_text(label=summary(garwork$department), vjust=2) + #shows count
  geom_text(label=paste(round(summary(garwork$department)/sum(summary(garwork$department))*100,0),"% of Total"), vjust=4) +  #shows percentage
  theme_ggpbar() +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = "Arial",colour="#616161",face = "bold",size=8),
        legend.position = "top")+
  ggtitle(label = "Frequency Distribution: Department",subtitle = "") #instead of using labs() as shown above
#we observe almost equal number of observations within each department

#day
data <- data.frame(Percent = round(summary(garwork$day)*100/sum(summary(garwork$day)),1), Day = levels(garwork$day))
data <- as.matrix(data)
data <- data[order(data[,1], na.last = TRUE, decreasing = TRUE),]
data <- data.frame(data)
data$Day <- as.factor(data$Day)
data$Percent <- as.integer(data$Percent)
ggplot(data, aes(x = "", y = "", fill = Day)) +
  geom_bar(aes(x = "", y = Percent, fill = Day), width = 1, stat = "identity", color = "white") +
  coord_polar("y") +    #makes it a pie
  geom_text(aes(x = "", y = Percent,label = Percent), position = position_stack(vjust = 0.5)) +  #creates the label within the plot
  labs(x = NULL, y = NULL, fill = NULL, title = "Frequency Distribution: Days(in %)") +   #creates the Plot Title
  theme_ggpbar() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),    #strip major gridlines
        panel.grid.minor = element_blank(),    #strip minor gridlines
        plot.title = element_text(family = "Arial",colour="#616161",face = "bold",size=13, hjust = 0.5),
        legend.text = element_text(family = "Arial",colour="#616161",size=8),
        legend.position = "top")
#we notice a very similar percentage of data from each day except on Saturday 

#team
data <- data.frame(Count = summary(garwork$team), Team = levels(garwork$team))
data <- as.matrix(data)
data <- data[order(data[,1], na.last = TRUE, decreasing = TRUE),]
data <- data.frame(data)
data$Team <- as.factor(data$Team)
data$Count <- as.integer(data$Count)
data$ymax = cumsum(data$Count)
data$ymin = c(0, head(data$ymax, n=-1)) # Compute the bottom of each rectangle
# Make the Donut Plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Team)) +
  geom_rect( color = 'white') +
  geom_text( x=3.5, aes(y=(data$ymax + data$ymin) / 2, label=Count), size=5, color='#616161') + # label for counts
  coord_polar(theta="y") + # turns it into a pie chart
  xlim(c(2, 4)) + #turns into donut chart
  labs(title = "Frequency Distribution: Team")+
  theme_ggpbar()+
  theme(
    #grid elements
    panel.grid.major = element_blank(),    #strip major gridlines
    panel.grid.minor = element_blank(),    #strip minor gridlines
    axis.ticks = element_blank(),          #strip axis ticks
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_text(family = "Arial",colour="#616161",face = "bold",size=10),
    legend.text = element_text(family = "Arial",colour="#616161",size=8),
  )
#we can observe that the number of data collected from each team is different and random
#team 8 and team 2 has most data while team 11 and team 5 has the least data points in the database

#no_of_style_change
data <- data.frame(Count = summary(garwork$no_of_style_change), NoofChange = levels(garwork$no_of_style_change))
ggplot(data, aes(x = "", y = Count, fill = NoofChange)) +
  geom_bar(aes(x = "", y = Count, fill = NoofChange), width = 1, stat = "identity", color = "white") +
  coord_polar("y") + 
  geom_text(aes(x = "", y = Count,label = Count), position = position_stack(vjust = 0.6)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Frequency Distribution: Number of Style Changes") +
  theme_ggpbar() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),    #strip major gridlines
        panel.grid.minor = element_blank(),    #strip minor gridlines
        plot.title = element_text(family = "Arial",colour="#616161",face = "bold",size=13, hjust = 0.5),
        legend.text = element_text(family = "Arial",colour="#616161",size=8),
        legend.position = "top")
#we observe a huge difference in the data distribution among the different levels in the variables



##Numerical Variables------

NV <- subset(garwork, select = -c(quarter,department,day,team,no_of_style_change)) #subset of only numerical variables
pairs(NV ,main = "Scatterplot Matrix for Numerical Variables")
corrplot(cor(NV), method = 'number')
#We don't observe any obvious correlation among the variables or with our predictor variable from the scatter plot
#But the correlation plot captures the correlation between over_time, smv and no_of_workers & also idle_men and idle_time

skewness(NV)
windowsFonts(A = windowsFont("Arial"))  # Specify font

par(mfrow=c(2,2))
# targeted_productivity
varXX <- garwork$targeted_productivity
skewness(varXX) #highly negative skewed variable
hist(varXX, main = "Targeted Productivity", xlab = "targeted productivity(in %)", col = "ORANGE", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is highly negative skewed

# smv
varXX <- garwork$smv
skewness(varXX) #slightly positive skewed variable
hist(varXX, main = "Time Allocated", xlab = "time allocated(in minutes)", col = "#809012", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is slightly positive skewed

# over_time
varXX <- garwork$over_time
skewness(varXX) #slightly positive skewed variable
hist(varXX, main = "Over Time", xlab = "over time allocated(in minutes)", col = "#66c9de", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is slightly positive skewed

# incentive
varXX <- garwork$incentive
skewness(varXX) #highly positive skewed variable
hist(varXX, main = "Financial Incentive", xlab = "financial incentive(in BDT)", col = "#F48FB1", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is highly positive skewed

# idle_time
varXX <- garwork$idle_time
skewness(varXX) #highly positive skewed variable
hist(varXX, main = "Idle Production Time", xlab = "idle production time(in minutes)", col = "#1E88E5", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is highly positive skewed

# idle_men
varXX <- garwork$idle_men
skewness(varXX) #highly positive skewed variable
hist(varXX, main = "Idle Workers", xlab = "idle workers(number)", col = "#BA5FE7", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is highly positive skewed

# no_of_workers
varXX <- garwork$no_of_workers
skewness(varXX) #slightly negative skewed variable
hist(varXX, main = "Number of Workers", xlab = "number of workers(count)", col = "#2E7D32", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is slightly positive skewed

# actual_productivity
varXX <- garwork$actual_productivity
skewness(varXX) #slightly negative skewed variable
hist(varXX, main = "Actual Productivity", xlab = "actual productivity(in %)", col = "#FFD54F", family="A", cex.lab=1, cex.axis=0.9, cex.main=1.2, prob=TRUE, cex.sub = 0.7,sub = paste("Skewness = ",round(skewness(varXX),1), sep = ""))
x <- seq(min(varXX), max(varXX), length = length(varXX))
f <- dnorm(x, mean = mean(varXX), sd = sd(varXX))
lines(x, f, col = "red", lwd = 2)
#the histogram confirms our comment that the data is slightly positive skewed

#Hypothesis Questions:
#Q1: smv doesn't affect the actual productivity
#Q2: number of workers affect the actual productivity by 5%



##Define Base Model------

# below code: scatter plot for the full model
#scatterplotMatrix(~Y+smv+incentive+over_time+idle_time+idle_men+no_of_workers+quarter+department+day+team+no_of_style_change,garwork,smooth=FALSE)
#We will use R2adj as a criteria to select models between two models

mod <- lm(Y ~ smv+over_time+incentive+idle_time+idle_men+no_of_workers, garwork) #model with only numerical variables
summary(mod)
#Anova test to confirm we could remove variables which doesn't reduce the unexplained variability in the response
anova(update(mod, ~ . -over_time),mod) #p=0.1893; consider alpha=0.05
anova(update(mod, ~ . -idle_time),mod) #p=0.3286
#model update with removing above two variables
mod <- lm(Y ~ smv+incentive+idle_men+no_of_workers, garwork)
summary(mod) #numerical model

#check significance for each categorical variable
anova(update(mod, ~ . +quarter),mod) #p=1.69e-5 
mod <- lm(Y ~ smv+incentive+idle_men+no_of_workers+quarter, garwork)
anova(update(mod, ~ . +department),mod) #p=1.69e-5 
mod <- lm(Y ~ smv+incentive+idle_men+no_of_workers+quarter+department, garwork)
anova(update(mod, ~ . +day),mod) #p=0.5304
anova(update(mod, ~ . +team),mod) #p=2.2E-16 
mod <- lm(Y ~ smv+incentive+idle_men+no_of_workers+quarter+department+team, garwork)
anova(update(mod, ~ . +no_of_style_change),mod) #p=6.2E-9 
mod <- lm(Y ~ smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change, garwork)
summary(mod) #numerical+categorical model



##Interaction Terms------

#referencing the correlation plot, considering only two way interaction terms
mod.2 <- lm(Y ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2, garwork)
anova(mod,mod.2) #2.2e-16
# p-value is very less than 0.05, we can say that we need 2 way interactive terms
mod <- lm(Y ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2, garwork)
summary(mod)

#remove non-significant interaction terms one by one and remove them from the model
anova(update(mod, ~ . -smv:department),mod) #p=0.1935; consider alpha=0.05

#Final Model
mod <- lm(Y ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2 -smv:department -incentive:no_of_workers -incentive:idle_men -incentive:no_of_style_change -idle_men:no_of_workers -smv:incentive -smv:idle_men -smv:quarter -smv:team -smv:no_of_style_change -quarter:department -department:no_of_style_change -quarter:no_of_style_change -no_of_workers:no_of_style_change -idle_men:no_of_style_change -idle_men:team -idle_men:department -idle_men:quarter,garwork)
summary(mod)



##Check Assumptions------

#residual and leverage and scale location plots
par(mfrow=c(2,2))
plot(mod, pch=as.numeric(garwork$department), col=as.numeric(garwork$no_of_style_change))
legend(x = "bottom",title="Department",legend = levels(garwork$department), pch=as.numeric(garwork$department), cex= 0.7, box.lwd = 0.2, ncol = nlevels(garwork$department))
legend(x = "top",title="Style Change",legend = levels(garwork$no_of_style_change), pch=as.numeric(garwork$no_of_style_change), col=c(1:nlevels(garwork$no_of_style_change)), cex= 0.7, text.col = c(1:nlevels(garwork$quarter)), ncol = nlevels(garwork$quarter))

plot(mod, pch=as.numeric(garwork$team), col=as.numeric(garwork$quarter))
legend(x = "bottom",title="Team",legend = levels(garwork$team), pch=as.numeric(garwork$team), cex= 0.5, box.lwd = 0.2, ncol = nlevels(garwork$team))
legend(x = "top",title="Quarter",legend = levels(garwork$quarter), pch=as.numeric(garwork$quarter), col=c(1:nlevels(garwork$quarter)), cex= 0.5, text.col = c(1:nlevels(garwork$quarter)), ncol = nlevels(garwork$quarter))

shapiro.test(rstandard(mod))

car::residualPlots(mod, col=as.numeric(garwork$no_of_style_change), pch=as.numeric(garwork$department))
car::residualPlots(mod, col=as.numeric(garwork$team), pch=as.numeric(garwork$quarter))

#Normality:
par(mfrow=c(1,1))
plot(mod, which=2)
#The Q-Q plot shows that the points are highly biased on the negative axis - left skew
#and the Shapiro-Wilk test confirms with a very low p-value that there is a risk with the assumption of normality.
#Hence, the assumption of normal distribution is violated with the model.
#Therefore we will have to address this with proper transformations.

#Linearity
plot(mod, which=1)
residualPlots(mod)
#The residual-fitted plot should show a straight line or equal distribution of points on top and bottom sides of the line to prove linearity.
#The plot from the given model shows a bias and hence rejects the assumption of linearity.
#Looking further, we do observe a quadratic trend in the residual plots for idle_men
#Hence, we need these to be fixed with necessary transformations.

#Homoscedasticity:
ncvTest(mod)
#When the data follows homoscedasticity, we observe equal distribution of points in the residual-fitted and residual-predictor plots.
#We can also use the Breusch-Pagan test, where the p-value > 0.05 gives us enough proof to agree that the data follows the assumption of homoscedasticity.
#The residual-fitted values plot show a quadratic trend.
#The Scale-location plot also can confirm with a strong trend that the assumption of homoscedasticity is invalid.
#From the above residual-fitted and residual plots for the predictors using our data, we observe an uneven spread of the residuals. Also, the p-value from the Breusch-Pagan test is small.
#Thus confirming heteroscedasticity.
#We should make appropriate transformations to correct them.

#Observing the residual vs fitted values curve, we can state that there is no strong linear relationship in our final model.
#From the individual residual plots, we come to the same conclusion that there is no variable with strong positive or negative linear relationship.
#We do have few outliers but they don't seem to have much leverage in the new model.

#Influential Points:
plot(mod, which=4)
influencePlot(mod)
#Studying the above residual-Leverage plot and by referring the rule of thumb for Cook's distance,
#we can state that we definitely have numerous high influential points. 
#Among them, points 1131,113,1150,692,730,784,930 seems to be the most influential
ols_plot_cooksd_chart(mod)
cooksd <- cooks.distance(mod)
influential <- which(cooksd > (4/nrow(garwork)))
garwork <- garwork[-influential,]
Y <- garwork$actual_productivity
mod <- lm(Y ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2 -smv:department -incentive:no_of_workers -incentive:idle_men -incentive:no_of_style_change -idle_men:no_of_workers -smv:incentive -smv:idle_men -smv:quarter -smv:team -smv:no_of_style_change -quarter:department -department:no_of_style_change -quarter:no_of_style_change -no_of_workers:no_of_style_change -idle_men:no_of_style_change -idle_men:team -idle_men:department -idle_men:quarter,garwork)
summary(mod)
plot(mod, which=5)



##Transformations-----

#For Linearity:
mod1 <- lm(Y~idle_men,garwork)
invResPlot(mod1)
#looking at the invResPlot, we can say that the appropriate lambda value is 0 for this transformation as idle_men as values 0.
#Thus I will be introducing a log term of idle_men instead of idle_men
modL <- update(mod, ~ . -idle_men +log(idle_men+1))
plot(modL, which=1)
residualPlots(modL)
summary(modL)
#Checking the residual vs idle_men plot after the transformation, we can say that the linearity assumption is valid for the variable.
#The transformation on idle_men didn't result in improvement in the R^2adj levels and hence no transformation was included. 

#we also observe a slight quadratic trend in the residual vs smv plots
mod1 <- lm(Y~smv,garwork)
invResPlot(mod1)
#looking at the invResPlot, we can say that the appropriate lambda value is 0 for this transformation as idle_men as values 0.
#Thus I will be introducing a log term of idle_men instead of idle_men
modL <- update(mod, ~ . -smv +(smv)^6)
plot(modL, which=1)
residualPlots(modL)
summary(modL)
#Checking the residual vs smv plot after the transformation, we can say that the linearity assumption is valid for the variable.
#The transformation on smv didn't result in improvement in the R^2adj levels and hence no transformation was included. 

#For Normality, Homoscedasticity and Linearity:
mod <- lm((Y)^2 ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2 -smv:department -incentive:no_of_workers -incentive:idle_men -incentive:no_of_style_change -idle_men:no_of_workers -smv:incentive -smv:idle_men -smv:quarter -smv:team -smv:no_of_style_change -quarter:department -department:no_of_style_change -quarter:no_of_style_change -no_of_workers:no_of_style_change -idle_men:no_of_style_change -idle_men:team -idle_men:department -idle_men:quarter,garwork)
plot(mod, which=1)
residualPlots(mod)
summary(mod)
#The residual-fitted plot shows a straight line or equal distribution of points - hence proving linearity.

par(mfrow=c(2,2))
plot(mod, pch=as.numeric(garwork$department), col=as.numeric(garwork$no_of_style_change))
legend(x = "bottom",title="Department",legend = levels(garwork$department), pch=as.numeric(garwork$department), cex= 0.7, box.lwd = 0.2, ncol = nlevels(garwork$department))
legend(x = "top",title="Style Change",legend = levels(garwork$no_of_style_change), pch=as.numeric(garwork$no_of_style_change), col=c(1:nlevels(garwork$no_of_style_change)), cex= 0.7, text.col = c(1:nlevels(garwork$quarter)), ncol = nlevels(garwork$quarter))

shapiro.test(rstandard(mod))
#assumption of normality is still not valid

ncvTest(mod)
#The residual-fitted values plot show no trend.
#The Scale-location plot shows a strong trend that the assumption of homoscedasticity is invalid.
#Using the Breusch-Pagan test, where the p-value > 0.05
#Thus confirming homoscedasticity.

##Final Model -------------------------------------------------------------

mod <- lm((Y)^2 ~ (smv+incentive+idle_men+no_of_workers+quarter+department+team+no_of_style_change)^2 -smv:department -incentive:no_of_workers -incentive:idle_men -incentive:no_of_style_change -idle_men:no_of_workers -smv:incentive -smv:idle_men -smv:quarter -smv:team -smv:no_of_style_change -quarter:department -department:no_of_style_change -quarter:no_of_style_change -no_of_workers:no_of_style_change -idle_men:no_of_style_change -idle_men:team -idle_men:department -idle_men:quarter,garwork)
summary(mod)
print(sort(mod$coefficients,decreasing = TRUE))
plot(mod)

par(mfrow=c(1,1))
plot(mod, which=2)
plot(mod, which=1)
residualPlots(mod)

ncvTest(mod)

# Hypothesis Test 1
# B_smv=0,i.e. smv doesn't affect the actual productivity
mod2 <- update(mod, ~. -smv)
anova(mod2,mod) #p-value 0.1774
#With p-value = 0.1774; we don't have enough proof to reject H0.
#Thus it is very likely that the standard time allocated for a task(in minutes) might not have impact on the actual productivity among the teams.

# Hypothesis Test 2
# B_no_of_workers = 0.05; i.e. number of workers affect the actual productivity by 5%
mod2 <- update(mod, ~. -no_of_workers + offset(I(0.05*no_of_workers)))
anova(mod2,mod) #p-value 2.2e-16
#With p-value = 2.2e-16; we have enough proof to reject H0.
#Thus it is very unlikely to increase productivity by 5% by increasing the number of workers.
