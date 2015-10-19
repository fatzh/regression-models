##
## Regression models - Coursera - Course Project
##


## ---- loadPackages --------------
library('ggplot2')
library('dplyr')
library('tidyr')
#library('ggfortify')

## ---- loadData
data(mtcars)

## ---- exploAnalysis1
dim(mtcars)
g <- ggplot(data=mtcars, aes(x=mpg, fill=as.factor(am))) + 
    geom_histogram(binwidth=.5) +
    ggtitle("MPG by type of transmission") + 
    ylab("Number of vehicules") + 
    scale_fill_discrete(name="Transmission", labels = c("automatic", "manual"))

## ---- exploAnalysis
dim(mtcars)
head(mtcars, 5)
# make am a factor, and set better level names
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")
# convert variables to factor
mtcars$vs <- as.factor(mtcars$vs)
glimpse(mtcars)
summary(mtcars[,'am'])

## ---- exploplot

g <- ggplot(data=mtcars, aes(x=am, y=mpg)) + 
    geom_boxplot() + 
    xlab("Transmission") + 
    ylab("MPG") +
    ggtitle("MPG by transmission")

## ---- inference
var(mtcars[mtcars$am == 'Automatic',]$mpg)
var(mtcars[mtcars$am == 'Manual',]$mpg)

## ---- MTlm
mdl.mar <- lm(mpg ~ am, data=mtcars)
summary(mdl.mar)$coef

## ----stepSearch
mdl.opt = step(lm(data = mtcars, mpg ~ .),direction = "both")

## ---- plotDiagModelMarginal
#autoplot(mdl.mar, label.size = 3)

## ---- plotDiagModelOptimal
par(mfrow = c(1,1))
plot(mdl.opt, which=1)
plot(mdl.opt, which=2)
plot(mdl.opt, which=3)
plot(mdl.opt, which=5)

## ---- 95% mpg increase manual
sumc <- summary(mdl.opt)$coefficients
sumc[4,1] + c(-1, 1)*qt(.975, mdl.opt$df ) * sumc[4,2]