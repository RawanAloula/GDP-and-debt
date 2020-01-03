GDP and dept 

library(plyr)

# load data 
debt <-read.csv("debt_Data.csv")

dim(debt)
head(debt)


# average GDP growth (per country)
GDP_Avg <- daply(debt, .(Country), mean.growth)
signif(GDP_Avg,3)


# average GDP growth (per year)
GDP_Avg_year <- daply(debt, .(Year), mean.growth)
signif(GDP_Avg_year[c('1972', '1989')],3)

# plot the GDP/year 
x <- data.frame(GDP_Avg_year)
plot(rownames(x), x$GDP_Avg_year, xlab = "Year", ylab = "GDP_Avg per year", type="l")


# correlation between GDP & dept 
signif(cor(debt$growth,debt$ratio),3)

# correlation per country 
corr <- function(debt) {
  return(cor(debt$growth,debt$ratio))
}

corr.country <- daply(debt, .(Country), corr)
signif(mean(corr.country),3)


# explore visually 
hist(corr.country, xlab = "correlation coefficient", breaks = 10)


#