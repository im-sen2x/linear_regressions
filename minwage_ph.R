library(SDSFoundations)

wage = read.csv("wage_philippines.csv")
cor(wage)

wagetrain = subset(wage, year < 2015)
wagetest = subset(wage, year > 2014)

plot(wage$gdp_growth, wage$mw_increase)
plot(wage$inflation, wage$mw_increase)
plot(wage$cpi, wage$mw_increase)
plot(wage$lfpr, wage$mw_increase)
plot(wage$unemployment, wage$mw_increase)
plot(wage$cfdi_inflow, wage$mw_increase)

linFit(wage$gdp_growth, wage$mw_increase)
linFit(wage$inflation, wage$mw_increase)
linFit(wage$cpi, wage$mw_increase)
linFit(wage$lfpr, wage$mw_increase)
linFit(wage$unemployment, wage$mw_increase)
linFit(wage$cfdi_inflow, wage$mw_increase)


model1 = lm(mw_increase ~ inflation + unemployment, data=wagetrain)
summary(model1)

predict(model1)
wagetrain$mw_increase

pred1 = predict(model1, newdata=wagetest)
pred1
wagetest$mw_increase

SSE = sum((wagetest$mw_increase - pred1)^2)
SST = sum((wagetest$mw_increase - mean(wagetrain$mw_increase))^2)

1 - SSE/SST

#This program was made for the purpose of having insights of what factors consitute
#the % increase in the minimun wage in the Philippines. I used data from 1999-2017 of 
#7 variables that might have a relationship with the %increase in the minimum wage in the Philippines.
#The 7 variables are as follows: annual GDP growth, inflation rate, CPI, Labor Force Participation Rate, Unemployment Rate, and %increase in FDI's
#Out of the 7 variables as expected inflation gives the strongest correlation to towards the increase in min. wage
#The next variable that had a reasonable correlation with the min. wage is the unemployment rate
#The unemployment rate gives a modest positive correlation.
#It is shown at high levels of unemployment(right side of the u.r. graph) min wage hike are the highest
#one possible explanation for this is if the demand for employment is low and having the supply of workers
#constant, the wage of the employed workers are low thus the government thinks that it is right to increase the 
#workers wages near the equilibrium price, even if it knows there might be an increase in unemployment
#since the wage is so low the increase might be substantial. On the other side of the graph(left side of the u.r. graph)
#at low unemployment levels, there are a lot of people working and their wages are higher than that of the previous scenario,
#and closer to the equilibrium price. So the government opts for a small increase in wages enough to offset inflation
#and not too high to prevent firms from not hiring people.
#I opted to choose only 2 variables(inflation and unemployment) in my regression model even there are other varibales
#that will increase my R^2 values, since some of them are correlated. The training set R^2 is 0.4334 and the test set R^2 is
#0.0416. It mean that my model is not good in predicting the %increase. I assume that there are other economics variables
#that are better suited in predicting the %increase in the min.wage.
#The most interesting finding that I've found is that gdp growth is inversely related to the increase in the min.wage, this idea is counter intuitive since we assume when the economy grows
#the will be more benefits for society. It's possible to say that even the economy grows and the total pie increases those
#who are at the bottom of the don't reap the benefits of this economic growth, thus we have a distribution dillema. 
#This this also strengthens the claim of sticky wages since even if the economy grows at a higher pace the increase in wages tend to be smaller.
