#install.packages("readxl")
library("readxl")
library(tibble)
yuxi_data = read_excel('/Users/yuxichen/biostat620/project1/data_yuxi.xlsx')
meng_data = read_excel('/Users/yuxichen/biostat620/project1/data_meng.xlsx')
mary_data = read_excel('/Users/yuxichen/biostat620/project1/data_mary.xlsx')
whole_data = read_excel('/Users/yuxichen/biostat620/project1/data_whole.xlsx')

#create interaction term for individual data
yuxi_data$SocialSTmin_lag1_procrast = yuxi_data$SocialSTmin_lag1 * yuxi_data$procrastination
meng_data$SocialSTmin_lag1_procrast = meng_data$SocialSTmin_lag1 * meng_data$procrastination
mary_data$SocialSTmin_lag1_procrast = mary_data$SocialSTmin_lag1 * mary_data$procrastination

#create 1 as coefficient for beta zero
yuxi_data = add_column(yuxi_data, d = 1, .before = "Pickups")
meng_data = add_column(meng_data, d = 1, .before = "Pickups")
mary_data = add_column(mary_data, d = 1, .before = "Pickups")

#yuxi_data = subset(yuxi_data, select = -c(gender) )
#meng_data = subset(meng_data, select = -c(gender) )
#mary_data = subset(mary_data, select = -c(gender) )

#let us convert dataframe into matrix
x1 = data.matrix(yuxi_data[2:7])
y1 = data.matrix(yuxi_data[1])
x2 = data.matrix(meng_data[2:7])
y2 = data.matrix(meng_data[1])
x3 = data.matrix(mary_data[2:7])
y3 = data.matrix(mary_data[1])


##analysis data a little bit, conclude: all data for social screen time seem to be temporal independent.
#yuxi: almost temporal independent
acf(yuxi_data$SocialSTmin)
#meng: temporal independent
acf(meng_data$SocialSTmin)
#mary: alsotemporal independent
acf(mary_data$SocialSTmin)

#now, let us begin federated learning.
##############################first, coefficient for beta_zero: basically add one to the X matrix
#x1^2 + x2^2 + x3^2
square1 = t(x1) %*% x1
square2 = t(x2) %*% x2
square3 = t(x3) %*% x3
sum_square = square1 + square2 + square3

#x1^y1 + x2^y2 + x3^y3
xy1 = t(x1) %*% y1
xy2 = t(x2) %*% y2
xy3 = t(x3) %*% y3
sum_xy = xy1 + xy2 + xy3

############################################# beta got from federated learning is really close to beta got from model, perfect
beta = solve(sum_square) %*% sum_xy

model = lm(whole_data$SocialSTmin ~ Pickups + procrastination   
                            +factor(gender) + SocialSTmin_lag1 + SocialSTmin_lag1*procrastination,
           data = whole_data)

############### standard deviation
#square_y
square1y = t(y1) %*% y1
square2y = t(y2) %*% y2
square3y = t(y3) %*% y3
sum_squarey = square1y + square2y + square3y

#error square
error_square = sum_squarey - 2 * t(beta) %*% sum_xy +  t(beta) %*% sum_square %*% beta
#sigma square
sigma = error_square/(81 - 6)

##############################################standard error : almost all the same value with model summary, perfect
##############################################hypothesis testing is also almost the same with model summary
model_summary = summary(model)
#for beta zero
se0 = sqrt(sigma / sum_square['d','d'])
##reject 
t0 = beta[1,]/se0
t = qt(0.025, 81-6, lower.tail = FALSE) 

#for beta1
se1 = sqrt(sigma * solve(sum_square)['Pickups','Pickups'])
##do not reject 
t1 = beta[2,]/se1

#for beta2
se2 = sqrt(sigma * solve(sum_square)['procrastination','procrastination'])
##reject 
t2 = beta[3,]/se2

#for beta3
se3 = sqrt(sigma * solve(sum_square)['gender','gender'])
##not reject
t3 = beta[4,]/se3

#for beta4
se4 = sqrt(sigma * solve(sum_square)['SocialSTmin_lag1','SocialSTmin_lag1'])
##not reject
t4 = beta[5,]/se4

#for beta6
se5 = sqrt(sigma * solve(sum_square)['SocialSTmin_lag1_procrast','SocialSTmin_lag1_procrast'])
##not reject
t5 = beta[6,]/se5

#############################################Calculating AIC
##################### very close to model summary. 
rss = error_square
aic = log(rss) * 81 + 2 * 6

library('stats')
model_aic = AIC(model)

#############################################Calculating adjusted r square, almost the same as the original
average_y = (sum(y1) + sum(y2) + sum(y3)) / 81
tss = sum_squarey - 81 * average_y^2
r_square = 1 - (rss/(81-6))/(tss/(81-1))

#########plot model diagnostic
plot(model)

library(equatiomatic)
extract_eq(model)


par(mfrow = c(2, 2))
plot(model)
