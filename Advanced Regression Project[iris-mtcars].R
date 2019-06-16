library(ggplot2)
Planets <- c('Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto')
Distance <- c(0.39, 0.72 ,1.00, 1.52, 5.20, 9.54, 19.19, 30.06, 39.53)
Period <- c(0.24, 0.61, 1.00, 1.88, 11.86, 29.46, 84.01, 164.79, 248.54) 

kepler = data.frame(Planets, Distance, Period)
kepler
a= lm( Period~Distance)
summary(a)

ggplot(data = kepler, aes(x = Distance, y = Period)) + 
  geom_point(color='green') +
  geom_smooth(method = "lm", se = FALSE)

b= lm(log(Period, base=10)~ log(Distance, base=10))
summary(b)
D = (log(Period, base=10))
P = (log(Distance, base=10))
data.frame(Planets, log(Distance, base=10),log(Period, base=10))
#기울기가 1.5임을 알 수 있따. 
#T=R ^(3/2) 이기 때문에 loga =0, a=1이 되고 n = 1.5=3/2가 되는 것을 알 수 있다. 
#따라서 케플러의 법칙은 성립한다. 


ggplot(data = kepler, aes(x = D, y = P)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

#http://saegil-lab.kr/?p=407


#####Number4
barplot(table(mtcars$gear),main = "Gear Distribution", xlab = "#Gears")

Counts <- table(mtcars$mpg, mtcars$gear)
barplot(Counts, main = "Car distribution by Gear, mpg", xlab = "#Gears", col  = c("Red", "Yellow", "Blue"), legend = rownames(Counts))

boxplot(mpg~gear,data=mtcars, main="MPG average and variance by Gear", 
        xlab="Number of Gear", ylab="Miles Per Gallon")

summary(mtcars)
allmt = lm(data = mtcars, mpg ~ .)
summary(allmt)

nmd1 = lm(data = mtcars, mpg ~ wt + gear + am)
summary(nmd1)

nnn = lm(data = mtcars, mpg ~wt+gear)
summary(nnn)
nnn1 = lm(data = mtcars, mpg ~ -1+ I(wt^2) + gear)
summary(nnn1)
AIC(nmd1, nnn)

onmd = lm(data = mtcars, mpg ~ -1+I(wt^2) + ordered(gear))
summary(onmd)

onmd1 = lm(data = mtcars, mpg ~ -1+ordered(wt^2) + gear)
summary(onmd1)
AIC(nnn1,onmd, onmd1)
BIC(nnn1,onmd, onmd1)



###############################################
res1 <- lm(mpg ~ wt + factor(cyl), mtcars)
#anova(res1)
res2 <- lm(mpg ~ wt + cyl, mtcars)
#anova(res2)

res3 <- lm(mpg ~ -1 + wt + cyl, mtcars)
#anova(res3)

res4 <- lm(mpg ~ -1 + wt + I(cyl-6), mtcars)
#anova(res4)

AIC(res1, res2, res3, res4)
BIC(res1, res2, res3, res4)


ff <- factor(mtcars$cyl)
p = ggplot() + 
  #geom_line(data = mtcars, aes(x = -1+ wt + ff, y = mpg), color = "blue") +
  geom_line(data = mtcars, aes(x = wt + cyl, y = mpg), color = "red")  +
  geom_line(data = mtcars, aes(x = -1 + wt + cyl, y = mpg), color = "green")  +
  geom_line(data = mtcars, aes(x =  -1 + wt + I(cyl-6), y = mpg), color = "yellow") +
  xlab('x') +
  ylab('mpg')

names(iris)<-c('sl','sw','pl','pw','sp')
ress <- lm(sl~ .,iris)
summary(ress)


ress1 <- lm(sl~ pl, iris)
ress2 <- lm(sl~-1+pl, iris)
summary(ress1)
summary(ress2)
AIC(ress1, ress2)
BIC(ress1, ress2)

rest1= lm(sl~pl,iris)
rest2= lm(sl~pw*pl,iris)
rest3= lm(sl~pw+pl,iris)
rest4= lm(sl~pw:pl,iris)
rest5= lm(sl~I(pl^2)*pw,iris)
rest6= lm(sl~I(pw^2)+pl,iris)
rest7= lm(sl~pl*I(pw^2),iris)
rest8= lm(sl~pw+I(pl^2),iris)
AIC(rest1, rest2, rest3, rest4, rest5, rest6, rest7, rest8)
BIC(rest1, rest2, rest3, rest4, rest5, rest6, rest7, rest8)









res11 = lm(sl~sw+sp,iris)
res12 = lm(sl~sw*sp,iris)
AIC(res11, res12)
library(ggplot2)
ggplot(iris)+geom_point(aes(x=sw, y=sl))+stat_smooth(method='lm', aes(x=sw, y=sl, col=sp))
p<-ggplot(data=iris,aes(x=sw,y=sl,colour=sp))+geom_point()+geom_smooth(method="lm")







summary(mtcars)
allmt = lm(data = mtcars, mpg ~ .)
summary(allmt)

nmd1 = lm(data = mtcars, mpg ~ wt + qsec + am)
nmd2 = lm(data = mtcars, mpg ~ wt + qsec)
nmd3 = lm(data = mtcars, mpg ~ wt )
nmd4 = lm(data = mtcars, mpg ~ -1 + wt + qsec +am)
nmd5 = lm(data = mtcars, mpg ~ -1 + wt + qsec)
nmd6 = lm(data = mtcars, mpg ~ -1 + wt )
nmd7 = lm(data = mtcars, mpg ~ wt*qsec)
nmd8 = lm(data = mtcars, mpg ~ wt*qsec*am)
nmd9 = lm(data = mtcars, mpg ~ -1+ wt*qsec*am)
nmd10 = lm(data = mtcars, mpg ~ -1+ wt*qsec)
nmd11 = lm(data = mtcars, mpg ~ -1+ I(wt^2) + qsec + am)
nmd12 = lm(data = mtcars, mpg ~ -1+ I(wt^2) + qsec)
nmd13 = lm(data = mtcars, mpg ~ -1+ I(wt^2))
nmd14 = lm(data = mtcars, mpg ~ I(wt^2))
nmd15 = lm(data= mtcars, mpg ~ -1+ wt:qsec +am)
nmd16 = lm(data= mtcars, mpg ~ -1+ wt:qsec)

AIC(nmd1, nmd2,nmd3,nmd4,nmd5,nmd6,nmd7,nmd8,nmd9,nmd10,nmd11,nmd12,nmd13,nmd14, nmd15, nmd16)

BIC(nmd1, nmd2,nmd3,nmd4,nmd5,nmd6,nmd7,nmd8,nmd9,nmd10,nmd11,nmd12,nmd13,nmd14, nmd15, nmd16)

