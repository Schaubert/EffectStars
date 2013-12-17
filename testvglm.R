library(VGAM)
library(EffectStars)

data(election)

options(contrasts=c('contr.treatment','contr.poly'))

formula <- Partychoice ~ Age + Religion + Democracy + Pol.Interest + 
  Unemployment + Highschool + Union + West + Gender

data = election

m1 <- vglm(formula, data = data,
           family=multinomial())



m3 <- update(m1, contrast=list(Religion="contr.sum"))

ef1 <- star.nominal(formula,data,symmetric=FALSE,refLevel=5,pred.coding="effect")

ef1$coef
t(coef(m3,T))

