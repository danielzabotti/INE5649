
library(XLConnect, pos=4)
setwd("/Users/daniel/Google Drive/UFSC-SIN/INE5649 - Técnicas Estatisticas de Predição")
.Workbook <- 
  loadWorkbook("/Users/daniel/Google Drive/UFSC-SIN/INE5649 - Técnicas Estatisticas de Predição/Aula 24-04-2014/Apartamentos Criciuma completo.xls")
ApartamentosCriciuma <- readWorksheet(.Workbook, "Parte_dos_dados")
remove(.Workbook)
library(relimp, pos=4)
showData(ApartamentosCriciuma, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
scatterplot(valorDolar~ArePriv, reg.line=lm, smooth=TRUE, spread=TRUE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, 
  data=ApartamentosCriciuma)
scatterplot(valorDolar~ArePriv, reg.line=lm, smooth=FALSE, spread=TRUE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, 
  data=ApartamentosCriciuma)
scatterplot(valorDolar~ArePriv, reg.line=lm, smooth=TRUE, spread=TRUE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, 
  data=ApartamentosCriciuma)
scatterplot(valorDolar~ArePriv, reg.line=FALSE, smooth=FALSE, spread=FALSE, id.method='mahal', id.n = 2, boxplots='xy', 
  span=0.5, data=ApartamentosCriciuma)
RegLinearValorDolarAreaPriv <- lm(valorDolar~ArePriv, data=ApartamentosCriciuma)
summary(RegLinearValorDolarAreaPriv)
scatterplot(valorDolar~ArePriv, reg.line=lm, smooth=FALSE, spread=FALSE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, 
  data=ApartamentosCriciuma)
ApartamentosCriciuma$fitted.RegLinearValorDolarAreaPriv <- fitted(RegLinearValorDolarAreaPriv)
ApartamentosCriciuma$residuals.RegLinearValorDolarAreaPriv <- residuals(RegLinearValorDolarAreaPriv)
scatterplot(residuals.RegLinearValorDolarAreaPriv~fitted.RegLinearValorDolarAreaPriv, reg.line=lm, smooth=FALSE, spread=FALSE, 
  id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=ApartamentosCriciuma)
ApartamentosCriciuma$ArePrivLog <- with(ApartamentosCriciuma, log(ArePriv))
ApartamentosCriciuma$valorDolarLog <- with(ApartamentosCriciuma, log(valorDolar))
scatterplot(valorDolarLog~ArePrivLog, reg.line=lm, smooth=FALSE, spread=FALSE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, 
  data=ApartamentosCriciuma)
RegLinearLogDolarAreaPriv <- lm(valorDolarLog~ArePrivLog, data=ApartamentosCriciuma)
summary(RegLinearLogDolarAreaPriv)
ApartamentosCriciuma$fitted.RegLinearLogDolarAreaPriv <- fitted(RegLinearLogDolarAreaPriv)
ApartamentosCriciuma$residuals.RegLinearLogDolarAreaPriv <- residuals(RegLinearLogDolarAreaPriv)
scatterplot(residuals.RegLinearLogDolarAreaPriv~fitted.RegLinearLogDolarAreaPriv, reg.line=lm, smooth=FALSE, spread=FALSE, 
  id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=ApartamentosCriciuma)

