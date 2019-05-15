#Instala e carrega pacotes

pacotes<-c("tidyverse","ResourceSelection","car","rcompanion")

lapply(pacotes,install.packages)

lapply(pacotes,library, character.only=TRUE)
  
  
#Carrega os dados

load("dadosreg1.Rda")


#Regressão (Hipertensão =1) 

#Efeitos Principais
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

modt<-glm(hipert~c006+c009+idadecat+vdd004+IMC+RCE+indicec+w00303,family=binomial(link="logit"),data=dadosreg)
summary(modt)

#Todos os efeitos principais são significativos

#Interações

mod2<-glm(hipert~c006+idadecat+c009+vdd004+IMC+w00303+c006*c009+c006*idadecat,family=binomial(link="logit"),data=dadosreg)
summary(mod2)

#Tira a intração entre raça e idade e deixa interação entre sexo e idade

mod3<-glm(hipert~c006+RCE+indicec+c009+vdd004+w00303+idadecat+idadecat*c006,family=binomial(link="logit"),data=dadosreg)
summary(mod3)


#Modelo nulo

modnull<-glm(hipert~1,family=binomial(link="logit"),data=dadosreg)
summary(modnull)

#Escolha do Modelo

anova(modnull,modt,test="Chisq") #Comparação com o modelo nulo

#Fica-se com o modelo realizado

drop(modt)
anova(modt,test="Chisq")

#Testa a bondade do ajuste

hl <- hoslem.test(modt$y, fitted(modt))
hl


#Linearidade

crPlots(mod1)
ceresPlots(mod1)

#Gráfico dos resíduos

res<-residuals(modt,type="deviance")
plot(fitted(modt),res,xlab="Valores Ajustados",ylab="Resíduos", ylim=max(abs(res))*c(-1,1))
abline(h=0,lty=2)
abline(h=2,col="green")
abline(h=-2,col="green")

#Fazer filtro dos resíduos do desvio padrão

x<-which(res>2)
y<-which(res<=-2)

#5% da amostra é igual 2544.8
#1294+84 é menor. Logo os resíduos neste critério estão adequados
#Pearson é 2496 e deviance é 1507
#Testa Multicolinearidade


vif(modt)#Não há multicolineridade 
vif(mod3) #Há multicolineridade nas variáveis c006 e na interação.

#Para auxiliar a interpretação
#Modelo com interação

confint.default(mod3,level=0.95)
round(cbind(OR=exp(coef(mod3))),exp(confint.default(mod3,level=0.95)))

#Odds Ratio

exp(coef(mod3))

#Modelo sem interação

confint.default(modt,level=0.95)
round(cbind(OR=exp(coef(modt))),exp(confint.default(modt,level=0.95)))

#Odds Ratio

exp(coef(modt))


#Pseudo R2
nagelkerke(modt)
           

#Teste de outlier

outlierTest(modt)

#Gráfico

plot(predict(modt),residuals(modt),xlab="Valores Preditos",ylab="Resíduos")
abline(h=0,lty=2,col="grey")
lines(lowess(predict(modt),residuals(modt)),col="red",lwd=2)

table(dadosreg$hipert)
#0.2300967 probabilidade de ter hipertensão (prevalência)
#0.7699033 probabilidade de não se ter hipertensão (prevalência)

hipert.pred<-1
hipert.pred[fitted(modt)> 0.5]<-1
hipert.pred[fitted(modt)<= 0.5]<-0

classDF <- data.frame(response = dadosreg$hipert,predicted=hipert.pred)

xtabs(~ response + predicted, data = classDF)

#Casos preditos corretamente 10396 0.2042597
#casos preditos incorretamente 40500 0.7957403
#com os valores diretos
#utilizando a probabilidade do evento corretamente 11505 0.2243005 correto
#incorreto 39391 0.7739508
#0.9153159 sensibilidade

attach(dadosreg)

prop.table(table(hipert,c006))
prop.table(table(hipert,c009))
prop.table(table(hipert,vdd004))
prop.table(table(hipert,w00303))
prop.table(table(hipert,indicec))
prop.table(table(hipert,IMC))
prop.table(table(hipert,RCE))

detach(dadosreg)




