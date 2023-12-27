IUD <- c(10,13,18,19,23,30,36,38,54,56,59,75,
         93,97,104,107,107,107)
status <- c(1,0,0,1,0,1,1,0,0,0,1,1,1,1,0,1,0,0)


library(survival)
Sduga <- survfit(Surv(IUD,status)~1,conf.type="plain")
summary(Sduga)
plot(Sduga,conf.int = T, main = "Dugaan Fungsi Survival dengan Kaplan-Maier", 
     xlab = "Waktu (Minggu)",
     ylab = "Dugaan Fungsi Survival")

plot(Sduga, main=expression(paste("Estimasi Kaplan-Meier", hat(H)(t))),
     xlab = "Waktu survival (T)", 
     ylab = "Comulative hazard", fun="cumhaz")
summary(Sduga, fun="cumhaz")

hduga <- -log(Sduga$surv)
hduga

#Membandingkan fungsi survival
grup <- rep(c(1,0),c(21,21))      #Grup 1 : MP6  dan grup 0 : placebo
waktu <- c(10,7,32,23,22,6,16,34,32,25,
           11,20,19,6,17,35,6,13,9,6,
           10,1,22,3,12,8,17,2,11,8,
           12,2,5,4,15,8,23,5,11,4,1,8)
status <- c(c(1,1,0,1,1,1,1,0,0,0,0,0,0,
              1,0,0,1,1,0,0,0),rep(1,21))      #all placebos are dead

Sduga2 <- survfit(Surv(waktu,status)~grup,conf.type="plain")
summary(Sduga2)

plot(Sduga2,conf.int = F, main = "Dugaan Fungsi Survival dengan Kaplan-Maier 2 Grup", 
     xlab = "Waktu (Minggu)",
     ylab = "Dugaan Fungsi Survival 2 Grup")

library(survminer)
data1<-data.frame(waktu,status,grup)
ggsurvplot(Sduga2,data = data1, 
           pval = T,pval.method = T, # Menampilkan p-value uji log rank
           conf.int = TRUE, # Menampilkan interval konfidensi
           risk.table = TRUE) # Menampilkan tabel risiko

#Uji log rank
ujilogrank <-  survdiff(Surv(waktu,status)~grup)
ujilogrank

#Model Regresi Cox
library(KMsurv)
data()
data(larynx)
larynx
?larynx
attach(larynx)
Z1 <- 1*(larynx$stage==2)
Z2 <- 1*(larynx$stage==3)
Z3 <- 1*(larynx$stage==4)
coxfit <- coxph(Surv(time,delta)~Z1+Z2+Z3+age)
summary(coxfit)

#Asumsi PH
PHtest <- cox.zph(coxfit)
PHtest





