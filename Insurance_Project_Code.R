# Insurance Project

insu <- read.csv('F:/Courses/Simpli Learn/Data Analytics/Data Science with R/Live Classes/Projects/insurance_factor_identification/insurance_factor_identification.csv')

View(insu)

dim(insu)

str(insu)

insu$Kilometres <- as.factor(insu$Kilometres)

insu$Zone<-as.factor(insu$Zone)

insu$Make<-as.factor(insu$Make)

str(insu)

insu$Zone <- as.factor(insu$Zone)


#1st answer

summary(insu)

#2nd answer

# correlation between claims & payment

cor(insu$Payment,insu$Claims)

plot(insu$Payment,insu$Claims)

ggplot(data=insu,mapping=aes(x=Claims,y=Payment))+geom_point()

# correlation between insured & payment

cor(insu$Payment,insu$Insured)

plot(insu$Payment,insu$Insured)

ggplot(data=insu,mapping=aes(x=Insured,y=Payment))+geom_point()


# 3rd answer

sel <- sample.split(insu$Zone,SplitRatio = 0.7)

ins_train <- subset(insu,sel==TRUE)

ins_test <- subset(insu,!sel)

payment_model <- lm(Payment~.,data=ins_train)

summary(payment_model)

payment_model <- lm(Payment ~ Kilometres+Insured+Claims,data=ins_train)

summary(payment_model)

# answer 4

insu %>% group_by(Zone) %>% summarise(avg_ins=mean(Insured),avg_claims=mean(Claims),avg_payment=mean(Payment))

insu %>% group_by(Kilometres) %>% summarise(avg_ins=mean(Insured),avg_claims=mean(Claims),avg_payment=mean(Payment))

insu %>% group_by(Bonus) %>% summarise(avg_ins=mean(Insured),avg_claims=mean(Claims),avg_payment=mean(Payment))

#answer 5

claim_model <- lm(Claims~.,data=insu)

summary(claim_model)

claim_model <- lm(Claims ~ Kilometres+Bonus+Insured,data=insu)

summary(claim_model)

claim_model <- lm(Claims ~ Bonus+Insured,data=insu)

summary(claim_model)


