age.dist.cases <- data.frame(ages=c(9,19,29,39,49,59,69,79,120), cases=c(416, 549, 3619, 7600, 8571, 10008, 8583, 3918, 1408))
age.dist.china <- read.csv("../../data/China-2019.csv")
age.dist.china$total <- age.dist.china$M+age.dist.china$F

temp.ages <- vector(mode="numeric",length=length(age.dist.cases$ages))
for (i in seq(length(temp.ages))) {
    if (i <=8) {
        temp.ages[i] <- sum(age.dist.china$total[(2*i-1):(2*i)])
    } else {
        temp.ages[i] <- sum(age.dist.china$total[(2*i-1):length(age.dist.china$total)])
    }
}

age.dist.cases$pop <- temp.ages

##define this:
symptomatic.proportion <- 0.5

age.dist.cases$symp <- age.dist.cases$cases*sum(age.dist.cases$pop)*symptomatic.proportion/age.dist.cases$pop/sum(age.dist.cases$cases)

symp.dist <- age.dist.cases$symp
names(symp.dist) <- paste0(c(0,
                             age.dist.cases$ages[-length(age.dist.cases$ages)]+1),
                           "-",
                           age.dist.cases$ages)

jpeg("../figures/report_figure_age_symptoms.jpeg", width=4.5,height=3,res=300,units = "in")
barplot(symp.dist,ylim=c(0,1),ylab="Proportion symptomatic",xlab="Age")
dev.off()
