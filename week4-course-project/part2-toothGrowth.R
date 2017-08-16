tooth <- datasets::ToothGrowth
str(tooth)
library(ggplot2)

#see summary of dataset
by(tooth$len, INDICES = list(tooth$dose, tooth$supp), summary)


#see spread of growth by dosage and compare supp
g <- ggplot(tooth, aes(as.factor(dose), len))
g + geom_boxplot(aes(color = supp))

#see grid broken down by dosage and supp
t <- ggplot(tooth, aes(dose, len))
t + geom_point(aes(color = supp)) + facet_grid(as.factor(dose) ~ supp)

#see plot and mean of len by dose and supp
t + geom_point(aes(color = supp)) + geom_line(data = avg, aes(group = supp, color = supp))


#see density of len broken down by dosage and supp
b <- ggplot(tooth, aes(len)) 
b + geom_density() + facet_grid(as.factor(dose)~supp) 


#hyp1 irrespective of supplement type, mean growth of dosage 2 > 1 > 0.5
t.test(len ~ dose, paired = F, var.equal = F, data = subset(tooth, dose != 2))  #mu(dose=0.5) != mu(dose=1.0)
t.test(len ~ dose, alternative = "less", paired = FALSE, var.equal = FALSE, data = subset(tooth, dose != 2), conf.level = .975)  #mu(dose =0.5) < mu(dose=1.0)

t.test(len ~ dose, paired = F, var.equal = F, data = subset(tooth, dose != 1)) #mu(dose=0.5) != mu(dose=2.0)
t.test(len ~ dose, paired = F, var.equal = F, data = subset(tooth, dose != 0.5)) #mu(dose=1) != mu(dose=2)




#hyp2 for the same dosage, mean growth of OJ > VC except when dose = 2
t.test(len ~ supp, alternative = "greater", paired = F, var.equal = F, data = subset(tooth, dose == 0.5), conf.level = .975) #OJ > VC ("greater" implies OJ > VC because in levels(tooth$supp) the first level is OJ)
t.test(len ~ supp, alternative = "greater", paired = F, var.equal = F, data = subset(tooth, dose == 1), conf.level = .975) #OJ > VC ("greater" implies OJ > VC because in levels(tooth$supp) the first level is OJ)
t.test(len ~ supp, paired = F, var.equal = F, data = subset(tooth, dose == 2))  #OJ = VC 

#hyp2.1 When dose = 2, var of VC > var of OJ & mean of VC = mean of OJ
var.test(len ~ supp, data = subset(tooth, dose == 2)) #p-value of result is 0.96 so there is no significant difference & we cannot reject the hyp that variances are equal

