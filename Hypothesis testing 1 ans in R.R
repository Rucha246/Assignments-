attach(Cutlets)
#to see the data given is normal or nnot we have to perform shapiro test#
shapiro.test(Unit.A)
shapiro.test(Unit.B)
########Varience Test########
var.test(Unit.A,Unit.B)# Varience test
#p value=0.3136-> so 0.3136>0.05 -> p high null fly=>equal variences

####### 2 sample t test#######

t.test(Unit.A,Unit.B, alternative = "two.sided", conf.level = 0.95, correct=TRUE)# Two sample t test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 < 0.05 accept null Hypothesis 
# unequal means

t.test(Unit.A,Unit.B, alternative ="greater", var.equal = TRUE)









