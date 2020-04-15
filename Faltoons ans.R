library(readr)
attach(Faltoons)
head(Faltoons)
nrow(Faltoons)
attach(Faltoons)
df_faltoons1 = as.data.frame(cbind(c(Weekdays,Weekend),rep(c("Weekdays","Weekend"),c(400,400)))) # 1 for Female; and 2 for Male
colnames(df_faltoons1) = c("Gender","Day")
df_flt2 = table(df_faltoons1);df_flt2

#hypothesis testing###
#Ho-male versus female walking in to store dosenot differ based on day of the week
#ha-male versus female walking in to the store differ based on the day of week
# Chisquare test for difference in Attributes, Here one Attribute is Gender (Male and Female) and another Attribute is day (Weekedays,weekends)
chisq.test(df_tab2) # P-value is 0.00008543 < 0.05 , Hence we reject the null hypothesis
#conclusion = male versus female walking in to the store differ based on day of the week