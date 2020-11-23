# gradrates_2020_10_25.R simulating the model with differentiated brands, Section 4, Line 286
# gradrates_2020_10_23.R simulating the model with iso-elastic demand, Section 4, Line 286
# gradrates_2020_9_13.R revising the model, introducing mu as varying degree of competition
# gradrates_2020_9_14.R Start revising regressions
# gradrates_200830.R posted 1st draft gradrates_19.tex
########
# Libraries: 
library(ggplot2); theme_set(theme_bw())
library(dplyr)# for lag and lead functions
library(plyr)# for join
library(stargazer)# diplay regression results
#
# Function definitions: Cummulative Annual Growth Rate
CAGR_formula <- function(FV, PV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}
# testing the formula
CAGR_formula(110, 100, 1)
CAGR_formula(110, 100, 2)
CAGR_formula(121, 100, 2)

setwd("~/Papers/gradrates/gradrates_coding")
dir()
# Load data on HS and college graduation rates
#grad_1 = read.csv("college_high_school_grad_rates.csv")
#saveRDS(grad_1, "college_high_school_grad_rates.rds")
grad_2 = readRDS("college_high_school_grad_rates.rds")
dim(grad_2)
names(grad_2)
head(grad_2)
range(grad_2$Year)

# Load data on income inequality
#inc_1 = read.csv("Measures_income_dispersion.csv")
#saveRDS(inc_1, "income_inequality.rds")
inc_2 = readRDS("income_inequality.rds")
dim(inc_2)
names(inc_2)
head(inc_2)
range(inc_2$Year)
# rename variables (shorten)
inc_3 = inc_2
names(inc_3)
colnames(inc_3)[colnames(inc_3)=="Gini.index.of.income.inequality"] = "Gini"
colnames(inc_3)[colnames(inc_3)=="Mean.logarithmic.deviation.of.income"] = "MLD"
colnames(inc_3)[5] = "Atkinson_0.25"
colnames(inc_3)[6] = "Atkinson_0.50"
colnames(inc_3)[7] = "Atkinson_0.75"
colnames(inc_3)[8] = "Lowest_quintile"
colnames(inc_3)[9] = "Second_quintile"
colnames(inc_3)[10] = "Third_quintile"
colnames(inc_3)[11] = "Fourth_quintile"
colnames(inc_3)[12] = "Highest_quintile"
colnames(inc_3)[13] = "Top_5_percent"

# Load data on earning by education degree
#earn_1 = read.csv("Earning_by_edu.csv")
#str(earn_1)
#saveRDS(earn_1, "Earning_by_edu.rds")
earn_2 = readRDS("Earning_by_edu.rds")
dim(earn_2)
names(earn_2)
head(earn_2)
range(earn_2$Year)
length(earn_2$Year) # num years
str(earn_2)

# Load per-capital GDP
#dir()
#gdp_pp_1 = read.csv("gdp-per-cap.csv")
#str(gdp_pp_1)
#saveRDS(gdp_pp_1, "GDP_PP.rds")
gdp_pp_2 = readRDS("GDP_PP.rds")
dim(gdp_pp_2)
names(gdp_pp_2)
head(gdp_pp_2)
range(gdp_pp_2$Year)
#
gdp_pp_3 = subset(gdp_pp_2, select = c(Year, GDP.PP))
names(gdp_pp_3)
colnames(gdp_pp_3)[2] =  "GDP_PP"
names(gdp_pp_3)

### Plotting HS and college graduation rates: Fig 1
names(grad_2)
#
ggplot(grad_2) + geom_line(aes(x=Year, y=High_school)) + geom_point(aes(x=Year, y=High_school)) + geom_line(aes(x=Year, y=College))+ geom_point(aes(x=Year, y=College)) + annotate("text", x=1953, y=50, label="High school", size = 5) + annotate("text", x=1995, y=30, label="College", size = 5) + ylab("Percent of graduates (%)") + scale_x_continuous(breaks = seq(1940, 2020, 10)) + scale_y_continuous(breaks = seq(0, 100, 10)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))

## Discussion of Figure 1 (graduation rates)
names(grad_2) # high school
range(grad_2$Year)
grad_2[grad_2$Year==1940, ]
grad_2[grad_2$Year==2019, ]
grad_2[grad_2$High_school>38 & grad_2$High_school < 42, ] # crossing %40 HS rates
grad_2[grad_2$High_school>48 & grad_2$High_school < 52, ] # crossing %50 HS rates
grad_2[grad_2$High_school>58 & grad_2$High_school < 62, ] # crossing %60 HS rates
grad_2[grad_2$High_school>69 & grad_2$High_school < 72, ] # crossing %70 HS rates
grad_2[grad_2$High_school>79 & grad_2$High_school < 82, ] # crossing %80 HS rates
grad_2[grad_2$High_school>89 & grad_2$High_school < 92, ] # crossing %90 HS rates
#
# compounded annual growth rate of high school grad rates
100*CAGR_formula(grad_2[grad_2$Year==2019, "High_school"], grad_2[grad_2$Year==1940, "High_school"], 79) 

# College
grad_2[grad_2$Year==1940, ]
grad_2[grad_2$Year==2019, ]
grad_2[grad_2$College>9 & grad_2$College < 11, ] # crossing %10 HS rates
grad_2[grad_2$College>19 & grad_2$College < 21, ] # crossing %20 HS rates
grad_2[grad_2$College>29 & grad_2$College < 31, ] # crossing %30 HS rates
#
100*CAGR_formula(grad_2[grad_2$Year==2019, "College"], grad_2[grad_2$Year==1940, "College"], 79) 

### Plotting income inequality measures
names(inc_3)
#
ggplot(inc_3) + geom_line(aes(x=Year, y=Gini, color="Gini")) + geom_point(aes(x=Year, y=Gini, color="Gini", shape="Gini")) + geom_line(aes(x=Year, y=MLD, color = "MLD")) + geom_point(aes(x=Year, y=MLD, color="MLD", shape="MLD")) + geom_line(aes(x=Year, y=Theil, color="Theil")) + geom_point(aes(x=Year, y=Theil, color="Theil", shape="Theil")) + geom_line(aes(x=Year, y=Atkinson_0.50, color="Atkinson")) + geom_point(aes(x=Year, y=Atkinson_0.50, color="Atkinson", shape="Atkinson")) + guides(color=guide_legend(title="Index")) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + ylab("Inequality index") + annotate("text", x=1975, y=0.16, label="Atkinson", size = 5) + annotate("text", x=1975, y=0.29, label="Theil", size = 5) + annotate("text", x=2010, y=0.5, label="Gini", size = 5) + annotate("text", x=2010, y=0.61, label="MLD", size = 5) + guides(shape=FALSE)+ guides(color=FALSE) # remove legend

## Discussion of Fig 2: Measures of income inequality
names(inc_3)
range(inc_3$Year)
length(inc_3$Year)
inc_3[inc_3$Year==1967, c(1,2,3,4,6) ]
inc_3[inc_3$Year==2018, c(1,2,3,4,6) ]


### Fig 3: Plotting income inequality quitiles
names(inc_3)
#
ggplot(inc_3) + geom_line(aes(x=Year, y= Lowest_quintile, color="1st (lowest)")) + geom_point(aes(x=Year, y=Lowest_quintile, color="1st (lowest)", shape="1st (lowest)")) + geom_line(aes(x=Year, y= Second_quintile, color="2nd")) + geom_point(aes(x=Year, y=Second_quintile, color="2nd", shape="2nd")) + geom_line(aes(x=Year, y= Third_quintile, color="3rd")) + geom_point(aes(x=Year, y=Third_quintile, color="3rd", shape="3rd")) + geom_line(aes(x=Year, y= Fourth_quintile, color="4th")) + geom_point(aes(x=Year, y=Fourth_quintile, color="4th", shape="4th")) + geom_line(aes(x=Year, y= Highest_quintile, color="5th (highest)")) + geom_point(aes(x=Year, y=Highest_quintile, color="5th (highest)", shape="5th (highest)")) + geom_line(aes(x=Year, y= Top_5_percent, color="Top 5%")) + geom_point(aes(x=Year, y=Top_5_percent, color="Top 5%", shape="Top 5%")) + guides(color=guide_legend(title="Quintiles", )) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + ylab("Shares of household income of  quintiles (%)") + scale_y_continuous(breaks = seq(0, 55, 5)) + scale_x_continuous(breaks = seq(1965, 2020, 5)) + annotate("text", x=1975, y=46, label="Highest quintile", size = 5) + annotate("text", x=1975, y=26, label="4th quintile", size = 5) + annotate("text", x=1985, y=19, label="Top 5%", size = 5) + annotate("text", x=2000, y=16, label="3rd quintile", size = 5) + annotate("text", x=1975, y=12, label="2nd quintile", size = 5)+ annotate("text", x=1975, y=6, label="Lowest quintile", size = 5)+ guides(shape=FALSE)+ guides(color=FALSE) # remove legend

## start discussion of Figure 3 (income quitiles)
names(inc_3)
dim(inc_3)
range(inc_3$Year)
length(inc_3$Year)
inc_3[inc_3$Year==1967, c(8:13) ]
inc_3[inc_3$Year==2018, c(8:13) ]
#
100*CAGR_formula(inc_3[inc_3$Year==2018, "Lowest_quintile"], inc_3[inc_3$Year==1967, "Lowest_quintile"], 52)
100*CAGR_formula(inc_3[inc_3$Year==2018, "Second_quintile"], inc_3[inc_3$Year==1967, "Second_quintile"], 52)
100*CAGR_formula(inc_3[inc_3$Year==2018, "Third_quintile"], inc_3[inc_3$Year==1967, "Third_quintile"], 52)
100*CAGR_formula(inc_3[inc_3$Year==2018, "Fourth_quintile"], inc_3[inc_3$Year==1967, "Fourth_quintile"], 52)
100*CAGR_formula(inc_3[inc_3$Year==2018, "Highest_quintile"], inc_3[inc_3$Year==1967, "Highest_quintile"], 52)
100*CAGR_formula(inc_3[inc_3$Year==2018, "Top_5_percent"], inc_3[inc_3$Year==1967, "Top_5_percent"], 52)

### Plotting earning by HS, college, grad Fig 4
names(earn_2)
range(earn_2$Year)
length(earn_2$Year)
head(earn_2)
dim(earn_2)
earn_2_fig4.df = earn_2

# define 5 variables: college_earn_div_hs_earn and college_earn_div_below_hs
earn_2_fig4.df$college_earn_div_hs_earn = earn_2_fig4.df$College/earn_2_fig4.df$High_school
earn_2_fig4.df$graduate_earn_div_hs_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$High_school
earn_2_fig4.df$college_earn_div_below_hs_earn = earn_2_fig4.df$College/earn_2_fig4.df$Below_High_School
earn_2_fig4.df$graduate_earn_div_below_hs_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$Below_High_School
earn_2_fig4.df$graduate_earn_div_college_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$College
names(earn_2_fig4.df)
head(earn_2_fig4.df)

ggplot(earn_2_fig4.df) + geom_line(aes(x=Year, y=college_earn_div_below_hs_earn)) + geom_point(aes(x=Year, y=college_earn_div_below_hs_earn)) + geom_point(aes(x=Year, y=college_earn_div_hs_earn)) + geom_line(aes(x=Year, y=college_earn_div_hs_earn)) + geom_point(aes(x=Year, y=graduate_earn_div_college_earn)) + geom_line(aes(x=Year, y=graduate_earn_div_college_earn)) + annotate("text", x=1982, y=2.45, label=expression(paste(frac(College, Below_HS))), size = 5) + annotate("text", x=1982, y=1.76, label=expression(paste(frac(College, HS))), size = 5) + annotate("text", x=1994, y=1.38, label=expression(paste(frac(Graduate, College))), size = 5) + ylab("Earning ratios by education attainment") + scale_x_continuous(breaks = seq(1975, 2020, 5)) + scale_y_continuous(breaks = seq(1, 3, 0.25)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + theme(plot.margin = unit(c(0.3,0.6,0,0), "cm"))

## discussion of Figure 4 (earning by HS, college, graduate)
names(earn_2_fig4.df)
range(earn_2_fig4.df$Year)
temp1=round(earn_2_fig4.df[earn_2_fig4.df$Year==1975, c("college_earn_div_below_hs_earn", "college_earn_div_hs_earn", "graduate_earn_div_college_earn")], digits = 2)
temp2=round(earn_2_fig4.df[earn_2_fig4.df$Year==2018, c("college_earn_div_below_hs_earn", "college_earn_div_hs_earn", "graduate_earn_div_college_earn")],digits = 2)
#
(round(100*(temp2-temp1)/temp1, digits=1))# % change in earning ratios


#
# growth in HS earning [not using, switching Figure 4 to earning ratios]
# FV_HS = earn_2[earn_2$Year==2018, "High_school"]
# PV_HS = earn_2[earn_2$Year==1975, "High_school"]
# 100*(FV_HS-PV_HS)/PV_HS # total growth
# 100*CAGR_formula(FV_HS, PV_HS, 44) # commulative annual growth
# # growth in College earning
# FV_College = earn_2[earn_2$Year==2018, "College"]
# PV_College = earn_2[earn_2$Year==1975, "College"]
# 100*(FV_College-PV_College)/PV_College # total growth
# 100*CAGR_formula(FV_College, PV_College, 44) # commulative annual growth
# # growth in Graduate earning
# FV_Graduate = earn_2[earn_2$Year==2018, "Graduate"]
# PV_Graduate = earn_2[earn_2$Year==1975, "Graduate"]
# 100*(FV_Graduate-PV_Graduate)/PV_Graduate # total growth
# 100*CAGR_formula(FV_Graduate, PV_Graduate, 44) # commulative annual growth
# 
# # Growth in gap: College vs. HS [not used in paper]
# (FV_gap_College_HS = FV_College - FV_HS)
# (PV_gap_College_HS = PV_College - PV_HS)
# (100*(FV_gap_College_HS - PV_gap_College_HS)/PV_gap_College_HS) # total growth
# (100*CAGR_formula(FV_gap_College_HS, PV_gap_College_HS, 44)) # commulative annual growth
# # Growth in gap: Graduate vs. HS
# (FV_gap_Graduate_HS = FV_Graduate - FV_HS)
# (PV_gap_Graduate_HS = PV_Graduate - PV_HS)
# (100*(FV_gap_Graduate_HS - PV_gap_College_HS)/PV_gap_College_HS) # total growth
# (100*CAGR_formula(FV_gap_Graduate_HS, PV_gap_Graduate_HS, 44)) # commulative annual growth


### Start regressions of inequality measures on HS and college graduation rates
#
# Create a data frame with inequality measures and graduation rates (combine the 2 df)
names(inc_3)
names(grad_2)
d_1.df = join(inc_3, grad_2, by="Year", type="left")# skip 1940-1966 which inc_3 does not have
names(d_1.df)
d_2.df = d_1.df[, -c(8:13)]
names(d_2.df)
dim(d_2.df)
d_2.df$Year

# merging GDP per capita
d_3.df = join(d_2.df, gdp_pp_3, by = "Year", type = "left")
names(d_3.df)
dim(d_3.df)
range(d_3.df$Year)
head(d_3.df)

# Merging with earning (reducing num obs from to 52 to 44 )
d_4.df = d_3.df
names(d_4.df)
dim(d_4.df)
head(d_4.df)
colnames(d_4.df)[colnames(d_4.df)=="High_school"] = "High_school_frac"
colnames(d_4.df)[colnames(d_4.df)=="College"] = "College_frac"
names(d_4.df)

dim(earn_2)
names(earn_2)
head(earn_2)
earn_3.df = earn_2
colnames(earn_3.df)[colnames(earn_3.df)=="All"] = "All_earn"
colnames(earn_3.df)[colnames(earn_3.df)=="Below_High_School"] = "Below_high_school_earn"
colnames(earn_3.df)[colnames(earn_3.df)=="High_school"] = "High_school_earn"
colnames(earn_3.df)[colnames(earn_3.df)=="Assoc_some_college"] = "Assoc_some_college_earn"
colnames(earn_3.df)[colnames(earn_3.df)=="College"] = "College_earn"
colnames(earn_3.df)[colnames(earn_3.df)=="Graduate"] = "Graduate_earn"
names(earn_3.df)

d_5.df = join(d_4.df, earn_3.df, by="Year", type="right")
dim(d_5.df)
names(d_5.df)
head(d_5.df)

# define 5 variables: college_earn_div_hs_earn and college_earn_div_below_hs
d_5.df$college_earn_div_hs_earn = d_5.df$College_earn/d_5.df$High_school_earn
d_5.df$graduate_earn_div_hs_earn = d_5.df$Graduate_earn/d_5.df$High_school_earn
d_5.df$college_earn_div_below_hs_earn = d_5.df$College_earn/d_5.df$Below_high_school_earn
d_5.df$graduate_earn_div_below_hs_earn = d_5.df$Graduate_earn/d_5.df$Below_high_school
d_5.df$graduate_earn_div_college_earn = d_5.df$Graduate_earn/d_5.df$College_earn
names(d_5.df)
head(d_5.df)
range(d_5.df$Year)
# Reorder from 1975 to 2018 (needed only for %changes)
#d_6.df = d_5.df[order(d_5.df$Year), ]
#head(d_6.df)


## Regressing Gini only w.r.t different variables
#gini_model_full = Gini ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn + graduate_earn_div_below_hs_earn + graduate_earn_div_hs_earn + GDP_PP
gini_model_full = Gini ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn + graduate_earn_div_college_earn + GDP_PP
(gini_reg_full = lm(gini_model_full, data = d_5.df))
summary(gini_reg_full)
#
#gini_model_no_GDP = Gini ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn + graduate_earn_div_below_hs_earn + graduate_earn_div_hs_earn
gini_model_no_GDP = Gini ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn + graduate_earn_div_college_earn 
(gini_reg_no_GDP = lm(gini_model_no_GDP, data = d_5.df))
summary(gini_reg_no_GDP)
#
gini_model_earn_only = Gini ~ college_earn_div_below_hs_earn + college_earn_div_hs_earn + graduate_earn_div_college_earn 
(gini_reg_earn_only = lm(gini_model_earn_only, data = d_5.df))
summary(gini_reg_earn_only)
#
gini_model_college_only = Gini ~ College_frac 
(gini_reg_college_only = lm(gini_model_college_only, data = d_5.df))
summary(gini_reg_college_only)

# LaTeX table summarizing 4 regression results
#stargazer(gini_reg_college_only, gini_reg_earn_only, gini_reg_no_GDP, gini_reg_full)
# below without GDP
stargazer(gini_reg_college_only, gini_reg_earn_only, gini_reg_no_GDP)

# gradrates_2020_10_25.R simulating the model with diff brands, Section 4, Line 286
a=1 # alpha
b=1 # beta
phi = 12 # gamma
wn = 1
lam = 0.5 # lambda
s1 = 0.8 # sigma (high deg of subst)
s2 = 0.2 # sigma (low deg of subst)

# lower bound on phi relative to wn: phi > (2 a + b)
wn*(2*a+b)

# eq (19)
(wd1 = 2*((a*lam*phi - wn*(a*b*lam-s1-2)))/(2*a^2*lam+s1+2))
(wd2 = 2*((a*lam*phi - wn*(a*b*lam-s2-2)))/(2*a^2*lam+s2+2))

# eq (20)
(q1=(phi-wn*(2*a+b))/(2*a^2*lam+s1+2))
(q2=(phi-wn*(2*a+b))/(2*a^2*lam+s2+2))

(p1 = (wn*(s1+1)*(2*a+b) + phi*(2*a^2*lam +1))/(2*a^2*lam+s1+2))
(p2 = (wn*(s2+1)*(2*a+b) + phi*(2*a^2*lam +1))/(2*a^2*lam+s2+2))

(profit1 = ((phi-wn*(2*a+b))/(2*a^2*lam+s1+2))^2)
(profit2 = ((phi-wn*(2*a+b))/(2*a^2*lam+s2+2))^2)

### end of code gradrates ###
###### Unused code begins ###########

# # Prepare for regressions on rates of change instead of values
# d_7.df = d_6.df
# names(d_7.df)
# d_7.df$Gini_rate = 100*(d_7.df$Gini - lag(d_7.df$Gini))/lag(d_7.df$Gini)
# head(d_7.df$Gini_rate)
# d_7.df$College_rate = 100*(d_7.df$College_frac - lag(d_7.df$College_frac))/lag(d_7.df$College_frac)
# head(d_7.df$College_rate)
# d_7.df$High_school_rate = 100*(d_7.df$High_school_frac - lag(d_7.df$High_school_frac))/lag(d_7.df$High_school_frac)
# head(d_7.df$High_school_rate)
# d_7.df$College_earn_rate = 100*(d_7.df$College_earn - lag(d_7.df$College_earn))/lag(d_7.df$College_earn)
# head(d_7.df$College_earn_rate)
# d_7.df$college_earn_div_hs_earn_rate = 100*(d_7.df$college_earn_div_hs_earn - lag(d_7.df$college_earn_div_hs_earn))/lag(d_7.df$college_earn_div_hs_earn)
# head(d_7.df$college_earn_div_hs_earn_rate)
# d_7.df$college_earn_div_below_hs_earn_rate = 100*(d_7.df$college_earn_div_below_hs_earn - lag(d_7.df$college_earn_div_below_hs_earn))/lag(d_7.df$college_earn_div_below_hs_earn)
# head(d_7.df$college_earn_div_below_hs_earn_rate)
# #
# gini_rate_model = Gini_rate ~ College_rate + college_earn_div_below_hs_earn_rate + college_earn_div_hs_earn_rate
# (gini_rate_reg = lm(gini_rate_model, data = d_7.df))
# summary(gini_rate_reg)

# 4 regression models of measures of inequality 
# gini_model = Gini ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn
# mld_model = MLD ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn
# theil_model = Theil ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn
# atkinson_model = Atkinson_0.50 ~ College_frac + college_earn_div_below_hs_earn + college_earn_div_hs_earn
# # #
# (gini_reg = lm(gini_model, data = d_5.df))
# summary(gini_reg)
# (mld_reg = lm(mld_model, data = d_5.df))
# summary(mld_reg)
# (theil_reg = lm(theil_model, data = d_5.df))
# summary(theil_reg)
# (atkinson_reg = lm(atkinson_model, data = d_5.df))
# summary(atkinson_reg)

# Not using the graph below for Figure 4, switching to ratios
#ggplot(earn_2) + geom_line(aes(x=Year, y=High_school)) + geom_point(aes(x=Year, y=High_school)) + geom_line(aes(x=Year, y=College))+ geom_point(aes(x=Year, y=College)) + geom_line(aes(x=Year, y=Graduate))+ geom_point(aes(x=Year, y=Graduate)) + annotate("text", x=2007, y=25000, label="High school", size = 5) + annotate("text", x=2000, y=40000, label="College", size = 5) + annotate("text", x=1999, y=60000, label="Graduate", size = 5) + ylab("Average annual earning (USD)") + scale_x_continuous(breaks = seq(1975, 2020, 5)) + scale_y_continuous(breaks = seq(0, 100000, 5000)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + theme(plot.margin = unit(c(0.3,0.6,0,0), "cm"))


### simulating the model in Section 3 
# gradrates_2020_9_23.R revising model introducing mu
# Parameters
# L = 120# total labor force
# wn = 1# numeraire = nondegree wage
# l = 8# initial lambda
# b = 0.9# beta
# m = 1# initial mu
# a = 2# alpha
# #
# zhat = function(wd, l, b){
#   (wd - 2*wn)/l
# }
# # below: equilibrium: implicit solution for wd (mu=1!!!)
# eq10 = function(wd) {
#   (wd/(a *b^2 * wn))^(1/(b-1)) - L*(wd-2*wn)/l
# }
# # below: equilibrium: implicit solution for wd (mu>1!!!)
# xxx
# 
# # point A in Figure 5
# l = 3; m = 1
# eq10_a = function(wd) {
#   L*(wd/(a*b*m * wn))^(1/(b-1)) - L*(wd-2*wn)/l
# }
# (wd_a = uniroot(eq10_a, lower = 0, upper = 100)$root) 
# # verify condition in Result 2 (appendix A) is satisfied (>0)
# #b*log(wd_a/(wn*a*b^2)) + 2*(b-1)
# #
# (zhat_a = zhat(wd_a, l, b))
# #(x_a= - 2*L*(wd_a - 2*wn - l)/l)
# (y_a= L*(wd_a-2*wn)/l)
# (profity_a = (wd_a/m - wd_a)*y_a)
# # check d workers have positive income after paying for edu cost
# (cost_a = l*L*zhat_a^2/2)# total edu cost
# (wd_a*zhat_a*L+zhat_a*profity_a)# total wage income
# (py_a = wd_a/m)
# (px_a=1)
# (Id_a = wd_a*zhat_a*L + zhat_a*profity_a - cost_a)# total net income of d consumers
# (yd_a = zhat_a*L*(py_a/(a*b*px_a))^(1/(b-1)))# total demand by d consumers
# (yn_a = (1-zhat_a)*L*(py_a/(a*b*px_a))^(1/(b-1)))# total demand by n consumers
# (yn_a+yd_a)# verify agg demand for Y equals...
# y_a# agg supply
# #
# (xd_a = (Id_a - py_a*yd_a)/px_a)
# (In_a = (1-zhat_a)*L*2*wn)+ (1-zhat_a)*profity_a
# (xn_a = (In_a - py_a*yn_a)/px_a)
# (xn_a + xd_a)# verify agg demand for X equals...
# (x_a= 2*L*(1-zhat_a))# supply of X [difference may result from the computation of Id (income of degree workers)]
# 
# # point B in Figure 5
# l = 2; m = 1
# eq10_b = function(wd) {
#   L*(wd/(a*b*m * wn))^(1/(b-1)) - L*(wd-2*wn)/l
# }
# (wd_b = uniroot(eq10_b, lower = 0, upper = 100)$root) 
# # verify condition in Result 2 (appendix A) is satisfied (>0)
# #b*log(wd_b/(wn*a*b^2)) + 2*(b-1)
# #
# (zhat_b = zhat(wd_b, l, b))
# #(x_b= - 2*L*(wd_b - 2*wn - l)/l)
# (y_b= L*(wd_b-2*wn)/l)
# (profity_b = (wd_b/m - wd_b)*y_b)
# # check d workers have positive income after paying for edu cost
# (cost_b = l*L*zhat_b^2/2)# total edu cost
# (wd_b*zhat_b*L+zhat_b*profity_b)# total wage income
# (py_b = wd_b/m)
# (px_b=1)
# (Id_b = wd_b*zhat_b*L + zhat_b*profity_b - cost_b)# total net income of d consumers
# (yd_b = zhat_b*L*(py_b/(a*b*px_b))^(1/(b-1)))# total demand by d consumers
# (yn_b = (1-zhat_b)*L*(py_b/(a*b*px_b))^(1/(b-1)))# total demand by n consumers
# (yn_b+yd_b)# verify agg demand for Y equals...
# y_b# agg supply
# #
# (xd_b = (Id_b - py_b*yd_b)/px_b)
# (In_b = (1-zhat_b)*L*2*wn)+ (1-zhat_b)*profity_b
# (xn_b = (In_b - py_b*yn_b)/px_b)
# (xn_b + xd_b)# verify agg demand for X equals...
# (x_b= 2*L*(1-zhat_b))# supply of X [difference may result from the computation of Id (income of degree workers)]
# 
# # point C in Figure 5
# l = 2; m = b# beta, monopoly y
# eq10_c = function(wd) {
#   L*(wd/(a*b*m * wn))^(1/(b-1)) - L*(wd-2*wn)/l
# }
# (wd_c = uniroot(eq10_c, lower = 0, upper = 100)$root) 
# # verify condition in Result 2 (appendix A) is satisfied (>0)
# #b*log(wd_c/(wn*a*b^2)) + 2*(b-1)
# #
# (zhat_c = zhat(wd_c, l, b))
# #(x_c= - 2*L*(wd_c - 2*wn - l)/l)
# (y_c= L*(wd_c-2*wn)/l)
# (profity_c = (wd_c/m - wd_c)*y_c)
# # check d workers have positive income after paying for edu cost
# (cost_c = l*L*zhat_c^2/2)# total edu cost
# (wd_c*zhat_c*L+zhat_c*profity_c)# total wage income
# (py_c = wd_c/m)
# (px_c=1)
# (Id_c = wd_c*zhat_c*L + zhat_c*profity_c - cost_c)# total net income of d consumers
# (yd_c = zhat_c*L*(py_c/(a*b*px_c))^(1/(b-1)))# total demand by d consumers
# (yn_c = (1-zhat_c)*L*(py_c/(a*b*px_c))^(1/(b-1)))# total demand by n consumers
# (yn_c+yd_c)# verify agg demand for Y equals...
# y_c# agg supply
# #
# (xd_c = (Id_c - py_c*yd_c)/px_c)
# (In_c = (1-zhat_c)*L*2*wn)+ (1-zhat_c)*profity_c
# (xn_c = (In_c - py_c*yn_c)/px_c)
# (xn_c + xd_c)# verify agg demand for X equals...
# (x_c= 2*L*(1-zhat_c))# supply of X [difference may result from the computation of Id (income of degree workers)]
# 
# # summary
# L
# a
# b
# round(c(wd_a, wd_b, wd_c), digits = 2)
# round(c(zhat_a, zhat_b, zhat_c), digits = 3)
# round(c(x_a, x_b, x_c), digits = 1)
# round(c(y_a, y_b, y_c), digits = 1)
# round(c(py_a, py_b, py_c), digits = 1)

