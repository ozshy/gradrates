# gradrates_2020_11_26.R deleting previous simulations and adding Section 4 (back to the technology change explanation), line 289
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

### gradrates_2020_11_26.R New Section 4: Back to the technology change explanation, line 289, corresponding to gradrates_2020_11_26.dfw
wd.f = function(x){
  values = (198*x + 95)/(9*(2*x^2+5))
  return(values)
}
#
wd_net.f = function(x){
  values = (x*(99-19*x))/(9*(2*x^2+5))
  return(values)
}
#
gap.f = function(x){
  values = x*(99 - 19*x)/(5*(2*x^2+5))
  return(values)
}
#
gap_net.f = function(x){
  values = x*(99 - 19*x)/(10*(2*x^2+5))
  return(values)
}
#
ld.f = function(x){
  values = 4*x*(99 - 19*x)/(9*(2*x^2+5))
  return(values)
}
ln.f = function(x){
  values = ld.f(x)/x
  return(values)
}

# section 4: tech change simulations
#ggplot(data = data.frame(x = 0), mapping = aes(x = x))# dummy dataset
(y_arrow=0) # y location of a horizontal arrow
#
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = gap.f, color = "red", linetype = 1, size=1.2) + 
  annotate(geom = "text", x=0.6, y=2.3, label = expression("Gap"), color = "red", size=8) +
  stat_function(fun = ld.f, color = "black", linetype = 1, size=1.2) +
  annotate(geom = "text", x=0.6, y=4.6, label = expression("L"^"d"), color = "black", size=8) +
  stat_function(fun = ln.f, color = "blue", linetype = 1, size=1.2) +
  annotate(geom = "text", x=0.6, y=7.3, label = expression("L"^"n"), color = "blue", size=8) +
  labs(x=expression("Degree workers productivity  parameter ("*alpha*")"), y = "") +
  scale_x_continuous(breaks = seq(0, 2, 0.25), limits = c(0,2)) +
  annotate("segment", x = 1, xend = 1.75, y = y_arrow, yend = y_arrow, colour = "black", size=0.5, alpha=1, arrow=arrow(type="closed", length = unit(0.2, "cm"))) +
  annotate("segment", x = 1, xend = 0.25, y = y_arrow, yend = y_arrow, colour = "black", size=0.5, alpha=1, arrow=arrow(type="closed", length = unit(0.2, "cm"))) +
  annotate(geom = "text", x=1.4, y=0.3, label="degree workers are LESS productive", size=5)  +
  annotate(geom = "text", x=0.6, y=0.3, label="degree workers are MORE productive", size=5) + 
  theme(axis.title.x = element_text(size = rel(1.5)))

  


### end of code gradrates ###
