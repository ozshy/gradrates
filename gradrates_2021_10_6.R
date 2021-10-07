# gradrates_202_5_27.R  start revision for JOLR
# gradrates_2020_11_27.R  first submission to JOLR
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
library(zoo)# to approx NAs in df
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
#grad_1 = read.csv("edu_ages_25_29.csv")
#saveRDS(grad_1, "edu_ages_25_29.rds")
grad_2 = readRDS("edu_ages_25_29.rds")
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
str(gdp_pp_3)

# Loading markup data
dir()
# markup_1.df = read.table("markup_raw.txt", col.names = T)
# str(markup_1.df)
# markup_1.df
# markup_2.df = markup_1.df
# colnames(markup_2.df) = markup_1.df[1,]
# #markup_2.df
# markup_3.df = markup_2.df[-1,]
# #markup_3.df
# str(markup_3.df)
# markup_4.df = as.data.frame(sapply(markup_3.df, as.numeric))
# str(markup_4.df)
# markup_4.df$year = as.integer(markup_4.df$year)
# colnames(markup_4.df)[1] = "Year"
# saveRDS(markup_4.df, "markup.rds")
markup_5.df = readRDS("markup.rds")
names(markup_5.df)
str(markup_5.df)

### Plotting HS and college graduation rates: Fig 1
names(grad_2)
#
ggplot(grad_2) + geom_line(aes(x=Year, y=High_school_and_higher)) + geom_point(aes(x=Year, y=High_school_and_higher)) + geom_line(aes(x=Year, y=College_and_higher))+ geom_point(aes(x=Year, y=College_and_higher)) + geom_line(aes(x=Year, y=Associate_and_higher))+ geom_point(aes(x=Year, y=Associate_and_higher))+ geom_line(aes(x=Year, y=MA_and_higher))+ geom_point(aes(x=Year, y=MA_and_higher)) + annotate("text", x=1980, y=90, label="High school or higher", size = 5) + annotate("text", x=1960, y=20, label="College or higher", size = 5) + annotate("text", x=2000, y=45, label="Associate or higher", size = 5) + annotate("text", x=2000, y=10, label="MA or higher", size = 5) + ylab("Percent of graduates (%)") + scale_x_continuous(breaks = seq(1940, 2020, 10)) + scale_y_continuous(breaks = seq(0, 100, 10)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))

## Discussion of Figure 1 (graduation rates)
names(grad_2) # high school
range(grad_2$Year)
grad_2[grad_2$Year==1940, ]
grad_2[grad_2$Year==2020, ]
grad_2[grad_2$High_school_and_higher>35 & grad_2$High_school_and_higher < 44, ] # crossing %40 HS rates
grad_2[grad_2$High_school_and_higher>45 & grad_2$High_school_and_higher < 54, ] # crossing %50 HS rates
grad_2[grad_2$High_school_and_higher>55 & grad_2$High_school_and_higher < 64, ] # crossing %60 HS rates
grad_2[grad_2$High_school_and_higher>65 & grad_2$High_school_and_higher < 74, ] # crossing %70 HS rates
grad_2[grad_2$High_school_and_higher>75 & grad_2$High_school_and_higher < 84, ] # crossing %80 HS rates
grad_2[grad_2$High_school_and_higher>85 & grad_2$High_school_and_higher < 94, ] # crossing %90 HS rates
#
# compounded annual growth rate of high school grad rates
100*CAGR_formula(grad_2[grad_2$Year==2020, "High_school_and_higher"], grad_2[grad_2$Year==1940, "High_school_and_higher"], 80) 

# College
names(grad_2)
grad_2[grad_2$Year==1940, ]
grad_2[grad_2$Year==2020, ]

grad_2[grad_2$College_and_higher>=10, c("Year", "College_and_higher") ] # crossing %10 HS rates
#
grad_2[grad_2$College_and_higher>=20, c("Year", "College_and_higher") ] # crossing %20 HS rates
#
grad_2[grad_2$College_and_higher>=30, c("Year", "College_and_higher") ] # crossing %30 HS rates#
#
grad_2[grad_2$College_and_higher>=35, c("Year", "College_and_higher") ] # crossing %35 HS rates#
#
100*CAGR_formula(grad_2[grad_2$Year==2020, "College_and_higher"], grad_2[grad_2$Year==1940, "College_and_higher"], 80) 

### Plotting income inequality measures [Removed from v.61 and on]
names(inc_3)
#
ggplot(inc_3) + geom_line(aes(x=Year, y=Gini, color="Gini")) + geom_point(aes(x=Year, y=Gini, color="Gini", shape="Gini")) + geom_line(aes(x=Year, y=MLD, color = "MLD")) + geom_point(aes(x=Year, y=MLD, color="MLD", shape="MLD")) + geom_line(aes(x=Year, y=Theil, color="Theil")) + geom_point(aes(x=Year, y=Theil, color="Theil", shape="Theil")) + geom_line(aes(x=Year, y=Atkinson_0.50, color="Atkinson")) + geom_point(aes(x=Year, y=Atkinson_0.50, color="Atkinson", shape="Atkinson")) + guides(color=guide_legend(title="Index")) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + ylab("Inequality index") + annotate("text", x=1975, y=0.16, label="Atkinson", size = 5) + annotate("text", x=1975, y=0.29, label="Theil", size = 5) + annotate("text", x=2010, y=0.5, label="Gini", size = 5) + annotate("text", x=2010, y=0.61, label="MLD", size = 5) + guides(shape=FALSE)+ guides(color=FALSE) # remove legend

## Discussion of Fig inequality: Measures of income inequality [Removed since v.61]
names(inc_3)
range(inc_3$Year)
length(inc_3$Year)
inc_3[inc_3$Year==1967, c(1,2,3,4,6) ]
inc_3[inc_3$Year==2018, c(1,2,3,4,6) ]


### Fig 3: Plotting income inequality quintiles [Not used in paper]
names(inc_3)
#
ggplot(inc_3) + geom_line(aes(x=Year, y= Lowest_quintile, color="1st (lowest)")) + geom_point(aes(x=Year, y=Lowest_quintile, color="1st (lowest)", shape="1st (lowest)")) + geom_line(aes(x=Year, y= Second_quintile, color="2nd")) + geom_point(aes(x=Year, y=Second_quintile, color="2nd", shape="2nd")) + geom_line(aes(x=Year, y= Third_quintile, color="3rd")) + geom_point(aes(x=Year, y=Third_quintile, color="3rd", shape="3rd")) + geom_line(aes(x=Year, y= Fourth_quintile, color="4th")) + geom_point(aes(x=Year, y=Fourth_quintile, color="4th", shape="4th")) + geom_line(aes(x=Year, y= Highest_quintile, color="5th (highest)")) + geom_point(aes(x=Year, y=Highest_quintile, color="5th (highest)", shape="5th (highest)")) + geom_line(aes(x=Year, y= Top_5_percent, color="Top 5%")) + geom_point(aes(x=Year, y=Top_5_percent, color="Top 5%", shape="Top 5%")) + guides(color=guide_legend(title="Quintiles", )) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + ylab("Shares of household income of  quintiles (%)") + scale_y_continuous(breaks = seq(0, 55, 5)) + scale_x_continuous(breaks = seq(1965, 2020, 5)) + annotate("text", x=1975, y=46, label="Highest quintile", size = 5) + annotate("text", x=1975, y=26, label="4th quintile", size = 5) + annotate("text", x=1985, y=19, label="Top 5%", size = 5) + annotate("text", x=2000, y=16, label="3rd quintile", size = 5) + annotate("text", x=1975, y=12, label="2nd quintile", size = 5)+ annotate("text", x=1975, y=6, label="Lowest quintile", size = 5)+ guides(shape=FALSE)+ guides(color=FALSE) # remove legend

## start discussion of Figure (income quitiles) [Not used in paper]
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

### Plotting earning (ratios) by HS, college, grad Fig 2 [old Figure 4]
names(earn_2)
range(earn_2$Year)
length(earn_2$Year)
head(earn_2)
dim(earn_2)
earn_2_fig4.df = earn_2
str(earn_2_fig4.df)

# define 5 variables: college_earn_div_hs_earn and college_earn_div_below_hs
earn_2_fig4.df$college_earn_div_hs_earn = earn_2_fig4.df$College/earn_2_fig4.df$High_school
earn_2_fig4.df$graduate_earn_div_hs_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$High_school
earn_2_fig4.df$college_earn_div_below_hs_earn = earn_2_fig4.df$College/earn_2_fig4.df$Below_high_school
earn_2_fig4.df$graduate_earn_div_below_hs_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$Below_high_school
earn_2_fig4.df$graduate_earn_div_college_earn = earn_2_fig4.df$Graduate/earn_2_fig4.df$College
names(earn_2_fig4.df)
head(earn_2_fig4.df)

ggplot(earn_2_fig4.df) + geom_line(aes(x=Year, y=college_earn_div_below_hs_earn)) + geom_point(aes(x=Year, y=college_earn_div_below_hs_earn)) + geom_point(aes(x=Year, y=college_earn_div_hs_earn)) + geom_line(aes(x=Year, y=college_earn_div_hs_earn)) + geom_point(aes(x=Year, y=graduate_earn_div_college_earn)) + geom_line(aes(x=Year, y=graduate_earn_div_college_earn)) + annotate("text", x=1982, y=2.45, label=expression(paste(frac(College, Below_HS))), size = 5) + annotate("text", x=1982, y=1.76, label=expression(paste(frac(College, HS))), size = 5) + annotate("text", x=1994, y=1.38, label=expression(paste(frac(Graduate, College))), size = 5) + ylab("Earning ratios by education attainment") + scale_x_continuous(breaks = seq(1975, 2020, 5)) + scale_y_continuous(breaks = seq(1, 3, 0.25)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + theme(plot.margin = unit(c(0.3,0.6,0,0), "cm"))

## discussion of Figure 2 [old fig 4] (earning by HS, college, graduate) 
names(earn_2_fig4.df)
range(earn_2_fig4.df$Year)
(temp1=round(earn_2_fig4.df[earn_2_fig4.df$Year==1975, c("college_earn_div_below_hs_earn", "college_earn_div_hs_earn", "graduate_earn_div_college_earn")], digits = 2))
(temp2=round(earn_2_fig4.df[earn_2_fig4.df$Year==2019, c("college_earn_div_below_hs_earn", "college_earn_div_hs_earn", "graduate_earn_div_college_earn")],digits = 2))
#
(round(100*(temp2-temp1)/temp1, digits=1))# % change in earning ratios

### Start regressions
names(grad_2)# education attainment as in Fig 1
range(grad_2$Year)
grad_2$Year # gaps in earlier years, needs to be interpolated
dim(grad_2)
head(grad_2)
# adding missing years with NA
(year_1.vec = 1940:2020)
length(year_1.vec)
year_1.df = data.frame(Year=year_1.vec)
str(year_1.df)
grad_3.df = join(year_1.df, grad_2, by="Year", type="left")
dim(grad_3.df)
names(grad_3.df)
# deleting assoc and higher degrees and MA and higher
grad_4.df = subset(grad_3.df, select = c(Year, High_school_and_higher, College_and_higher))
names(grad_4.df)
str(grad_4.df)
# Approximating NAs
grad_5.df = grad_4.df
names(grad_5.df)
# approx NAs (library zoo)
grad_5.df = as.data.frame(na.approx(grad_4.df, rule=2))
str(grad_5.df)
head(grad_4.df,12)
head(grad_5.df,12)
tail(grad_4.df)
tail(grad_5.df)
sum(is.na(grad_5.df))# verify no NAs

# merging grad_5.df with markup_5.df
str(markup_5.df)
range(markup_5.df$Year)
str(grad_5.df)
range(grad_5.df$Year)

reg_1.df = join(markup_5.df, grad_5.df, by="Year", type="left")# merging grad5 into markup5 with smaller range
str(reg_1.df)
reg_1.df
names(reg_1.df)

# merging with gdp per capita
str(gdp_pp_3)
reg_2.df = join(reg_1.df, gdp_pp_3, by="Year", type="left")
str(reg_2.df)
range(reg_2.df$Year)
length(reg_2.df$Year)# num years in the regression panel

# merging with earning 
earn_3.df = earn_2
str(earn_3.df)
range(earn_3.df$Year)
reg_3.df = join(subset(reg_2.df, Year %in% c(1975:2016)), subset(earn_3.df, Year %in% c(1975:2016)), by="Year")
str(reg_3.df)
length(reg_3.df$Year)# num years in final regression panel
range(reg_3.df$Year)
names(reg_3.df)

# define ratio college earn to HS earn (top graph in Fig 2)
reg_4.df = reg_3.df
(reg_4.df$earn_ratio = reg_4.df$College/reg_4.df$High_school)


# define regression model
reg_1.model = formula(earn_ratio ~ College_and_higher + High_school_and_higher + Aggregate_Markup + GDP_PP)

(reg_1.lm = lm(reg_1.model, data = reg_4.df))
summary(reg_1.lm)

# regression model without GDP PP
reg_2.model = formula(earn_ratio ~ College_and_higher + High_school_and_higher + Aggregate_Markup)

(reg_2.lm = lm(reg_2.model, data = reg_4.df))
summary(reg_2.lm)

# regression model w/o markup (only edu)
reg_3.model = formula(earn_ratio ~ College_and_higher + High_school_and_higher)

(reg_3.lm = lm(reg_3.model, data = reg_4.df))
summary(reg_3.lm)


# LaTeX table summarizing 3 regression results, Table 1
#stargazer(reg_3.lm, reg_2.lm, reg_1.lm)
stargazer(reg_3.lm, reg_2.lm)


# correlation between markup and PP GDP
round(with(reg_4.df, cor(Aggregate_Markup, GDP_PP)),2)
# which explains why significance shifted from aggregate markup to GPD. Hence, I remove real GDP from the regression. 

### Constructing a graph (not in paper) of rising markup based on markup data by the authors of QJE (2020)
markup_5.df
range(markup_5.df$Year)

ggplot(markup_5.df, aes(x=Year, y = Aggregate_Markup)) + geom_point() + geom_line() + ylab("Aggregate price markup") + scale_x_continuous(breaks = seq(1955, 2016, 5)) + scale_y_continuous(breaks = seq(1.2, 1.7, 0.05)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + theme(plot.margin = unit(c(0.3,0.6,0,0), "cm"))

### end of code gradrates ###
