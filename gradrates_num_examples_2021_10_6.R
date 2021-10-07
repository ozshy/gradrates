# numerical examples for gradrates_62.tex
########
# Libraries: 
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# for math in ggplot
#
# Model parameters
alpha = 1/10
beta = 1/10
N = 500
delta = 1
wn = 10
rho = 0.9
tau = 5
mu1 = 0 #markup
mu2 = 1.5

# equation (11) 
(profita = delta*alpha*N/2)

#equation (13) competitive price and degree wage (in the paper, it is p^e not p^c)
(pc = (4*wn*(alpha*(1+rho)+beta*rho) + alpha*rho*(5*delta+4*tau))/(4*rho))
# subs back into (11)
(wdc = (pc-beta*wn-alpha*delta)/alpha )
# in (13)
(wdc = (4 *wn*(1+rho) + rho*(delta + 4*tau))/(4*rho))

# eq (14) [should be zero under competition]
(gapi1 = rho*(((1+mu1)*pc - beta*wn - alpha*delta)/alpha - delta/4 - tau) -wn*(1+rho))

# eq (15)
(gapw1 = ((1+mu1)*pc - wn*(alpha+beta) - alpha*delta)/alpha   )

# Result 2 (compare with competition above)
(gapi2 = rho*(((1+mu2)*pc - beta*wn - alpha*delta)/alpha - delta/4 - tau) -wn*(1+rho))
#
(gapw2 = ((1+mu2)*pc - wn*(alpha+beta) - alpha*delta)/alpha   )

## Checking consistency of results (revenue versus cost versus profit, not for the paper)
pc# (in the paper, it is p^e not p^c, eq (13))
N
(revenue1 = N*(1+mu1)*pc)
(revenue2 = N*(1+mu2)*pc)
#
(ld1 = alpha*N)# num degree workers
(ld2 = alpha*N)# num degree workers
#
(ln1 = beta*N)# num nondegree workers
(ln2 = beta*N)# num nondegree workers
#
(wd1 = ((1+mu1)*pc-beta*wn-alpha*delta)/alpha )
(wd2 = ((1+mu2)*pc-beta*wn-alpha*delta)/alpha )
#
(cost1 = wd1*ld1 + wn*ln1)
(cost2 = wd2*ld2 + wn*ln2)
#
(profit1 = delta*alpha*N/2)# eq 11, single firm
(profit2 = delta*alpha*N/2)
#
revenue1 - cost1 # combined 2 firms (twice the profit of a single firm)
revenue2 - cost2 # combined 2 firms
# => the model is consistent under any mu
# check if degree or nondegree workers are paid below or above their VMP
#
(vmp1 = (1+mu1)*pc/alpha)# the same for degree and nondegree workers because I assumed alpha = beta
(vmp2 = (1+mu2)*pc/alpha)#
wn # below VMP
wd1# below VMP
wd2# below VMP

### section 6: Disequilibrium Result 3
# eq. (18) and (19) in paper
(ldd = alpha*N)
(Ld1 = (N*rho*(4*(1+mu1)*pc - 4*wn*(alpha+beta) -5*alpha*delta))/(4*(wn+rho*tau)))
(Ld2 = (N*rho*(4*(1+mu2)*pc - 4*wn*(alpha+beta) -5*alpha*delta))/(4*(wn+rho*tau)))
(ldn1 = Ld1 - ldd)
(ldn2 = Ld2 - ldd)

#
(rdd1 = ldd/Ld1)
(rdd2 = ldd/Ld2)

### Section 7 (wn is proportional to VMPL)
# adding a parameter phi
# need an upper bound Assumption 3
beta*rho/(alpha*(1+rho)+beta*rho)
phi = 0.25
#
#eq. (22)
(pe_phi = alpha*beta*rho*(5*delta + 4*tau)/(4*((1-phi)*beta*rho - alpha*phi*(1+rho))))
#
(wn1_phi = phi*(1+mu1)*pe_phi/beta)
(wn2_phi = phi*(1+mu2)*pe_phi/beta)
#
(wd1_phi = ((1-phi)*(1+mu1)*pe_phi-alpha*delta)/alpha)
(wd2_phi = ((1-phi)*(1+mu2)*pe_phi-alpha*delta)/alpha)
#
(gapi1_phi = rho*((pe_phi*((1+mu1)*(1-phi))-alpha*delta)/alpha -delta/4-tau) - pe_phi*phi*(1+mu1)*(1+rho)/beta)# should be zero, but close (probably rounding issue)
(gapi2_phi = rho*((pe_phi*((1+mu2)*(1-phi))-alpha*delta)/alpha -delta/4-tau) - pe_phi*phi*(1+mu2)*(1+rho)/beta)
#
(gapw1_phi = (pe_phi*((1+mu1)*(1-phi))-alpha*delta)/alpha - pe_phi*phi*(1+mu1)/beta )
#
(gapw2_phi = (pe_phi*((1+mu2)*(1-phi))-alpha*delta)/alpha - pe_phi*phi*(1+mu2)/beta )

# start plotting graph for section 7
mu.vec = seq(0, 1, 0.01)# markup
wn_phi.vec = phi*(1+mu.vec)*pe_phi/beta# nondegree wage
wd_phi.vec = ((1-phi)*(1+mu.vec)*pe_phi-alpha*delta)/alpha# degree wage
vmp.vec = (1+mu.vec)*pe_phi/alpha# value of marginal product (either factor because alpha=beta)

# making it a dataframe and ggplot
phi.df = data.frame(mu.vec, wn_phi.vec, wd_phi.vec, vmp.vec)
dim(phi.df)
#
ggplot(phi.df)+ 
  geom_line(aes(x=mu.vec, y=vmp.vec), color="red", size = 1.2)+
  geom_line(aes(x=mu.vec, y=wn_phi.vec), color="blue", size = 1.2, linetype = "dashed")+
  geom_line(aes(x=mu.vec, y=wd_phi.vec), color="black", size = 1.2, linetype= "twodash")+
  scale_y_continuous(breaks = seq(0, 60, 5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.6, size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), axis.title=element_text(size=18)) +
  ylab(TeX("$w^n, \\; w^d, \\; VMP_L$"))+
  xlab(TeX("$Price \\, markup\\, (\\mu)$"))+ 
  annotate("text", x = 0.75, y = 47, label=TeX("$VMP_L$"), hjust=0, size = 6)+ 
  annotate("text", x = 0.5, y = 29, label=TeX("$W^d$"), hjust=0, size = 6)+ 
  annotate("text", x = 0.5, y = 8, label=TeX("$W^n$"), hjust=0, size = 6)
