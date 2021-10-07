# numerical examples for gradrates_62.tex
########
# Libraries: 
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

#equation (13) competitive price and degree wage
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

### section 6: Disequilibrium Result 3
# eq. (18) in paper


# eq. (19) in paper
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
(wn1_phi = phi*(1+mu1)*pe/beta)
(wn2_phi = phi*(1+mu2)*pe/beta)
#
(wd1_phi = ((1-phi)*(1+mu1)*pe-alpha*delta)/alpha)
(wd2_phi = ((1-phi)*(1+mu2)*pe-alpha*delta)/alpha)
#
(gapi1_phi = rho*((pe_phi*((1+mu1)*(1-phi))-alpha*delta)/alpha -delta/4-tau) - pe_phi*phi*(1+mu1)*(1+rho)/beta)# should be zero, but close (probably rounding issue)
(gapi2_phi = rho*((pe_phi*((1+mu2)*(1-phi))-alpha*delta)/alpha -delta/4-tau) - pe_phi*phi*(1+mu2)*(1+rho)/beta)
#
(gapw1_phi = (pe_phi*((1+mu1)*(1-phi))-alpha*delta)/alpha - pe_phi*phi*(1+mu1)/beta )
#
(gapw2_phi = (pe_phi*((1+mu2)*(1-phi))-alpha*delta)/alpha - pe_phi*phi*(1+mu2)/beta )

# start plotting graph for section 7
mu.vec = seq(0, 1, 0.01)
