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

### section 5: Disequilibrium Result 3
(ldd = alpha*N)
(Ld1 = (N*rho*(4*(1+mu1)*pc - 4*wn*(alpha+beta) -5*alpha+tau))/(4*(wn+rho*tau)))
(Ld2 = (N*rho*(4*(1+mu2)*pc - 4*wn*(alpha+beta) -5*alpha+tau))/(4*(wn+rho*tau)))
#
(rdd1 = ldd/Ld1)
(rdd2 = ldd/Ld2)
