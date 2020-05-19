###################chp1 Ex1 Randomization Test
library("Sleuth3")  # Do this step every time

rm(list=ls());   # Clear workspace

set.seed(0101);  # Set seed for reproducibility

case0101  # Data for the "Creativity" example described on page 2

dim(case0101); names(case0101);  

boxplot(Score ~ Treatment, data=case0101)

# Data suggest that higher scores will result from Intrinsic treatment


# Re-order the factor levels of Treatment variable

foo <- levels(case0101$Treatment)

case0101$Treatment <- factor(case0101$Treatment, levels=foo[2:1])

rm(foo)

boxplot(Score ~ Treatment, data=case0101)

# Done


with(case0101, tapply(Score, Treatment, summary))  # Sumary statistics


n <- with(case0101, tapply(Score, Treatment, length))

ybar <- with(case0101, tapply(Score, Treatment, mean))

s <- with(case0101, tapply(Score, Treatment, sd))

round(cbind(n, ybar, s), 2)  # Disply 


# Conduct randomization test to determine if diff in mean scores is 
#  statistically significant.

I <- case0101$Treatment == "Intrinsic";  

E <- case0101$Treatment == "Extrinsic";

y <- case0101$Score

T.obs <- mean(y[I]) - mean(y[E]); T.obs;  # y1bar - y2bar = 4.144

T.sim <- rep(NA, 1e5)  # half-million simulations, change this if you like

for(t in 1:length(T.sim)){ 
  yr <- sample(y)  # random re-ordering of data
  T.sim[t] <- mean(yr[I]) - mean(yr[E])  # test stat given randomization
}

rm(yr) 


hist(T.sim, freq=F, right=F)  # Approx randomization dist of test-stat

abline(v=T.obs, lty=2); abline(v=-T.obs, lty=2);  

# How far out in the tail is our observed test statistic value?


p.value <- mean( abs(T.sim) > abs(T.obs) );  p.value;


# Two-sided p-value is .005, fairly strong indication that the Intrinsic 
#  treatment results in higher creativity scores



#########chp1 Ex2 Density
rm(list=ls());  # Clear workspace

set.seed(0102)  # Set seed for reproducibility

library("Sleuth3")  

case0102  # Data for the "Discrimination" example described on page 4

dim(case0102); names(case0102);  

boxplot(Salary ~ Sex, data=case0102)

# Salaries definitely appear higher for male employees


# Plot "density curves" (smoothed-out histograms) of M and F salaries

xr <- range(case0102$Salary) * c(0.9, 1.1)

den.f <- with(case0102, density(Salary[Sex=="Female"]))

den.m <- with(case0102, density(Salary[Sex=="Male"]))

max(den.f$y) > max(den.m$y)  # Do female curve first

plot(den.f$y ~ den.f$x, type="l", xlim=xr, xlab="Salary", ylab="Density")

lines(den.m, lty=2)

legend("topright", inset=.05, lty=1:2, legend=c("Female","Male"))


############chp2 Ex1 Two sample t test
library("Sleuth3")  # Do this step every time

rm(list=ls());   # Clear workspace

set.seed(0201);  # Set seed for reproducibility


case0201  # The finch beaks data described on page 29 

# If there is statistical evidence that beak depths in the 1978 
#  population are on average greater than 1976, then there is 
#  empirical support for Darwin's theory of natural selection.

dim(case0201); names(case0201);  

boxplot(Depth ~ Year, data=case0201)

# Does appear that beaks of 1978 birds are somewhat longer than in 1976



with(case0201, tapply(Depth, Year, summary))  # Sumary statistics


n <- with(case0201, tapply(Depth, Year, length))

ybar <- with(case0201, tapply(Depth, Year, mean))

s <- with(case0201, tapply(Depth, Year, sd))

round(cbind(n, ybar, s), 4)  # Disply 



# Estimated difference in means

as.numeric( ybar[2] - ybar[1] )

# Estimate that mean beak depth in 1978 exceeds that in 1976 by 0.6685 mm



# Test  NH: mean_1976 = mean_1978  versus  AH: mean_1976 < mean_1978

# Based on box plots, normality and equal variance assumptions appear to 
#  be reasonable in this problem.

# Need pooled standard deviation 

s.p <- as.numeric(sqrt(((n[1]-1)*s[1]^2+(n[2]-1)*s[2]^2)/(n[1]+n[2]-2)))

s.p  # pooled sample SD is 0.9730 

SE <- as.numeric( s.p * sqrt( 1/n[1] + 1/n[2] ) );  SE;

# Standard error of estimated diff in means is 0.1459

t.stat <- as.numeric( (ybar[2] - ybar[1] - 0) / SE );  t.stat;

# Observed t-ratio is 4.583 

# Diff in sample means is four-and-a-half standard errors greater than 
#  what we'd expect if the population means were equal

# For p-value, compare this t-ratio to the t-dist with df = n1 + n2 - 2 

df <- as.numeric( n[1] + n[2] - 2 ); df; 

p.value <- 1 - pt(t.stat, df=df); p.value;

# p-value is very close to zero

# If the population means were equal, the probability of obtaining 
#  a 1978 sample means that exceeds the 1976 sample mean by the 
#  amount we observed would be less than 1 in 200,000, very unlikely.  


# Do a 95% confidence interval for the mean difference

conf <- 0.95; alpha <- 1 - conf;

t.mult <- qt(1 - alpha/2, df=df); t.mult;

as.numeric(ybar[2] - ybar[1]) + c(-1,1) * t.mult * SE

# We can be 95% confident that the mean beak depth in 1978 exceeded 
#  that in 1976 by between 0.38 and 0.96 mm

# Using the R function t.test() we obtain:

test <- t.test(Depth~Year,data=case0201,alternative="less",var.equal=T)

test  # Find t.stat = 4.5833 and p.val = 4.325e-06 in the output below

CI <- t.test(Depth ~ Year, data=case0201, var.equal=TRUE)$conf.int

CI  # 95% CI for mu[1978] - mu[1976] is the negative of this
# So from 0.3807 mm to 0.9564 mm, just like we got above



###############chp2 Ex2
rm(list=ls()); library("Sleuth3");

case0202  #  These are the schizophrenia data described on page 31

# Data on 15 twins, one afflicted with schizophrenia and one not
# Response variable is volume of left hippocampus, measured by MRI

# Quantity of interest here is the difference:  Unaffected - Affected
y <- with(case0202, Unaffected - Affected)

n <- length(y);  ybar <- mean(y);  s <- sd(y); 

n; round(ybar, 3); round(s, 3);

# Test NH: mean diff = 0  
t.stat <- (ybar - 0) / (s / sqrt(n)); t.stat;

# Sample mean difference is 3.2 SEs greater than we'd expect if 
#  true mean difference were zero.
p.value <- 2 * (1 - pt(abs(t.stat), df=n-1)); p.value;

# The probability of observing such a difference, if the population 
#  mean difference were zero, would be .006.  

# So the data give fairly compelling evidence that indeed schizophrenia 
#  is associated with a smaller left hippocampus

# How much smaller?  Do a 95% CI!
conf <- 0.95; alpha <- 1 - conf;

t.mult <- qt(1 - alpha/2, df=n-1); t.mult;

CI.diff <- ybar + c(-1,1) * t.mult * s / sqrt(n)

round(CI.diff, 3)

# We are 95% confident that the left hippocampus volume of a 
#  schizophrenic is between 0.067 and 0.331 cubic cm less than 
#  that of their unafflicted twin.  

# Use the R function t.test
t.test(y)
with(case0202, t.test(Unaffected, Affected, paired=T))



###########chp2 ex2 non parametric
rm(list=ls()); library("Sleuth3"); set.seed(0202);

case0202  #  These are the schizophrenia data described on page 31

# Data on 15 twins, one afflicted with schizophrenia and one not

# Response variable is volume of left hippocampus, measured by MRI


with(case0202, t.test(Unaffected, Affected, paired=T))

# We are 95% confident that the left hippocampus volume of a 
#  schizophrenic is between 0.067 and 0.331 cubic cm less than 
#  that of their unafflicted twin.  

# See section 4.4 on nonparametric methods for paired data

# The sign test is the simplest.  If H0 is true, each diff is 
#  as likely to be positive as negative.  Take as a test 
#  statistic the number of differences that are positive.

n <- dim(case0202)[1]; n;

K <- with(case0202, sum(Unaffected - Affected > 0));  K;

# For 14 of the 15 pairs, this difference was positive

# If the null hypothesis were true we'd expect half to be.

binom.test(K, n, p=.5, alternative="two.sided")

# p-value < .001, data give compelling evidence that indeed 
#  the left hippocampus is more likely to be larger in the 
#  unaffected twin.


# The other nonparametric test introduced in section 4.4 is 
#  the Wilcoxon signed-rank test.

# Not to be confused with the Wilcoxon rank-sum test, which 
#  is applicable to data sets of two independent samples 
#  (like Ch04_Ex2).  

with(case0202, wilcox.test(Unaffected, Affected, paired=T))

# Testing against two-sided alternative get p-value = .002

# Same conclusion


# Code our own Wilcoxon signed-rank test, so we see what's 
#  going on.

Diff <- case0202$Unaffected - case0202$Affected;  Diff;

sum( (Diff > -.01) & (Diff < .01) )  # Confirm no zeros.

n <- length(Diff)

Mag <- abs(Diff); Rank <- rank(Mag); Pos <- (Diff > 0);  

Data <- data.frame(Mag=Mag, Pos=as.numeric(Pos), Rank=Rank)

Data;  rm(Diff, Mag, Rank, Pos);

Data <- Data[order(Data$Rank), ]; rownames(Data) <- 1:n;  

Data

# Question:  Why is 0.19 ranked 9 and 10?  Why not 9.5?  

# Yena Lee solved this problem for us, pointing out that to 
#  the computer, those two 0.19s were not exactly the same!

# The solution is to round the differences.  If we round to 
#  two decimal places no info is lost (since that's how the 
#  data were reported), and the miniscule difference that 
#  resulted from computer arithmetic vanishes.  

# So let's try this again.

Diff <- case0202$Unaffected - case0202$Affected;  

Diff <- round(Diff, 2);  Diff;

sum( (Diff > -.01) & (Diff < .01) )  # Confirm no zeros.

n <- length(Diff)

Mag <- abs(Diff); Rank <- rank(Mag); Pos <- (Diff > 0);  

Data <- data.frame(Mag=Mag, Pos=as.numeric(Pos), Rank=Rank)

Data;  rm(Diff, Mag, Rank, Pos);

Data <- Data[order(Data$Rank), ]; rownames(Data) <- 1:n;  

Data

# That's better.  

# Now the test

S <- with(Data, sum(Rank[Pos==1]));  S;

# We'll do a random sampling of the 2^15 possible +/- sequences

S.sim <- rep(NA, 5e5)  # Half-million simulations 

for(t in 1:length(S.sim))
{
  Pos.samp <- rbinom(n, 1, .5)
  S.sim[t] <- sum(Data$Rank[Pos.samp==1])
}

hist(S.sim, freq=F, right=F); abline(v=S, lty=2);

p.value <- 2 * mean(S.sim >= S);  p.value;

# Two-sided p-value is .0022, essentially the same thing we got
#  from wilcox.test() way up there.  


############chp3 ex1
rm(list=ls());  set.seed(0301);  

library("Sleuth3")

case0301

dim(case0301); names(case0301);

# This is the cloud seeding data set described on page 59
# Response variable is volume of rainfall

# Change the order of the levels of Treatment

Data <- case0301  # Work with data.frame called "Data" instead

table(Data$Treatment)

foo <- levels(Data$Treatment)

Data$Treatment <- factor(Data$Treatment, levels=foo[2:1])

table(Data$Treatment)

# Boxplots
boxplot(Rainfall ~ Treatment, data=Data)
# The data are right-skewed, which makes sense because most days 
#  there is little rain, and some days it rains very heavily

# A log-transformation will probably be useful

# We will use the Box-Cox method to search for the "correct" 
#  power transformation 
library(MASS)

boxcox(Rainfall ~ Treatment, data=Data)
# Now zoom in on the interesting part of this plot

boxcox(Rainfall ~ Treatment, data=Data, lambda=seq(-.5,.5,.01))
# Log transformation definitely seems the way to go

boxplot(log(Rainfall) ~ Treatment, data=Data)
# Data seems to suggest there is indeed higher rainfall on the 
#  seeded days than the unseeded

# Do a two-sample t-test (equal variance assumption seems 
#  reasonable as well)
t.test(log(Rainfall) ~ Treatment, data=Data, var.equal=T)
# p-value = 0.014 -- should we take half of this for a one-sided 
#  alternative?  

# Note that t.test returns the CI for mu1 - mu2
# I would prefer to interpret seeded minus unseeded
result <- t.test(log(Rainfall) ~ Treatment, data=Data, var.equal=T)
result$conf.int
CI <- -as.numeric(result$conf.int)[2:1]; CI; exp(CI);
# We are 95% confident that the mean log-rainfall on a seeded day is 
#  between 0.24 and 2.05 greater than that on an unseeded day.

# Better to give interpretation on original scale, if possible.
# With the log-transformation it is!

# We are 95% confident that the median rainfall on a seeded day is 
#  between 1.27 and 7.74 times that on an unseeded day.


#####################chp4 ex1
rm(list=ls());  set.seed(0401);
library("Sleuth3")
case0401
dim(case0401); names(case0401);
# We are interested in testing the null hypothesis that 
#  risk of O-ring failure is same regardless of temperature, 
#  versus the alternative that risk is higher in Cool than 
#  in Warm.

# Clearly a t-test is not appropriate, because the data are 
#  not even close to normal (discrete variable taking values 
#  0, 1, 2, 3), and the sample sizes are tiny.

# We can do a nonparameteric test, however, and get an exact 
#  p-value.

# In class we showed that the exact p-value is 0.00988

# Here we will do a permutation test by random sampling from 
#  the (24 choose 4) = 10,626 possible re-arrangements of the 
#  24 values into 4 Cool days and 20 Warm 

# The permutation test is just like the randomization test of 
#  Chapter 1 Example 1; in the context of a randomized experiment 
#  we call it a randomization test, for observational studies 
#  it's a permutation test.

# In Statistical Sleuth they use the two-sample t-stat for a 
#  test statistic.  We can do something even simpler though, 
#  just take the total number of incidents on Cool days.  
attach(case0401)

T.obs <- sum(Incidents[Launch=="Cool"]); T.obs; # there were 6 total cold failures

n.1 <- sum(Launch=="Cool");  n.2 <- sum(Launch=="Warm");

T <- rep(NA, 1e4)  # We'll do a half-million simulations 

for(s in 1:length(T)){
  samp <- sample(n.1+n.2, n.1)
  T[s] <- sum(Incidents[samp])
}

rm(s, samp)

(p.value <- mean(T >= T.obs));  # Should be close to .00988

hist(T, freq=F, right=F, breaks=seq(-0.5, 7.5, 1))

abline(v=T.obs, lty=2)

detach(case0401)


#############chp4 ex2
rm(list=ls());  set.seed(0402);  

library("Sleuth3")

case0402

dim(case0402); names(case0402);

# Reverse the levels of Treatment so Modified is first 
#  (Default in R is alphabetical order)

case0402$Treatment

foo <- levels(case0402$Treatment)

case0402$Treatment <- factor(case0402$Treatment, 
                             levels=foo[2:1]); rm(foo);

case0402$Treatment

# Done
Data <- case0402  # Call our dataframe "Data" from here on

Data <- Data[order(Data$Time), ]  # Order by Time

Data$Rank <- rank(Data$Time)

Data

rownames(Data) <- 1:(dim(Data)[1])

Data

T.obs <- sum(with(Data, Rank[Treatment=="Modified"]))

T.obs # Observed sum of M-group ranks is T.obs = 137

# Take a random sample of the (28 choose 14) possible 
#  rearrangements of 14 M's and 14 C's

T <- rep(NA, 1e4)  # We'll do a half million

n.1 <- sum(Data$Treatment=="Modified")

n.2 <- sum(Data$Treatment=="Conventional")

for(s in 1:length(T)){
  samp <- sample(n.1+n.2, n.1)
  T[s] <- sum(Data$Rank[samp])
}

rm(s, samp)

hist(T, freq=F, right=F);  abline(v=T.obs, lty=2);

p.value <- mean(T <= T.obs);  p.value;

# p-value < .001, so pretty strong evidence that the 
#  modified method leads to faster solutions on average


# Compare this to the p-value based on Normal approximation

# Note!  That's normal approximation to the sampling dist of 
#  T = sum(Ranks), not to the data! 

# We're still doing nonparameterics here!

R <- Data$Rank;  R.bar <- mean(R);  s.R <- sd(R);  rm(R); 

pnorm(T.obs+0.5,mean=n.1*R.bar,sd=s.R*sqrt(n.1*n.2/(n.1+n.2)))

# p.value is approximately .0013

# The R function wilcox.test() does this calculation for us

wilcox.test(Time ~ Treatment, alternative="less", data=Data)

# Note that R reports a test statistic of W = 32 instead of 
#  our T = 137.

# The W-statistic takes the sum of the ranks and subtracts off 
#  the min possible value, which is n.1*(n.1+2)/2 = 105


# We can also get a confidence interval for the difference in 
#  means.  It assumes that the distributions have the same 
#  shape and only a different location, i.e., a "shift model"

wilcox.test(Time ~ Treatment, data=Data, conf.int=T)

# We estimate that the Modified teaching method reduces the 
#  average solution time by between 57 and 160 seconds

# Though the upper bound of this interval is not reliable, 
#  owing to the censoring in the study design. 


################chp5 ex1
library("Sleuth3");  rm(list=ls());  
case0501

# Diet and longevity (rats) example, page 114
dim(case0501); names(case0501);
boxplot(Lifetime ~ Diet, data=case0501)


# Re-order the factor levels
foo <- levels(case0501$Diet)

foo <- foo[c(4,1,3,5,6,2)]

case0501$Diet <- factor(case0501$Diet, levels=foo); rm(foo);

boxplot(Lifetime ~ Diet, data=case0501)

# That's the figure we wanted!
# There is indeed some indication that reduced calorie 
#  diets lead to greater longevity (in rats)

# Power transformation?
library(MASS); boxcox(Lifetime ~ Diet, data=case0501)

# Box-Cox suggests a power greater than 1, this is because 
#  of the low outliers (long right tail)

# I would not transform these data though
boxplot(Lifetime ~ Diet, data=case0501)

# Fit the one-way ANOVA model with 6 separate means, then 
#  take a look at the diagnostic plots -- residuals versus 
#  fitted values, and normal probabilty plot of residuals
m1 <- lm(Lifetime ~ 0 + Diet, data=case0501)  

# The 0 means no-intercept model, thus the estimated effects 
#  will be the sample means
summary(m1)

# See, kinda nice this way.  R-squared is not meaningful though.
# Model checking plots
op <- par(mfrow=c(1,2))
plot(resid(m1) ~ fitted(m1));  qqnorm(resid(m1));
par(op)
# There's that short right tail, that's why Box-Cox was suggesting 
#  a power greater than 1.  I'm not terribly worried.


# Do confidence intervals for the individual means, like shown in 
#  Dispay 5.2 on page 115
n <- with(case0501, tapply(Lifetime, Diet, length))

Avg <- with(case0501, tapply(Lifetime, Diet, mean))

SD <- with(case0501, tapply(Lifetime, Diet, sd))

round(cbind(n, Avg, SD), 2)

# Individual 95% confidence intervals for the treatment means

# Method 1:  Ybar[i] +/- t.mult * s[i] / sqrt(n[i])

t.mult <- qt(.975, df=n-1);  t.mult;

LB.0 <- Avg - t.mult * SD / sqrt(n); 

UB.0 <- Avg + t.mult * SD / sqrt(n); 

round(cbind(n, Avg, SD, LB.0, UB.0), 2)

# Display 5.2 on page 115

# We would get narrower intervals (on average) using pooled SD

(df <- sum(n-1)); (s.p <- sqrt(sum((n-1)*SD^2) / sum(n-1)))

# Method 2:  Ybar[i] +/- t.mult * s_p / sqrt(n[i])

t.mult <- qt(.975, df=df);  t.mult;

LB.1 <- Avg - t.mult * s.p / sqrt(n); 

UB.1 <- Avg + t.mult * s.p / sqrt(n); 

round(cbind(n, Avg, s.p, LB.1, UB.1), 2)

# This is not Display 5.2 on page 115.  These CIs use the pooled 
#  SD, the Right Thing if we're assuming constant variance.

# Note the group means are the model parameters in m1.  So

round(confint(m1), 2)  #  gives the same CIs!


# Finally let's do individual 95% CIs for the five pairwise 
#  differences of interest:

# A = mu[3] - mu[2]
# B = mu[4] - mu[3]
# C = mu[6] - mu[3]
# D = mu[5] - mu[3]
# E = mu[2] - mu[1]

# Ybar[r] - Ybar[s] +/- t.mult * s.p * sqrt(1/n[r] + 1/n[s])

CI <- function(r,s)
{ 
  Avg[r] - Avg[s] + c(-1,1) * t.mult * s.p * sqrt(1/n[r] + 1/n[s])
}

A <- CI(r=3, s=2)

B <- CI(r=4, s=3)

C <- CI(r=6, s=3)

D <- CI(r=5, s=3)

E <- CI(r=2, s=1)

CI.diffs <- rbind(A, B, C, D, E); 

colnames(CI.diffs) <- c("LB", "UB");

round(CI.diffs, 2)

# See interpretation of these results in the "Statistical 
#  Conclusion" section on page 116

























