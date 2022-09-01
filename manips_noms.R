setwd("~/Documents/Data/survey_graoully/")

library(bandit4abtest)
library(varhandle)

rm(list=ls())

data0 = read.csv("raw_data.csv")

## Traitement donnees ##

data = data0[2:nrow(data0),]

rewards = data[,16:ncol(data)]
rewards = (-1)*rewards

# Transformation des noms en binaire #

binary_names = to.dummy(data$name, "names")
dt = as.data.frame(binary_names)
dt$duration = data$duration

# Linucb #

linucb = LINUCB(dt, visitor_reward = rewards)
cum_reg_linucb = cumulativeRegretAverage(linucb$choice, rewards)

liste_choix = as.vector(linucb$choice)

regret_final = max(cum_reg_linucb)

linucb$theta_hat
linucb$theta

liste_choix



# Verifier si recompenses stationnaires + melanger donnees.

### Shuffling datas ###

set.seed(42)
rows = sample(nrow(dt))
dt1 = dt[rows,]
rewards1 = rewards[rows,]

linucb1 = LINUCB(dt1, visitor_reward = rewards1)
cumulativeRegretAverage(linucb1$choice, rewards1)

linucb1$theta_hat
linucb1$theta

linucb1$choice

### Duplication et shuffling dataframe ###

set.seed(10)
dt0 = dt
for (i in 1:4) {
  dt0 = rbind(dt0,dt0)
}

rows = sample(nrow(dt0))
df = as.data.frame(dt0[rows,])
rewards0 = rewards[rows,]

nrow(df)
linucb0 = LINUCB(df, visitor_reward = rewards0)

p = list(1,5,4,3)
which.max(p)

S = matrix(rep(0,2*4), nrow = 2, ncol = 4)
colnames(S) <- paste('bandit', 1:4)
rownames(S) <- c("average reward","trials")


S = matrix(1:9, nrow = 3, ncol = 3)
S
c(1:3)[-which.max(S[1,])]

sample(c(1:3)[-which.max(S[1,])] , 1)



#### Epsilon greedy :

epsilon_greedy_alloc = EpsilonGreedy(rewards, epsilon = 0.1)
