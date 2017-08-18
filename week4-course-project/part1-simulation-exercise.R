set.seed(2309)
sim <- rexp(1000, 0.2)
simsam <- matrix(nrow = 1000, ncol = 40)
for (i in 1:1000) {
    simsam[i,] = sample(sim, 40, replace = FALSE)
}


means <- apply(simsam, 1, mean)
hist(means)
plot(density(means))
mean(means)
mean(sim)
var(means)
var(sim)

vars <- apply(simsam, 1, var)
mean(vars)
var(sims)
hist(vars)
plot(density(vars))
abline(v = 25)


