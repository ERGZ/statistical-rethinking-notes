library(ggplot2)

grid_size <- 1000
p_grid <- seq(0, 1, length.out = grid_size)
prior <- rep(1, grid_size)/grid_size
ll <- dbinom(6, 9, p_grid)
post <- ll * prior
post <- post/sum(post)

qplot(x=p_grid, y=post, geom="line")

# we can sample from this

post_sample <- sample(p_grid, prob=post, size = 10000, replace = TRUE)
qplot(x=post_sample, geom="histogram")

# what post probability lies below p 0.5
sum(post[p_grid < 0.5])
sum(post_sample < 0.5)/10000

sum(post_sample > 0.1 & post_sample < .7)/10000


# get range for defined mass
quantile(post_sample, .8)
quantile(post_sample, c(0.1, .9))
HPDI(post_sample, prob = 0.5)


# MAP
p_grid[which.max(post)]
chainmode(post_sample, adj=0.01)
mean(post_sample)
median(post_sample)


dbinom(0:2, size=2, prob=.7)

# generating observations
w <- rbinom(10000, size=9, prob=post_sample)

# view distribute of samples
qplot(x=w, geom="histogram")
