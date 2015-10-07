library(plyr)
library(dplyr)
library(ggplot2)

set.seed(2015)

n <- 250
test1 <- data_frame(x = 2*rnorm(n), y = rnorm(n)^2+1, cluster = 1)
test2 <- data_frame(x = 2*rnorm(n), y = rnorm(n)^2+5, cluster = 2)

c<-rbind(test1,test2)

ggplot(c, aes(x, y)) + geom_point()

####

library(broom)

plot_kmeans <- function(dat, k) {
        clust <- dat %>% ungroup %>% dplyr::select(x, y) %>% kmeans(k)
        ggplot(broom::augment(clust, dat), aes(x, y)) + geom_point(aes(color = .cluster)) +
                geom_point(aes(x1, x2), data = tidy(clust), size = 10, shape = "x") +
                labs(color = "K-means assignments")
}

plot_kmeans(c, 2)


####

clusters<-kmeans(x=c[,1:2], centers=2)
#plot.new()
plot(x=c[,1:2], col=clusters$cluster)
points(clusters$centers, col=1:5, pch=8)
#plot.new()

