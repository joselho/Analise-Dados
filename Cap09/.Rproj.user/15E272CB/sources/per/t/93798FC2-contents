bina <- as.data.frame(replicate(10, sample(c(0,1), 15, replace = T)))
bina

d1 <- dist(bina, method = "binary")
d2 <- daisy(bina, metric = "gower")

hc <- hclust(d2, method = "average")
plot(hc, cex = 0.6, hang = -1)
rect.hclust(hc, k = 3)
gr <- cutree(hc, k = 3)
table(gr)

aggregate(bina, by=list(gr), mean)
