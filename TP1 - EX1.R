iris
class(iris)

View(iris)

nrow(iris)

ncol(iris)

colnames(iris)

summary(iris)

iris[,c("Sepal.Length", "Species")]

iris[50:100,]

mean(iris$Sepal.Length)

median(iris$Sepal.Length)

sd(iris$Petal.Length)

quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))