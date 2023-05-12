m = read.csv("C:/Users/Payal/Downloads/time_series.dat", header = FALSE)
x1 = as.matrix(m[2:(nrow(m) - 1),])
x2 = as.matrix(m[1:(nrow(m) - 2),])
y = as.matrix(m[3:(nrow(m)),])
n = nrow(m)
X = cbind(x1, x2)
a = solve(t(X) %*% X , t(X) %*% y)   #estimating alpha1 and alpha2
y_hat = X %*% a
error = (y - y_hat)
sse=error*error      #calculating sum of squared error
var_sse=var(sse)     #calculating variance of SSE
cat("Alpha1 = ", a[1], "\nAlpha2 = ", a[2], "\nVariance of error = ", var_sse, "\n")