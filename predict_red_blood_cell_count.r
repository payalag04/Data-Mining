m=read.csv("C:/Users/Payal/Downloads/ais.csv",stringsAsFactors=FALSE, sep=",")
head(m)
plot(m)
X = as.matrix(m[,3:12]);   # X has predictor variables 3 to 12
X
y = as.vector(m[,2]);      # Y has the 2nd variable which is considered as response
y
cat("===========(a)==========");
ahat = solve(t(X) %*% X, t(X) %*% y)   # formula for normal equation/ generic method for regression
ahat
# calculating a and b 
n=length(X)
xbar = sum(X)/n
ybar = sum(y)/n;
xybar = sum(X*y)/n
xsqbar = sum(X*X)/n
b = (ybar*xsqbar-xbar*xybar)/ (xsqbar - xbar*xbar)  
a = (ybar - b)/xbar
a #[1] 0.08807811
b #[1] -4.523169
cat("a = ", a , "b = ", b, "\n");

cat("===========(b)==========");

# calculation of sse
yhat = X %*% ahat	 # contains the predictions of y values
yhat      
error = y - yhat
error
sse = sum(error*error) 
sse #[1] 5.909294

cat("==========(c)===========");

n = nrow(X);
n
X1 = as.matrix(m[,3:12]);  # X1 has only 3:12 predictor variables
head(X1)
p=as.matrix(colnames(m[,3:12]))
p
for (d in 1:10) {
Z=as.matrix(X1[,-d])
a = solve(t(Z) %*% Z, t(Z) %*% y)
yhat = Z %*% a
error = y - yhat
sse = sum(error*error)
cat("with ", p[d], " variable not considered, sse is ", sse, "\n");
}
