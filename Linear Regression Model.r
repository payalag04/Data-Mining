X= read.table("C:/Users/Payal/Downloads/pred1.dat.txt", sep ="", header=FALSE);
Y= read.table("C:/Users/Payal/Downloads/resp1.dat.txt", sep ="", header=FALSE);

X_half1<-X[1:500,] #dividing pred1 into first half
Y_half1<-Y[1:500,] #dividing resp1 into first half
X_half2<-X[501:1000,]  #dividing pred1 into second half
Y_half2<-Y[501:1000,]  #dividing resp1 into second half
x1<- data.matrix(X_half1)
y1<- data.matrix(Y_half1)
x2<- data.matrix(X_half2)
y2<- data.matrix(Y_half2)
w = solve(t(x1) %*% x1, t(x1) %*% y1) #estimate of w produced by solving the normal equations, ˆw.


#4b
#using estimate of w on the 2nd half of the data set, we get estimated response variables, ˆy
m=matrix(0, nrow=500, ncol=1);
yhat=matrix(0, nrow=500, ncol=1);
yhat = x2 %*% w
for (i in 1:500) 
{ 
    m[i]=yhat[i]-y2[i]
    }
err=sum(m*m);
err   #5.721507


#pred1 and resp2

X= read.table("C:/Users/Payal/Downloads/pred2.dat.txt", sep ="", header=FALSE);
Y= read.table("C:/Users/Payal/Downloads/resp2.dat.txt", sep ="", header=FALSE);

X_half1<-X[1:500,] #dividing pred1 into first half
Y_half1<-Y[1:500,] #dividing resp1 into first half
X_half2<-X[501:1000,]  #dividing pred1 into second half
Y_half2<-Y[501:1000,]  #dividing resp1 into second half
x1<- data.matrix(X_half1)
y1<- data.matrix(Y_half1)
x2<- data.matrix(X_half2)
y2<- data.matrix(Y_half2)
w = solve(t(x1) %*% x1, t(x1) %*% y1) #estimate of w produced by solving the normal equations, ˆw.


#4b
#using estimate of w on the 2nd half of the data set, we get estimated response variables, ˆy
m=matrix(0, nrow=500, ncol=1);
yhat=matrix(0, nrow=500, ncol=1);
yhat = x2 %*% w
for (i in 1:500) 
{ 
    m[i]=yhat[i]-y2[i]
    }
err=sum(m*m);
err  #32984664




