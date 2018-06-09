task1 <- read_excel("Desktop/task1.xlsx")
num <- task1$`№`
before <- task1$Before
after <- task1$After

plot(num,before,type='o',pch='o',col='blue',lty=1)
points(num,after,pch='+',col='red')
lines(num,after,col='red',lty=2)

legend(1,700,legend=c('before','after'),col=c('blue','red'),pch=c('o','+'),lty=c(1,2))
