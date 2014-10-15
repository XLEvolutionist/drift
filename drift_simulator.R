#a simple function to mimic genetic drift.
drift<-function(N=20,p=0.5,gen=20,reps=10, w=1) {
	#set up a matrix to fill
	m<-matrix(nrow = gen, ncol = reps, byrow = T)
	#for each rep
	for ( j in 1:reps ) {
		p1<-p
		#for each generation
		for ( i in 1:gen) {
			A<-rbinom(1,size=2*N,p1)
			p1<-A/(2*N)
			p1<-p1*w
			if ( p1 > 1) {p1=1}
			m[i,j]<-p1
		}#
	}#for
	#for each column find if it's fixed or lost
	lost<-do.call(pmin, lapply(1:nrow(m), function(i)m[i,]))
	fixed<-do.call(pmax, lapply(1:nrow(m), function(i)m[i,]))
	lost<-length(lost[lost==0])
	fixed<-length(fixed[fixed==1])
	xscale<-seq(0,dim(m)[1])
	m<-rbind(rep(p,dim(m)[2]),m)
	pl<-plot(xscale,m[,1], ylim = c(0,1), type = "n", ylab = "Allele frequency",
		xlab="generations", main = paste("f(A)=",p ,"; Population size=",N , "; lost=", lost, "; fixed=" , fixed, "; w = ", w), xlim =c(0,dim(m)[1]))
	pl<-matlines(xscale, m, col = "black", lty = 1, lwd = 0.6)
	ave<-rowMeans(m)
	pl<-lines(xscale, ave , col = "red", lwd = 2)
	z<-list("matrix"=m,
	"plot"=pl, "mean"=ave)
	return(z)
}#drift

#test<-drift(reps = 10, p =0.5, gen=30, N=500)

