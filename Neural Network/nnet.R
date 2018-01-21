library(neuralnet)
data("infert")
set.seed(1988)
nn<-neuralnet(case~age+parity+induced+spontaneous,data=infert,hidden=2,err.fct='ce',linear.output = FALSE)
#hidden=2 implies a single hidden layer with 2 neurons
nn$result.matrix
plot(nn)
# blue lines indicate the biases
# threshold is the difference between successive iterations default is 0.01
nn$covariate #inputs for all the cases
nn$net.result # gives the values for the calculated weights
nn$response # actual values for target class
out<-cbind(nn$covariate,nn$net.result[[1]],nn$response)
colnames(out)<-c('Age','Parity','Induced','Spontaneous','nn-output','Original Response')
out<-cbind(out,ifelse(out[,5]<0.5,0,1))
err<-mean(abs(out[,6]-out[,7]))
err
# learning rate should ideally be slow so that the algorithm learns slowly and accurately
nn2<-neuralnet(case~age+parity+induced+spontaneous,data=infert,hidden=2,err.fct='ce',linear.output = FALSE,algorithm = 'backprop',learningrate = 0.01)
