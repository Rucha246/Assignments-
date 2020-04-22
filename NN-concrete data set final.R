str(concrete)

summary(concrete)
library(ggplot2)
library(reshape2)
library(gridExtra)
melted_concrete = melt(concrete)
tail(melted_concrete)
qplot(x=value, data=melted_concrete) + facet_wrap(~variable, scales='free')
normalise = function(x) {
  return( (x-min(x)) / (max(x) - min(x)))
}
concrete_norm = as.data.frame(lapply(concrete, normalise))
summary(concrete_norm)
 # splitting up data in train and test dataset 
c.train = concrete_norm[1:773, ]
c.test  = concrete_norm[774:1030, ]
# loading requred packege
install.packages(grid)
install.packages("MASS")
install.packages("neuralnet")
install.packages("NeuralNetTools")
# Let's start by building the simplest possible multilayer feedforward network with only one single hidden node
c.model = neuralnet::neuralnet(data=c.train,
                    strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age)
# Let's see the coeffs:
NeuralNetTools::plotnet(c.model)


# Let's now run on the test set
model.results = neuralnet::compute(c.model, c.test[1:8])

# model.results is a list of 2 components: neurons and net.results. We want the latter
predicted_strength = model.results$net.result

# because this is a numeric prediction, we can't use a confusion matrix, so we'll use cor() instead
cor(predicted_strength, c.test$strength)# 0.8063693

c.model2 = neuralnet::neuralnet(data=c.train,
                     strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                     hidden = 5)

NeuralNetTools::plotnet(c.model2)

model.results2 = neuralnet::compute(c.model2, c.test[1:8])
cor(model.results2$net.result, c.test$strength) 0.9339573



