#------------------------#
# GLM and NR assigment 2 #
#------------------------#

library(MASS)

data(ships)
ships = ships[ships$service != 0,]
ships$year = as.factor(ships$year)
ships$period = as.factor(ships$period)

set.seed(11)
n = floor(0.8 * nrow(ships))
index = sample(seq_len(nrow(ships)), size = n)

train = ships[index, ]
test = ships[-index, ]
head(train)
summary(train)

# Poisson Regression Model
poisson_model = glm(incidents ~ type + period + year + offset(log(service)), 
                    data = train, 
                    family = poisson)

# This is a count model

# Prediction and MSPE 
test$predicted_incidents = predict(poisson_model, newdata = test, type = "response")
MSPE = mean((test$incidents - test$predicted_incidents)^2)
MSPE
