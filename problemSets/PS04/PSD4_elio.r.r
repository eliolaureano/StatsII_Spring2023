# we install the library
install.packages("eha")


library(eha)


library(survival)


# we get the data set
data <- infants

# Fit the Cox proportional hazards model using the "coxph" function
coxph_model <- coxph(Surv(enter,exit, event) ~ age + sex, data = data)

# we obtain the statistics of the model
summary(coxph_model)

# Interpretación de los coeficientes
cat("The mother's age is inversely related to the risk of infant death, with a decrease of", round((exp(coef(coxph_model)["age"]) - 1) * 100, 2), "% in risk for each additional year of age.\n")
cat("Male infants have a", round((1 - exp(coef(coxph_model)["sexboy"])) * 100, 2), "% lower risk of death than female infants.\n")


