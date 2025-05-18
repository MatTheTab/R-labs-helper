setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("mileage.csv", sep=";")
data

plane = lm(mileage~octane + speed, data=data)
plane

predict(plane, data.frame(octane=90,speed=60))

# If determination coefficient is 80% or bigger -> good, else, look for something better