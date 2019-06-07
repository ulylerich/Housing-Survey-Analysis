# Read csv file
library(readxl)
feedback <- read_excel("Clean Housing Affordability  Engagement Survey.xls")

#check data structures
str(feedback)

#transform varaiables
feedback$Cost_of_rental_housing_concerns <- as.factor(feedback$Cost_of_rental_housing_concerns)
feedback$Quality_of_rental_housing_concern <- as.factor(feedback$Quality_of_rental_housing_concern)
feedback$Location_of_rental_housing_concern <- as.factor(feedback$Location_of_rental_housing_concern)
feedback$Ability_to_get_a_mortgage_concern <- as.factor(feedback$Ability_to_get_a_mortgage_concern)
feedback$Availability_of_homes_to_buy_concern <- as.factor(feedback$Availability_of_homes_to_buy_concern)
feedback$Preserving_neighborhood_character_concern <- as.factor(feedback$Preserving_neighborhood_character_concern)
feedback$Housing_options_workshop <- as.factor(feedback$Housing_options_workshop)
feedback$Community_safety_workshop <- as.factor(feedback$Community_safety_workshop)
feedback$Gentrification_workshop <- as.factor(feedback$Gentrification_workshop)
feedback$History_of_housing_discrimination_workshop <- as.factor(feedback$History_of_housing_discrimination_workshop)
feedback$Land_use_zoning_workshop <- as.factor(feedback$Land_use_zoning_workshop)
feedback$Student_housing_workshop <- as.factor(feedback$Student_housing_workshop)
feedback$Homelessness_workshop <- as.factor(feedback$Homelessness_workshop)
feedback$Environmental_concerns_workshop <- as.factor(feedback$Environmental_concerns_workshop)
feedback$Transportation_workshop <- as.factor(feedback$Transportation_workshop)
feedback$Home_ownership_workshop <- as.factor(feedback$Home_ownership_workshop)
feedback$Zip_Code <- as.factor(feedback$Zip_Code)
feedback$Districts <- as.factor(feedback$Districts)
feedback$household_expenditure <- as.numeric(feedback$household_expenditure)
feedback$Personnal_Expenditure <- as.numeric(feedback$Personnal_Expenditure)
feedback$income_percent_mortgage <- as.factor(feedback$income_percent_mortgage)
feedback$Cost_burdened <- as.factor(feedback$Cost_burdened)

#plot liker scale for the survey
library(devtools)
library(ggplot2)
library(xtable)
library(likert)

#plot  housing issues only
mytitle <- "How interested are you in learning more about the following issues?"
mylevels <- factor(c("Very interested", "Somewhat interested", "Not interested", "I don't know what this is"))
#subset data for issues only
feedbackwork <- feedback[c(8:16)]

feed <-likert(as.data.frame(feedbackwork)) 

#plot
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

p1 <- plot(feed) + ggtitle(mytitle)

p2 <- plot(feed, type = 'heat') + ggtitle(mytitle) + 
  theme(legend.position = 'none')
p1
p2

#plot liker scale Housing Concerns
mytitle1 <- "What are your biggest housing Concerns?"
mylevels1 <- factor(c("Very concern", "Somewhat concern", "Not concern", "I don't know what this is"))

#subset housing concern data only
feedbackcon <- feedback[c(1:6)]

feed1 <-likert(as.data.frame(feedbackcon)) 

#plot
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

p3 <- plot(feed1, center = 2.5) + ggtitle(mytitle1)

p4 <- plot(feed1, type = 'heat') + ggtitle(mytitle1) + 
  theme(legend.position = 'none')
p3
p4

#subset concerns only and plot top 4 with ggplot
feedback3 <- feedback[c(1:6,19,20,23,24)]

#plot top 4 by percent of income spent on mortgage
p9 <- ggplot(feedback3, aes(x = Cost_of_rental_housing_concerns, fill = income_percent_mortgage))+
  geom_bar(width = 0.5,stat = "count")+
  xlab("Cost of Rental Housing Concern")+
  ylab("Total count")+
  labs(fill="Income Percent mortgage") + 
  theme(legend.position = "none")

p10 <- ggplot(feedback3, aes(x = Quality_of_rental_housing_concern, fill = income_percent_mortgage))+
   geom_bar(width = 0.5,stat = "count")+
   xlab("Quality of Rental House Concern")+
   ylab(" ")+
   labs(fill="Income Percent mortgage")+
   theme(legend.position = "none")
 
p11 <- ggplot(feedback3, aes(x = Preserving_neighborhood_character_concern, fill = income_percent_mortgage))+
   geom_bar(width = 0.5,stat = "count")+
   xlab("Preserving neighborhood character concern")+
   ylab("Total count")+
   labs(fill="Income Percent mortgage")+
   theme(legend.position = "none")
 
p12 <- ggplot(feedback3, aes(x = Location_of_rental_housing_concern, fill = income_percent_mortgage))+
   geom_bar(width = 0.5,stat = "count")+
   xlab("Location of rental housing concern")+
   ylab(" ")+
   labs(fill="Income Percent mortgage")+
   theme(legend.position = "right")
 
 #combine graph
 grid.arrange(p9,p10,p11,p12, ncol = 2, top = "Top 4 Housing Concerns By Percent of Income spend on Mortgage")
 

#subset only income pecent, cost burden and zip codes and omit na
feedback4 <- feedback[c(19,23,24)]

library(dplyr)
feedbackgroup <- group_by(feedback4, income_percent_mortgage,Zip_Code, Cost_burdened)
feedbackcount <- summarise(feedbackgroup, count = n())
 
#graph income percent mortgage
ggplot(feedbackcount, aes(x = income_percent_mortgage, y = count))+
  geom_bar(stat = "identity", fill = "purple") + 
  xlab(" ")+
  ylim(c(0,150))+
  ylab("Value")

#Combine Cost burdened vs not cost burdened using income percent mortgage and graph with ggplot 
feedbackcount$pct <- feedbackcount$count/sum(feedbackcount$count)
ggplot(feedbackcount, aes(x = Cost_burdened, y = count))+
  geom_bar(stat = "identity", fill = "purple") + 
  ylim(c(0,100))+
  xlab(" ")+
  ylab("Value")


#graph household expenditure
feedback5 <- feedback[c(19,21,23,24)]
hist(feedback5$household_expenditure, col = "light blue", ylim = c(0,100), main = "Housing Household Expenditure", 
     xlab = "Expenditure", ylab =  "value")

#subset cost burden and check interest
feedbackcostb <- subset(feedback, Cost_burdened == "Cost-burdened")
feedbackcostb1 <- feedbackcostb[c(8:16)]
mytitle2 <- "How interested are you in learning more about the following issues?burdened"
mylevels2 <- factor(c("Very interested", "Somewhat interested", "Not interested", "I don't know what this is"))

feedcostb <-likert(as.data.frame(feedbackcostb1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedcostb) + ggtitle(mytitle2)

plot(feedcostb, type = 'heat') + ggtitle(mytitle2) + 
  theme(legend.position = 'none')

#subset non-cost burdened and check interest
feedbackcostnb <- subset(feedback, Cost_burdened == "Not cost-burdened")
feedbackcostnb1 <- feedbackcostnb[c(8:16)]
mytitle3 <- "How interested are you in learning more about the following issues?nburdened"
mylevels3 <- factor(c("Very interested", "Somewhat interested", "Not interested", "I don't know what this is"))

feedcostnb <-likert(as.data.frame(feedbackcostnb1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedcostnb) + ggtitle(mytitle3)

plot(feedcostnb, type = 'heat') + ggtitle(mytitle3) + 
  theme(legend.position = 'none')

#subset this doesn't apply to me and check interest
feedbackcostna <- subset(feedback, Cost_burdened == "This doesn't apply to me")
feedbackcostna1 <- feedbackcostna[c(8:16)]
mytitle4 <- "How interested are you in learning more about the following issues?napply"
mylevels4 <- factor(c("Very interested", "Somewhat interested", "Not interested", "I don't know what this is"))

feedcostna <-likert(as.data.frame(feedbackcostna1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedcostna) + ggtitle(mytitle4)

plot(feedcostna, type = 'heat') + ggtitle(mytitle4) + 
  theme(legend.position = 'none')

#subset non cost burdened and plot concern 

feedbackconnb <- subset(feedback, Cost_burdened == "Not cost-burdened")
feedbackconnb1 <- feedbackconnb[c(1:6)]

mytitle5 <- "What are your biggest housing Concerns?nburdened"
mylevels5 <- factor(c("Very concern", "Somewhat concern", "Not concern", "I don't know what this is"))

feedconnb <-likert(as.data.frame(feedbackconnb1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedconnb, center = 2.5) + ggtitle(mytitle5)

plot(feedconnb, type = 'heat') + ggtitle(mytitle5) + 
  theme(legend.position = 'none')

#subset cost burdened and plot concern
feedbackconb <- subset(feedback, Cost_burdened == "Cost-burdened")
feedbackconb1 <- feedbackconb[c(1:6)]

mytitle6 <- "What are your biggest housing Concerns?burdened"
mylevels6 <- factor(c("Very concern", "Somewhat concern", "Not concern", "I don't know what this is"))

feedconb <-likert(as.data.frame(feedbackconb1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedconb, center = 2.5) + ggtitle(mytitle6)

plot(feedconb, type = 'heat') + ggtitle(mytitle6) + 
  theme(legend.position = 'none')

# subset this doesn't apply and plot concern
feedbackcona <- subset(feedback, Cost_burdened == "This doesn't apply to me")
feedbackcona1 <- feedbackcona[c(1:6)]

mytitle7 <- "What are your biggest housing Concerns?napply"
mylevels7 <- factor(c("Very concern", "Somewhat concern", "Not concern", "I don't know what this is"))

feedcona <-likert(as.data.frame(feedbackcona1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedcona, center = 2.5) + ggtitle(mytitle7)

plot(feedcona, type = 'heat') + ggtitle(mytitle7) + 
  theme(legend.position = 'none')

#subset very interested in environmental worshop only and plot their concern
feedbackenv <- subset(feedback, Environmental_concerns_workshop == "Very interested")
feedbackenv1 <- feedbackenv[c(1:6)]

mytitle8 <- "What are your biggest housing Concerns?very interested in env"
mylevels8 <- factor(c("Very concern", "Somewhat concern", "Not concern", "I don't know what this is"))

feedenv <-likert(as.data.frame(feedbackenv1)) 
#plot issues only
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

plot(feedenv, center = 2.5) + ggtitle(mytitle8)

plot(feedenv, type = 'heat') + ggtitle(mytitle8) + 
  theme(legend.position = 'none')

#check cost burden of people very interested in environ workshop
feedbackenv2 <- feedbackenv[c(19,21,23,24)]
feedbackenvgroup <- group_by(feedbackenv2, income_percent_mortgage,Zip_Code, Cost_burdened)
feedbackenvcount <- summarise(feedbackenvgroup, count = n())

#graph cost burdened vs not cost burduned for very interest in environ workshop
ggplot(feedbackenvcount, aes(x = Cost_burdened, y = count))+
  geom_bar(stat = "identity", fill = "orange") + 
  ylim(c(0,100))+
  xlab("People very interested in environment workshop")+
  ylab("Value")
  
#bar graph of cost burdened and include percentage in the y_axis with percent label
library(scales)
ggplot(feedback, aes(x = Cost_burdened))+
  geom_bar((aes(y = (..count..)/sum(..count..))), fill = "purple") + 
  geom_text(aes(y = ((..count..)/sum(..count..)),
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  xlab(" ") + 
  ylab("Percentage")
