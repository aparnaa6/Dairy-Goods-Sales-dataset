library(shiny)
library(gsheet)
library(tidyverse)
library(corrplot)
library(plotly)
library(FactoMineR)
library(ggplot2)
dairy<-read.csv("d:/R/dairy_dataset.csv")
View(dairy)
library(dbplyr)
#Remove columns
dairy<-select(dairy,-c('Date','Product.ID','Production.Date','Expiration.Date'))
View(dairy)
summary(dairy)

#Visualization
bar_plot <- ggplot(dairy, aes(x = Brand, y = Approx..Total.Revenue.INR.)) +
  geom_bar(stat = "identity", fill="maroon") +
  labs(title = "Bar Plot of Total Revenue by Brand") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Show the plot
print(bar_plot)

bar_plot <- ggplot(dairy, aes(x = Storage.Condition, y = Approx..Total.Revenue.INR.)) +
  geom_bar(stat = "identity", fill="orange") +
  labs(title = "Bar Plot of Total Revenue by Storage Condtion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Show the plot
print(bar_plot)

bar_plot <- ggplot(dairy, aes(x = Product.Name, y = Approx..Total.Revenue.INR.)) +
  geom_bar(stat = "identity", fill="pink") +
  labs(title = "Bar Plot of Total Revenue by Product name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Show the plot
print(bar_plot)

bar_plot <- ggplot(dairy, aes(x = Location, y =Approx..Total.Revenue.INR. )) +
  geom_bar(stat = "identity", fill="lightgreen") +
  labs(title = "Bar Plot of Total Revenue by Location") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Show the plot
print(bar_plot)
# Calculate the counts of each sales channel
sales_channel_counts <- table(dairy$Sales.Channel)

# Create a pie chart
pie(sales_channel_counts,
    labels = names(sales_channel_counts),
    main = "Distribution of Sales Channels",
    col = rainbow(length(sales_channel_counts)),
    cex = 0.8,  # Adjust label size
    clockwise = TRUE,
    radius = 1)
# Add percentage labels
percent_labels <- round(100 * sales_channel_counts / sum(sales_channel_counts), 1)
label <- paste(names(sales_channel_counts), "\n", percent_labels, "%", sep = "")
legend("topright", legend = label, cex = 0.8, bty = "n")



#converting categorical to numerical
dairy$Farm.Size <- as.integer(factor(dairy$Farm.Size))
dairy$Location <- as.integer(factor(dairy$Location))
dairy$Product.Name <- as.integer(factor(dairy$Product.Name))
dairy$Brand <- as.integer(factor(dairy$Brand))
dairy$Storage.Condition <- as.integer(factor(dairy$Storage.Condition))
dairy$Customer.Location <- as.integer(factor(dairy$Customer.Location))
dairy$Sales.Channel <- as.integer(factor(dairy$Sales.Channel))

#Scale
dairy<-scale(dairy,center = FALSE)
dairy
dairy<-as.data.frame(dairy)
#Model
library(caTools)
set.seed(123)
newdairy<-sample.split(dairy$Approx..Total.Revenue.INR.,SplitRatio = 0.7)
train_data<-subset(dairy, newdairy == TRUE)
test_data<-subset(dairy, newdairy == FALSE)
summary(dairy)
model<-lm(Approx..Total.Revenue.INR.~.,train_data)
summary(model)

pred<-predict(model,train_data)
pred


