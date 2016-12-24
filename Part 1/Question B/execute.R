library(ggplot2)

mushroom <- read.csv("mushrooms.csv", header = TRUE, na.strings = "?")


capShapePlot <- ggplot(mushroom,aes(x=factor(cap.shape, labels = c("Bell", "Conical", "Flat", "Knobbed", "Sunken", "Convex")),
fill=factor(class, labels = c("Edible", "Poisonous")))) + geom_bar(position = "dodge", width = 0.5) + 
        xlab("Cap Shape") + ylab("Total Count") + labs(fill = "Mushroom Class") + 
        scale_fill_manual(values=c("green3", "red3"))

bruisesPlot <- ggplot(mushroom, aes(x = factor(bruises, labels = c("No bruises", "Bruises")), fill = 
                                            factor(class, labels = c("Edible", "Poisonous")))) +
        geom_bar(position = "dodge", width = 0.5) +
        xlab("Bruises") +
        ylab("Total Count") +
        labs(fill = "Mushroom Class") +
        scale_fill_manual(values=c("green3", "red3"))

capPlot <- ggplot(mushroom, aes(x = factor(cap.color, labels = c("Buff", "Cinnamon", "Red", "Gray", "Brown", "Pink", "Green", "Purple", "White", "Yellow"))
                                , fill = factor(class, labels = c("Edible", "Poisonous")))) +
        geom_bar(position = "dodge", width = 0.5) +
        xlab("Cap Color") +
        ylab("Total Count") +
        labs(fill = "Mushroom Class") +
        scale_fill_manual(values=c("green3", "red3"))

#plot
print(capPlot)
