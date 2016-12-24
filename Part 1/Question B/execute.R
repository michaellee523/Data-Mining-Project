library(ggplot2)

mushroom <- read.csv("mushrooms.csv", header = TRUE, na.strings = "?")

mushroom$class <- factor(mushroom$class, labels = c("Edible", "Poisonous"))
mushroom$cap.shape <- factor(mushroom$cap.shape, labels = c("Bell", "Conical", "Flat", "Knobbed", "Sunken", "Convex"))
mushroom$bruises <- factor(mushroom$bruises, labels = c("No bruises", "Bruises"))
mushroom$cap.color <- factor(mushroom$cap.color, labels = c("Buff", "Cinnamon", "Red", "Gray", "Brown", "Pink", "Green", "Purple", "White", "Yellow"))

capShapePlot <- ggplot(mushroom,aes(x=mushroom),
fill=class) + geom_bar(position = "dodge", width = 0.5) + 
        xlab("Cap Shape") + ylab("Total Count") + labs(fill = "Mushroom Class") + 
        scale_fill_manual(values=c("green3", "red3"))

bruisesPlot <- ggplot(mushroom, aes(x = bruises, fill = class)) +
        geom_bar(position = "dodge", width = 0.5) +
        xlab("Bruises") +
        ylab("Total Count") +
        labs(fill = "Mushroom Class") +
        scale_fill_manual(values=c("green3", "red3"))

capPlot <- ggplot(mushroom, aes(x = cap.color, fill = class)) +
        geom_bar(position = "dodge", width = 0.5) +
        xlab("Cap Color") +
        ylab("Total Count") +
        labs(fill = "Mushroom Class") +
        scale_fill_manual(values=c("green3", "red3"))

capByBruisePlot <- ggplot(mushroom, aes(x = cap.color, fill = class)) +
        geom_bar(position = "dodge", width = 0.5) +
        facet_wrap(~bruises + cap.shape, labeller = label_context) +
        ggtitle("Bruises, Cap Shape") +
        xlab("Cap Color") +
        ylab("Total Count") +
        labs(fill = "Mushroom Class") +
        scale_fill_manual(values=c("green3", "red3"))

#plot
print(capByBruisePlot)
