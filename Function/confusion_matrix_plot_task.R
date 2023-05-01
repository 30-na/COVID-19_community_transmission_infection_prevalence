# Load the caret package
library(caret)

# Generate example data
set.seed(123)
actual <- sample(c("A", "B", "C"), 100, replace = TRUE)
predicted <- sample(c("A", "B", "C"), 100, replace = TRUE)

# Create confusion matrix
cm <- confusionMatrix(factor(predicted), factor(actual))

# Plot the confusion matrix using the plotconfusion function
plotconfusion(cm,
              predname="Predicted\nLevel",
              refname="Undiagnosed\nPrevalence Level",
              addtitle=TRUE)
