x <- c(12, 18, 9, 15)

barplot(x,
        horiz = TRUE,
        main = "Verkäufe pro Quartal",
        names.arg = c("Q1", "Q2", "Q3", "Q4"),
        col = "lightgreen")



x <- c(12, 18, 9, 15)

barplot(x,
        main = "Verkäufe pro Quartal",
        names.arg = c("Q1", "Q2", "Q3", "Q4"),
        col = c("lightblue", "green", "red", "pink"))
