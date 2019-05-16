#Larry McQuaid
#MET CS 688
#Due: 3/26/19

install.packages('tm')

#1.	Using a standard R function (not the c() function), create a variable named x1 which contains five equally spaced numbers.

x1 <- seq(1, 10, 2)
x1

#2.	Find the mean of all elements of x1 except the second.

mean(x1[-2])

#3.	Create a list My.List where the first list element is a two-column data frame containing x1 and the vector [2, 1, 4, 7, 8], and the second list element is the logical vector [T, F, NA, T, F].

x2 <- c(2, 1, 4, 7, 8)
x3 <- c(T, F, NA, T, F)
My.List <- list(data.frame(x1, x2), x3)
My.List

#4.	Use "lapply" to add elementwise the 2 columns of the first element in My.List 

lapply(My.List[1], function(x) x1+x2)

