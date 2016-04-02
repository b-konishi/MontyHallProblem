
door = 3
monty <- function(change) {
  choice = as.integer(runif(1, min=1, max=door+1))
  if (choice != 1) {
    choice = 0
  }
  if (change == 1) {
    if (choice == 1) {
      choice = 0
    } else {
      choice = 1
    }
  }
  return (choice)
}

sum = 1000
data = data.frame(NotChange=0, Change=0, Max=0)
plot(0, 0, xlim=c(0, sum), ylim=c(0,sum),
     xlab="Sample", ylab="Correct Answers", cex=0)
cols <- c("red", "blue", "green")
labels <- colnames(data)
legend("topleft", legend=labels, col=cols, pch=16)

readline()
for (i in 1:sum) {
  data$Max = data$Max + 1
  if (monty(0) == 1) {
    data$NotChange = data$NotChange + 1
  }
  if (monty(1) == 1) {
    data$Change = data$Change + 1
  }

  points(i, data$Max, col=cols[3], pch=16, cex=1)
  points(i, data$NotChange, col=cols[1], pch=16, cex=1)
  points(i, data$Change, col=cols[2], pch=16, cex=1)
}
print("probability")
print(data$NotChange/sum * 100)
print("probability")
print(data$Change/sum * 100)
