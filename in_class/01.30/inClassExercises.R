data(swiss)
# ?swiss
# View(swiss)

SnP500 = read.csv("././DATA/SP500.csv", header=TRUE)
head(SnP500)

table(SnP500$GICS.Sub.Industry)
unique(SnP500$GICS.Sub.Industry)
# ?unique

class(SnP500$GICS.Sector)

HsB = read.csv("././DATA/HsB.csv", header=TRUE)
head(HsB)
tail(HsB)

colnames(HsB)

View(HsB)

x = c(12,56,31, -5, 7)
y = mean(x)
y
z = x ^ 2
z

example(sum)
