y = matrix(1:20, nrow=5, ncol=4, byrow=TRUE)
dim(y)

y = matrix(1:20, nrow=5, ncol=4)
dim(y)

rna = c(1,26,24,68)

rnames = c("gene1", "gene2")
cnames = c("sample1", "sample2")


X = matrix(rna, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames,cnames))

X
X[1,]
X[,2]
X[1,2]
attributes(X)

a = list(1, "a", TRUE, 1+4i)
a

?data.frame
id = c(1,2,3,4)
color = c("red", "white", "blue", "yellow")
status = c('pass', 'fail', 'fail','pass','pass')
example1 = data.frame(id, color, status)
names(example1)
names(example1) = c("ID", 'COLOR', "RESULT")
