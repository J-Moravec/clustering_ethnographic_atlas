data = read.table("var_type_and_AICdiff.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
data_type = data
data_type = data_type[order(data_type[,3]),]
write.table(
    data_type, file="var_type_and_AICdiff.tex",
    quote=FALSE,
    row.names=FALSE,
    col.names=TRUE,
    sep= " & ",
    eol = "\\\\\n"
    )


types = unique(data[,3])
res = list()
for(type in types){
    subdata = data[ data[,3] == type, 2]
    res[[type]] = c(mean(subdata), length(subdata))
    }

res = do.call(rbind, res)
names = rownames(res)
res = res[order(names), ]
res = res[order(res[,1]),]

write.table(
    res, file="var_type_summary.tex",
    quote = FALSE,
    row.names = TRUE,
    col.names = FALSE,
    sep = " & ",
    eol = "\\\\\n"
    )
