# Charging of data
data=read.table(file = 'dataR.csv',sep=';',header=TRUE)

#removing the columns with no names (ie those whose name begins by Unnamed)
new_names = c()
for (i in names(data)) if (!any(grep("Unnamed",i))) new_names = c(new_names,i)
data=data[new_names]

# Treatment of gender questions
# Creation of a new column: gender HHH which takes the value of gender resp if the respondent is HHH or the one of the HHH instead

