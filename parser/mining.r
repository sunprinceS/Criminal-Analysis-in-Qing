library("tm")
library("rJava")
library("Rwordseg")
library("tmcn")
library("slam")
library("XML")
library("irlba")
library("futile.matrix")

### Read All Require Data
xmlfile <- xmlTreeParse("d_0001.xml")

xmltop = xmlRoot(xmlfile) # Read XML
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
# summary(plantcat_df)



