hsb2 <- read.table('https://stats.idre.ucla.edu/stat/r/faq/hsb2.csv', header=T, sep=",")
hsb2[1:10,]

str(hsb2)

str(datasites)

l <- reshape(hsb2, 
             varying = c("read", "write", "math", "science", "socst"), 
             v.names = "score",
             timevar = "subj", 
             times = c("read", "write", "math", "science", "socst"), 
             new.row.names = 1:1000,
             direction = "long")

l.sort <- l[order(l$id),]
l.sort[1:10,]

w <- reshape(l.sort, 
             timevar = "subj",
             idvar = c("id", "female", "race", "ses", "schtyp", "prog"),
             direction = "wide")

w[1:10,]

test <- reshape(datasites, 
                timevar = "site_ID", ## different measurements per site
                idvar = c("year", "MAIN_BAS", "origin"), ## values that don't vary per year
                direction = "wide")

test
l.sort <- l[order(l$id),]