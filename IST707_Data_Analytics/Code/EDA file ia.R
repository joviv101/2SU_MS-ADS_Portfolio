# EDA on the gender gap data


df <- read.csv('D:\\Documents\\888_school\\06_IST_707\\final_project\\/gap-development (2).tsv',sep="\t")
head(df)
str(df)


library(ggplot)

t <- as.matrix(table(df$Pronoun) )
t

dfdf <- as.data.frame(t)

dfdf$pronoun <- rownames(t)
dfdf$
ggplot(dfdf, aes(pronoun, V1))+
  geom_col()


#training pronoun data is not even.
ggplot(dfdf, aes(pronoun, dfdf$V1) ) +
  geom_bar(stat = "identity")

hist(df$A.offset)
rug(df$A.offset)
