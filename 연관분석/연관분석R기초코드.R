install.packages("arules")
install.packages('arulesViz')
library(arules)
library(arulesViz)

a_list <- list(c("a","b","c"), c("a","b"), c("a","b","d"), c("c","e"),
               c("a","b","d","e"))
names(a_list) <- paste("Group", c(1:5), sep="")

a_list

trans <- as(a_list, "transactions") # transactions 클래스로 변환
trans

summary(trans)

## 연관 규칙 생성

rules <- apriori(trans)
summary(rules)
inspect(rules)

plot(rules, method='grouped')

# 유용한놈 뽑기 위해 최소 지지도, 신뢰도 각 0.8 설정
sum.rules <- apriori(trans, parameter=list(support=0.8, confidence=0.8))
summary(sum.rules)
inspect(sum.rules)
