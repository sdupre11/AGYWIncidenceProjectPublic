

cdplot(as.factor(age)~inc, data = allData)


ggplot(data) +
  aes(x = age, y = inc) +
  geom_boxplot() + theme_minimal()
