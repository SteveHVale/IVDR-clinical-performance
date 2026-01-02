library(ggplot2)

fpr<-c(0.5,1:9)
sTT<-"Triple test"
sQT<-"Quadruple test"
sCT<-"Combined test"
sIT<-"Integrated test"

points1 <- data.frame(
  x = c(4.1, 8.3, 2.9),
  y = c(48, 75, 73),
  ymin = c(28,51,44),
  ymax = c(69,90,90),
  graph = sTT
)

points2 <- data.frame(
  x = c(7),
  y = c(81),
  ymin = c(72),
  ymax = c(89),
  graph = sQT
)

points3 <- data.frame(
  x = c(3.3, 2.7),
  y = c(85, 81.8),
  ymin = c(56,79),
  ymax = c(100,84.3),
  graph = sCT
)

points4 <- data.frame(
  x = c(2.1, 3.3),
  y = c(87, 88.4),
  ymin = c(74,81.6),
  ymax = c(95,91.5),
  graph = sIT
)

estPoinst1<-data.frame(
  x = c(1,5),
  y = c(55.4,76.9),
  graph= sTT
)

estPoinst2<-data.frame(
  x = c(1,5),
  y = c(69.6,86.3),
  graph= sQT
)

estPoinst3<-data.frame(
  x = c(1,5),
  y = c(80.8,90.4),
  graph= sCT
)

estPoinst4<-data.frame(
  x = c(1,5),
  y = c(91.3,96.5),
  graph= sIT
)

lines1 <- data.frame(
  x = fpr,
  y = c(46.7,55.4,64,69.8,73.8,76.9,79.4,81.4,83,84.5),
  graph = sTT
)

lines2 <- data.frame(
  x = fpr,
  y = c(61.8,69.6,77.2,81.3,84,86.3,87.8,89.1,90.2,91.1),
  graph = sQT
)

lines3 <- data.frame(
  x = fpr,
  y = c(76.1,80.8,85.1,87.5,89.2,90.4,91.3,92.1,92.7,93.3),
  graph = sCT
)

lines4 <- data.frame(
  x = fpr,
  y = c(88.3,91.3,93.8,95.1,95.9,96.5,96.9,97.3,97.5,97.8),
  graph = sIT
)

points_all <- rbind(points1, points2, points3, points4)
estpoints_all <-rbind(estPoinst1,estPoinst2,estPoinst3,estPoinst4)
lines_all <- rbind(lines1, lines2, lines3, lines4)
lines_all <- lines_all[order(lines_all$graph, lines_all$x), ]

p<-ggplot() +
  geom_point(
    data = points_all,
    aes(x = x, y = y),
    size = 2
  ) +
  geom_point(
    data = estpoints_all,
    aes(x = x, y = y),
    size = 2,
    shape = 15
  ) +
  geom_errorbar(data=points_all, aes(x=x,ymin=ymin,ymax=ymax),width=0.2) + 
  geom_line(
    data = lines_all,
    aes(x = x, y = y),
    linewidth = 0.5
  ) +
  facet_wrap(~ graph, ncol = 2,axes="all") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "False positive rate (%)",
    y = "Detection rate (%)",
  ) +
  theme_minimal()+
  theme(axis.line = element_line())

print(p)
