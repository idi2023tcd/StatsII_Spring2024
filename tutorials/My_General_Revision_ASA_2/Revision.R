#
#My Exercises- APPLIED STATISTICS II INCLUDING ALL LECTURES AND TUTORIALS REVISIONS

View(mtcars)
dim(mtcars)
names(mtcars)
getwd()
#######
ggplot(mtcars, aes(x=mpg, y=disp))+
  geom_point()+
  geom_smooth(method="lm")

pdf("test.pdf")
ggplot(mtcars, aes(x=mpg, y=disp))+
  geom_point()+
  geom_smooth(method="lm")
dev.off()