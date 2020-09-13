

library(tree)
data("iris")
View(iris)


iris_tree= tree(Species~.,data=iris)
plot(iris_tree)
text(iris_tree)
pred_tree=as.data.frame(predict(iris_tree,iris))
pred_tree["final"]=NULL

for(i in 1:nrow(pred_tree)){
  pred_tree[i,"final"]=ifelse(pred_tree[i,"setosa"]>0.5,"setosa",ifelse(pred_tree[i,"versicolor"]>0.5,"versicolor","virginica"))
   
}

mean(pred_tree$final==iris$Species)#97% accuracy

library(gmodels)
CrossTable(iris$Species,pred_tree$final)
