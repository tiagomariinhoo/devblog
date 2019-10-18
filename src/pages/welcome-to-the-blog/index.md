---
title: RStudio study!
date: "2019-10-17"
featuredImage: './featured.jpg'
---

Some scripts about RStudio.

<!-- end -->

## Lorem ipsum dolor sit amet, consectetur adipiscing elit

Sed urna justo, scelerisque consectetur pharetra vitae, facilisis vel diam. Maecenas auctor enim a volutpat mattis. Morbi sit amet turpis a purus ornare pellentesque sit amet et ipsum. Suspendisse imperdiet mi at felis aliquet, nec consectetur arcu dignissim. Sed vitae diam maximus, maximus diam ac, scelerisque mi.

*   Morbi sit amet turpis a purus
*   Etiam tempor ultricies mi
    *   Maecenas auctor enim a volutpat mattis
*   Sed urna justo, scelerisque consectetur pharetra vitae

Nam nec augue vel nisl placerat faucibus. Donec congue **nulla quis nunc** sagittis placerat. Pellentesque non tincidunt velit, cursus porttitor tellus. Suspendisse pulvinar tortor at _augue aliquam sagittis_. Duis non pulvinar augue. Ut tristique dignissim ligula, eget tempus diam molestie non. Nulla ultrices eleifend rutrum. Mauris convallis sollicitudin dui, pulvinar suscipit velit. Maecenas viverra finibus metus vitae blandit.

### Pellentesque consectetur facilisis venenatis

Nam ullamcorper, orci nec tempor hendrerit, lorem nunc laoreet diam, vel gravida sem mi quis augue. Nunc odio velit, facilisis quis dictum non, facilisis quis felis. Vivamus [elementum dapibus nibh](https://google.com), eget aliquet nunc luctus maximus. Sed finibus risus eget ultrices maximus. Aliquam commodo consectetur diam eget tristique. Nunc quis erat quis felis fringilla tempus. Cras tempor nibh dolor, ac lacinia lacus ultrices eu.

> Quisque tempor nulla turpis, ut placerat arcu lobortis nec. Aenean sed vehicula nisi. Nullam vitae placerat enim. Etiam hendrerit enim vel tempor fermentum. Morbi rutrum euismod ipsum a luctus.

Morbi et libero id metus tempor imperdiet eget non mi. Mauris pulvinar quis enim at placerat. Vestibulum vitae dapibus lectus, ut elementum est.

### Pellentesque facilisis venenatis mi, sit amet molestie nisl ornare et

Morbi posuere facilisis eros vel euismod. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Etiam malesuada dapibus dolor non cursus. Sed in turpis justo. Cras sed hendrerit nulla. Sed ornare, leo et suscipit tincidunt, justo diam sollicitudin risus, vitae vulputate nulla augue et lacus.

1.  Morbi posuere facilisis
2.  Pellentesque habitant morbi tristique
3.  Etiam malesuada dapibus

Ut vel ligula ante. Proin quis metus magna. Nulla nec dui vulputate, semper orci in, sagittis dolor. Mauris dictum neque non fermentum consectetur. Integer vel pellentesque ex, ut tincidunt quam.

#### Sed ac orci a dolor venenatis vulputate

Sed sollicitudin, turpis ac malesuada dapibus, magna dui semper orci, in congue justo felis quis ante. Phasellus consequat et dolor fringilla accumsan. Pellentesque ullamcorper porttitor dolor et imperdiet.

![Space](./space.jpg)

Pellentesque consectetur facilisis venenatis. Nam ullamcorper, orci nec tempor hendrerit, lorem nunc laoreet diam, vel gravida sem mi quis augue. Nunc odio velit, facilisis quis dictum non, facilisis quis felis. Vivamus elementum dapibus nibh, eget aliquet nunc luctus maximus. Sed finibus risus eget ultrices maximus. Aliquam commodo consectetur diam eget tristique. Nunc quis erat quis felis fringilla tempus. Cras tempor nibh dolor, ac lacinia lacus ultrices eu.

##### Nulla nec dui vulputate, semper orci in, sagittis dolor

Mauris dictum neque non fermentum consectetur. Integer vel pellentesque ex, ut tincidunt quam. Sed ac orci a dolor venenatis vulputate. Sed sollicitudin, turpis ac malesuada dapibus, magna dui semper orci, in congue justo felis quis ante. Phasellus consequat et dolor fringilla accumsan. Pellentesque ullamcorper porttitor dolor et imperdiet.

## Loading Library

Text about Loading Library

```R
##################### Load Library #####################
library(RWeka)
library(e1071)
library(gmodels)
#library(C50)
library(caret)
library(irr)
library(randomForest)
```

## About Functions

Text about Functions

```R
##################### Functions #####################
# Precision
precision <- function(tp, fp){
  
  precision <- tp/(tp+fp)
  
  return(precision)
}

# Recall
recall <- function(tp, fn){
  
  recall <- tp/(tp+fn)
  
  return(recall)
}

# F-measure
f_measure <- function(tp, fp, fn){
  
  f_measure <- (2*precision(tp,fp)*recall(tp,fn))/(recall(tp,fn) + precision(tp, fp))
  
  return(f_measure)
}

measures <- function(test, pred){
  
  true_positive <- 0
  true_negative <- 0
  false_positive <- 0
  false_negative <- 0
  
  for(i in 1:length(pred)){
    if(test[i] == TRUE && pred[i] == TRUE){
      true_positive <- true_positive + 1
    }else if(test[i] == FALSE && pred[i] == FALSE){
      true_negative <- true_negative + 1
    }else if(test[i] == FALSE && pred[i] == TRUE){
      false_negative <- false_negative + 1
    }else if(test[i] == TRUE && pred[i] == FALSE){
      false_positive <- false_positive + 1
    }
  }
  
  measures <- c(precision(true_positive,false_positive), 
                recall(true_positive,false_negative), 
                f_measure(true_positive,false_positive,false_negative))
  
  return(measures)
}
```

##Techniques
Text about Techniques

```R
##################### Techniques #####################
executeJ48 <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- J48(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeNaiveBayes <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- naiveBayes(train, train$Smell, laplace = 1)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeC50 <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- C5.0(train, train$Smell)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeSVM <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- svm(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeOneR <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- OneR(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeJRip <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- JRip(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
  
}

executeRandomForest <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- randomForest(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
}

executeSMO <- function(dataset, folds){
  results <- lapply(folds, function(x) {
    train <- dataset[-x, ]
    test <- dataset[x, ]
    model <- SMO(train$Smell~ ., data = train)
    pred <- predict(model, test)
    
    results <- measures(test$Smell, pred)
    
    return(results)
  })
}
```

###### ras aliquet ipsum ut enim pellentesque, id varius quam placerat

Maecenas non scelerisque leo. Sed id purus fringilla, consequat magna non, faucibus neque. Cras ornare nisi a lectus ultricies convallis. Integer tristique dictum eros, et elementum ante consectetur eget. Phasellus sollicitudin est vestibulum suscipit pellentesque. Duis in eros cursus magna laoreet aliquam vel a lectus. Nulla ut nisi vitae ipsum sollicitudin vestibulum. Aenean sit amet mattis odio. Vestibulum ultrices sed ipsum nec pretium. Integer non turpis nunc. Praesent tincidunt tincidunt alique

Integer mollis dolor non libero placerat, ut efficitur nisi ultricies. Etiam ac lacinia urna, eget ornare nunc. Suspendisse eget eros id turpis gravida semper. Duis ornare lorem et est pellentesque, quis rhoncus leo vestibulum. Nullam eu nulla ut elit rutrum iaculis. Cras a laoreet elit, in aliquet erat. Sed interdum varius posuere. Pellentesque eget luctus erat. Nam quis sem in ligula efficitur bibendum.