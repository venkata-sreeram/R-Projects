## Missing Value Treatment

sum(is.na(combi$Item_Weight))

#Imputing Missing Value
missing_index = which(is.na(combi$Item_Weight))

for(i in missing_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}

sum(is.na(combi$Item_Weight))

# 0 missing values! It means we have successfully imputed the missing data in the feature.




# Replacing 0's in Item_Visibility variable
# Similarly, zeroes in Item_Visibility variable can be replaced with Item_Identifier wise mean values of Item_Visibility. It can be visualized in the plot below.

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#Let's replace the zeroes.
# replacing 0 in Item_Visibility with mean
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
}

#After the replacement of zeroes, We'll plot the histogram of Item_Visibility again. In the histogram, we can see that the issue of zero item visibility has been resolved.
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)



#-------------------------------------------------------------------------------------------------------------------------

