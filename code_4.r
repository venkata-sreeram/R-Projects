## Feature Engineering


perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene","Household", "Soft Drinks")
names(combi)


# create a new feature 'Item_Type_new' 
#ifelse(combi$Item_Type %in% perishable, "perishable",ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
#combi[,Item_Type_new := ifelse(combi$Item_Type %in% perishable, "perishable",ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

Item_Type_new = ifelse(combi$Item_Type %in% perishable, "perishable",ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))

combi = cbind(Item_Type_new,combi)
names(combi)

#substr(combi$Item_Identifier, 1, 1) # 1 stands for start letter and ends with 1 letter only
#substr(combi$Item_Identifier, 1, 2) # 1 stands for start letter and ends with 2 letter only

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))




#combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
Item_category = substr(combi$Item_Identifier, 1, 2)

names(combi)
combi = cbind(Item_category,combi)
names(combi)

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

# years of operation of outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


#ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3)



# creating new independent variable - Item_MRP_clusters
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", 
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]
#-------------------------------------------------------------------------------------------------------------------------

