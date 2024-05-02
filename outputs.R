ggplot(trees, aes(x = tree_dbh)) + #checking distribution of data, we see that it is skewed right but not an issues since n>30
  geom_density() +
  theme_classic() +
  xlim(0,75)+ #set x-axis interval
  labs(x = "Tree Diameter at Breast Height (inches)", title = "Distribution of Tree Size", subtitle = "Philadelphia, PA 2023")
---------------------------------------------------------------------------
  
#frequency of trees by name and that tree's average dbh, would help focus on planting these trees
treename_and_freq <- left_join(tree_diameters, tree_freq, by = "tree_name")
hightreename_and_freq<- treename_and_freq %>%   arrange(desc(tree_freq), desc(avg_diameter))
lowtreename_and_freq<- treename_and_freq 
head(lowtreename_and_freq)
head(hightreename_and_freq)


---------------------------------------------------------------------------

  ggplot(tree_diameters, aes(x = tree_name, y = avg_diameter)) +
  geom_point(size=0.1) +
  labs(title = "Average Diameter by Tree Name, 2023",
       x = "Tree Name",
       y = "Average Diameter") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 3, angle = 150, vjust = 0.5, hjust=0.5)) 
#scatter plot to visualize theaverage diameter for each tree type, 
#trees that aren't as freq in the dataset are found to have a lower so maybe average 
#diameter wouldnt be best for this analysis NOT BEING USED IN FINAL

ggplot(tree_diameters, aes(y = tree_name, x = avg_diameter, fill = tree_name)) +
  geom_col() +  # Using geom_col() for bar plot
  labs(title = "Average Diameter of Each Tree",
       y = "Tree Name",
       x = "Average Diameter (dbh)", 
       subtitle = "Philadelphia, PA 2023") +
  theme_classic() +
  guides(fill = FALSE) +
  theme(axis.text.y = element_text(size = 2, vjust = -0.5, hjust=0.5)) #bar graph of diameters by name

ggplot(tree_diameters, aes(y = fct_reorder(tree_name, avg_diameter), x = avg_diameter, fill = tree_name)) +
  geom_col() +  # Using geom_col() for bar plot
  labs(title = "Average Diameter of Each Tree",
       y = "Tree Name",
       x = "Average Diameter (dbh)", 
       subtitle = "Philadelphia, PA 2023") +
  theme_classic() +
  guides(fill = FALSE) +
  theme(axis.text.y = element_text(size = 2, vjust = -0.5, hjust = 0.5)) #in order of diam, more smaller than
#larger, a visual for freq outputs

ggplot(tree_diameters, aes(x = avg_diameter)) + 
  geom_density() +
  theme_classic()+
  xlim(0, 80) +
  labs(x = "Mean Tree Diameter (BH)", y = "Density", title = "Density Plot of Average Tree Diameters", 
       subtitle = "Philadelphia, PA 2023") 
#checking distribution of data, we see that it is skewed right but makes sense given values for tree diameters
#usually smaller number; also since we have sample size greatherthan/equal to 30 skwedness is not reqd to be fixed

---------------------------------------------------------------------------

#MAPPING
#all trees in Philly, there are obvious clusters
ggplot() +
  geom_sf(data = philly.tracts.2020) + #county outline
  geom_sf(data = phl.tri.sf,color = "blue", alpha = 0.5,  size = 0.00001)+ #long and lat of trees
  theme_classic() +
  labs(title = "Trees Planted Throughout Philadelphia", subtitle = "Mapped by Census-Tract County")

#diameters map across Philly, obvious clusters regarding size, which also means the tree name/type of tree further
#supporting the information above
ggplot() + 
  geom_sf(data = philly.tracts.2020) + #county outline
  geom_sf(data = joined_data, aes(color = dbh_category), size= 0.075) +
  labs(title = "City of Philadelphia Trees by Diameter at Breast Height (DBH)", subtitle = "Mapped by Census-Tract County", color = "Diameter Category") +  # Change the legend title for color
  theme_classic()
---------------------------------------------------------------------------
  
#ANOVA: It assesses whether there is a statistically significant variation in the average diameter 
#between different tree names.
  

aov<- aov(trees$tree_dbh ~ trees$tree_name)
summary(aov)

aov_result <- aov(tree_dbh ~ tree_name, data = trees)
anova_table <- anova(aov_result)

res_aov #the output suggests that there is a significant amount of
#variation in average diameter that can be explained by differences in tree types. 
#However, it also warns us to interpret these results cautiously due to the potential for unbalanced data, 
#which might influence the robustness of our conclusions.
  
tuk <- TukeyHSD(aov(avg_diameter ~ tree_name, data = tree_diameters))

plot (tuk)
pairwise_result
#Tukey's HSD test complements ANOVA by providing detailed information about specific group differences, 
#which enhances the interpretation and understanding of the results obtained from the analysis of variance.
#Given "ABIES FRASERI - FRASER FIR" is listed first, it suggests that the average diameter of "ABIES FRASERI - FRASER FIR"
#is higher than that of "ABIES BALSAMEA - BALSAM FIR" by approximately 2.6 units. This means that, on average, the trees
#belonging to the "ABIES FRASERI - FRASER FIR" group have a diameter approximately
#2.6 units larger than the trees in the "ABIES BALSAMEA - BALSAM FIR" group.
---------------------------------------------------------------------------
#Pearson's correlation coefficient measures the strength and direction of the linear relationship between 
#two variables. 
  
long_test #Based on these findings, we can conclude that there is a statistically significant moderately weak positive 
#correlation between the variables Y and tree_dbh.The positive sign indicates a positive relationship between the 
#variables Y and tree_dbh, implying that as the values of one variable increase,the values of the other variable tend to 
#increase as well, and vice versa. However, the strength of this relationship is only moderately weak.

lat_test #looking to see if theres a correlation between a tree's X and Y location w its given diameter at breast height. 
#Based on these findings, we can conclude that while there is a statistically significant positive correlation between 
#the variables X and tree_dbh, the strength of this relationship is very weak.This means that as the values of one variable increase, the values of the other variable also tend to increase,
#and vice versa. However, since the correlation is very close to zero, the strength of this relationship is very weak.

#looking to see if theres a correlation between a tree's freq/count and its X, Y location

long_test1 #The positive sign of indicates a positive relationship between the variables X and tree_freq, implying that 
#as the values of one variable increase, the values of the other variable tend to increase as well, and vice versa. we 
#can conclude that there is a statistically significant very weak positive correlation between the variables X and tree_freq. 

lat_test1#we can conclude that there is a statistically significant very weak positive correlation between the variables 
#Y and tree_freq. The positive sign of indicates a positive relationship between the variables Y and tree_freq, 
#implying that as the values of one variable increase, the values of the other variable tend to increase as well, 
#and vice versa, though a weak relationship. 
                      
