#	Import your data into R.
#	Check your data.
#	Visualize the data using box plots.
#	Compute Kruskal-Wallis test.




#METHOD 1: go through the col to go over categorical and apply to numerical Kruskal.test(variable ~ group)
variables <- colnames(df)
for(i in 1:length(variablesIndf)) {
  if(variables[i] == 'Categorical_var') {
    next
  } else {
    kruskal.test(env_fact[,i], env_fact$Categorical_var)
  }
}

# METHOD 2: build a numeric matrix and  apply to all rows x= matrix g=cluster groups
row_kruskalwallis(x, g)





#	Interpret.
#	Multiple pairwise-comparison between groups.