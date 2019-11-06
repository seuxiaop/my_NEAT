## INITILIZATION ##
rm(list=ls())
library(dplyr)
source("add_new_connection.R")
source("add_new_node.R")
source("nn_init.R")
source("nn_plot.R")
source("get_node_info.R")
source("weight_mutation.R")
source("nn_mutation.R")
source("nn_eval.R")
source("get_sub_species.R")

## 
my_fun <- function(x){
  
  return(1/(1+exp(-4.9*x)))
}


## test for XORS problem 

pop_size <- 150
input_size <- 3
output_size <- 1



## parameter initilization ##
mutation_tracking <- data.frame(
  name = NULL, new_node_id = NULL,
  new_marker1 = NULL, In1= NULL, Out1 = NULL,
  new_marker2 = NULL, In2= NULL, Out2 = NULL
)

max_node <- input_size + output_size
max_marker <- input_size * output_size


## do neat initialization 


neat_pop <- lapply(rep(input_size, 150), FUN= nn_init)



## get initial fitness

nn_eval(nn1,fun_act=my_fun, input = c(1,0,0) )

eval1 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,0,0) ))
eval2 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,0,1) ))
eval3 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,1,0) ))
eval4 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,1,1) ))
eval_final <- 4 - (abs(eval1) + abs(1 - eval2) + abs(1-eval3) + abs(eval4))

## get initial sub_species
pop_rep <- list(neat_pop[[sample(pop_size,1)]])
species_vec <- get_sub_species(neat_pop, pop_rep, dist_torlerance=3) 


##
summary_df <- data.frame(id= 1:pop_size, fitness_init = eval_final, species = species_vec)





##

nn1 <- neat_pop[[1]]
nn2 <- neat_pop[[2]]

for(i in 1:10){
  
  nn1 <- nn_mutation(
    nn = nn1,
    mutation_tracking = mutation_tracking,
    max_node = max_node,
    max_marker= max_marker, 
    type = sample(3,1)
  )
  mutation_tracking = nn1$mutation_tracking
  max_node = nn1$max_node
  max_marker= nn1$max_marker
  nn1 <- nn1$nn
}

nn_plot(nn1$connect_df)


for(i in 1:10){
  
  nn2 <- nn_mutation(
    nn = nn2,
    mutation_tracking = mutation_tracking,
    max_node = max_node,
    max_marker= max_marker, 
    type = sample(3,1)
  )
  mutation_tracking = nn2$mutation_tracking
  max_node = nn2$max_node
  max_marker= nn2$max_marker
  nn2 <- nn2$nn
}

nn_plot(nn2$connect_df)


#nn_eval(nn = nn1,fun_act = my_fun , input = c(0.1,1,1))



