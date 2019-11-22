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
source("nn_dist.R")
source("nn_mate.R")
## 
my_fun <- function(x){
  
  return(1/(1+exp(-4.9*x)))
  #return(ifelse(x>0,1,0))
}


## test for XORS problem 

pop_size <- 150
input_size <- 3
output_size <- 1
mutation_rate <- 0.8
mutation_only_rate <- 0.25
# new_connection_rate <- 0.05
# new_node_rate <- 0.03
max_gen <- 5
weight_mutation_rate <- 0.8
node_mutation_rate <- 0.03
link_mutation_rate <- 0.06
## parameter initilization ##
gen_id <- 0

mutation_tracking <- data.frame(
  name = NULL, new_node_id = NULL,
  new_marker1 = NULL, In1= NULL, Out1 = NULL,
  new_marker2 = NULL, In2= NULL, Out2 = NULL
)

max_node <- input_size + output_size
max_marker <- input_size * output_size


## do neat initialization 


neat_pop <- lapply(rep(input_size, pop_size), FUN= nn_init)
max_fit <- 0
while(gen_id <100){
  cat("gen_id =", gen_id,"\n")
  ## eval current genration
  source("case_eval.R")
  print(species_summary)
  
  ## get next generation
  source("case_reproduction2.R")
 
  max_fit <- max(neat_pop_fitness) 
  cat("max_fit=", max_fit, "\n")
  
  gen_id <- gen_id + 1
  
  ## reset mutation_tracking 
  mutation_tracking <- data.frame(
    name = NULL, new_node_id = NULL,
    new_marker1 = NULL, In1= NULL, Out1 = NULL,
    new_marker2 = NULL, In2= NULL, Out2 = NULL
  )
}



best_nn <- neat_pop[[which(neat_pop_fitness == max(neat_pop_fitness))[1]]]
best_fitness <- max(neat_pop_fitness)
nn_plot(best_nn$connect_df)

eval1 <- nn_eval(nn = best_nn, fun_act = my_fun, input = c(1,0,0))
eval2 <- nn_eval(nn = best_nn, fun_act = my_fun, input = c(1,0,1))
eval3 <- nn_eval(nn = best_nn, fun_act = my_fun, input = c(1,1,0))
eval4 <- nn_eval(nn = best_nn, fun_act = my_fun, input = c(1,1,1))

eval_final <- 4 - (abs(eval1)^2 + abs(1 - eval2)^2 + abs(1-eval3)^2 + abs(eval4)^2)
eval_final
