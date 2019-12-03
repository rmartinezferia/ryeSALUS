source("library/salus_simpleParms.R")
source("library/salus_optim_run.R")
library(optimParallel)

x <- read.csv("assets/crop_parm_table.csv", stringsAsFactors = F) %>%
  filter(optim) %>%
  `rownames<-`(.$Parm) %>%
  select(Value,Min,Max) %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  mutate_all(as.numeric)

control <- list(maxit = 200,
                fnscale = -1,
                factr=0.001/.Machine$double.eps,
                parscale = unlist(ceiling(1*x[1,]/max(x[1,]))))

cl <- makeCluster(11) # set the number of processor cores
setDefaultCluster(cl=cl) # set 'cl' as default cluster

res <- optimParallel(unlist(x[1,]),
                     fn = salus_optim_run,
                     lower = unlist(x[2,]),
                     upper = unlist(x[3,]),
                     control = control)
res
salus_optim_run(res$par)
saveRDS(res,"data/optim_rye.rds")

