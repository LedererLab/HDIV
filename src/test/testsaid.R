# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


sa_id <- Sys.getenv('SLURM_ARRAY_TASK_ID') 
print(sa_id)
print(str(sa_id))