


## new args -- snr_target

# generate X, err, y ----

generate_data = function(n, p, n_sparse, r, n_cluster, pr_z, sim_id, snr_target) {
  
  seed_0 = sim_id * 777 
  
  # X ----
  
  set.seed(seed_0)
  X = matrix(rnorm(n = n * p, mean = 0, sd = 1), nrow = n, ncol = p) # nxp order
  # summary(X)
  dim(X)
  
  # X = matrix(runif(n = n * p, min = -5, max = 5), nrow = n, ncol = p) # nxp order
  # dim(X)
  
  
  
  
  # gamma ----
  
  set.seed(seed_0)
  gam_non_zero = sample(x = 1:p, size = n_sparse, replace = FALSE)
  gam  = rep(0, p) # matrix(0, nrow = p, ncol = 1)
  gam[gam_non_zero] = 1
  gam
  
  
  
  # alpha ----
  
  set.seed(seed_0)
  
  alpha = gen_alpha(gam, alpha_target_range, seed_0)
  alpha[alpha != 0]
  # alpha
  plot(alpha)
  
  var(X %*% alpha)
  
  
  
  # var(err) ----
  
  if(n_sparse == 0){
    
    err_var_target = err_init_var
  }
  
  if(n_sparse > 0){
    
    snr_num = var(X %*% alpha)
    snr_num 
    
    set.seed(seed_0)       
    snr_target = runif(n = 1, min = snr_target_range[1], max = snr_target_range[2])
    snr_target
    
    err_var_target = snr_num/snr_target 
    err_var_target = as.vector(err_var_target)
    err_var_target
    
  } 
  
  err_var_target
  
  # b ----
  
  # set.seed(seed_0)
  # b = gen_b(var_e = err_var_target, r)
  # b
  # 
  # 
  # # z ----
  # 
  # set.seed(seed_0)
  # z = sample(1:n_cluster, n, replace = TRUE, prob = pr_z)
  # table(z)
  
  
  
  # err ----
  
  err = numeric()
  # 
  # r_z = r[z] 
  # b_z = b[z]
  # 
  # for(i in 1:n) {
  #   
  #   set.seed(seed_0 * i + 777)
  #   err[i] = ald::rALD(n = 1, mu = 0, sigma = b_z[i], p = r_z[i])
  #   
  # }
  
  err = rt(n, df = 5, ncp = 0)
  
  
  
  var(err)
  
  plot(err)
  
  plot(density(err))
  
  
  # for(k in 1:n_cluster) {
  #   
  #   clus = which(z == k)
  #   
  #   print(mean(err[clus]))
  #   
  # }
  
  ## Mean is not zero -- can't ' simplify expn for snr -- mix var (rough2 -- p -155, 156)
  
  
  # y ----
  
  y = numeric()
  
  y = X %*% alpha + err
  
  # snr ----
  snr_actual = var(X %*% alpha)/var(err)
  snr_actual 
  snr_target
  
  return(list("n" = n, "p" = p, "n_sparse" = n_sparse, "n_cluster" = n_cluster, "seed_0" = seed_0,
              "X" = X,  "gam" = gam, "alpha" = alpha,
              # "r" = r, "b" = b, "pr_z" = pr_z, "z" = z,
              "err" = err, "y" = y, "snr_actual" = snr_actual, "snr_target" = snr_target))
  
}


# data_info = generate_data(n, p, n_sparse, r = r_true, b = b_true, n_cluster, pr_z = pr_true, seed_0)
# data_info$alpha


# mean, var quantile check of err ----

check_mean_var = function(r, b, pr_z, z, err) {
  
  # prev var exprsn is simplification of current expn -- exactly same 
  # for better understanding used predefined fn form
  
  info = as.data.frame(matrix(nrow = 1, ncol = 6))
  colnames(info) = c("Pop_Mean", "Sam_Mean", "Pop_Var", "Sam_Var", "Pop_rth_quantile",  "Sam_rth_quantile"); info
  
  pop_mean_clus = ((1 - 2 * r)/(r * (1 - r))) * b
  pop_mean_clus
  pop_mean = sum(pr_z * pop_mean_clus)
  pop_mean
  mean(err)
  
  # pop_var_clus = ((1 - 2 * r + 2 * r^2)/((r * (1 - r))^2)) * (b^2) # var(err | tau)
  pop_var_clus = var_e_fn_r(r) * (b^2) # var(err) -- should match with my input
  pop_var_clus
  pop_var = sum(pr_z * (pop_var_clus + pop_mean_clus^2)) - pop_mean^2
  pop_var
  var(err)
  
  sam_err_quant = quantile(err, r)
  sam_err_quant
  
  quantile(err[z == 1], r[1])
  quantile(err[z == 2], r[2])
  
  info[1, ] = c(pop_mean, mean(err), pop_var, var(err), 0, sam_err_quant)
  
  return(info)
  
}



# TODO to work on it 
# check_mean_var(r = r_true, b = b_true, pr_z = pr_true, z = data_info$z, err = data_info$err)
# r = r_true
# b = b_true
# pr_z = pr_true
# z = data_info$z
# err = data_info$err



