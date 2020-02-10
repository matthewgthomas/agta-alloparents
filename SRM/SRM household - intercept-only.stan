data{
    int N;
    int N_i__j_ID;
    int N_dyads;
    int y[N];
    // real ln_offset[N];
    int i_ID[N];
    int j_ID[N];
    int ij_dyad_ID[N];
    int i_in_dyad[N];
}

parameters{
    real a;
    
    // Node level effects cov matrix
    matrix[2,N_i__j_ID] z_i__j_ID;
    vector<lower=0>[2] sigma_i__j_ID;
    cholesky_factor_corr[2] L_Rho_i__j_ID;
    
	// DYAD level covariance matrix
    matrix [2,N_dyads] z_dyad_ID;
    real<lower=0> sigma_dyad_ID;
    cholesky_factor_corr[2] L_Rho_dyad_ID;
}

transformed parameters{
	// Define all matrices
	matrix [N_i__j_ID,2] v_i__j_ID;
	matrix [N_dyads,2] v_dyad_ID;
	
	{
		vector[2] sigma_temp;
		
	v_i__j_ID = (diag_pre_multiply ( sigma_i__j_ID, L_Rho_i__j_ID ) * z_i__j_ID)';
	
	sigma_temp[1] = sigma_dyad_ID;
	sigma_temp[2] = sigma_dyad_ID;
	v_dyad_ID = (diag_pre_multiply ( sigma_temp, L_Rho_dyad_ID) * z_dyad_ID)';
	}
}

model{
    vector[N] lambda;
    
    //priors
    a ~ normal( 0 , 5 );
    
    to_vector (z_i__j_ID) ~ normal (0,1);
    to_vector (z_dyad_ID) ~ normal (0,1);
    
    sigma_i__j_ID ~ exponential (1);
    sigma_dyad_ID ~ exponential (1);
    
    L_Rho_i__j_ID ~ lkj_corr_cholesky(4);
    L_Rho_dyad_ID ~ lkj_corr_cholesky(4);

// LIKELIHOOD
    for ( i in 1:N ) {
        
        lambda[i] = 
        a
        
        // + ln_offset[i]
 
       	+ v_i__j_ID [i_ID[i],1] 
       	+ v_i__j_ID [j_ID[i],2]
       	+ v_dyad_ID [ ij_dyad_ID[i], i_in_dyad[i] ]
       	;
      	}
      	
      	// update target
      	y ~ poisson_log (lambda);
      	}
      	
generated quantities{
	matrix[2,2] Rho_i__j_ID;
	matrix[2,2] Rho_dyad_ID;
	real<lower=0> variance_i;
	real<lower=0> variance_j;
	real<lower=0> variance_ij;
	real<lower=0> sum_variance;
	real<lower=0> VPC_i;
	real<lower=0> VPC_j;
	real<lower=0> VPC_ij;
	//vector[N] log_lik;
	
	Rho_i__j_ID = L_Rho_i__j_ID * L_Rho_i__j_ID';
	Rho_dyad_ID = L_Rho_dyad_ID * L_Rho_dyad_ID';
	
	variance_i = sigma_i__j_ID[1]^2;
	variance_j = sigma_i__j_ID[2]^2;
	variance_ij = sigma_dyad_ID^2;
	
	sum_variance = variance_i + variance_j + variance_ij;
	
	VPC_i = variance_i / sum_variance;
	VPC_j = variance_j / sum_variance;
	VPC_ij = variance_ij / sum_variance;
	
	/*
	for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | a
 
       	+ v_i__j_ID [i_ID[i],1] 
       	+ v_i__j_ID [j_ID[i],2]
       	+ v_dyad_ID [ ij_dyad_ID[i], i_in_dyad[i] ]);
  }
  */
}
