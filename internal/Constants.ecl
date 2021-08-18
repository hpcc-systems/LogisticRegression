/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

// Constants used by Logistic Regression.  Most of these are the
//nominal values used by the Model data set.  A few are used to
//control behavior.
EXPORT Constants := MODULE
  EXPORT UNSIGNED2 limit_card := 10000;  // upper limit for count
  EXPORT REAL8 default_epsilon := 0.00000001;
  EXPORT REAL8 default_ridge := 0.00001;
  EXPORT UNSIGNED4 local_cap := 5000000 : STORED('LR_LOCAL_MATRIX_CAP');
  // model record identifiers
  EXPORT id_base := 0;
  EXPORT id_iters := id_base + 1;
  EXPORT id_delta := id_iters + 1;
  EXPORT id_correct := id_delta + 1;
  EXPORT id_incorrect := id_correct + 1;
  EXPORT id_stat_set := [id_iters, id_delta, id_correct, id_incorrect];
  EXPORT id_betas := id_incorrect + 1;
  // model record bands for the by independent column groups
  EXPORT id_betas_coef := 0;
  EXPORT id_betas_SE := 1;
  // base record columns
  EXPORT base_builder := 1; // columns
  EXPORT base_max_iter := base_builder + 1;
  EXPORT base_epsilon := base_max_iter + 1;
  EXPORT base_ind_vars := base_epsilon + 1;
  EXPORT base_dep_vars := base_ind_vars + 1;
  EXPORT base_obs := base_dep_vars + 1;
  EXPORT base_cls := base_obs + 1;
  // base record column 1 (builder) values
  EXPORT builder_irls_local := 1; // value of (0,1) when IRLS local used
  EXPORT builder_irls_global := 2; //value of (0,1)
  EXPORT builder_softmax := 3;
END;
