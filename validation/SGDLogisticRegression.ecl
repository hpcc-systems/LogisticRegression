/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT python3 as python;

// Compare the result to
l_rst := RECORD
  UNSIGNED4 n_iter;
  REAL sc;
END;

DATASET(l_rst) SGDpy() := EMBED(Python)
  from sklearn.datasets.samples_generator import make_blobs
  from sklearn.linear_model import SGDClassifier
  X, y = make_blobs(n_samples=500000, centers=8, n_features=10, cluster_std=0.4, random_state=0)
  result = []
  for i in range(X.shape[0]):
    row = [(i + 1)] + list(X[i]) + [int(y[i]) + 1]
    result.append(tuple(row))
  sgd = SGDClassifier(loss = 'log', learning_rate = 'constant', eta0 = 0.0005, tol = 0.0001, max_iter = 150)
  sgd.fit(X, y)
  n_iter = sgd.n_iter_
  sc=sgd.score(X,y)
  result.append((n_iter, sc))
  return result
ENDEMBED;

SGDpy();