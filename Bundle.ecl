/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

IMPORT Std;
EXPORT Bundle := MODULE(Std.BundleBase)
  EXPORT Name := 'LogisticRegression';
  EXPORT Description := 'Logistic Regression implementation';
  EXPORT Authors := ['HPCCSystems'];
  EXPORT License := 'http://www.apache.org/licenses/LICENSE-2.0';
  EXPORT Copyright := 'Copyright (C) 2021 HPCC Systems®';
  EXPORT DependsOn := ['ML_Core 3.2.1', 'PBblas'];
  EXPORT Version := '4.0.0';
  EXPORT PlatformVersion := '7.6.36';
END;
