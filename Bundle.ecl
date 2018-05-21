IMPORT Std;
EXPORT Bundle := MODULE(Std.BundleBase)
  EXPORT Name := 'LogisticRegression';
  EXPORT Description := 'Logistic Regression implementation';
  EXPORT Authors := ['HPCCSystems'];
  EXPORT License := 'http://www.apache.org/licenses/LICENSE-2.0';
  EXPORT Copyright := 'Copyright (C) 2017 HPCC Systems®';
  EXPORT DependsOn := ['ML_Core 3.2.1', 'PBblas'];
  EXPORT Version := '3.0.0';
  EXPORT PlatformVersion := '6.2.0';
END;
