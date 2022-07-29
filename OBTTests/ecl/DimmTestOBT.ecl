/*##############################################################################
    
    HPCC SYSTEMS software Copyright (C) 2022 HPCC Systems®.
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
       
       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
############################################################################## */

// This is the unit_test_dimm file from the validation folder
// with modifications so it can be incorporated into the OBT
// and tested regularly

IMPORT LogisticRegression AS LR;
IMPORT Std.BLAS.Types AS Types;

Types.matrix_t X := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
Types.matrix_t A2 := [2.0, 3.0];
Types.matrix_t A3 := [1.0, 2.0, 3.0];
Types.matrix_t C := [10.0, 20.0, 30.0, 40.0, 50.0, 60.0];
Types.matrix_t Inp1 := [2,4,6,12,15,18];
Types.matrix_t Inp2 := [2,6,6,12,10,18];
Types.matrix_t Inp3 := [1,4,4,10,9,18];
Types.matrix_t Inp4 := [2,12,4,15,6,18];
Types.matrix_t Inp5 := [34,68,102,144,180,216];

TestRec := RECORD
  STRING test_name;
  UNSIGNED2 rows;
  UNSIGNED2 cols;
  Types.matrix_t mat;
END;

STRING Desc1 := 'Right diag';
STRING Desc2 := 'Left diag';
STRING Desc3 := 'Tran, Right';
STRING Desc4 := 'Tran, Left';
STRING Desc5 := 'Right, C, alpha, beta';

// Dataset with all of the inputs
Inputs := DATASET([{Desc1, 3, 2, Inp1}, {Desc2, 2, 3, Inp2},
                {Desc3, 2, 3, Inp3}, {Desc4, 2, 3, Inp4},
                {Desc5, 3, 2, Inp5}], TestRec);

// Run tests
Test1 := LR.dimm(FALSE, FALSE, FALSE, TRUE, 3, 2, 2, 1.0, X, A2);
Test2 := LR.dimm(FALSE, FALSE, TRUE, FALSE, 2, 3, 2, 1.0, A2, X);
Test3 := LR.dimm(TRUE, FALSE, FALSE, TRUE, 2, 3, 3, 1.0, X, a3);
Test4 := LR.dimm(FALSE, TRUE, TRUE, FALSE, 2, 3, 2, 1.0, A2, X);
Test5 := LR.dimm(FALSE, FALSE, FALSE, TRUE, 3, 2, 2, 2.0, X, A2, 3.0, C);

// Dataset of tests
Tests := DATASET([{Desc1, 3, 2, Test1}, {Desc2, 2, 3, Test2},
                 {Desc3, 2, 3, Test3}, {Desc4, 2, 3, Test4},
                 {Desc5, 3, 2, Test5}], TestRec);

// Comparision Record
Cell := RECORD
  STRING test_name;
  UNSIGNED2 r;
  UNSIGNED2 c;
  REAL8 v;
END;

Cell cvt2cell(TestRec tr, UNSIGNED2 c) := TRANSFORM
  SELF.r := ((c-1) % tr.rows) + 1;
  SELF.c := ((c-1) DIV tr.rows) + 1,
  SELF.v := tr.mat[c],
  SELF.test_name :=tr.test_name;
END;

InputCells := NORMALIZE(Inputs, COUNT(LEFT.mat), cvt2cell(LEFT, COUNTER));
TestCells := NORMALIZE(Tests, COUNT(LEFT.mat), cvt2cell(LEFT, COUNTER));

Compare := RECORD
  STRING test_name;
  UNSIGNED2 r;
  UNSIGNED2 c;
  REAL8 v_std;
  REAL8 v_test;
  BOOLEAN err;
  BOOLEAN missing;
END;

Compare cmpr(Cell s, Cell t) := TRANSFORM
  SELF.test_name := IF(s.test_name<>'', s.test_name, t.test_name);
  SELF.r := IF(s.r <> 0, s.r, t.r);
  SELF.c := IF(s.c <> 0, s.c, t.c);
  SELF.v_std := s.v;
  SELF.v_test := t.v;
  SELF.err := t.v <> s.v OR s.test_name <> t.test_name;
  SELF.missing := s.test_name='' OR t.test_name='';
END;

Result := JOIN(InputCells, TestCells,
            LEFT.test_name=RIGHT.test_name AND LEFT.r=RIGHT.r
            AND LEFT.c=RIGHT.c,
            cmpr(LEFT, RIGHT), FULL OUTER);
            
Errors := SORT(Result(err), test_name, c, r);
Status := IF(EXISTS(errors), 'Failed', 'Passed');

SEQUENTIAL(OUTPUT(Status, NAMED('Message')), OUTPUT(Errors, NAMED('Error_List')));
