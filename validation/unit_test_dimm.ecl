IMPORT $.^ AS LR;
IMPORT Std.BLAS.Types AS Types;
Types.matrix_t x := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
Types.matrix_t a2 := [2.0, 3.0];
Types.matrix_t a3 := [1.0, 2.0, 3.0];
Types.matrix_t c := [10.0, 20.0, 30.0, 40.0, 50.0, 60.0];
Types.matrix_t std1 := [2,4,6,12,15,18];
Types.matrix_t std2 := [2,6,6,12,10,18];
Types.matrix_t std3 := [1,4,4,10,9,18];
Types.matrix_t std4 := [2,12,4,15,6,18];
Types.matrix_t std5 := [34,68,102,144,180,216];
Test_Rec := RECORD
  STRING test_name;
  UNSIGNED2 rows;
  UNSIGNED2 cols;
  Types.matrix_t mat;
END;
STRING desc1 := 'Right diag';
STRING desc2 := 'Left diag';
STRING desc3 := 'Tran, Right';
STRING desc4 := 'Tran, Left';
STRING desc5 := 'Right, C, alpha, beta';
std := DATASET([{desc1, 3, 2, std1}, {desc2, 2, 3, std2},
                {desc3, 2, 3, std3}, {desc4, 2, 3, std4},
                {desc5, 3, 2, std5}], Test_Rec);
// Run tests
test1 := LR.dimm(FALSE, FALSE, FALSE, TRUE, 3, 2, 2, 1.0, x, a2);
test2 := LR.dimm(FALSE, FALSE, TRUE, FALSE, 2, 3, 2, 1.0, a2, x);
test3 := LR.dimm(TRUE, FALSE, FALSE, TRUE, 2, 3, 3, 1.0, x, a3);
test4 := LR.dimm(FALSE, TRUE, TRUE, FALSE, 2, 3, 2, 1.0, a2, x);
test5 := LR.dimm(FALSE, FALSE, FALSE, TRUE, 3, 2, 2, 2.0, x, a2, 3.0, c);
test := DATASET([{desc1, 3, 2, test1}, {desc2, 2, 3, test2},
                 {desc3, 2, 3, test3}, {desc4, 2, 3, test4},
                 {desc5, 3, 2, test5}], Test_Rec);
// Compares
Cell := RECORD
  STRING test_name;
  UNSIGNED2 r;
  UNSIGNED2 c;
  REAL8 v;
END;
Cell cvt2cell(Test_Rec tr, UNSIGNED2 c) := TRANSFORM
  SELF.r := ((c-1) % tr.rows) + 1;
  SELF.c := ((c-1) DIV tr.rows) + 1,
  SELF.v := tr.mat[c],
  SELF.test_name :=tr.test_name;
END;
std_cells := NORMALIZE(std, COUNT(LEFT.mat), cvt2cell(LEFT, COUNTER));
test_cells := NORMALIZE(test, COUNT(LEFT.mat), cvt2cell(LEFT, COUNTER));

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
rslt := JOIN(std_cells, test_cells,
            LEFT.test_name=RIGHT.test_name AND LEFT.r=RIGHT.r
            AND LEFT.c=RIGHT.c,
            cmpr(LEFT, RIGHT), FULL OUTER);
errors := SORT(rslt(err), test_name, c, r);
status := IF(EXISTS(errors), 'Failed', 'Passed');
EXPORT unit_test_dimm := SEQUENTIAL(
                            OUTPUT(status, NAMED('Message')),
                            OUTPUT(errors, NAMED('Error_List')));
