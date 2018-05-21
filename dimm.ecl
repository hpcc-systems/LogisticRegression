IMPORT Std.BLAS;
IMPORT Std.BLAS.Types AS Types;
/**
 * Matrix multiply when either A or B is a diagonal and is passed
 * as a vector.
 * <p>Computes: alpha*op(A) op(B) + beta*C where op() is transpose.
 * @param transposeA true when transpose of A is used.
 * @param transposeB true when transpose of B is used.
 * @param diagonalA true when A is the diagonal matrix.
 * @param diagonalB true when B is the diagonal matrix.
 * @param m number of rows in product.
 * @param n number of columns in product.
 * @param k number of columns/rows for the multiplier/multiplicand.
 * @param alpha scalar used on A.
 * @param A matrix A.
 * @param B matrix B.
 * @param beta scalar for matrix C.
 * @param C matrix C or empty.
 * @return result matrix in matrix_t format.
 * @see Std.BLAS.Types.matrix_t
 */

EXPORT  Types.matrix_t
        dimm(BOOLEAN transposeA, BOOLEAN transposeB,
             BOOLEAN diagonalA, BOOLEAN diagonalB,
             Types.dimension_t m, Types.dimension_t n, Types.dimension_t k,
             Types.value_t alpha, Types.matrix_t A, Types.matrix_t B,
             Types.value_t beta=0.0, Types.matrix_t C=[]) := BEGINC++
  __lenResult = sizeof(double) * m * n;
  if (!diagonala && !diagonalb) rtlFail(1000, "No diagonal matrix");
  if (diagonala && diagonalb) rtlFail(1000, "Both are matrices diagonal");
  if (diagonala && transposea) rtlFail(1000, "Illegal transpose of diagonal");
  if (diagonalb && transposeb) rtlFail(1000, "Illegal transpose of diagonal");
  __result = rtlMalloc(__lenResult);
  __isAllResult = false;
  double* rslt = (double*) __result;
  double* diag = (diagonala) ? (double*) a  : (double*) b;
  double* mat = (!diagonala) ? (double*) a  : (double*) b;
  double* mat_c = (double*) c;
  uint32_t inc_mat = (!transposea&&!transposeb) ?1 :(diagonala) ?n :k;
  uint32_t len_mat = m * n;
  uint32_t pos_mat = 0;
  uint32_t curr_row = 0;
  uint32_t curr_col = 0;
  uint32_t* pos_diag = (diagonala) ?&curr_row : &curr_col;
  for (uint32_t i=0; i<len_mat; i++) {
    if(curr_row >= m) {
      curr_row = 0;
      curr_col++;
    }
    rslt[i] = diag[*pos_diag] * mat[pos_mat] * alpha
            + ((beta!=0.0 && lenC>0) ? mat_c[i]*beta  : 0.0);
    curr_row++;
    pos_mat += inc_mat;
    if(pos_mat>=len_mat) pos_mat = pos_mat - len_mat + 1;
  }
ENDC++;
