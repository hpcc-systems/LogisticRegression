IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

// convenience aliases
id_betas := LR.Constants.id_betas;
id_betas_coef := LR.Constants.id_betas_coef;
id_betas_SE := LR.Constants.id_betas_SE;
id_base := LR.Constants.id_base;
base_ind_vars := LR.Constants.base_ind_vars;
Model_Coef := Types.Model_Coef;
/**
 * Extract the beta values form the model dataset.
 * @param mod_ds the model dataset
 * @return a beta values as Model Coefficient records, zero as the constant
 * term.
 */
EXPORT ExtractBeta(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  iv := mod_ds(id=id_base AND number=LR.Constants.base_ind_vars);
  Model_Coef pick(Core_Types.Layout_Model m, Core_Types.Layout_Model d) := TRANSFORM
    first_w := id_betas + (id_betas_coef*(d.value+1));
    last_w := first_w + d.value;  // ind vars does not include constant
    first_SE := id_betas +(id_betas_SE*(d.value+1));
    last_SE := first_SE + d.value;// ind vars does not include constant
    SELF.ind_col := (m.id - id_betas) % (d.value+1);
    SELF.dep_nom := m.number;
    SELF.wi := m.wi;
    SELF.w := IF(m.id BETWEEN first_w AND last_w, m.value, 0);
    SELF.SE := IF(m.id BETWEEN first_SE AND last_SE, m.value, 0);
  END;
  b := JOIN(mod_ds, iv,
            LEFT.wi=RIGHT.wi
            AND LEFT.id BETWEEN id_betas AND id_betas+2*(RIGHT.value+1)-1,
            pick(LEFT, RIGHT), LOOKUP);
  grp_b := GROUP(b, wi, dep_nom, ind_col, ALL);
  Model_Coef roll_c(Model_Coef base, Model_Coef incr) := TRANSFORM
    SELF.w := IF(base.w = 0, incr.w, base.w);
    SELF.SE := IF(base.SE = 0, incr.SE, base.SE);
    SELF := base;
  END;
  rslt := ROLLUP(grp_b, roll_c(LEFT, RIGHT), wi, dep_nom, ind_col,
                 UNORDERED, UNSTABLE);
  RETURN UNGROUP(rslt);
END;