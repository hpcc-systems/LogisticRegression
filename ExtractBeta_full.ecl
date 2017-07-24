IMPORT $ AS LR;
IMPORT LR.Types AS Types;
IMPORT ML_Core.Math AS Core_Math;
IMPORT ML_Core.Types AS Core_Types;
// convenience aliases
id_betas := LR.Constants.id_betas;
id_betas_coef := LR.Constants.id_betas_coef;
id_betas_SE := LR.Constants.id_betas_SE;
id_base := LR.Constants.id_base;
base_ind_vars := LR.Constants.base_ind_vars;
t_work_item := Core_Types.t_work_item;
Model := Core_Types.Layout_Model;
Full_Coef := Types.Full_Model_Coef;
// helpers
ivdf_rec := RECORD
  t_work_item wi;
  REAL8 df;
  REAL8 ind_vars;
END;
ivdf_rec make_ivdf(Model obs, Model vars) := TRANSFORM
  SELF.wi := obs.wi;
  SELF.df := obs.value - vars.value - 1;
  SELF.ind_vars := vars.value;
END;
Work := RECORD(Full_Coef)
  REAL8 df;
  REAL8 ind_vars;
END;
/**
 * Extract the coefficient information including confidence intervals
 * @param mod_ds the model information
 * @param level the significance value for the intervals
 * @return the coefficient information for the model
 */
EXPORT DATASET(Types.Full_Model_Coef)
       ExtractBeta_full(DATASET(Core_Types.Layout_Model) mod_ds,
                        REAL8 level=0.05):=FUNCTION
  iv := mod_ds(id=id_base AND number=LR.Constants.base_ind_vars);
  no := mod_ds(id=id_base AND number=LR.Constants.base_obs);
  ivdf := JOIN(no, iv, LEFT.wi=RIGHT.wi, make_ivdf(LEFT,RIGHT), LOOKUP);
  Work pick(Core_Types.Layout_Model m, ivdf_rec d) := TRANSFORM
    first_w := id_betas + (id_betas_coef*(d.ind_vars+1));
    last_w := first_w + d.ind_vars;  // ind vars does not include constant
    first_SE := id_betas +(id_betas_SE*(d.ind_vars+1));
    last_SE := first_SE + d.ind_vars;// ind vars does not include constant
    SELF.ind_col := (m.id - id_betas) % (d.ind_vars+1);
    SELF.dep_nom := m.number;
    SELF.wi := m.wi;
    SELF.w := IF(m.id BETWEEN first_w AND last_w, m.value, 0);
    SELF.SE := IF(m.id BETWEEN first_SE AND last_SE, m.value, 0);
    SELF.df := d.df;
    SELF.ind_vars := d.ind_vars;
    SELF := [];
  END;
  b := JOIN(mod_ds, ivdf,
            LEFT.wi=RIGHT.wi
            AND LEFT.id BETWEEN id_betas AND id_betas+2*(RIGHT.ind_vars+1)-1,
            pick(LEFT, RIGHT), LOOKUP);
  grp_b := GROUP(b, wi, dep_nom, ind_col, ALL);
  Work roll_c(Work base, Work incr) := TRANSFORM
    SELF.w := IF(base.w = 0, incr.w, base.w);
    SELF.SE := IF(base.SE = 0, incr.SE, base.SE);
    SELF := base;
  END;
  rolled := ROLLUP(grp_b, roll_c(LEFT, RIGHT), wi, dep_nom, ind_col,
                 UNORDERED, UNSTABLE);
  Full_Coef decorate(Work coef) := TRANSFORM
    margin := Core_Math.Distributions.T_PPF((1.0-level)/2, coef.df);
    Z := coef.w / coef.SE;
    SELF.z := Z;
    SELF.p_value := 2*(1.0 - Core_Math.Distributions.Normal_CDF(ABS(Z)));
    SELF.upper := coef.w + margin*coef.SE;
    SELF.lower := coef.w - margin*coef.SE;
    SELF := coef;
  END;
  rslt := PROJECT(UNGROUP(rolled), decorate(LEFT));
  RETURN rslt;
END;