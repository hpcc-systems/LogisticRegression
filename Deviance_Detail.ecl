IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $ AS LR;
IMPORT LR.Types AS Types;
// aliases
AnyField     := Core_Types.AnyField;
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Raw_Pred     := Types.Raw_Prediction;
Obs_Deviance := Types.Observation_Deviance;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
t_Discrete   := Core_Types.t_Discrete;

/**
 * Detail deviance for each observation.
 * @param dependents original dependent records for the model
 * @param predicts the predicted values of the response variable
 * @return the deviance information by observation and the log likelihood
 * of the predicted result.
 */
EXPORT DATASET(Types.Observation_Deviance)
       Deviance_Detail(DATASET(Core_Types.DiscreteField) dependents,
                 DATASET(Types.Raw_Prediction) predicts) := FUNCTION
  // match up prediction to actual
  Obs_Deviance pred_v_act(DiscreteField act, Raw_Pred prd):=TRANSFORM
    sgn := IF(act.value < prd.raw, -1, 1);
    SELF.classifier := prd.number;
    SELF.actual := act.value;
    SELF.predicted := IF(prd.raw>0.5, 1, 0);
    SELF.mod_ll := act.value*LN(prd.raw)+(1-act.value)*LN(1.0-prd.raw);
    SELF.mod_dev_component := 2*LN(1) - 2*SELF.mod_ll;
    SELF.mod_dev_residual := SQRT(SELF.mod_dev_component) * sgn;
    SELF := prd;
    SELF := [];
  END;
  avp := JOIN(dependents, predicts,
             LEFT.wi=RIGHT.wi AND LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,
             pred_v_act(LEFT, RIGHT));
  null_mu := TABLE(dependents, {wi, number, REAL8 mu:=AVE(GROUP, value)},
                   wi, number, FEW, UNSORTED);
  Obs_Deviance null_mod(Obs_Deviance od, RECORDOF(null_mu) m):=TRANSFORM
    sgn := IF(od.actual < m.mu, -1, 1);
    SELF.nil_ll := od.actual*LN(m.mu) + (1-od.actual)*LN(1.0-m.mu);
    SELF.nil_dev_component := 2*LN(1) - 2*SELF.nil_ll;
    SELF.nil_dev_residual := SQRT(SELF.nil_dev_component) * sgn;
    SELF := od;
  END;
  rslt := JOIN(avp, null_mu,
             LEFT.wi=RIGHT.wi AND LEFT.classifier=RIGHT.number,
             null_mod(LEFT, RIGHT), LOOKUP);
  RETURN rslt;
END;