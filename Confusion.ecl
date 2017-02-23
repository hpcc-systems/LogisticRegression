IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $ AS LR;
IMPORT LR.Types AS Types;
// aliases
AnyField     := Core_Types.AnyField;
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
t_Discrete   := Core_Types.t_Discrete;
Model_Coef   := Types.Model_Coef;
Classify_Result:= Core_Types.Classify_Result;
Confusion_Detail := Core_Types.Confusion_Detail;

/**
 * Detail confusion records to compare actual versus predicted response
 * variable values.
 * @param dependents the original response values
 * @param predicts the predicted responses
 * @return confusion counts by predicted and actual response values.
 */
EXPORT DATASET(Confusion_Detail)
       Confusion(DATASET(DiscreteField) dependents,
                 DATASET(DiscreteField) predicts) := FUNCTION
  //
  Confusion_Detail score(DiscreteField y, DiscreteField p) := TRANSFORM
    SELF.classifier := y.number;
    SELF.actual_class := y.value;
    SELF.predict_class := p.value;
    SELF.occurs := 1;
    SELF.correct := y.value = p.value;
    SELF := y;
  END;
  scored := JOIN(dependents, predicts,
                 LEFT.wi=RIGHT.wi AND LEFT.id=RIGHT.id
                 AND LEFT.number=RIGHT.number,
                 score(LEFT, RIGHT));
  srt_dtl := SORT(scored, wi, classifier, actual_class, predict_class);
  grp_dtl := GROUP(srt_dtl, wi, classifier, actual_class, predict_class);
  rolled := ROLLUP(grp_dtl, GROUP,
                   TRANSFORM(Confusion_Detail,
                             SELF.occurs := SUM(ROWS(LEFT), occurs),
                             SELF:=LEFT));
  RETURN rolled;
END;