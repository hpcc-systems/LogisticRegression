IMPORT $ AS LR;
IMPORT $.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

//Aliases
t_work_item := Core_Types.t_work_item;
t_FieldNumber := Core_Types.t_FieldNumber;
t_Discrete := Core_Types.t_Discrete;

/**
 * Binomial confusion matrix.  Work items with multinomial responses
 * are ignored by this function.  The higher value lexically is
 * considered to be the positive indication.
 * @param d confusion detail for the work item and classifier
 * @return confusion matrix for a binomial classifier
 */
EXPORT DATASET(Types.Binomial_Confusion_Summary)
       BinomialConfusion(DATASET(Core_Types.Confusion_Detail) d) := FUNCTION
  W := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    t_Discrete class_id;
  END;
  W normConfusion(Core_Types.Confusion_Detail cd, UNSIGNED c):=TRANSFORM
    SELF.class_id:=CHOOSE(c, cd.predict_class, cd.actual_class);
    SELF := cd;
  END;
  dn := NORMALIZE(d, 2,normConfusion(LEFT, COUNTER));
  class_list := DEDUP(dn, ALL, HASH);
  Positive := RECORD
    class_list.wi;
    class_list.classifier;
    t_Discrete pos_class := MAX(GROUP, class_list.class_id);
    UNSIGNED classes:=COUNT(GROUP);
  END;
  lb0 := TABLE(class_list, Positive, wi, classifier, FEW, UNSORTED);
  positive_labels := lb0(classes <= 2);
  Cell := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 true_positive;
    UNSIGNED8 true_negative;
    UNSIGNED8 false_positive;
    UNSIGNED8 false_negative;
    UNSIGNED8 cond_pos;       //condition positive
    UNSIGNED8 pred_pos;       //predicted condiiton positive
    UNSIGNED8 cond_neg;       //condition negative
    UNSIGNED8 pred_neg;       //predicted condition negative
    UNSIGNED8 obs;
  END;
  Cell cvt_detail(Core_Types.Confusion_Detail d, Positive p) := TRANSFORM
    correct := d.actual_class = d.predict_class;
    condition_positive := d.actual_class = p.pos_class;
    prediction_positive := d.predict_class=p.pos_class;
    SELF.wi := d.wi;
    SELF.classifier := d.classifier;
    SELF.true_positive := IF(correct AND condition_positive, d.occurs, 0);
    SELF.true_negative := IF(correct AND NOT condition_positive, d.occurs, 0);
    SELF.false_positive := IF(NOT correct AND prediction_positive, d.occurs, 0);
    SELF.false_negative := IF(NOT correct AND NOT prediction_positive, d.occurs, 0);
    SELF.cond_pos := IF(condition_positive, d.occurs, 0);
    SELF.pred_pos := IF(prediction_positive, d.occurs, 0);
    SELF.cond_neg := IF(NOT condition_positive, d.occurs, 0);
    SELF.pred_neg := IF(NOT prediction_positive, d.occurs, 0);
    SELF.obs := d.occurs;
  END;
  detail := JOIN(d, positive_labels,
                 LEFT.wi=RIGHT.wi AND LEFT.classifier=RIGHT.classifier,
                 cvt_detail(LEFT, RIGHT));
  srt_details := SORT(detail, wi, classifier);
  grp_details := GROUP(detail, wi, classifier);
  Cell roll_cells(Cell frst, DATASET(Cell) rws) := TRANSFORM
    SELF.true_positive := SUM(rws, true_positive);
    SELF.true_negative := SUM(rws, true_negative);
    SELF.false_positive := SUM(rws, false_positive);
    SELF.false_negative := SUM(rws, false_negative);
    SELF.cond_pos := SUM(rws, cond_pos);
    SELF.pred_pos := SUM(rws, pred_pos);
    SELF.cond_neg := SUM(rws, cond_neg);
    SELF.pred_neg := SUM(rws, pred_neg);
    SELF.obs := SUM(rws, obs);
    SELF := frst;
  END;
  scores := ROLLUP(grp_details, GROUP, roll_cells(LEFT, ROWS(LEFT)));
  Types.Binomial_Confusion_Summary cvt_summary(Cell c) := TRANSFORM
    SELF.prevalence := c.cond_pos / c.obs;
    SELF.accuracy := (c.true_positive+c.true_negative) / c.obs;
    SELF.true_pos_rate := c.true_positive / c.cond_pos;
    SELF.false_neg_rate := c.false_negative / c.cond_pos;
    SELF.false_pos_rate := c.false_positive / c.cond_neg;
    SELF.true_neg_rate := c.true_negative / c.cond_neg;
    SELF.pos_pred_val := c.true_positive / c.pred_pos;
    SELF.false_disc_rate := c.false_positive / c.pred_pos;
    SELF.false_omit_rate := c.false_negative / c.pred_neg;
    SELF.neg_pred_val := c.true_negative / c.pred_neg;
    SELF := c;
  END;
  RETURN PROJECT(scores, cvt_summary(LEFT));
END;
