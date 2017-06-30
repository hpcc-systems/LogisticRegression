IMPORT ML_Core.Types AS Core_Types;

EXPORT Types := MODULE
  EXPORT AnyField     := Core_Types.AnyField;
  EXPORT NumericField := Core_Types.NumericField;
  EXPORT DiscreteField:= Core_Types.DiscreteField;
  EXPORT Layout_Model := Core_Types.Layout_Model;
  EXPORT t_work_item  := Core_Types.t_work_item;
  EXPORT t_RecordID   := Core_Types.t_RecordID;
  EXPORT t_FieldNumber:= Core_Types.t_FieldNumber;
  EXPORT t_FieldReal  := Core_Types.t_FieldReal;
  EXPORT t_Discrete   := Core_Types.t_discrete;
  EXPORT t_Universe := UNSIGNED1;
  EXPORT Field_Desc := RECORD
    t_FieldNumber number;   // the column
    UNSIGNED4 cardinality;  // 0 for too many values
    REAL8 min_value;
    REAL8 max_value;
  END;
  EXPORT Data_Info := RECORD
    t_work_item wi;
    UNSIGNED4 dependent_fields;     // high number
    UNSIGNED4 dependent_records;    // high ID
    UNSIGNED4 independent_fields;   // high number
    UNSIGNED4 independent_records;  // high ID
    UNSIGNED4 dependent_count;
    UNSIGNED4 independent_count;
    DATASET(Field_Desc) dependent_stats;
    DATASET(Field_Desc) independent_stats;
  END;
  EXPORT NumericField_U := RECORD(NumericField)
    t_universe u;
  END;
  EXPORT DiscreteField_U := RECORD(DiscreteField)
    t_universe u;
  END;
  EXPORT Layout_Column_Map := RECORD
    t_work_item wi;
    t_FieldNumber orig_number;
    t_FieldNumber remap_number;
  END;
  EXPORT Classifier_Stats := RECORD
    t_FieldNumber   column;
    t_FieldReal     max_delta;
    UNSIGNED4       iterations;
    UNSIGNED4       correct;
    UNSIGNED4       incorrect;
  END;
  EXPORT Model_Report := RECORD
    t_work_item wi;
    UNSIGNED4   max_iterations;
    REAL8       epsilon;
    UNSIGNED4   dep_vars;
    UNSIGNED4   ind_vars;
    UNSIGNED8   obs;
    UNSIGNED2   builder;
    DATASET(Classifier_Stats) stats;
  END;
  EXPORT Binomial_Confusion_Summary := RECORD
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
    REAL8 prevalence;         //condition positives/total
    REAL8 accuracy;           //true positives+true negatives/total
    REAL8 true_pos_rate;      //true positives/condition positives
    REAL8 false_neg_rate;     //false negatives/condition positives
    REAL8 false_pos_rate;     //false positives/condition negatives
    REAL8 true_neg_rate;      //true negatives/condition negatives
    REAL8 pos_pred_val;       //true positives/predicted condition positive
    REAL8 false_disc_rate;    //false positives/predicted condition positives
    REAL8 false_omit_rate;    //false negatives /predicted condition negatives
    REAL8 neg_pred_val;       //true negatives/predicted condition negatives
  END;
  EXPORT Model_Coef := RECORD
    t_work_item wi;
    t_FieldNumber ind_col;
    t_FieldNumber dep_nom;
    t_FieldReal w;  // weight
    t_FieldReal SE; // standard error
  END;
  EXPORT Confidence_Model_Coef := RECORD(Model_Coef)
    REAL8 upper;
    REAL8 lower;
  END;
  EXPORT pval_Model_Coef := RECORD(Model_Coef)
    REAL8 z;
    REAL8 p_value;
  END;
  EXPORT Full_Model_Coef := RECORD(Model_Coef)
    REAL8 z;
    REAL8 p_value;
    REAL8 upper;
    REAL8 lower;
  END;
  EXPORT External_Coef := RECORD
    BOOLEAN isIntercept;
    STRING field_name;
    t_FieldReal w;
    t_FieldReal SE;
    REAL8 z;
    REAL8 p_value;
    REAL8 upper;
    REAL8 lower;
    t_FieldNumber ind_col;
  END;
  EXPORT External_Model := RECORD
    STRING work_item;
    STRING response_field;
    t_work_item wi;
    t_FieldNumber dep_nom;
    DATASET(External_Coef) coef;
  END;
  EXPORT Raw_Prediction := RECORD(AnyField)
    REAL8 raw;
  END;
  EXPORT Observation_Deviance := RECORD
    t_work_item wi;
    t_RecordID id;
    t_FieldNumber classifier;
    t_Discrete actual;
    t_Discrete predicted;
    REAL8 mod_ll;
    REAL8 mod_dev_component;
    REAL8 mod_dev_residual;
    REAL8 nil_ll;
    REAL8 nil_dev_component;
    REAL8 nil_dev_residual;
  END;
  EXPORT Deviance_Record := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 df; // degrees of freedom
    REAL8 deviance;
    REAL8 AIC;
  END;
  EXPORT AOD_Record := RECORD
    t_work_item wi;
    t_FieldNumber classifier;
    UNSIGNED8 residual_df;
    UNSIGNED8 df;
    REAL8 residual_dev;
    REAL8 deviance;
    REAL8 p_value;
  END;
  EXPORT FieldName_Mapping := RECORD
    STRING orig_name;
    STRING assigned_name;
  END;
  EXPORT WorkItem_Mapping := RECORD
    t_work_item wi;
    STRING orig_wi;
  END;
  EXPORT LUCI_Rec := RECORD
    STRING line;
  END;
  EXPORT LUCI_Model_Rqst := RECORD
    STRING model_id;        // model id
    STRING model_name;      // name on L1MD
    STRING response_field;  // name of the dependent field used in training
    SET OF STRING wi_list;  // can be ALL or one or more work item names
    STRING score_card_name; // score card name pattern
  END;
END;
