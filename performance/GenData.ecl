IMPORT ML_Core.Types AS Core_Types;
IMPORT STD;
/**
 * Synthetic data generation module for classification.
 *
 */
EXPORT GenData(UNSIGNED2 num_work_items=5, UNSIGNED2 num_columns=4,
               UNSIGNED4 num_records=1000, UNSIGNED2 num_classes=2,
               BOOLEAN stochastic=TRUE,
               REAL8 confusion=MAX(2.0/num_records,0.001),
               REAL8 low_var=0.8, REAL8 high_var=1.2) := FUNCTION
  Result_Layout := RECORD
    Core_Types.t_work_item wi;
    Core_Types.t_RecordID rid;
    Core_Types.DiscreteField resp_var;
    DATASET(Core_Types.NumericField) explan_vars;
  END;
  Value := RECORD
    Core_Types.t_FieldNumber col;
    UNSIGNED2 class;
    REAL8 v;
  END;
  Work_Item_Spec := RECORD
    Core_Types.t_work_item wi;
    UNSIGNED4 num_records;
    DATASET(Value) seeds;
  END;
  Work_Record_Spec := RECORD
    Core_Types.t_work_item wi;
    Core_Types.t_RecordID rid;
    UNSIGNED2 class;
    DATASET(Value) seeds;
    BOOLEAN confused;
  END;
  REAL8 draw_btw(REAL8 lb, REAL8 ub) := lb+((ub-lb)*((RANDOM()%1000)+1)/1000.0);
  wi_per_node := ((num_work_items-1) DIV Std.System.ThorLib.nodes())+1;
  Value gen_seed(UNSIGNED c) := TRANSFORM
    SELF.col := ((c-1) DIV num_classes) + 1;
    SELF.class := ((c-1) % num_classes);  // zero based
    SELF.v := (RANDOM() % 10000000) /10000;
  END;
  Work_Item_Spec gen_wi(UNSIGNED c):= TRANSFORM
    SELF.wi := 1 + Std.System.ThorLib.node() + (c-1)*Std.System.ThorLib.nodes();
    SELF.num_records := num_records*IF(stochastic, draw_btw(.9,1.1), 1.0);
    SELF.seeds := DATASET(num_columns*num_classes, gen_seed(COUNTER + NOFOLD(0)*c));
  END;
  wi_seeds := DATASET(wi_per_node, gen_wi(COUNTER), LOCAL) (wi<=num_work_items);
  Work_Record_Spec gen_obsrec(Work_Item_Spec wis, UNSIGNED c) := TRANSFORM
    SELF.wi := wis.wi;
    SELF.rid := c;
    SELF.class := RANDOM() % num_classes;
    SELF.seeds := wis.seeds;
    SELF.confused := ((REAL8)(RANDOM()))/4294967296.0 <= confusion;
  END;
  recs := NORMALIZE(wi_seeds, LEFT.num_records, gen_obsrec(LEFT, COUNTER));
  Core_Types.NumericField make_val(Value v, Core_Types.t_work_item wi,
                                   Core_Types.t_RecordID rid) := TRANSFORM
    SELF.wi := wi;
    SELF.id := rid;
    SELF.number := v.col;
    SELF.value := v.v * draw_btw(low_var, high_var);
  END;
  Result_Layout make_obs(Work_Record_Spec w) := TRANSFORM
    sel_class := IF(w.confused, (w.class+1)%num_classes, w.class);
    sel_seeds := w.seeds(class=sel_class);
    SELF.wi := w.wi;
    SELF.rid := w.rid;
    SELF.resp_var.wi := w.wi;
    SELF.resp_var.id := w.rid;
    SELF.resp_var.number := 1;
    SELF.resp_var.value := w.class;
    SELF.explan_vars := PROJECT(sel_seeds, make_val(LEFT, w.wi, w.rid));
  END;
  rslt := PROJECT(recs, make_obs(LEFT));
  RETURN rslt;
END;
