IMPORT $ AS LR;
IMPORT LR.Types;
IMPORT Std.Str;
IMPORT Std.System.ThorLib;
// aliases for convenience
External_model := Types.External_model;


/**
 * LUCI model file description of the model or models from the external
 * version.  The multi-score card per model case assumes that the score
 * card selection is based solely upon the work item field.  If this is
 * not the case, the L1SE records will need to be patched.
 *
 * The model id and name may have a "$" character that is updated to
 * match the work item when there are multiple models applied.  If the
 * strings do not have a "$" character, the work item string is appended.
 *
 * The score card name may have a "$" character which is updated to
 * match the work item. If the name is blank, the score card is named
 * for the work item.
 *
 * LUCI data fields may not contain comma characters.  This function
 * requires that the work item identification strings do not contain
 * characters that need special handling for CSV data.
 *
 * @param rqst the information to map work items to models
 * @param mod the model with the external field names applied
 * @param wi_field the field name holding the work item identification
 * string
 */
EXPORT DATASET(Types.LUCI_Rec)
      LUCI_Model(DATASET(Types.LUCI_Model_Rqst) rqst,
                 DATASET(Types.External_Model) mod,
                 STRING wi_field='work_item') := FUNCTION
  // merge request info to models
  ex_rq := RECORD(Types.LUCI_Model_Rqst)
    UNSIGNED4 rq_nominal;
  END;
  ex_rq enum_rq(Types.LUCI_Model_Rqst rq, UNSIGNED c) := TRANSFORM
    SELF.rq_nominal := (c-1)*ThorLib.nodes() + 1 + ThorLib.node();
    SELF := rq;
  END;
  w_nom := PROJECT(rqst, enum_rq(LEFT, COUNTER), LOCAL);
  w_rqst := RECORD
    UNSIGNED4 rq_nominal;
    STRING model_id;
    STRING model_name;
    STRING sc_name;
    DATASET(Types.External_Model) mods;
  END;
  w_rqst append_rq(Types.External_model m, ex_rq r) := TRANSFORM
    adj_id := Str.Find(r.model_id, '$', 1) > 0;
    adjd_id := Str.FindReplace(r.model_id,'$', m.work_item);
    adj_name := Str.Find(r.model_name, '$', 1) > 0;
    adjd_name := Str.FindReplace(r.model_name, '$', m.work_item);
    SELF.model_id := MAP(adj_id   => adjd_id,
                         adj_name => r.model_id + m.work_item,
                         r.model_id);
    SELF.model_name := MAP(adj_name => adjd_name,
                           adj_id   => r.model_name + ' ' + m.work_item,
                           r.model_name);
    SELF.sc_name := r.score_card_name;
    SELF.mods := DATASET([m], Types.External_Model);
    SELF.rq_nominal := r.rq_nominal;
  END;
  w_rq := JOIN(mod, w_nom,
               LEFT.response_field=RIGHT.response_field
               AND LEFT.work_item IN RIGHT.wi_list,
               append_rq(LEFT, RIGHT), LOOKUP, MANY);
  w_rqst roll_models(w_rqst cumm, w_rqst curr) := TRANSFORM
    SELF.mods := MERGE(SORTED(cumm.mods,wi), SORTED(curr.mods,wi), SORTED(wi));
    SELF := cumm;
  END;
  srted_w_rq := SORT(w_rq, rq_nominal, model_id);
  rolled_w_rq := ROLLUP(srted_w_rq, roll_models(LEFT, RIGHT), rq_nominal, model_id);
  ready_rq := PROJECT(rolled_w_rq,
                      TRANSFORM(w_rqst, SELF.rq_nominal:=COUNTER, SELF:=LEFT),
                      LOCAL);
  // generate LUCI records, with labels for merging
  Work_rec := RECORD(Types.LUCI_Rec)
    UNSIGNED4 rq_nominal;
    UNSIGNED2 seq;
  END;
  // first model record
  Work_rec make_L1MD(w_rqst r) := TRANSFORM
    SELF.rq_nominal := r.rq_nominal;
    SELF.seq := 1;
    SELF.line := 'L1MD,'
               + r.model_id + ',' + r.model_name + ','
               + IF(COUNT(r.mods)>1, 'multi,,', 'single,,');
  END;
  l1md := PROJECT(ready_rq, make_L1MD(LEFT));
  // prepare for score cards and model coefficients
  w_rq_m := RECORD
    UNSIGNED4 rq_nominal;
    STRING model_id;
    STRING score_card;
    STRING work_item;
    BOOLEAN need_se;
    UNSIGNED2 sc_seq;
    UNSIGNED2 se_seq;
    UNSIGNED2 coef_seq;
    DATASET(Types.External_Coef) coef;
  END;
  w_rq_m extract(w_rqst r, Types.External_model m, UNSIGNED c) := TRANSFORM
    append_wi := COUNT(r.mods) > 1 AND Str.Find(r.sc_name, '$', 1) = 0;
    insert_wi := COUNT(r.mods) > 1 AND Str.Find(r.sc_name, '$', 1) > 0;
    SELF.rq_nominal := r.rq_nominal;
    SELF.model_id := r.model_id;
    SELF.score_card := MAP(append_wi   => r.sc_name + m.work_item,
                    insert_wi   => Str.FindReplace(r.sc_name,'$',m.work_item),
                    m.work_item);
    SELF.sc_seq := 1 + c;
    SELF.se_seq := 1 + COUNT(r.mods) + c;
    SELF.need_se := COUNT(r.mods) > 1;
    SELF.coef_seq := 1 + 2 * COUNT(r.mods); // first seq for coef
    SELF.coef := m.coef;
    SELF.work_item := m.work_item;
  END;
  step1_sc := NORMALIZE(ready_rq, LEFT.mods, extract(LEFT, RIGHT, COUNTER));
  step2_sc := GROUP(step1_sc,rq_nominal, LOCAL);
  w_rq_m coef_seq(w_rq_m prev, w_rq_m curr) := TRANSFORM
    SELF.coef_seq := IF(prev.coef_seq>0, prev.coef_seq, curr.coef_seq)
                  + COUNT(prev.coef);
    SELF := curr;
  END;
  step3_sc := ITERATE(step2_sc, coef_seq(LEFT, RIGHT));
  ready_sc := UNGROUP(step3_sc);
  // score card records
  Work_rec make_L2SC(w_rq_m card) := TRANSFORM
    SELF.rq_nominal := card.rq_nominal;
    SELF.seq := card.sc_seq;
    SELF.line := 'L2SC,' + card.model_id + ','
              + card.score_card
              + ',' + (STRING)card.coef(isIntercept)[1].w
              + ',1.0/(1.0 + EXP(-Raw_point)),Y,Y,0,Y,1';
  END;
  l2sc := PROJECT(ready_sc, make_L2SC(LEFT));
  // score card election for models with multiple score cards
  need_se := ready_sc(need_se);
  Work_rec make_L2SE(w_rq_m card) := TRANSFORM
    SELF.rq_nominal := card.rq_nominal;
    SELF.seq := card.se_seq;
    SELF.line := 'L2SE,' + card.model_id + ','
              + card.score_card
              + ',' + wi_field + '=' + '\'' + card.work_item + '\'';
  END;
  L2se := PROJECT(need_se, make_l2se(LEFT));
  // model coefficients
  Work_Rec make_L3AT(w_rq_m card, Types.External_Coef coef, UNSIGNED c) := TRANSFORM
    SELF.rq_nominal := card.rq_nominal;
    SELF.seq := card.coef_seq + c;
    SELF.line := 'L3AT,' + card.model_id + ',' + card.score_card + ','
              + 'REAL,' + coef.field_name + ',,HIGH,0,'
              + (STRING)coef.w + ',FORMULA,0,0,0000';
  END;
  l3at := NORMALIZE(ready_sc, LEFT.coef(NOT isIntercept), make_L3AT(LEFT,RIGHT,COUNTER));
  rslt := MERGE(l1md, l2sc, l2se, l3at, SORTED(rq_nominal, seq), LOCAL);
  RETURN PROJECT(rslt, Types.LUCI_rec);
END;
