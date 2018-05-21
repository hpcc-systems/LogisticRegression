IMPORT $ AS LR;
IMPORT LR.Types;
empty := DATASET([], Types.WorkItem_mapping);
Ex_Model_Coef := RECORD(Types.Full_Model_Coef)
  BOOLEAN isIntercept;
  STRING field_name;
END;
nom_map := RECORD(Types.FieldName_Mapping)
  Types.t_FieldNumber col_num;
END;
nom_map cvt_map(Types.FieldName_Mapping name) := TRANSFORM
  SELF.col_num := (UNSIGNED) name.assigned_name;
  SELF := name;
END;

/**
  * Apply external labels for work items and field names to a model.
  * <p>Returns an expanded model that includes:<ul>
  * <li>coefficients</li>
  * <li>z and p-values</li>
  * <li>independent variable field names</li>
  * <li>dependent variable field names</li>
  * <li>work-item names</li></ul>
  *
  * @param mod_ds the model as returned from GetModel.
  * @param expl_map the relation of the explanatory or independent variables
  *         to the field names for those variables in FieldName_Mapping format.
  * @param resp_map the relation of the response variable column
  *                numbers to the field names in FieldName_Mapping format.
  * @param wi_map (optional) mapping of workitem strings to workitem nominals in
  *               FieldName_Mapping format.
  * @param level (optional) value for confidence intervals. Default = 0.05.
  * @return an expanded model in External_Model format.
  * @see Types.FieldName_Mapping
  * @see Types.External_Model
  **/
EXPORT DATASET(Types.External_Model)
      Named_Model(DATASET(Types.Layout_Model) mod_ds,
                  DATASET(Types.FieldName_Mapping) expl_map,
                  DATASET(Types.FieldName_Mapping) resp_map,
                  DATASET(Types.WorkItem_mapping) wi_map=empty,
                  REAL8 level=0.05) := FUNCTION
  // Extract full model
  m := LR.ExtractBeta_full(mod_ds, level);
  // prep names for explanatory variables
  used_var(STRING s) := REGEXFIND('^\\s*\\d+\\s*$',s);
  ind_map := PROJECT(expl_map(used_var(assigned_name)),cvt_map(LEFT));
  // label the coefficients
  Ex_Model_Coef get_name(Types.Model_Coef mc, nom_map nom) := TRANSFORM
    SELF.isIntercept := mc.ind_col=0;
    SELF.field_name := nom.orig_name; // no record for intercept, so blank
    SELF := mc;
    SELF := [];
  END;
  named_coef := JOIN(m, ind_map, LEFT.ind_col=RIGHT.col_num,
                     get_name(LEFT, RIGHT), LOOKUP, LEFT OUTER);
  // roll the coefficients into child dataset
  Types.External_Model emod(Ex_Model_Coef mc, DATASET(Ex_Model_Coef) rws):=TRANSFORM
    SELF.wi := mc.wi;
    SELF.dep_nom := mc.dep_nom;
    SELF.coef := PROJECT(SORT(rws, ind_col), Types.External_Coef);
    SELF := [];
  END;
  grp_named_coef := GROUP(named_coef, wi, dep_nom, ALL);
  ext_mod := ROLLUP(grp_named_coef, GROUP, emod(LEFT, ROWS(LEFT)));
  // prep response variable names
  dep_map := PROJECT(resp_map(used_var(assigned_name)), cvt_map(LEFT));
  // label response variables
  Types.External_Model get_resp(Types.External_Model m, nom_map nom) := TRANSFORM
    SELF.response_field:=nom.orig_name;
    SELF := m;
  END;
  named_resp := JOIN(ext_mod, dep_map, LEFT.dep_nom=RIGHT.col_num,
                     get_resp(LEFT,RIGHT), LOOKUP, LEFT OUTER);
  // apply work item names
  Types.External_Model wi(Types.External_Model m, Types.WorkItem_mapping wim) := TRANSFORM
    SELF.work_item := IF(wim.orig_wi<>'', wim.orig_wi, (STRING)m.wi);
    SELF := m;
  END;
  rslt := JOIN(named_resp, wi_map, LEFT.wi=RIGHT.wi,
               wi(LEFT, RIGHT), LOOKUP, LEFT OUTER);
  RETURN rslt;
END;