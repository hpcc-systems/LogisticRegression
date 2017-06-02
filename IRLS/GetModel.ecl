IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $.^ AS LR;
IMPORT LR.Constants;
IMPORT LR.Types;
IMPORT $ AS IRLS;
IMPORT STD.System.ThorLib;
//Aliases for convenience
AnyField     := Core_Types.AnyField;
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber := Core_Types.t_FieldNumber;
t_FieldReal   := Core_Types.t_FieldReal;
empty_data := DATASET([], NumericField);
Data_Info := Types.Data_Info;
cap := Constants.local_cap;
/**
 * Generate logistic regression model from training data.  The size
 * of the inputs is used to determin which work items are processed
 * with purely local operations (the data is moved once as necessary)
 * or with global operations supporting a work item to use multiple
 * nodes.
 * @param independents the independent values
 * @param dependents the dependent values.
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to pupulate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 * @return coefficient matrix plus model building stats
 */
EXPORT DATASET(Layout_Model)
      GetModel(DATASET(NumericField) independents,
               DATASET(DiscreteField) dependents,
               UNSIGNED max_iter=200,
               REAL8 epsilon=Constants.default_epsilon,
               REAL8 ridge=Constants.default_ridge) := FUNCTION
  // determine which work items are local versus global
  stats := LR.DataStats(independents, dependents, FALSE);
  wi_Map := RECORD
    t_work_item wi;
    BOOLEAN run_global;
  END;
  wi_Map mark_wi(Data_Info di) := TRANSFORM
    obs := MAX(di.independent_records, di.dependent_records);
    dims := di.independent_fields + 1;
    SELF.wi := di.wi;
    SELF.run_global := dims*dims > cap OR obs*dims > cap;
  END;
  raw_map := PROJECT(stats, mark_wi(LEFT));
  // Count cases
  cases := TABLE(raw_map, {globals:=SUM(GROUP, IF(run_global, 1, 0)),
                               locals:=SUM(GROUP, IF(run_global, 0, 1))},
                 FEW, UNSORTED);
  all_globals := EXISTS(cases(globals>0));
  wi_Map update_map(wi_Map wim, RECORDOF(cases) s) := TRANSFORM
    SELF.run_global := MAP(s.globals=0                  => FALSE,
                           s.locals>ThorLib.nodes()/2   => wim.run_global,
                           TRUE);
    SELF := wim;
  END;
  process_map := JOIN(raw_map, cases, TRUE, update_map(LEFT,RIGHT), ALL);
  // divide inputs
  local_ind := JOIN(independents, process_map(NOT run_global),
                    LEFT.wi=RIGHT.wi,
                    TRANSFORM(NumericField, SELF:=LEFT),
                    LOOKUP);
  local_dep := JOIN(dependents, process_map(NOT run_global),
                    LEFT.wi=RIGHT.wi,
                    TRANSFORM(DiscreteField, SELF:=LEFT),
                    LOOKUP);
  global_ind := JOIN(independents, process_map(run_global),
                     LEFT.wi=RIGHT.wi,
                     TRANSFORM(NumericField, SELF:=LEFT),
                     LOOKUP);
  global_dep := JOIN(dependents, process_map(run_global),
                    LEFT.wi=RIGHT.wi,
                    TRANSFORM(DiscreteField, SELF:=LEFT),
                    LOOKUP);
  // process data
  local_model := IRLS.getModel_local(local_ind, local_dep,
                                     max_iter, epsilon, ridge);
  global_model:= IRLS.getModel_global(global_ind, global_dep,
                                      max_iter, epsilon, ridge);
  rslt := IF(EXISTS(process_map(run_global)), global_model)
        + IF(EXISTS(process_map(NOT run_global)), local_model);
  RETURN rslt;
END;
