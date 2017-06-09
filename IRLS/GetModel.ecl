IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $.^ AS LR;
IMPORT LR.Constants;
IMPORT LR.Types;
IMPORT $ AS IRLS;
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
    BOOLEAN run_local;
  END;
  wi_Map mark_wi(Data_Info di) := TRANSFORM
    SELF.wi := di.wi;
    SELF.run_local := di.independent_fields*di.independent_fields < cap
                  AND di.independent_records*di.independent_fields<cap;
  END;
  process_map := PROJECT(stats, mark_wi(LEFT));
  local_wi := process_map(run_local);
  global_wi := process_map(NOT run_local);
  // partition data
  local_dep := JOIN(dependents, local_wi, LEFT.wi=RIGHT.wi,
                    TRANSFORM(DiscreteField, SELF:=LEFT), LOOKUP, FEW);
  local_ind := JOIN(independents, local_wi, LEFT.wi=RIGHT.wi,
                    TRANSFORM(NumericField, SELF:=LEFT), LOOKUP, FEW);
  global_dep:= JOIN(dependents, global_wi, LEFT.wi=RIGHT.wi,
                    TRANSFORM(DiscreteField, SELF:=LEFT), LOOKUP, FEW);
  global_ind:= JOIN(independents, global_wi, LEFT.wi=RIGHT.wi,
                    TRANSFORM(NumericField, SELF:=LEFT), LOOKUP, FEW);
  // process data
  local_model := IRLS.getModel_local(local_ind, local_dep,
                                     max_iter, epsilon, ridge);
  global_model:= IRLS.getModel_global(global_ind, global_dep,
                                      max_iter, epsilon, ridge);
  rslt := IF(EXISTS(local_wi), local_model)
        + IF(EXISTS(global_wi), global_model);
  RETURN rslt;
END;
