IMPORT $ AS LogisticRegression;
IMPORT LogisticRegression.Types AS Types;
IMPORT LogisticRegression.Constants AS Constants;
IMPORT ML_Core.Types AS Core_Types;
// convenient aliases
Data_Info := Types.Data_Info;
Field_Desc := Types.Field_Desc;
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
// working definitions
Flat_Field_Desc := RECORD(Types.Field_Desc)
  Core_Types.t_work_item wi;
END;

/**
  * Produce summary information about the datasets.
  * <p>When field_details = FALSE, indicates the range
  * for the x and y (independent and dependent) columns.
  * <p>When field_details = TRUE, the cardinality, minimum, and maximum
  * values are returned.  A zero cardinality is returned when the field
  * cardinality exceeds the Constants.limit_card value.
  * <p>Note that
  * a column of all zero values cannot be distinguished from a missing
  * column.
  *
  * @param indep data set of independent variables.
  * @param dep data set of dependent variables.
  * @param field_details Boolean directive to provide field level info.
  * @return a data set of information on each work item in Data_Info format.
  * @see Types.Data_Info
  * @see Constants.limit_card
  */
EXPORT DATASET(Types.Data_Info)
       DataStats(DATASET(Core_Types.NumericField) indep,
                  DATASET(Core_Types.DiscreteField) dep,
                  BOOLEAN field_details=FALSE) := FUNCTION
  // assemble details for independent and dependent data
  // dependent details, treat as roughly grouped by work item
  l1_dep := GROUP(dep(field_details), wi, LOCAL);
  l1_dep_srt := SORT(l1_dep, number , value);
  l1_dep_grp := GROUP(l1_dep_srt, number);
  l1_dep_sgl := DEDUP(l1_dep_grp, value);
  l1_dep_top := TOPN(l1_dep_sgl, Constants.limit_card+1, value);
  // rough groups reduced, local reduction
  l1_dep_mm := TABLE(l1_dep_sgl,
                     {wi, number, min_v:=MIN(GROUP, value),
                      max_v:=MAX(GROUP, value)},
                     wi, number, FEW, UNSORTED, LOCAL);
  g1_dep_mm := TABLE(l1_dep_mm,
                     {wi, number, min_value:=MIN(GROUP, min_v),
                      max_value:=MAX(GROUP, max_v)},
                     wi, number, FEW, UNSORTED);
  l1_dep_cr := TABLE(l1_dep_top, {wi, number, card:=COUNT(GROUP)},
                     wi, number, FEW, UNSORTED, LOCAL);
  g1_dep_cr := TABLE(l1_dep_cr, {wi, number, cardinality:=SUM(GROUP,card)},
                     wi, number, FEW, UNSORTED);
  g1_dep := JOIN(g1_dep_cr, g1_dep_mm,
                 LEFT.wi=RIGHT.wi AND LEFT.number=RIGHT.number,
                 TRANSFORM(Flat_Field_Desc, SELF:=LEFT, SELF:=RIGHT));
  // independent details, same treatment
  l1_ind := GROUP(indep(field_details), wi, LOCAL);
  l1_ind_srt := SORT(l1_ind, number , value);
  l1_ind_grp := GROUP(l1_ind_srt, number);
  l1_ind_sgl := DEDUP(l1_ind_grp, value);
  l1_ind_top := TOPN(l1_ind_sgl, Constants.limit_card+1, value);
  // rough groups reduced, local reduction
  l1_ind_mm := TABLE(l1_ind_sgl,
                    {wi, number, min_v:=MIN(GROUP, value),
                     max_v:=MAX(GROUP, value)},
                    wi, number, FEW, UNSORTED, LOCAL);
  g1_ind_mm := TABLE(l1_ind_mm,
                     {wi, number, min_value:=MIN(GROUP,min_v),
                      max_value:=MAX(GROUP,max_v)},
                     wi, number, FEW, UNSORTED);
  l1_ind_cr := TABLE(l1_ind_top, {wi, number, card:=COUNT(GROUP)},
                     wi, number, FEW, UNSORTED, LOCAL);
  g1_ind_cr := TABLE(l1_ind_cr, {wi, number, cardinality:=SUM(GROUP,card)},
                     wi, number, FEW, UNSORTED);
  g1_ind := JOIN(g1_ind_cr, g1_ind_mm,
                 LEFT.wi=RIGHT.wi AND LEFT.number=RIGHT.number,
                 TRANSFORM(Flat_Field_Desc, SELF:=LEFT, SELF:=RIGHT));

  // assemble summary work item data
  t_dep := TABLE(dep, {wi, dependent_fields:=MAX(GROUP, number),
                       dependent_records:=MAX(GROUP, id),
                       dependent_count:=COUNT(GROUP)},
                 wi, FEW, UNSORTED);
  t_ind := TABLE(indep, {wi, independent_fields:=MAX(GROUP, number),
                         independent_records:=MAX(GROUP, id),
                         independent_count:=COUNT(GROUP)},
                 wi, FEW, UNSORTED);
  t := JOIN(t_dep, t_ind, LEFT.wi=RIGHT.wi,
            TRANSFORM(Data_Info, SELF:=LEFT, SELF:=RIGHT, SELF:=[]));
  Data_Info add_stats(Data_Info par,
                      DATASET(Flat_Field_Desc) d,
                      BOOLEAN ind):=TRANSFORM
    stats := PROJECT(d, Field_Desc);
    SELF.dependent_stats := IF(ind, par.dependent_stats, stats);
    SELF.independent_stats := IF(ind, stats, par.independent_stats);
    SELF := par;
  END;
  d_added := DENORMALIZE(t, g1_dep(field_details), LEFT.wi=RIGHT.wi,
                          GROUP, add_stats(LEFT, ROWS(RIGHT), FALSE));
  i_added := DENORMALIZE(d_added, g1_ind(field_details), LEFT.wi=RIGHT.wi,
                          GROUP, add_stats(LEFT, ROWS(RIGHT), TRUE));
  RETURN i_added;
END;
