/*##################################################################################
## HPCC SYSTEMS software Copyright (C) 2017,2021 HPCC Systems.  All rights reserved.
################################################################################# */

/**
 * Append an enumeration of string contents to be used as work items.
 * This macro produces 2 external symbols, dsOut and dsOut_Map.  The
 * _Map dataset captures the relationship between the strings that name
 * the work items and the nominal assigned.
 * @param dsIn  the input recordset
 * @param dsOut the symbol to use for the appended data
 * @param src_field a field name
 * @param wi_name the field name for the work item value assigned
 */

EXPORT enum_workitems(dsIn, dsOut, src_field, wi_name) := MACRO
  #UNIQUENAME(types);
  IMPORT $.^.internal AS LR;
  IMPORT LR.Types AS %types%;
  #UNIQUENAME(t1);
  #UNIQUENAME(t2);
  #UNIQUENAME(f1);
  %t1% := TABLE(dsIn,
                {STRING orig_wi:=src_field, UNSIGNED4 wi:=0},
                src_field, FEW, UNSORTED);
  %t2% := PROJECT(%t1%, TRANSFORM(%types%.Workitem_Mapping,
                                  SELF.wi:=COUNTER, SELF:=LEFT));
  #UNIQUENAME(d1);
  #UNIQUENAME(l1);
  #UNIQUENAME(trn);
  %l1% := RECORD(RECORDOF(dsIn))
    UNSIGNED4 wi_name;
  END;
  %l1% %trn%(RECORDOF(dsIn) base, Types.Workitem_Mapping wm):=TRANSFORM
    SELF.wi_name := wm.wi;
    SELF := base;
  END;
  dsOut := JOIN(dsIn, %t2%, LEFT.src_field=RIGHT.orig_wi,
                %trn%(LEFT, RIGHT), LOOKUP, FEW);
  #EXPAND(#TEXT(dsOut)+'_Map := ' + %'t2'% +';');
ENDMACRO;