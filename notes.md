Variable    | Levels                                        | Control file (81_Mlo)                                          | r4ss ctl object
----------- | --------------------------------------------- | -------------------------------------------------------------- | ---------------
Growth      | internal, external                            | L_at_Amin, L_at_Amax (L1, L2, k) and parameter_offset_approach | MG_parms
Steepness   | 3 levels                                      | SR_BH_steep                                                    | SR_parms
Natmort     | medium, low, estimate, somewhat low, very low | NatM_Lorenzen_Fem_GP_1                                         | MG_parms
RecProp     | 0.3, 0.5, 0.7                                 | RecrDist_GP_1_area_1_month_2                                   | MG_parms
Movement    | base, very low, high                          | MoveParm                                                       | MG_parms
DataWts     | age, weight, length                           | Input variance adjustments factors                             | Variance_adjustment_list


CPUE series | NZ, observer
Fballpark   | 0.2@2001


--------------------------------------------------------------------------------

Growth  Steepness  Natmort  RecProp  Movement  DataWts
2     * 3        * 4      * 3      * 3       * 3
