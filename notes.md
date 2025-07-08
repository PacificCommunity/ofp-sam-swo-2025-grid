Variable    | Levels                                 | Control file                                | R object
----------- | -------------------------------------- | ------------------------------------------- | --------------------------
Growth      | internal, external                     | `L_at_Amin` and `parameter_offset_approach` | `MG_parms`
Steepness   | 0.7, 0.8, 0.9                          | `SR_BH_steep`                               | `SR_parms`
Natmort     | estimated, Hamel-Cope, 10th percentile | `NatM_Lorenzen_Fem_GP_1`                    | `MG_parms`
DataWts     | 2*age, 2*weight, 2*length              | `Input variance adjustments factors`        | `Variance_adjustment_list`
RecProp     | 0.3, 0.5, 0.7                          | `RecrDist_GP_1_area_1_month_2`              | `MG_parms`

Grid size:

```
Growth  Steepness  Natmort  DataWts  RecProp
2     * 3        * 3      * 3      * 3       = 162
```

Probably not in grid:

Variable    | Levels                                 | Control file                                | R object
----------- | -------------------------------------- | ------------------------------------------- | --------------------------
Movement    | base, low, high                        | `MoveParm`                                  | `MG_parms`
CPUE series | NZ, observer                           |                                             |
Fballpark   | 0.2@2001                               |                                             |
