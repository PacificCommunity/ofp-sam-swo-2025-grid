Variable    | Levels                                 | Control file                                | R object
----------- | -------------------------------------- | ------------------------------------------- | --------------------------
Growth      | internal, external                     | `L_at_Amin` and `parameter_offset_approach` | `MG_parms`
Steepness   | 0.7, 0.8, 0.9                          | `SR_BH_steep`                               | `SR_parms`
Natmort     | estimated, Hamel-Cope, 10th percentile | `NatM_Lorenzen_Fem_GP_1`                    | `MG_parms`
DataWts     | 2\*age, 2\*weight, 2\*length           | `Input variance adjustments factors`        | `Variance_adjustment_list`
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


I'm putting this here. I have tried to make it comprehensive, describing each of the changes for each of the 6 uncertainty axes. The number of options has changed slightly from our earlier discussion, Arni (sorry some things developed slightly, out of necessity!). This gives a basic grid model "main branch" of 360 models - for which we have separate "CPUE branches", based on a different base model for each CPUE branch, either 2 (NZ out PICT 4 flag CPUE in) or 3 (aspirationally - NZ out, EU in, without the last two years) of these CPUE branches, giving 720 or 1080 models respectively.
 
You may already know some of these changes documented and coded, Kyuhan, but I think it doesn't hurt to repeat them, as a check - and some I have never told you before, or modified slightly. I hope this makes sense!
 
I would run the main branch first as a test 360 models. Firing off the other branch (or two), ought to be routine - but this has slightly smaller batches of files to send to condor?? Less risk splitting it? Perhaps?? More logical? Perhaps??
 
Axis Number | Axis label | Dimension of axis
------------|------------|------------------
1 | Steepness | 3
2 | Recruitment Proportion | 2
3 | Movement | 3
4 | Data Weighting  | 5
5 | Growth | 2
6 | Mortality | 2
 
<img width="877" height="222" alt="image" src="https://github.com/user-attachments/assets/41205018-6d18-4c6b-8cbe-160f93159599" />
