# Crop_Water_Allocation
This is a set of ant colony optimization (ACO) variants coded by FORTRAN to optimize crop and water allocation problem for two case studies, (1) the case study from Kumar and Khepar (1980) and (2) the case study in River Murray. 

Four ACO variants include:

1. ACO-SDVO: a “standard” ACO algorithm which uses static decision variable options (SDVOs) but does not incorporate domain knowledge through the use of visibility factors (VFs).

2. ACO-DDVO: an ACO algorithm that uses the dynamic decision variable option (DDVO) adjustment introduced by Nguyen et al. (2016) but does not incorporate domain knowledge through the use of VFs.

3. ACO-SDVO-VF: an ACO algorithm which uses SDVOs and incorporates domain knowledge through the use of VFs.

4. ACO-DDVO-VF: an ACO algorithm that uses the Nguyen et al. (2016) DDVO adjustment and incorporates domain knowledge through the use of VFs.

References

Kumar, R. & Khepar, S.D., 1980. Decision models for optimal cropping patterns in irrigations based on crop water production functions. Agricultural Water Management, 3 (1), 65-76.

Nguyen, D.C.H., Maier, H.R., Dandy, G.C. & Ascough II, J.C., 2016. Framework for computationally efficient optimal irrigation scheduling using ant colony optimization. Environmental Modelling & Software.
