SUBROUTINE print_SA_random_loop_file_headers(num_1, num_a)

	! Aaron Zecchin, July 2002
	! Printing headers to respective files

	USE ant_print_control

	INTEGER :: num_1, num_a

	print_count = 1

	IF(print_list == 1) THEN
		WRITE(21,0001)
		WRITE(21,0002) num_a, num_1
		WRITE(21,0001)
	END IF

	IF(print_num /= 0) THEN
		WRITE(22,0001)
		WRITE(23,0001)
		WRITE(22,0002) num_a, num_1
		WRITE(23,0002) num_a, num_1
		WRITE(22,0001)
		WRITE(23,0001)
	END IF 

	IF(print_sum == 1) THEN
		WRITE(24,0001)
		WRITE(24,0002) num_a, num_1
		WRITE(24,0001)
	END IF

	0001 FORMAT('::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::')
	0002 FORMAT(1x, I5, '   RANDOM LOOP NO.',1x, I5)

END SUBROUTINE print_SA_random_loop_file_headers
 
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE SA_program_output

	! Aaron Zecchin, August 2002
	! Call subroutines to print out information for sensitivity runs
	! INPUT:
	! OUTPUT:
    
	CALL print_SA_summary
	CALL print_SA_random_loop_results
	CALL print_SA_detailed_random_loop_results

END SUBROUTINE SA_program_output

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE print_SA_summary

	! Aaron Zecchin, August 2002
	! Prints out summary information for sensitivity runs
	! INPUT:
	! OUTPUT:

	USE sensitivity_analysis

	INTEGER, DIMENSION(max_sa) :: max_e_num, min_e_num, min_count
	INTEGER :: l, i, j
	REAL, DIMENSION(max_sa) :: avg_e_num, max_val, min_val, avg_val
	REAL :: diff, small_val
	PARAMETER(small_val = 0.00000000001)

	DO l = 1, max_sa ! Looping through all sensitivity analysis parameter vals (*)
		min_count(l) = 1
		! determining minimum random loop val's, average vals and maximum vals
		avg_val(l) = sa(l)%rand(1)%val
		max_val(l) = sa(l)%rand(1)%val
		min_val(l) = sa(l)%rand(1)%val
		avg_e_num(l) = sa(l)%rand(1)%evaluation_num
		max_e_num(l) = sa(l)%rand(1)%evaluation_num
		min_e_num(l) = sa(l)%rand(1)%evaluation_num
		DO i = 2, max_rand ! loop through random loop results (**)
			! Determining data for vals
			!   determining min vals
			diff = ABS(sa(l)%rand(i)%val - min_val(l))
			IF(diff < small_val)THEN                        ! checking for equality
				min_count(l) = min_count(l) + 1
			ELSE IF(sa(l)%rand(i)%val < min_val(l))THEN ! checking for smaller val
				min_val(l) = sa(l)%rand(i)%val
				min_count(l) = 1 
			END IF
			IF(max_val(l) < sa(l)%rand(i)%val) max_val(l) = sa(l)%rand(i)%val ! determining max val
			! Determining data for evaluation numbers
			IF(max_e_num(l) < sa(l)%rand(i)%evaluation_num) max_e_num(l) = sa(l)%rand(i)%evaluation_num
			IF(min_e_num(l) > sa(l)%rand(i)%evaluation_num) min_e_num(l) = sa(l)%rand(i)%evaluation_num
			! summating all averages
			avg_val(l) = avg_val(l) + sa(l)%rand(i)%val
			avg_e_num(l) = avg_e_num(l) + sa(l)%rand(i)%evaluation_num
		END DO ! end loop through random loop results         (**)
		avg_val(l) = avg_val(l) / REAL(max_rand)
		avg_e_num(l) = avg_e_num(l) / max_rand
	END DO !Looping through all sensitivity analysis parameter vals            (*)

	WRITE(25,*)
	WRITE(25,*)'Sensitivity Analysis Parameter data;'
	WRITE(25,*)
	WRITE(25,*)'Variable parameter = ', para_name
	WRITE(25,*)'No, of vals = ', max_sa
	WRITE(25,*)'Range of vals = ',sa(1)%para,' to ', sa(max_sa)%para
	WRITE(25,*)

	WRITE(25,*)'Performance summary;'
	WRITE(25,*) '(', max_rand,' random number runs )'
	WRITE(25,*)
	WRITE(25,2103)'Parameter', 'Times Min', 'Minimum', 'Average', 'Maximum', 'Minimum',     'Average',   'Maximum'
	WRITE(25,2103)'val',     'Was Found', 'val',   'val',   'val',  'eval. no.',   'eval. no.', 'eval. no.'
	DO l = 1, max_sa
		WRITE(25,2104) sa(l)%para, min_count(l), min_val(l), avg_val(l), max_val(l), min_e_num(l), avg_e_num(l), max_e_num(l)
	END DO
	WRITE(25,*) 
	WRITE(25,*)

	2103 FORMAT(2x, 8(A12, 1x))                                               ! header for summary
	2104 FORMAT(2x, f9.4, 1x, I12, 1x, 3(f12.3, 1x), I12, 1x, f11.2, 1x, I12) ! rows of summary

END SUBROUTINE print_SA_summary

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE print_SA_random_loop_results

	! Aaron Zecchin, August 2002
	! Prints out  information for random loops
	! INPUT:
	! OUTPUT:

	USE sensitivity_analysis

	INTEGER :: l, i

	WRITE(25,*)'Random Number Loop Results;'
	WRITE(25,*)

	DO l = 1, max_sa
		WRITE(25,*) para_name,' = ',sa(l)%para
		WRITE(25,*)
		WRITE(25,2101)'No.', 'Minimum found', 'Evaluation Number'
		DO i = 1, max_rand
			WRITE(25,2102) i, sa(l)%rand(i)%val, sa(l)%rand(i)%evaluation_num
		END DO
		WRITE(25,*)
	END DO
	WRITE(25,*)

	2101 FORMAT(2x, A3, 1x, 2(A20,1x))       ! header of random loop results
	2102 FORMAT(2x, I3, 1x, f20.5, 1x, I20) ! rows for random loop results

END SUBROUTINE print_SA_random_loop_results

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SUBROUTINE print_SA_detailed_random_loop_results

	! Aaron Zecchin, April 2002
	! Prints out combinations (path selections) for each ant stored in the sa type
	! INPUT:
	! OUTPUT:

	USE ant_graph
	USE sensitivity_analysis
	USE water_model

	INTEGER :: num_it, aco_type,poo,ff ! input
	INTEGER :: i, j, m, l, k, r, q, wq, c_order,ss,p           ! counters
	INTEGER :: output_res,output_irr,output_water,output_land           ! dummy to store ants selections
    real(8) :: water_use, net_return, area_planted
    real(8) :: total_water_use, total_net_return, total_area_planted
    
	WRITE(25,*)
	WRITE(25,*) 'Detailed Random Number Loop Results'
	WRITE(25,*)
    
	DO i = 1, max_sa
		WRITE(25,*) '::::::::::::::::::::::::::::::::::::::::::'
		WRITE(25,*) para_name,' = ',sa(i)%para
		WRITE(25,*) '::::::::::::::::::::::::::::::::::::::::::'
		DO j = 1, max_rand
			WRITE(25,*)'RANDOM LOOP NO.    = ', j
			WRITE(25,*)'val                = ', sa(i)%rand(j)%val
			WRITE(25,*)'COST               = ', sa(i)%rand(j)%cost
			WRITE(25,*)'PENALTY COST       = ', sa(i)%rand(j)%pen_cost
			WRITE(25,*)'EVALUATION NUMBER  = ', sa(i)%rand(j)%evaluation_num
			WRITE(25,*)
            
            total_water_use = 0.0
            total_net_return = 0.0
            total_area_planted = 0.0
            
            do m = 1, n_tree
                do p = 1, n_sea
                    write(25, *) "season", p
                    DO k = 1, n_crop(p)
                        WRITE(25,*) "       crop ", k, seasons(p)%name_crop(k)
                        water_use = 0.0
                        net_return = 0.0
                        area_planted = 0.0
                        do l = 1, max_path
                            q = sa(i)%rand(j)%tree(m)%season(p)%dec_crop(l)
                            if (q == k) then
                                wq = sa(i)%rand(j)%tree(m)%season(p)%crop(q)%dec_water(l)
                                water_use = water_use + tree(m)%dec(l)%season(p)%crop(q)%opt_water(&
                                        wq)%property*array_areas(l)
				                net_return = net_return + sa(i)%rand(j)%tree(m)%season(p)%crop(q)%net_return(l)
                                area_planted = area_planted + sa(i)%rand(j)%tree(m)%season(p)%crop(q)%area_planted(l)
                            end if
                        end do
                
                        if ((seasons(p)%bsea(k) == 0).AND.(seasons(p)%name_crop(k) /= "dryland")) then
                                total_water_use = total_water_use + water_use
                                total_net_return = total_net_return + net_return
                                total_area_planted = total_area_planted + area_planted
                        end if
                
                        WRITE(25,*) water_use /area_planted !*10
                        WRITE(25,*) net_return
                        WRITE(25,*) area_planted
                    END DO
                end do
			    WRITE(25,*)
                WRITE(25,*) total_water_use
                WRITE(25,*) total_net_return
                WRITE(25,*) total_area_planted
            end do
		END DO
		WRITE(25,*)
	END DO
	2403 FORMAT(1x, A3, 2x, 2(A10, 2x))
	2404 FORMAT(1x, I3,2X,I3, 2x, I10, 2x, f10.4,2x,I3)

END SUBROUTINE print_SA_detailed_random_loop_results  
     
