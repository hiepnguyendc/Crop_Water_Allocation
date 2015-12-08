!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! PROGRAM main
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
PROGRAM main1
  
	! Aaron Zecchin, April 2003
	! ACO program that performs a single parameter sensitivity analysis 
  
	USE sensitivity_analysis
    use r_num
  
	INTEGER :: num_sa, i, j
    character(50) :: randomfile
    
     integer, dimension(30) :: starting_point = [51, 403, 792, 1121, 1411, 2286, 3598, 4357, 5798, 6128,&
                                                23, 382, 841, 1309, 1780, 2576, 3101, 4900, 5254, 7302,&
                                                95, 648, 901, 1976, 2721, 3824, 4503, 5603, 6728, 7543]

	CALL ACO_initialisation   !initalsing all relevent paramters/variables used in ACOA
    
	CALL SA_initialisation_routines !initialising all variables used in sensitivity analysis
    
    do i = 1,1
        defaultsd = starting_point(i)
        CALL SA_open_files
  
	    DO num_sa = 1, max_sa       !number of times to complete a senstivity analysis
		    CALL SA_internal_initialisation(num_sa)
		    DO num_rand = 1, max_rand          ! entering loop for random starting locations
			    CALL ACO_run									! the ACOA is implemented here      
			    CALL SA_collect_run_data(num_sa, num_rand)      ! stores ACO data in point type
		    END DO       
        END DO
	    CALL SA_program_output
    end do

END PROGRAM
