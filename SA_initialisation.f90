! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE SA_initialisation_routines
! SUBROUTINE SA_initialise_parameters
! SUBROUTINE SA_open_files
! SUBROUTINE print_SA_header_summary_file
! SUBROUTINE summary_AS_SA
!
! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine SA_initialisation_routines

! Aaron Zecchin, 10 July 2002
! Calls programs to initialise/read in variables

	CALL SA_initialise_parameters !(max_it, max_sa_main, max_rand_main)
	
end subroutine SA_initialisation_routines

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine SA_initialise_parameters
 
	use ACO_input
	use ant_graph
	use ant_colony
	use sensitivity_analysis
	USE water_model

	integer :: max_sa_main
	integer :: max_rand_main
    integer :: i,j,k,l,r

	open(UNIT = 55, FILE = "SA_input.txt", STATUS = "unknown") 

	high_val = 10.0**20.0

	read(55,*) max_sa
	read(55,*) max_rand
	read(55,*) para_name

	max_sa_main  = max_sa
	max_rand_main = max_rand
	max_evaluation = max_it * max_ant

	allocate(sa(max_sa))

	do i = 1, max_sa
		allocate(sa(i)%rand(max_rand))                 ! allocating rand arrays in sa type
		do j = 1, max_rand
            allocate(sa(i)%rand(j)%tree(n_tree))
            do k = 1, n_tree
                allocate(sa(i)%rand(j)%tree(k)%season(n_sea))
                do m = 1, n_sea
                    allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(n_crop(m)))
                    allocate(sa(i)%rand(j)%tree(k)%season(m)%dec_crop(max_path))
                    allocate(sa(i)%rand(j)%tree(k)%season(m)%prop_crop(max_path))
			        do l = 1, n_crop(m)
				        allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(l)%dec_water(max_path))
				        allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(l)%prop_water(max_path))
                        allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(l)%net_return(max_path))
                        allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(l)%water_use(max_path))
                        allocate(sa(i)%rand(j)%tree(k)%season(m)%crop(l)%area_planted(max_path))
                    end do
                end do
            end do
			sa(i)%rand(j)%val = high_val               ! setting to high value1
		end do
	end do

	read(55,*)(sa(i)%para, i = 1, max_sa)

	close(55)

end subroutine SA_initialise_parameters

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
    SUBROUTINE SA_open_files

! Michael Leonard, 20 July 2002
! Hiep Nguyen, edited 19 Feb 2014

    use ACO_input

    if (aco_type == 1) call SA_open_files_AS
    if (aco_type == 5) call SA_open_files_MMAS
    
    END SUBROUTINE SA_open_files

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    
SUBROUTINE SA_open_files_AS

! Hiep Nguyen, 19 Feb 2014

    use ACO_input
    use ant_colony
    use r_num
    use para_as
    
    character (50) :: filename
    REAL(8) :: alpha1, beta1
    
    alpha1 = alpha
    beta1 = beta
    alpha1 = alpha1 * 10
    beta1 = beta1 * 10
    
    write (filename, 100) int(alpha1), int(beta1), max_it, max_ant, defaultsd
    
    OPEN(UNIT = 25, FILE = filename)

100 FORMAT ("AS_",I6,"_",I6,"_",I6,"_",I6,"_",I5,".txt")
    
END SUBROUTINE SA_open_files_AS

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
SUBROUTINE SA_open_files_MMAS

! Hiep Nguyen, 19 Feb 2014

    use ACO_input
    use ant_colony
    use r_num
    use para_mmas
    use water_model
    
    character (60) :: filename
    REAL(8) :: alpha1, beta1, rho1
    
    alpha1 = alpha
    beta1 = beta
    rho1 = rho
    
    alpha1 = alpha1 * 10
    beta1 = beta1 * 10
    rho1 = rho1 * 10
    
    write (filename, 200) int(alpha1), int(beta1), max_it, max_ant, int(rho1), int(al_yr_res), defaultsd
    
    OPEN(UNIT = 25, FILE = filename)

200 FORMAT ("MMAS_",I6,"_",I6,"_",I6,"_",I6,"_",I6,"_",I8,"_",I5,".txt")
    
END SUBROUTINE SA_open_files_MMAS

! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    SUBROUTINE print_SA_header_summary_file

! Aaron Zecchin, July 25 2002
! Prints out to summary file

    USE ant_colony
    USE ant_graph
	USE ant_store
	USE ACO_input
	USE ant_print_control

    WRITE(25,*)'Summary file for ACO'
    WRITE(25,*)
    WRITE(25,*)'ACO data;'
    WRITE(25,*)
    WRITE(25,*)'No. of ants       = ', max_ant
    WRITE(25,*)'No. of iterations = ', max_it
    WRITE(25,*)'Initial Pheromone = ', tau_0
    WRITE(25,*)
    
    IF(aco_type == 5) CALL summary_MMAS_SA

    WRITE(25,*)'Problem formulation data;'
    WRITE(25,*)
    WRITE(25,*)'No. of decision points   = ', max_path
    WRITE(25,*)'Zero cost                = ', zero_cost
    WRITE(25,*)'Random number seeds      = ', seed_ran_1, seed_ran_2, seed_ran_3, seed_ran_4, seed_ran_5
	WRITE(25,*)'Number or results stored = ', max_store
    WRITE(25,*) 
    WRITE(25,*)'Print Settings'
	WRITE(25,*) print_list, print_sum, print_num
    WRITE(25,*)
	WRITE(25,*)

    END SUBROUTINE print_SA_header_summary_file

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SUBROUTINE summary_AS_SA

    USE para_as

    WRITE(25,*)'ACO type = Ant System (AS)'
    WRITE(25,*) 'Alpha   = ', alpha
    WRITE(25,*) 'Beta    = ',beta
    WRITE(25,*) 'Rho     = ', rho
    WRITE(25,*) 'Q       = ',q
    WRITE(25,*)

    END SUBROUTINE summary_AS_SA

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SUBROUTINE summary_MMAS_SA

    USE para_mmas

    WRITE(25,*)'ACO type = Max-Min Ant System (MMAS)'
    WRITE(25,*) 'Alpha   = ', alpha
    WRITE(25,*) 'Beta    = ',beta
    WRITE(25,*) 'Rho     = ', rho
	WRITE(25,*) 'Q       = ',q
    WRITE(25,*) 'Delta   = ',delta
    WRITE(25,*) 'Pbest   = ',pbest
    WRITE(25,*) 'Freq_g  = ',freq_g
    WRITE(25,*)

    END SUBROUTINE summary_MMAS_SA
