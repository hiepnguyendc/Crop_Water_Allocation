!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE prob_determination(num_it, aco_type,itr,n_ant,dpts)
! SUBROUTINE prob_AS(itr,n_ant,dpts)
! SUBROUTINE prob_MMAS(itr,n_ant,dpts)
! SUBROUTINE prob_standard(alpha, beta,itr,n_ant,dpts)
!
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
   
  subroutine prob_determination(num_it, aco_type,itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002, modified by Joanna Szemis
! Determines probability distrubutions for each decision point for the selected aco if the ACO type is not ACS
! calls print_prob

    INTEGER :: num_it,aco_type				!number of current iterations, aco type chosen
    INTEGER :: n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    INTEGER :: dum = 1						!Passed into print_prob as this variable location is only used for ACS
    integer :: cur_sea                      !current season

! Determining aco type, note ACS (type 2) is not called as the probability distribution of the ACS changes locally

    if(aco_type==1) call prob_AS(itr,n_ant,dpts,count_dur,cur_sea)
    IF(aco_type==5) CALL prob_MMAS(itr,n_ant,dpts,count_dur,cur_sea)

   end subroutine prob_determination
    
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
  subroutine prob_AS(itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002
! Call prob_standard with AS parameters

    use para_as
    integer::n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    integer :: cur_sea               !current season

    call prob_standard_crop(alpha, beta,itr,n_ant,dpts,count_dur,cur_sea)

  end subroutine prob_AS
     
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    SUBROUTINE prob_MMAS(itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002
! Call prob_standard with MMAS parameters

    USE para_mmas

    integer::n_ant,dpts,itr,count_dur		!number of current ants, decision points and tree
    integer :: cur_sea               !current season
    
    CALL prob_standard_crop(alpha,beta,itr,n_ant,dpts,count_dur,cur_sea)

    END SUBROUTINE prob_MMAS

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::   
 
   subroutine prob_standard_crop(alpha,beta,itr,n_ant,dpts,count_dur,cur_sea)

! Aaron Zecchin, April 2002, modified by Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, October 2012
! Determines the probability distribution using generic ACO weighting function
! INPUT: alpha, beta, ant-graph[ max_path, path(i)%max_edge, path(i)%edge(j)%eta, path(i)%edge(j)%tau ]
! OUTPUT: ant_graph[ path(i)%edge(j)%prob ]
! Note i = 1, max_path, and j = 1, path(i)%max_edge

    use ant_graph
    use ant_colony
	
	real(8) :: alpha, beta				
    real(8) :: tot_prob_crop
	real(8) :: tot_prob_water
    real(8) :: tot_prob_land
    integer :: n_ant,dpts,itr,count_dur
    integer :: k,l,r,m
    integer :: cur_sea               !current season
    
    if (ant(n_ant)%tree(itr)%season(cur_sea)%sea_status == 0) then
        !first two loops to calculate the decision points to choose crops
        !preliminary weighting/probability calculations
        if (bstatus == 0) then
            tot_prob_crop = 0.00
	        do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%max_opt_crop
                if ((ant(n_ant)%tree(itr)%season(cur_sea)%max_status(r) == 0).AND.&
                        (ant(n_ant)%tree(itr)%season(cur_sea)%min_status(r) == 0).AND.&
                        (seasons(cur_sea)%bsea(r) == 0).AND.&
                        (ant(n_ant)%tree(itr)%season(cur_sea)%crop(r)%water_status == 0)) then
		            tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob=&
                        tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%tau**alpha &
			            *tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%heu**beta
		            tot_prob_crop=tot_prob_crop+tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob
                else
                    tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob = 0.0
                end if
            end do
            ! Actual prob calculations
            if (tot_prob_crop > 0) then
	            do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%max_opt_crop
		            tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob=&
                        tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob/tot_prob_crop
		            !prob_crop=tree(itr)%dec(dpts)%crop(l)%opt_water(r)%prob
!                    print *, r, tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%prob
                end do
            end if
        else    ! bstatus == 1
            tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(:)%prob = 0.0
        end if
    else    ! ant(n_ant)%tree(itr)%season(cur_sea)%sea_status == 1
        tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(:)%prob = 0.0
    end if

211 FORMAT(2x, I3, 2x, I3, 2x, I3, 2x, I3)

   end subroutine prob_standard_crop
 
    
