!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE ACO_run
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 subroutine ACO_run
   ! Aaron Zecchin, April 2003, modified by Joanna Szemis October 2010
   ! ACO_run performs all the required routines for a single run of ACO
      
	use ACO_input
	use ant_colony
	use ant_graph
   
	integer :: n_it,n_ant,i,p,q
	integer :: j,k,count,count2
	integer :: l,r
    real(8) :: cuarea
    
	call initialise_ACO_runtime_parameters
   
	do n_it = 1, max_it                    ! entering iteration loop        
 		do n_ant = 1, max_ant                 ! entering ant loop          
            ant(n_ant)%water_accumulated = 0.0
            do i=1,n_tree
                do p = 1, n_sea
                    ant(n_ant)%sea_cur_area(p) = 0
                    seasons(p)%wuse_crop(:) = -1.0
                    do k = 1, n_crop(p)                 
                        ant(n_ant)%tree(i)%season(p)%crop(k)%area_accumulated = 0
                        ant(n_ant)%tree(i)%season(p)%crop(k)%area_planted(:) = 0
                    end do
                end do
                
				!Initialising the counter for the duration which cuts out possible routes
				do q = 1, max_path
                    bstatus = 0
                    wbstatus = 0
                    do p = 1, n_sea
                        call heuristic_crop(n_it,n_ant,i,q,1,p)
					    call prob_determination(n_it,aco_type,i,n_ant,q,1,p)	! sets probabilities for ant loop
					    ! there are two ways to determine probabilities for ant loop. here is the first one. the second one
					    ! will calculate for the whole decision tree, meaning this subroutine will be run before this loop.
					    call path_selection(n_it,n_ant,i,q,1,p)				    ! calling path selection subroutine                  
                        if (seasons(p)%name_crop(ant(n_ant)%tree(i)%season(p)%dec_crop(q)) /= "dryland") then
                            call heuristic_wuse(n_it,n_ant,i,q,1,p)
                            call prob_determination_wateruse(n_it,aco_type,i,n_ant,q,1,p)
                            call path_selection_wateruse(n_it,n_ant,i,q,1,p)                            
                        end if
                    end do
                end do
			end do           
			call path_evaluation(n_it, n_ant)
		end do
		call global_routines(n_it)  
	end do
       
211 FORMAT(2x, I3, 2x, I3, 2x, I3, 2x, I3)
 end subroutine
