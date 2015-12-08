!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE path_selection(num_it, num_ant,itr,dpts)
! SUBROUTINE selection_standard(num_ant,itr,dpts)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
  
 subroutine path_selection(num_it,num_ant,itr,dpts,count_dur,cur_sea)
 
! Aaron Zecchin, April 2002
! calls further subroutines (dependant on aco type) to determine the path that ant "num_ant" is to take
 
   use ant_colony
   use ACO_input
 
   integer :: num_it,num_ant		!current number of iterations and ants
   INTEGER :: itr,dpts,count_dur	!current decision tree and decision point
   integer :: cur_sea               !current season

   iF(aco_type==5) CALL selection_standard(num_ant,itr,dpts,count_dur,cur_sea)
 
 end subroutine path_selection
 
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
 subroutine selection_standard(num_ant,itr,dpts,count_dur,cur_sea)
 
 ! Aaron Zecchin, April 2002 modified Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, February 2014
 ! determines path that num_ant is to take
 
	use ant_graph
	use ant_colony
	use r_num
    use water_model
   
	real(8) :: sum_prob_crop, sum_prob_water, wuse
	integer :: flag,j,i,r,q, checkb, m
	integer :: num_ant,itr,dpts,count_dur,cur_sea
	real(8) :: sum_area, sum_used_area, water_rest,water_avai
      
    !  selection of edges
    !*************** SELECTION - CROPS *************************
    if (bstatus == 1) then
        ant(num_ant)%sea_cur_area(cur_sea) = ant(num_ant)%sea_cur_area(cur_sea) + array_areas(dpts)
        j = ant(num_ant)%tree(itr)%season(cur_sea-1)%dec_crop(dpts)
        do i = 1, n_crop(cur_sea)
            if (seasons(cur_sea-1)%name_crop(j) == seasons(cur_sea)%name_crop(i)) then
                ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = i
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(i)%area_planted(dpts) = array_areas(dpts)
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(i)%area_accumulated = &
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(i)%area_accumulated + array_areas(dpts)
            end if
        end do
        wbstatus = 1
    else
        flag = 0
	    sum_prob_crop = 0.00
	    j = 0
	    ant(num_ant)%tree(itr)%season(cur_sea)%random_crop(dpts) = grnd()     ! generating random number

	    do while ((flag == 0).AND.(j < tree(itr)%dec(dpts)%season(cur_sea)%max_opt_crop))
		    j = j + 1
            sum_prob_crop = sum_prob_crop + tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(j)%prob
		    if (ant(num_ant)%tree(itr)%season(cur_sea)%random_crop(dpts) <= sum_prob_crop) then
			    flag = 1
                ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = j
		    end if
        end do
    
	    ! This occurs only if ant(num_ant)%random(i) = 1.0
	    if(flag == 0) then
            ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = tree(itr)%dec(dpts)%season(cur_sea)%max_opt_crop
        end if            
        if (seasons(cur_sea)%name_crop(ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)) /= "dryland") then
            ant(num_ant)%sea_cur_area(cur_sea) = ant(num_ant)%sea_cur_area(cur_sea) + array_areas(dpts)
        end if
        ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_planted(dpts) = array_areas(dpts)
        ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated = &
            ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated + array_areas(dpts)
            
        if (seasons(cur_sea)%ws_sea(j) == 3) bstatus = 1
    end if ! bstatus == 1

end subroutine selection_standard
