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
 
   if(aco_type==1) call selection_standard(num_ant,itr,dpts,count_dur,cur_sea)
   iF(aco_type==5) CALL selection_standard(num_ant,itr,dpts,count_dur,cur_sea)
 
 end subroutine path_selection
 
 !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
 subroutine selection_standard(num_ant,itr,dpts,count_dur,cur_sea)
 
 ! Aaron Zecchin, April 2002 modified Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, October 2012
 ! determines path that num_ant is to take
 
	use ant_graph
	use ant_colony
	use r_num
    use qsort_c_module
    use water_model
   
	real(8) :: sum_prob_crop, sum_prob_water, wuse
	integer :: flag,j,i,r,q, checkb, m
	integer :: num_ant,itr,dpts,count_dur,cur_sea
	real(8) :: sum_area, sum_used_area, water_rest,water_avai
    real(8) :: ran_num
      
    !  selection of edges
    !*************** SELECTION - CROPS *************************
    !check status of maximum area in each season
    if (ant(num_ant)%tree(itr)%season(cur_sea)%sea_status == 1) then
        ant(num_ant)%sea_cur_dryarea(cur_sea) = ant(num_ant)%sea_cur_dryarea(cur_sea) + array_areas(dpts)
        do r = 1, n_crop(cur_sea)
            if (seasons(cur_sea)%name_crop(r) == "dryland") then
                ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = r
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%area_planted(dpts) = array_areas(dpts)
                 ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%area_accumulated = &
                        ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%area_accumulated + array_areas(dpts)
            end if
        end do
    else
        if (bstatus == 1) then
            ant(num_ant)%sea_cur_area(cur_sea) = ant(num_ant)%sea_cur_area(cur_sea) + array_areas(dpts)
            ant(num_ant)%sea_cur_dryarea(cur_sea) = ant(num_ant)%sea_cur_dryarea(cur_sea) + array_areas(dpts)
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
                j = tree(itr)%dec(dpts)%season(cur_sea)%max_opt_crop
                do while ((tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(j)%prob == 0).AND.(j > 1))
                    j = j - 1
                end do
                if (j > 1) then
                    ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = j
                else
                    do i = 1, n_crop(cur_sea)
                        if (seasons(cur_sea)%name_crop(i) == "dryland") then
                            ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts) = i
                        end if
                    end do
                end if
            end if
            
            if (seasons(cur_sea)%name_crop(ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)) /= "dryland") then
                ant(num_ant)%sea_cur_area(cur_sea) = ant(num_ant)%sea_cur_area(cur_sea) + array_areas(dpts)
            end if
                
            if (seasons(cur_sea)%ws_sea(j) == 3) bstatus = 1

            if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated < seasons(cur_sea)%min_crop_area(j)) then
                if (sea_min_area(cur_sea) > array_areas(dpts)) then
                    sea_min_area(cur_sea) = sea_min_area(cur_sea) - array_areas(dpts)
                else
                    sea_min_area(cur_sea) = 0
                end if
            end if
            
            ant(num_ant)%sea_cur_dryarea(cur_sea) = ant(num_ant)%sea_cur_dryarea(cur_sea) + array_areas(dpts)
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_planted(dpts) = array_areas(dpts)
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated = &
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated + array_areas(dpts)
        end if ! bstatus == 1
    end if !ant(num_ant)%tree(itr)%season(cur_sea)%sea_status == 1

end subroutine selection_standard
