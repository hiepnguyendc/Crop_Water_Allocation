!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine check_constraints(num_it,num_ant,itr,dpts,count_dur,cur_sea)

! Duc Cong Hiep Nguyen, February 2014

    use ant_graph
	use ant_colony

	integer :: num_ant,itr,dpts,count_dur,cur_sea
    
    call constraints_sea_max_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)
    if (ant(num_ant)%tree(itr)%season(cur_sea)%sea_status == 0) then
        call constraints_crop_max_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)
        call constraints_crop_min_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)
        call constraints_avai_water(num_it,num_ant,itr,dpts,count_dur,cur_sea)
    end if
    
end subroutine check_constraints

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine constraints_sea_max_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)

! Duc Cong Hiep Nguyen, February 2014

    use ant_graph
	use ant_colony
   
	integer :: i, j
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    
    if (sea_max_area(cur_sea) - ant(num_ant)%sea_cur_area(cur_sea) - array_areas(dpts) < 0.0) then
        ant(num_ant)%tree(itr)%season(cur_sea)%sea_status = 1         ! need to update the both-season crops 
    else
        if (cur_sea == 1) then
            if (sea_max_area(2) - ant(num_ant)%sea_cur_area(2) - array_areas(dpts) < 0.0) then
                do j = 1, n_crop(1)
                    if (seasons(1)%ws_sea(j) == 3) then
                        ant(num_ant)%tree(itr)%season(1)%max_status(j) = 1
                    end if
                end do
            end if
        end if
    end if
 
end subroutine constraints_sea_max_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_crop_max_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)

! Duc Cong Hiep Nguyen, February 2014
    !check the crops obtaining the maximum values
    
    use ant_graph
	use ant_colony
!    use water_model
   
	integer :: r
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    
    do r = 1,n_crop(cur_sea)
        ! here only check max_status for the same sub-areas; 
        ! if sub-areas are different, max_status may be 0 if 
        ! area_accumulated + sub-area < max_area
        if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%area_accumulated+&
                array_areas(dpts) > ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%max_area) then
            ant(num_ant)%tree(itr)%season(cur_sea)%max_status(r) = 1
        end if
    end do
    
end subroutine constraints_crop_max_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_crop_min_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)

! Duc Cong Hiep Nguyen, February 2014

    !check the crops with the minimum values
    
    use ant_graph
	use ant_colony
    use water_model
   
	integer :: r, AUnit, j, i
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    real(8) :: avai_water, water_min

    if ((sea_max_area(cur_sea) - ant(num_ant)%sea_cur_area(cur_sea) - array_areas(dpts) < sea_min_area(cur_sea)).OR.&
        (maxA_region - ant(num_ant)%sea_cur_dryarea(cur_sea) - array_areas(dpts) < sea_min_area(cur_sea))) then
        do r = 1,n_crop(cur_sea)
            if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(r)%area_accumulated >= seasons(cur_sea)%min_crop_area(r)) then
                ant(num_ant)%tree(itr)%season(cur_sea)%min_status(r) = 1
            end if
        end do
    end if
    
211 FORMAT(2x, I3, 2x, I3, 2x, I3)
    
end subroutine constraints_crop_min_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_avai_water(num_it,num_ant,itr,dpts,count_dur,cur_sea)
! Duc Cong Hiep Nguyen, February 2014

    !check the constraint on maximum area of each season
    
    use ant_graph
	use ant_colony
    use water_model
   
	integer :: r, AUnit, i,j
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    real(8) :: avai_water, need_water

    water_min = 0.0
    do i = 1, n_sea
        do j = 1,n_crop(i)
            if (seasons(i)%min_crop_area(j) > ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated) then
                AUnit = seasons(i)%min_crop_area(j)-&
                        ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated
                if (mod(AUnit,5) == 0) then
                    AUnit = int(AUnit/5)*7
                else
                    AUnit = (int(AUnit/5) + 1)*7
                end if
                if (seasons(i)%wuse_crop(j) /= 0) then
                    water_min = water_min + AUnit*(seasons(i)%wuse_crop(j)-1)*wcap
                end if
            end if
        end do
    end do
    
    do j = 1, n_crop(cur_sea)
        if (al_yr_res > ant(num_ant)%water_accumulated) then
            avai_water = al_yr_res - ant(num_ant)%water_accumulated
        else
            avai_water = 0.0
        end if
        if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated >= seasons(cur_sea)%min_crop_area(j)) then
            if (seasons(cur_sea)%wuse_crop(j) /= 0) then
                need_water = array_areas(dpts)*(seasons(cur_sea)%wuse_crop(j)-1)*wcap
            else 
                need_water = 0
            end if
            if (avai_water - water_min < need_water) then
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%water_status = 1
            end if
        end if
    end do
    
end subroutine constraints_avai_water
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_avai_crop_water(num_it,num_ant,itr,dpts,count_dur,cur_sea)

! Duc Cong Hiep Nguyen, February 2014

    !check the constraint on maximum area of each season
    
    use ant_graph
	use ant_colony
    use water_model
   
	integer :: r, AUnit, i,j
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea, cur_crop
    real(8) :: avai_water, need_water, water_min
    
    cur_crop = ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)
    avai_water = al_yr_res - ant(num_ant)%water_accumulated
    if (seasons(cur_sea)%wuse_crop(cur_crop) == 0) then
        if (seasons(cur_sea)%min_crop_area(cur_crop) > &
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%area_accumulated) then
            AUnit = seasons(cur_sea)%min_crop_area(cur_crop)
            if (mod(AUnit,5) == 0) then
                    AUnit = int(AUnit/5)*7
                else
                    AUnit = (int(AUnit/5) + 1)*7
            end if
            do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
                need_water = AUnit*tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%property
                if (avai_water < need_water) then   
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) = 1
                end if
            end do
        else
            water_min = 0.0
            do i = 1, n_sea
                do j = 1,n_crop(i)
                    if (seasons(i)%min_crop_area(j) > ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated) then
                        AUnit = seasons(i)%min_crop_area(j)-&
                                ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated
                        if (mod(AUnit,5) == 0) then
                            AUnit = int(AUnit/5)*7
                        else
                            AUnit = (int(AUnit/5) + 1)*7
                        end if
                        if (seasons(i)%wuse_crop(j) /= 0) then
                            water_min = water_min + AUnit*(seasons(i)%wuse_crop(j)-1)*wcap
                        end if
                    end if
                end do
            end do
            
            do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
                need_water = array_areas(dpts)*tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%property
                if (avai_water - water_min < need_water) then   
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) = 1
                end if
            end do
        end if
    end if !if (seasons(cur_sea)%wuse_crop(cur_crop) == 0)
    
end subroutine constraints_avai_crop_water
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
