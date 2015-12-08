!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine check_constraints(num_it,num_ant,itr,dpts,count_dur,cur_sea)

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

    use ant_graph
	use ant_colony
   
	integer :: i, j
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    
    if (sea_max_area(cur_sea) - ant(num_ant)%sea_cur_area(cur_sea) - array_areas(dpts) < 0.0) then
        ant(num_ant)%tree(itr)%season(cur_sea)%sea_status = 1         ! need to update the both-season crops 
    end if
 
end subroutine constraints_sea_max_area
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_crop_max_area(num_it,num_ant,itr,dpts,count_dur,cur_sea)
    !check the crops obtaining the maximum values
    
    use ant_graph
	use ant_colony
   
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
    !check the crops with the minimum values
    
    use ant_graph
	use ant_colony
    use water_model
   
	integer :: r, AUnit, j, i
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    real(8) :: avai_water, water_min

    if (sea_max_area(cur_sea) - ant(num_ant)%sea_cur_dryarea(cur_sea) - array_areas(dpts) < sea_min_area(cur_sea)) then
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
    !check the constraint on maximum area of each season
    
    use ant_graph
	use ant_colony
    use water_model
    use qsort_c_module
   
	integer :: r, i,j, k, l, dpts1
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea
    real(8) :: avai_water, need_water
    real(8) :: AUnit, maxb, minb   ! maximum and minimum values of remaining sub-areas
    real(8), allocatable, dimension(:) :: rem_areas, act_areas  ! remaining areas

    if ((n_dpts-dpts) == 0) then
        if (sea_min_area(cur_sea) > 0.0) then
            do j = 1, n_crop(cur_sea)
                if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated >= seasons(cur_sea)%min_crop_area(j)) then
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%water_status = 1
                end if
            end do
        else
            do j = 1, n_crop(cur_sea)
                if (al_yr_res > ant(num_ant)%water_accumulated) then
                    avai_water = al_yr_res - ant(num_ant)%water_accumulated
                else
                    avai_water = 0.0
                end if
                if (ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%area_accumulated >= seasons(cur_sea)%min_crop_area(j)) then
                    if (seasons(cur_sea)%wuse_crop(j) >= 0) then
                        need_water = array_areas(dpts)*(seasons(cur_sea)%wuse_crop(j)-1)*wcap
                    else 
                        need_water = 0
                    end if
                    if (avai_water < need_water) then
                        ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%water_status = 1
                    end if
                end if
            end do
        end if
    else
        dpts1 = n_dpts-dpts
        allocate(rem_areas(dpts1))
        allocate(act_areas(dpts1))
        rem_areas = array_areas((dpts+1):n_dpts)
        call QsortC(rem_areas,act_areas)
        water_min = 0.0
        do i = 1, n_sea
            do j = 1,n_crop(i)
                if (seasons(i)%min_crop_area(j) > ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated) then
                    AUnit = seasons(i)%min_crop_area(j)-&
                            ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated
                    k = 0
                    do while ((AUnit > 0.0).AND.(k < dpts1))
                        k = k + 1
                        AUnit = AUnit - rem_areas(k)
                    end do
                    k = n_dpts-dpts - k + 1
                    AUnit = array_areas(dpts)   !0.0
                    do l = k, n_dpts-dpts, 1
                        AUnit = AUnit + rem_areas(l)
                    end do
                    if (seasons(i)%wuse_crop(j) >= 0) then
                        water_min = water_min + AUnit*(seasons(i)%wuse_crop(j)-1)*wcap
                    else
                        water_min = water_min + AUnit*5*wcap
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
                if (seasons(cur_sea)%wuse_crop(j) >= 0) then
                    need_water = array_areas(dpts)*(seasons(cur_sea)%wuse_crop(j)-1)*wcap
                else 
                    need_water = 0
                end if
                if (avai_water - water_min < need_water) then
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(j)%water_status = 1
                end if
            end if
        end do
        deallocate(rem_areas)
        deallocate(act_areas)
    end if
    
end subroutine constraints_avai_water
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

subroutine constraints_avai_crop_water(num_it,num_ant,itr,dpts,count_dur,cur_sea)
    !check the constraint on maximum area of each season
    
    use ant_graph
	use ant_colony
    use water_model
    use qsort_c_module
   
	integer :: r, i,j, k, l, dpts1
	integer :: num_it,num_ant,itr,dpts,count_dur,cur_sea, cur_crop
    real(8) :: avai_water, need_water, water_min
    real(8) :: AUnit, maxb, minb   ! maximum and minimum values of remaining sub-areas
    real(8), allocatable, dimension(:) :: rem_areas, act_areas  ! remaining areas
    
    cur_crop = ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)
    avai_water = al_yr_res - ant(num_ant)%water_accumulated
    if (seasons(cur_sea)%wuse_crop(cur_crop) < 0) then
        if (seasons(cur_sea)%min_crop_area(cur_crop) > &
                ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%area_accumulated) then
            dpts1 = n_dpts-dpts
            allocate(rem_areas(dpts1))
            allocate(act_areas(dpts1))
            rem_areas = array_areas((dpts+1):n_dpts)
            call QsortC(rem_areas,act_areas)
            water_min = 0.0
            
            AUnit = seasons(cur_sea)%min_crop_area(cur_crop)-&
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%area_accumulated
            k = 0
            do while ((AUnit > 0.0).AND.(k < dpts1))
                k = k + 1
                AUnit = AUnit - rem_areas(k)
            end do
            k = n_dpts-dpts - k + 1
            AUnit = array_areas(dpts)
            do l = k, n_dpts-dpts, 1
                AUnit = AUnit + rem_areas(l)
            end do

            do r = 1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
                need_water = AUnit*tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%property
                if (avai_water < need_water) then   
                    ant(num_ant)%tree(itr)%season(cur_sea)%crop(cur_crop)%dec_water_status(r) = 1
                end if
            end do
            deallocate(rem_areas)
            deallocate(act_areas)
        else
            dpts1 = n_dpts-dpts
            allocate(rem_areas(dpts1))
            allocate(act_areas(dpts1))
            rem_areas = array_areas((dpts+1):n_dpts)
            call QsortC(rem_areas,act_areas)
            water_min = 0.0
            do i = 1, n_sea
                do j = 1,n_crop(i)
                    if (seasons(i)%min_crop_area(j) > ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated) then
                        AUnit = seasons(i)%min_crop_area(j)-&
                            ant(num_ant)%tree(itr)%season(i)%crop(j)%area_accumulated
                        k = 0
                        do while ((AUnit > 0.0).AND.(k < dpts1))
                            k = k + 1
                            AUnit = AUnit - rem_areas(k)
                        end do
                        k = n_dpts-dpts - k + 1
                        AUnit = 0.0
                        do l = k, n_dpts-dpts, 1
                            AUnit = AUnit + rem_areas(l)
                        end do
                        if (seasons(i)%wuse_crop(j) >= 0) then
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
    end if !if (seasons(cur_sea)%wuse_crop(cur_crop) < 0)
    
end subroutine constraints_avai_crop_water
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
