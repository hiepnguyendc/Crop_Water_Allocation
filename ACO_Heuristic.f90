!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine heuristic_crop(num_it,num_ant,itr,dpts,count_dur,cur_sea)

    use ant_graph
    use ant_colony
	use ACO_input
	USE water_model

    real(8) :: small_val = 0.000001
	integer :: num_it, num_ant,itr,dpts,count_dur,cur_sea
    integer :: i, j, k, r, l
    real(8) :: cur_water, cost_active
    real(8) :: avai_water
    real(8), dimension(7) :: wmax = [0.0, 5.5, 9.0, 9.0, 7.5, 6.0, 9.0]

	do r=1,n_crop(cur_sea)
        if (seasons(cur_sea)%name_crop(r) == "dryland") then
            tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost = 0.0
        else
            avai_water = al_yr_res - ant(num_ant)%water_accumulated
            if (seasons(cur_sea)%wuse_crop(r) >= 0) then
                if (avai_water >= array_areas(dpts)*(seasons(cur_sea)%wuse_crop(r)-1)*wcap) then
                    cur_water = (seasons(cur_sea)%wuse_crop(r)-1)*wcap
                    tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost = &
                        (seasons(cur_sea)%aa(r,1)*seasons(cur_sea)%aa(r,2) + &
                        seasons(cur_sea)%bb(r,1)*seasons(cur_sea)%bb(r,2)*cur_water + &
                        seasons(cur_sea)%cc(r,1)*seasons(cur_sea)%cc(r,2)*(cur_water**seasons(cur_sea)%cc(r,3))+&
                        seasons(cur_sea)%dd(r,1)*seasons(cur_sea)%dd(r,2)*(cur_water**seasons(cur_sea)%dd(r,3)))*&
                        seasons(cur_sea)%pcrop(r)-(seasons(cur_sea)%pcost(r)+pwater*cur_water)
                else
                    tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost = 0
                end if
            else
                cur_water = wmax(r)
                if (avai_water >= array_areas(dpts)*cur_water) then
                    tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost =&
                        (seasons(cur_sea)%aa(r,1)*seasons(cur_sea)%aa(r,2) + &
                        seasons(cur_sea)%bb(r,1)*seasons(cur_sea)%bb(r,2)*cur_water + &
                        seasons(cur_sea)%cc(r,1)*seasons(cur_sea)%cc(r,2)*(cur_water**seasons(cur_sea)%cc(r,3))+&
                        seasons(cur_sea)%dd(r,1)*seasons(cur_sea)%dd(r,2)*(cur_water**seasons(cur_sea)%dd(r,3)))*&
                        seasons(cur_sea)%pcrop(r)-(seasons(cur_sea)%pcost(r)+pwater*cur_water)
                else
                    if (avai_water > 0) then
                        cur_water = avai_water/array_areas(dpts)
                        tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost =&
                            (seasons(cur_sea)%aa(r,1)*seasons(cur_sea)%aa(r,2) + &
                            seasons(cur_sea)%bb(r,1)*seasons(cur_sea)%bb(r,2)*cur_water + &
                            seasons(cur_sea)%cc(r,1)*seasons(cur_sea)%cc(r,2)*(cur_water**seasons(cur_sea)%cc(r,3))+&
                            seasons(cur_sea)%dd(r,1)*seasons(cur_sea)%dd(r,2)*(cur_water**seasons(cur_sea)%dd(r,3)))*&
                            seasons(cur_sea)%pcrop(r)-(seasons(cur_sea)%pcost(r)+pwater*cur_water)
                    else
                        tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost = 0.0
                    end if !avai_water <= 0
                end if !avai_water <= array_areas(dpts)*cur_water
            end if !seasons(cur_sea)%wuse_crop(r) <= 0
        end if !seasons(cur_sea)%name_crop(r) /= "dryland"
        
		if (tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost < small_val) then	!Checking to see if cost is equal to zero
            if (seasons(cur_sea)%name_crop(r) == "dryland") then
			    tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%heu = 0.1 !1.0 / zero_cost   !if so then a virtual cost is used to determine visability
            else
                tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%heu = 0.0
            end if
		else																	!if not the actual option cost is used
			tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%heu = (1-1/tree(itr)%dec(dpts)%season(cur_sea)%opt_crop(r)%cost)
        end if
    end do
    
end subroutine heuristic_crop

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine heuristic_wuse(num_it,num_ant,itr,dpts,count_dur,cur_sea)

    use ant_graph
    use ant_colony
	use ACO_input
	USE water_model

    real(8) :: small_val = 0.00000001
	integer :: num_it, num_ant, itr, dpts, count_dur, cur_sea, cur_crop
    integer :: i, j, k, r, l
    real(8) :: avai_water, cur_cost
    
    cur_crop = ant(num_ant)%tree(itr)%season(cur_sea)%dec_crop(dpts)
    if (seasons(cur_sea)%wuse_crop(cur_crop) < 0) then
        if (seasons(cur_sea)%name_crop(cur_crop) == "dryland") then
            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water = 1
            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(1)%cost = 0.0
        else
            r = cur_crop
            avai_water = al_yr_res - ant(num_ant)%water_accumulated
            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water = seasons(cur_sea)%n_opt_water(cur_crop)
            if (avai_water >= 0) then
                do l = 1, seasons(cur_sea)%n_opt_water(cur_crop)
                    if (avai_water >= array_areas(dpts)*&
                        tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(l)%property) then
                        cur_water = tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(l)%property
                        cur_cost = (seasons(cur_sea)%aa(r,1)*seasons(cur_sea)%aa(r,2) + &
                            seasons(cur_sea)%bb(r,1)*seasons(cur_sea)%bb(r,2)*cur_water + &
                            seasons(cur_sea)%cc(r,1)*seasons(cur_sea)%cc(r,2)*(cur_water**seasons(cur_sea)%cc(r,3))+&
                            seasons(cur_sea)%dd(r,1)*seasons(cur_sea)%dd(r,2)*(cur_water**seasons(cur_sea)%dd(r,3)))*&
                            seasons(cur_sea)%pcrop(r)-(seasons(cur_sea)%pcost(r)+pwater*cur_water)
                        if (cur_cost > 0) then
                            tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(l)%cost = cur_cost
                        else
                            tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(l)%cost = 0.0
                        end if
                    else
                        tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(l)%cost = 0.0
                    end if
                end do
            else
                tree(itr)%dec(dpts)%season(cur_sea)%crop(r)%opt_water(:)%cost = 0.0
            end if !avai_water > 0
        end if !seasons(cur_sea)%name_crop(cur_crop) == "dryland"
        
	    do r=1,tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%max_opt_water
		    if (tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%cost < small_val) then	!Checking to see if cost is equal to zero
			    tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%heu = 0.0 !/ zero_cost		!if so then a virtual cost is used to determine visability
            else																	!if not the actual option cost is used
                if (r == 1) then
                    tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%heu = &
                            (1-1/tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%cost)
                else
                    if (tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%cost > &
                            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r-1)%cost) then
			            tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%heu = &
                            (1-1/tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%cost)
                    else
                        tree(itr)%dec(dpts)%season(cur_sea)%crop(cur_crop)%opt_water(r)%heu = 0.0
                    end if
                end if
            end if
        end do
    end if ! seasons(cur_sea)%wuse_crop(r) == 0
    
end subroutine heuristic_wuse
