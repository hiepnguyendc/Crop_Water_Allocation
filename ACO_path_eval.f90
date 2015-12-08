!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
! CONTAINS:
! SUBROUTINE path_evaluation(num_it, num_ant)
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
   
subroutine  path_evaluation(num_it, num_ant)
	!Joanna Szemis, October 2010, modified by Duc Cong Hiep Nguyen, October 2012
	!Sets ups decision variables chosen to be used in simulation model
	use ant_graph
	use ant_colony
	USE water_model
 
	integer :: num_it, num_ant			!current number or iterations and ants
	real(8) :: val,pure_cost			!objective,actual cost
	REAL(8) :: penalty_cost,penalty		!penalty cost and penalty
  
!	!Evaluating path fitness
	CALL evaluate_objective(val, pure_cost, penalty_cost, penalty,num_ant)

	! Storing relevant information in ant type
	ant(num_ant)%val      = val
	ant(num_ant)%cost     = pure_cost
	ant(num_ant)%pen_cost = penalty_cost
	ant(num_ant)%pen      = penalty

end subroutine
