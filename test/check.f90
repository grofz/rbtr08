  program check
    use basetree_m
    use rbtr_m
    implicit none
    integer, parameter :: I4B = selected_int_kind(9)
    type(basetree_t) :: tree
    type(rbtr_t) :: rbtree
    integer :: ires, ins(1)

    interface
      pure function ccc(a,b) result(ires)
        import I4B
        integer :: ires
        integer(I4B), intent(in) :: a(:), b(:)
      end function
    end interface

    tree = basetree_t()
    ires = tree % Insert([10], ccc)
    print *, 'ires 1 =', ires
    ires = tree % Insert([20], ccc)
    print *, 'ires 1 =', ires
    ires = tree % Insert([15], ccc)
    print *, 'ires 1 =', ires
    ires = tree % Insert([5], ccc)
    print *, 'ires 1 =', ires

  print *, "Put some tests in here!"

    rbtree = rbtr_t()
    ires = rbtree % Insert([10], ccc)
    print *, 'ires 1 =', ires
    ires = rbtree % Insert([20], ccc)
    print *, 'ires 1 =', ires


  end program check



  pure function ccc(a,b) result(ires)
    integer, parameter :: I4B = selected_int_kind(9)
    integer :: ires
    integer(I4B), intent(in) :: a(:), b(:)

    if (a(1) < b(1)) then
      ires = 1
    elseif (a(1) > b(1)) then
      ires = -1
    else
      ires = 0
    endif
  end function ccc
