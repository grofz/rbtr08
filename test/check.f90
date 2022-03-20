  program check
    implicit none
    integer :: n
    logical :: is_validated

    write(*,'(a)',advance='no') 'Items = ? '
    read(*,*) n
    print *

    call test2()

    is_validated = .false.
    if (n < 1001) is_validated = .true.
    call test3(n, is_validated)
  end program check



  subroutine test2
    use tree_tests_m
    use tree_m
    implicit none

    type(rbtr_t)     :: aobj, cobj
    type(basetree_t) :: bobj
    integer :: i
    procedure(cfun_abstract) compare_nodes_fun


    print *, 'RUNING TEST #2 '
    call Insert_nodes(aobj, [10, 7, 5, 2, 6, 3], compare_nodes_fun)
    call Insert_nodes(aobj, [1, 4, 8, 12, 16, 9], compare_nodes_fun)
    call Print_nodes(aobj)
    print *, 'Height of tree is: ', aobj % Height_range()
    print *
 call aobj % Display()

    call Print_nodes(bobj)
    call Insert_nodes(bobj, [10, 20, 30, 40, 50, 60], compare_nodes_fun)
    call Insert_nodes(bobj, [21], compare_nodes_fun)
    call Print_nodes(bobj)
    print *, 'Height of basetree is: ', bobj % Height_range()
    print *
 call bobj % Display()

    call Insert_nodes(cobj, [10, 5, 7, 8, 9, 11, 12, 13], compare_nodes_fun)
    call Print_nodes(cobj)
    print *, 'Height of tree is: ', cobj % Height_range()
    print *
 call cobj % Display()

  end subroutine test2



  subroutine test3(nsize, is_validated)
    use tree_tests_m
    use tree_m, only : rbtr_t, cfun_abstract

    integer, intent(in) :: nsize
    logical, intent(in) :: is_validated
    type(rbtr_t) :: tree
    integer, allocatable, dimension(:) :: y0, ys, yinside, yremoved
    integer :: i, ipart, hr(2)
    real :: log_of_size
    logical found, is_pass
    procedure(cfun_abstract) :: compare_nodes_fun

    print '(a)', 'RUNNING TEST #3'

    ! Fill by N items
    y0 = Get_array(nsize)
    ys = Shuffle_array(y0)
    call Insert_nodes(tree, ys, compare_nodes_fun, IS_VALIDATED)

    ! Height of the tree
    hr = tree % Height_range()
    log_of_size = log(real(tree%size())) / log(2.0)
    print '(a,i0,a,i0,a,f6.2,a,i0,a)', &
    &   'Tree size (hmin<log2(size)<hmax): ', &
    &   tree%size(),' (', hr(1),' <', log_of_size,' < ', hr(2),')'

    if (nsize < 1001) then
      call Print_nodes2(tree)
      call Print_nodes(tree)
    endif

    ! test that all items are present
    ys = Shuffle_array(y0)
    is_pass = .true.
    write(*,'(a)', advance='no') 'All inserted nodes exists?  '
    do i = 1, size(y0)
      found = tree % Exists(ys(i:i), compare_nodes_fun)
      if (.not. found) then
        is_pass = .false.
        exit
      endif
    enddo
    write(*,*) is_pass
    if (.not. is_pass) error stop 'test failed'


    ! Remove part of items
    ipart = nsize/2
    ys = Shuffle_array(y0)
    call Delete_nodes(tree, ys(1:ipart), compare_nodes_fun, IS_VALIDATED)
    yremoved = ys(1:ipart)
    yinside = ys(ipart+1:nsize)

    ! test, that removed are no longer in the tree
    yremoved = Shuffle_array(yremoved)
    yinside = Shuffle_array(yinside)
    is_pass = .true.
    write(*,'(a)', advance='no') 'All deleted nodes no longer exists?  '
    do i=1, size(yremoved)
      found = tree % Exists(yremoved(i:i), compare_nodes_fun)
      if (found) then
        is_pass = .false.
        exit
      endif
    enddo
    write(*,*) is_pass
    if (.not. is_pass) error stop 'test failed'

    ! test, that not removed items are present
    is_pass = .true.
    write(*,'(a)', advance='no') 'But remaining nodes still exists?  '
    do i=1, size(yinside)
      found = tree % Exists(yinside(i:i), compare_nodes_fun)
      if (.not. found) then
        is_pass = .false.
        exit
      endif
    enddo
    write(*,*) is_pass
    if (.not. is_pass) error stop 'test failed'

    ! some nodes left to be removed by basetree_destructor
    print '(a,i0)', 'Nodes left for destructor to remove: ',size(yinside)
    print *
    !call Delete_nodes(tree, yinside, compare_nodes_fun)

  end subroutine test3





  pure function compare_nodes_fun(a,b) result(ires)
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
  end function compare_nodes_fun
