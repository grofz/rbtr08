  program check
    implicit none
    integer :: n, tree_type
    logical :: is_validated
    real :: cpu_0, cpu_1

    call random_seed()
    ! note, that using random_seed causes valgrind to report some leaks
    write(*,'(a)',advance='no') 'Items = ? '
    read(*,*) n
    print *
    write(*,'(a)',advance='no') 'Type of tree (1:red-black 2:basic) = ? '
    read(*,*) tree_type
    print *

    call test2()
    call test1()

    is_validated = .false.
    if (n < 1001) is_validated = .true.
    call cpu_time(cpu_0)
    call test3(n, is_validated, tree_type)
    call cpu_time(cpu_1)
    cpu_1 = cpu_1 - cpu_0
    print *, 'Elapsed time: ',cpu_1, ' (per node:',cpu_1/real(n)
  end program check



  subroutine test1
    use tree_m
    use tree_tests_m
    implicit none
    type(rbtr_t) :: b_rb, c_rb
    type(basetree_t) :: a_rb, a_bas, b_bas, c_bas
    procedure(compare_fun) :: compare_nodes_fun
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr

    print *, 'RUNNING TEST #1'

    !a_rb = rbtr_t(compare_nodes_fun)
    a_rb = basetree_t(compare_nodes_fun)
    call Insert_nodes(a_rb, [10, 7, 5, 2, 6, 3] )
    call Insert_nodes(a_rb, [11, 17, 15, 12, 16, 13, 34, 56] )
    call Insert_nodes(a_rb, [31, 47, 65, 92, 26, 73, 84, 96] )
    call Insert_nodes(a_rb, [33, 44, 55, 66, 77, 88, 27, 41] )
    call a_rb % print()

    ! test update
    call a_rb % Firstnode(handle)
    call a_rb % Nextnode(handle)
    call a_rb % Nextnode(handle)
    print *, 'A is ', a_rb % Printcurrentnode(handle)
    print *,'... update xxx[by xxx ...'
    call a_rb % Updatecurrent(handle, [4], ierr)
    print *, '... error code =', ierr, a_rb % Isvalid_BST() !, a_rb % Isvalid_rbtree()
    call a_rb % print()

    b_rb = a_rb

    call Insert_nodes(a_rb, [35] )
    call Print_nodes(a_rb)
    call Print_nodes(b_rb)
    print *, ' validated? ', b_rb % isvalid_bst() , b_rb % isvalid_rbtree()

    call Delete_nodes(a_rb, [10, 7, 5, 2, 6, 3] )
    call Print_nodes(a_rb)
    call Print_nodes(b_rb)
    print *, 'end of t1'
  end subroutine test1



  subroutine test2
    use tree_tests_m
    use tree_m 
    implicit none

    type(rbtr_t)     :: aobj, cobj
    type(basetree_t) :: bobj
    integer :: i
    procedure(compare_fun) compare_nodes_fun

    ! Initialization of "basetree_t" objects is compulsory!
    aobj = rbtr_t(compare_nodes_fun)
    cobj = rbtr_t(compare_nodes_fun)
    bobj = basetree_t(compare_nodes_fun)


    print *, 'RUNING TEST #2 '
    call Insert_nodes(aobj, [10, 7, 5, 2, 6, 3] )
    call Insert_nodes(aobj, [1, 4, 8, 12, 16, 9])
    call Print_nodes(aobj)
    print *, 'Height of tree is: ', aobj % Height_range()
    print *
 call aobj % Print()

    call Print_nodes(bobj)
    call Insert_nodes(bobj, [10, 20, 30, 40, 50, 60])
    call Insert_nodes(bobj, [21])
    call Print_nodes(bobj)
    print *, 'Height of basetree is: ', bobj % Height_range()
    print *
 call bobj % Print()

    call Insert_nodes(cobj, [10, 5, 7, 8, 9, 11, 12, 13])
    call Print_nodes(cobj)
    print *, 'Height of tree is: ', cobj % Height_range()
    print *
 call cobj % Print()

  end subroutine test2



  subroutine test3(nsize, is_validated, tree_type)
    use tree_tests_m
    use tree_m, only : basetree_t, rbtr_t, compare_fun

    integer, intent(in) :: nsize, tree_type
    logical, intent(in) :: is_validated
    !type(rbtr_t) :: tree
    class(basetree_t), allocatable :: tree
    integer, allocatable, dimension(:) :: y0, ys, yinside, yremoved
    integer :: i, ipart, hr(2)
    real :: log_of_size
    logical found, is_pass
    procedure(compare_fun) :: compare_nodes_fun

    print '(a)', 'RUNNING TEST #3'


    select case(tree_type)
    case(1)
      print '(a)', 'RED-BLACK TREE'
      ! Initialization is compulsory
      allocate(rbtr_t :: tree) ! avoid segm fault?
      tree = rbtr_t(compare_nodes_fun)
    case(2)
      print '(a)', 'BASE TREE'
      ! Initialization is compulsory
      allocate(basetree_t :: tree) ! avoid segm fault?
      tree = basetree_t(compare_nodes_fun)
    case default
      error stop 'invalid selection'
    end select

    ! Fill by N items
    y0 = Get_array(nsize)
    ys = Shuffle_array(y0)
    call Insert_nodes(tree, ys, IS_VALIDATED)

    ! Height of the tree
    hr = tree % Height_range()
    log_of_size = log(real(tree%count())) / log(2.0)
    print '(a,i0,a,i0,a,f6.2,a,i0,a)', &
    &   'Tree size (hmin<log2(size)<hmax): ', &
    &   tree%count(),' (', hr(1),' <', log_of_size,' < ', hr(2),')'

    if (nsize < 1001) then
      call Print_nodes2(tree)
      call Print_nodes(tree)
    endif

    ! test that all items are present
    ys = Shuffle_array(y0)
    is_pass = .true.
    write(*,'(a)', advance='no') 'All inserted nodes exists?  '
    do i = 1, size(y0)
      found = tree % Isin(ys(i:i))
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
    call Delete_nodes(tree, ys(1:ipart), IS_VALIDATED)
    yremoved = ys(1:ipart)
    yinside = ys(ipart+1:nsize)

    ! test, that removed are no longer in the tree
    yremoved = Shuffle_array(yremoved)
    yinside = Shuffle_array(yinside)
    is_pass = .true.
    write(*,'(a)', advance='no') 'All deleted nodes no longer exists?  '
    do i=1, size(yremoved)
      found = tree % Isin(yremoved(i:i))
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
      found = tree % Isin(yinside(i:i))
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
    !call Delete_nodes(tree, yinside)

    deallocate(tree)
    deallocate(y0,ys,yinside,yremoved)

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
