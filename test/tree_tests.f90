  module tree_tests_m
    use basetree_m, only : basetree_t
    use rbtr_m, only : rbtr_t
    use tree_common_m, only : DAT_KIND, cfun_abstract
    implicit none
    private
    public Insert_nodes, Print_nodes, Print_nodes2, Delete_nodes
    public Shuffle_array, Get_array

    contains

    subroutine Insert_nodes(tree, arr_data, cfun, isvalidated)
      class(basetree_t), intent(inout) :: tree
      integer(DAT_KIND), intent(in) :: arr_data(:)
      procedure(cfun_abstract) :: cfun
      logical, optional, intent(in) :: isvalidated
!
! Insert all array elements to the tree.
! If "isvalidated" == TRUE, then validate tree at every loop
!
      integer :: i
      logical :: isvalidated0, isvalid_bst, isvalid_rb

      isvalidated0 = .false.
      if (present(isvalidated)) isvalidated0 = isvalidated

      do i = 1, size(arr_data)
        call tree % Insert(arr_data(i:i), cfun)

        if (isvalidated0) then
          isvalid_bst = tree % Isvalid_BST(cfun)
          select type (tree)
          class is (rbtr_t)
            isvalid_rb = tree % Isvalid_rbtree()
          class default
            isvalid_rb = .true.
          end select
          if (.not. (isvalid_rb .and. isvalid_bst)) exit
        endif
      enddo

      ! If not validated in the loop, validate at the end
      if (.not. isvalidated0) then
        isvalid_bst = tree % Isvalid_BST(cfun)
        select type (tree)
        class is (rbtr_t)
          isvalid_rb = tree % Isvalid_rbtree()
        class default
          isvalid_rb = .true.
        end select
      endif

      print *, 'Insert_nodes: new / current =', &
      & size(arr_data), tree % Size(), 'Validation: ', &
      & isvalid_bst, isvalid_rb
      
      if (.not. (isvalid_rb .and. isvalid_bst)) then
        print *, 'WARNING: VALIDATION FAILS (inset_nodes)'
      endif

    end subroutine Insert_nodes



    subroutine Print_nodes(tree)
      class(basetree_t), intent(inout) :: tree

      integer :: ierr, i
      integer, parameter :: ioneline = 10

      call tree % Firstnode(ierr=ierr)
      if (ierr /= 0) then
        print *, 'Tree is empty'
        return
      endif
      i = 0
      write(*,'(a)') 'Print nodes:'
      do
        if (ierr /= 0) exit
        write(*,'(a)',advance='no') tree % Printcurrentnode()//' '
        i = i + 1
        if (mod(i,ioneline)==0) write(*,*)
        call tree % Nextnode(ierr=ierr)
      enddo
      write(*,*)
    end subroutine Print_nodes



    subroutine Print_nodes2(tree)
      class(basetree_t), intent(inout) :: tree

      integer :: ierr, i
      integer, allocatable :: dat(:)
      integer, parameter :: ioneline = 15

      call tree % Resetnode(ierr=ierr)
      if (ierr /= 0) then
        print *, 'Tree is empty'
        return
      endif
      i = 0
      write(*,'(a)') 'Print nodes:'
      do
        dat = tree % Readnext(ierr)
        if (ierr /= 0) exit
        write(*,'(i0,a)',advance='no') dat, ' '
        i = i + 1
        if (mod(i,ioneline)==0) write(*,*)
      enddo
      write(*,*)
    end subroutine Print_nodes2



    subroutine Delete_nodes(tree, arr_data, cfun, isvalidated)
      class(basetree_t), intent(inout) :: tree
      integer(DAT_KIND), intent(in) :: arr_data(:)
      procedure(cfun_abstract) :: cfun
      logical, intent(in), optional :: isvalidated
!
! Delete all array elements from the tree
!
      integer :: i
      logical :: exists, isvalid_bst, isvalid_rb, isvalidated0

      isvalidated0 = .false.
      if (present(isvalidated)) isvalidated0 = isvalidated

      do i = 1, size(arr_data)
        exists = tree % Exists(arr_data(i:i), cfun)
        if (exists) then
          select type(tree)
          class is (rbtr_t)
            call tree % Delete()
          class default
            print *, 'normal deletion not implemented'
            exit
          end select
        else
          print *, 'node = ',arr_data(i),' not found'
        endif

        if (isvalidated0) then
          isvalid_bst = tree % Isvalid_BST(cfun)
          select type (tree)
          class is (rbtr_t)
            isvalid_rb = tree % Isvalid_rbtree()
          class default
            isvalid_rb = .true.
          end select
          if (.not. (isvalid_rb .and. isvalid_bst)) exit
        endif
      enddo

      ! If not validated in the loop, validate at the end
      if (.not. isvalidated0) then
        isvalid_bst = tree % Isvalid_BST(cfun)
        select type (tree)
        class is (rbtr_t)
          isvalid_rb = tree % Isvalid_rbtree()
        class default
          isvalid_rb = .true.
        end select
      endif
      print *, 'Deleted_nodes: current nodes =', &
      & tree % Size(), 'validated =', isvalid_bst, isvalid_rb

      if (.not. (isvalid_rb .and. isvalid_bst)) then
        print *, 'WARNING: VALIDATION FAILS (inset_nodes)'
      endif
    end subroutine Delete_nodes



    function Get_array(n) result (y)
      integer, allocatable :: y(:)
      integer, intent(in) :: n

      integer :: i

      allocate(y(n))
      do i=1, n
        y(i) = i
      enddo
    end function Get_array



    function Shuffle_array(yin) result(yout)
      integer, intent(in) :: yin(:)
      integer :: yout(size(yin))
!
! Copy items from yin to yout in a random order
!
      real :: xran(size(yin))
      integer :: i, nleft, ipick, yin0(size(yin))

      call random_number(xran)
      yin0 = yin

      do i = 1, size(yin)
        nleft = size(yin)-i+1
        ipick = int(xran(i)*nleft) + 1
        yout(i) = yin0(ipick)
        yin0(ipick) = yin0(nleft)
      enddo
    end function Shuffle_array





  end module tree_tests_m
