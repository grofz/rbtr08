  module tree_tests_m
    use tree_m, only : DAT_KIND, compare_fun, basetree_t, rbtr_t
    implicit none
    private
    public Insert_nodes, Print_nodes, Print_nodes2, Delete_nodes
    public Shuffle_array, Get_array

    contains

    subroutine Insert_nodes(tree, arr_data, isvalidated)
      class(basetree_t), intent(inout) :: tree
      integer(DAT_KIND), intent(in) :: arr_data(:)
      logical, optional, intent(in) :: isvalidated
!
! Insert all array elements to the tree.
! If "isvalidated" == TRUE, then validate tree at every loop
!
      integer :: i, nodes0, nodes1
      logical :: isvalidated0, isvalid_bst, isvalid_rb

      isvalidated0 = .false.
      if (present(isvalidated)) isvalidated0 = isvalidated
      print '(a,I0,a,L1,a)', &
        'Inserting nodes...   {N=',size(arr_data), &
        ' inter-validation=',isvalidated0,'}'

      nodes0 = tree % Count()
      do i = 1, size(arr_data)
        call tree % Add(arr_data(i:i))
        if (isvalidated0) then
          isvalid_bst = tree % Isvalid_BST()
          select type (tree)
          class is (rbtr_t)
            isvalid_rb = tree % Isvalid_rbtree()
          class default
            isvalid_rb = .true.
          end select
          if (.not. (isvalid_rb .and. isvalid_bst)) exit
        endif
      enddo
      nodes1 = tree % Count()

      ! If not validated in the loop, validate at the end
      if (.not. isvalidated0 .or. size(arr_data) < 1) then
        isvalid_bst = tree % Isvalid_BST()
        select type (tree)
        class is (rbtr_t)
          isvalid_rb = tree % Isvalid_rbtree()
        class default
          isvalid_rb = .true.
        end select
      endif

      print '(a,l1,a,l1)', &
          '  validated: red-black/bst ',isvalid_rb,'/', isvalid_bst
      if (.not. (isvalid_rb .and. isvalid_bst)) then
        error stop "Tree invalidated during Insert_nodes"
      endif

      print '(a,i0,ai0)', &
         '  nodes: before/after ',nodes0,'/',nodes1
      if (nodes1-nodes0 /= size(arr_data)) &
        error stop "Insertion error during Insert_nodes"

      print '(a)', '...done'
    end subroutine Insert_nodes



    subroutine Print_nodes(tree)
      class(basetree_t), intent(inout) :: tree

      integer :: ierr, i
      integer, parameter :: IONELINE = 10
      integer(DAT_KIND), allocatable :: handle(:)

      call tree % Firstnode(handle, ierr=ierr)
      if (ierr /= 0) then
        print '(a)', 'Print_nodes: tree is empty'
        return
      endif
      i = 0
      write(*,'(a)') 'Print_nodes:'
      do
        if (ierr /= 0) exit
        write(*,'(a)',advance='no') tree % Printcurrentnode(handle)//' '
        i = i + 1
        if (mod(i,IONELINE)==0) write(*,*)
        call tree % Nextnode(handle, ierr=ierr)
      enddo
      write(*,*)
    end subroutine Print_nodes



    subroutine Print_nodes2(tree)
      class(basetree_t), intent(inout) :: tree

      integer :: ierr, i
      integer, allocatable :: dat(:)
      integer, parameter :: IONELINE = 20
      integer(DAT_KIND), allocatable :: handle(:)

      if (tree % Isempty()) then
        print '(a)', 'Print_nodes2: tree is empty'
        return
      endif

      call tree % Resetcurrent(handle)
      i = 0
      write(*,'(a)') 'Print_nodes2:'
      do
        dat = tree % NextRead(handle, ierr)
        if (ierr /= 0) exit
        if (size(dat) /= 1) then
          print *, 'TODO - formatting might fail now...'
        endif
        write(*,'(i0,a)',advance='no') dat, ' '
        i = i + 1
        if (mod(i,IONELINE)==0) write(*,*)
      enddo
      write(*,*)
    end subroutine Print_nodes2



    subroutine Delete_nodes(tree, arr_data, isvalidated)
      class(basetree_t), intent(inout) :: tree
      integer(DAT_KIND), intent(in) :: arr_data(:)
      logical, intent(in), optional :: isvalidated
!
! Delete all array elements from the tree
!
      integer :: i, nodes0, nodes1
      logical :: exists, isvalid_bst, isvalid_rb, isvalidated0

      isvalidated0 = .false.
      if (present(isvalidated)) isvalidated0 = isvalidated
      print '(a,I0,a,L1,a)', &
        'Deleting nodes...   {N=',size(arr_data), &
        ' inter-validation=',isvalidated0,'}'

      nodes0 = tree % Count()
      do i = 1, size(arr_data)
        exists = tree % Isin(arr_data(i:i))
        if (exists) then
          !select type(tree)
          !class is (rbtr_t)
            call tree % remove(arr_data(i:i))
          !class default
          !  print *, 'TODO - basetree deletion not implemented'
          !  exit
          !end select
        else
          print '(a,i0,a)', '  node=',arr_data(i),' not found'
        endif

        if (isvalidated0) then
          isvalid_bst = tree % Isvalid_BST()
          select type (tree)
          class is (rbtr_t)
            isvalid_rb = tree % Isvalid_rbtree()
          class default
            isvalid_rb = .true.
          end select
          if (.not. (isvalid_rb .and. isvalid_bst)) exit
        endif
      enddo
      nodes1 = tree % Count()

      ! If not validated in the loop, validate at the end
      if (.not. isvalidated0 .or. size(arr_data) < 1) then
        isvalid_bst = tree % Isvalid_BST()
        select type (tree)
        class is (rbtr_t)
          isvalid_rb = tree % Isvalid_rbtree()
        class default
          isvalid_rb = .true.
        end select
      endif
      print '(a,l1,a,l1)', &
          '  validated: red-black/bst ',isvalid_rb,'/', isvalid_bst
      if (.not. (isvalid_rb .and. isvalid_bst)) then
        error stop "Tree invalidated during Delete_nodes"
      endif

      print '(a,i0,ai0)', &
         '  nodes: before/after ',nodes0,'/',nodes1
      if (nodes0-nodes1 /= size(arr_data)) &
          print '(a)', "WARNING: some nodes not deleted"

      print '(a)', '...done'
    end subroutine Delete_nodes



    function Get_array(n) result (y)
      integer, allocatable :: y(:)
      integer, intent(in) :: n
!
! Fill array by integers in the range <1; n>
!
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
