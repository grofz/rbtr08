  module tree_tests_m
    use basetree_m, only : basetree_t
    use rbtr_m, only : rbtr_t
    use tree_common_m, only : DAT_KIND, cfun_abstract
    implicit none
    private
    public Insert_nodes, Print_nodes

    contains

    subroutine Insert_nodes(tree, arr_data, cfun)
      class(basetree_t), intent(inout) :: tree
      integer(DAT_KIND), intent(in) :: arr_data(:)
      procedure(cfun_abstract) :: cfun
!
! Insert all array elements to the tree
!
      integer :: i

      do i = 1, size(arr_data)
        call tree % Insert(arr_data(i:i), cfun)
      enddo
      print *, 'Insert_nodes: new / current nodes =', &
      & size(arr_data), tree % Nodesf()

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

  end module tree_tests_m
