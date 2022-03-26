! Example #2
!
! As example #1, but we created our tree class where we
! over-ride the function Printcurrentnode
!
  program example
    use tree_m, only : tree_mold, DAT_KIND
    use mytree
    use usermodule, only : mytype, mytype_ptr, userfun
    implicit none

    type(mytype_ptr) :: adat
    type(mytree_t)   :: tree
    integer :: ierr
    integer(DAT_KIND), allocatable :: handle(:)

    print '(a)', 'Hello example #2'

    ! Compulsory association of user-function
    tree % cfun => userfun

    ! Allocate and insert three nodes (INSERT)
    allocate(adat % ptr)
    adat % ptr % a = 123.45
    call tree % Add(transfer(adat,tree_mold))

    allocate(adat % ptr)
    adat % ptr % a = 10.0
    call tree % Add(transfer(adat,tree_mold))

    allocate(adat % ptr)
    adat % ptr % a = 900.0
    call tree % Add(transfer(adat,tree_mold))

    ! Search for a node
    ! EXISTS, SIZE, ISVALID_BST, ISVALID_RBTREE, HEIGHT_RANGE
    write(*,'(a)',advance='no') 'Is the last element in the tree?  '
    write(*,'(L1)') tree % Isin(transfer(adat,tree_mold))

    print '(a,L1,1x,L2)', 'Is tree valid?  ', &
    &  tree % Isvalid_rbtree(), tree % Isvalid_bst()
    print '(a,i0,a,i0,1x,i0)', 'Tree size =', tree % size(), &
    &       ' height range=', tree % Height_range()

    ! Iterate tree
    call Iterator(tree)

    ! Remove nodes
    do
      if (tree % size() == 0) exit
      call tree % Firstnode(handle)
      adat = transfer(tree % Read(handle), adat)
      print *, '  node = ', adat % ptr % a, ' will be removed'
      call tree % remove(tree % Read(handle))
      deallocate(adat % ptr)

      call Iterator(tree)
    enddo

  contains
    subroutine Iterator(tree)
      class(mytree_t), intent(inout) :: tree

      ! Iterate all nodes
      call tree % Firstnode(handle, ierr)
      if (ierr == 0) then
        print *
        do
          print *, ' ___ = ', tree % Printcurrentnode(handle)
          call tree % Nextnode(handle, ierr)
          if (ierr /= 0) exit
        enddo
      else
        print *
        print *, 'Empty tree: '
      endif
      print *
    end subroutine Iterator

  end program example
