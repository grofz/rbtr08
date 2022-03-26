!
! Example #1
!
  program example
    use tree_m, only : rbtr_t, tree_mold, DAT_KIND
    use usermodule, only : mytype, mytype_ptr, userfun
    implicit none

    type(mytype_ptr) :: adat
    type(rbtr_t) :: tree
    integer :: ierr
    integer(DAT_KIND), allocatable :: handle(:)

    print '(a)', 'Hello example #1 of using red-black tree container rbtr_t'

    ! Tree initialization is compulsory
    tree = rbtr_t(userfun)

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
    print '(a,i0,a,i0,1x,i0)', 'Tree size =', tree % count(), &
    &       ' height range=', tree % Height_range()

    ! Iterate tree
    call Iterator(tree)

    ! Remove nodes
    do
      if (tree % count() == 0) exit
      call tree % Firstnode(handle)
      adat = transfer(tree % Read(handle), adat)
      print *, '  node = ', adat % ptr % a, ' will be removed'
      call tree % remove(tree % Read(handle))
      deallocate(adat % ptr)

      call Iterator(tree)
    enddo

  contains
    subroutine Iterator(tree)
      type(rbtr_t), intent(inout) :: tree
      integer(DAT_KIND), allocatable :: handle(:)

      ! Iterate all nodes
      if (.not. tree % Isempty()) then
        print *
        call tree % Resetcurrent(handle)
        do
          adat = transfer(tree % NextRead(handle, ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          print *, 'Node = ', adat % ptr % a
          print *, ' ___ = ', tree % Printcurrentnode(handle)
        enddo
      else
        print *
        print *, 'Empty tree: '
      endif
      print *
    end subroutine Iterator

  end program example
