!
! Example #1
!
  program example
    use tree_m, only : rbtr_t, tree_mold
    use usermodule, only : mytype, mytype_ptr, userfun
    implicit none

    type(mytype_ptr) :: adat
    type(rbtr_t) :: tree
    integer :: ierr

    print '(a)', 'Hello example #1 of using red-black tree container rbtr_t'

    ! Tree initialization is compulsory
    tree = rbtr_t(userfun)

    ! Allocate and insert three nodes (INSERT)
    allocate(adat % ptr)
    adat % ptr % a = 123.45
    call tree % Insert(transfer(adat,tree_mold), userfun)

    allocate(adat % ptr)
    adat % ptr % a = 10.0
    call tree % Insert(transfer(adat,tree_mold), userfun)

    allocate(adat % ptr)
    adat % ptr % a = 900.0
    call tree % Insert(transfer(adat,tree_mold), userfun)

    ! Search for a node
    ! EXISTS, SIZE, ISVALID_BST, ISVALID_RBTREE, HEIGHT_RANGE
    write(*,'(a)',advance='no') 'Is the last element in the tree?  '
    write(*,'(L1)') tree % Exists(transfer(adat,tree_mold), userfun)

    print '(a,L1,1x,L2)', 'Is tree valid?  ', &
    &  tree % Isvalid_rbtree(), tree % Isvalid_bst(userfun)
    print '(a,i0,a,i0,1x,i0)', 'Tree size =', tree % size(), &
    &       ' height range=', tree % Height_range()

    ! Iterate tree
    call Iterator(tree)

    ! Remove nodes
    do
      if (tree % size() == 0) exit
      call tree % Firstnode()
      adat = transfer(tree % Read(), adat)
      print *, '  node = ', adat % ptr % a, ' will be removed'
      deallocate(adat % ptr)
      call tree % Delete()

      call Iterator(tree)
    enddo

  contains
    subroutine Iterator(tree)
      type(rbtr_t), intent(inout) :: tree

      ! Iterate all nodes
      call tree % Resetnode(ierr)
      if (ierr == 0) then
        print *
        do
          adat = transfer(tree % Readnext(ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          print *, 'Node = ', adat % ptr % a
          print *, ' ___ = ', tree % Printcurrentnode()
        enddo
      else
        print *
        print *, 'Empty tree: '
      endif
      print *
    end subroutine Iterator

  end program example
