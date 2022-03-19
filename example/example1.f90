  program example
    use rbtr_m, only : rbtr_t
    !use tree_common_m, only : cfun_abstract
    use usermodule, only : mytype, mytype_ptr, mold, userfun
    implicit none

    type(mytype_ptr) :: adat
    type(rbtr_t) :: tree

    integer :: ierr

    !procedure(cfun_abstract) :: userfun

    print *, 'Hello example of using red-black tree container rbtr_t'

    ! Allocate and insert three nodes (INSERT)
    allocate(adat % ptr)
    adat % ptr % a = 123.45
    call tree % Insert(transfer(adat,mold), userfun)

    allocate(adat % ptr)
    adat % ptr % a = 10.0
    call tree % Insert(transfer(adat,mold), userfun)

    allocate(adat % ptr)
    adat % ptr % a = 900.0
    call tree % Insert(transfer(adat,mold), userfun)

    ! Search for a node
    ! EXISTS, SIZE, ISVALID_BST, ISVALID_RBTREE, HEIGHT_RANGE
    print *, 'Is the last element in the tree?'
    print *, 'Exists = ', tree % Exists(transfer(adat,mold), userfun)

    print *, 'Is tree valid =', tree % Isvalid_rbtree(), &
    &  tree % Isvalid_bst(userfun)
    print *, 'Tree size =', tree % size(), &
    &       ' height range=', tree % Height_range()

    ! Iterate tree
    call Iterator(tree)

    ! Remove nodes
    do
      if (tree % size() == 0) exit
      call tree % Firstnode()
      adat = transfer(tree % Read(), adat)
      print *, 'node = ', adat % ptr % a, ' will be removed'
      deallocate(adat % ptr)
      call tree % Delete()

      call Iterator(tree)
    enddo
    call Iterator(tree)


  contains
    subroutine Iterator(tree)
      type(rbtr_t), intent(inout) :: tree

      ! Iterate all nodes
      call tree % Resetnode(ierr)
      if (ierr == 0) then
        do
          adat = transfer(tree % Readnext(ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          print *, 'Node is = ', adat % ptr % a, &
                   ' stored as= ', tree % Printcurrentnode()
        enddo
      else
        print *, 'Empty tree: '
      endif
    end subroutine Iterator

  end program example
