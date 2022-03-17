  program check
    implicit none
    call test1()
  end program check


  subroutine test1()
    use basetree_m
    use rbtr_m
    use tree_common_m, only : TREE_ERR_OK, TREE_ERR_NONEXT
    implicit none
    integer, parameter :: I4B = selected_int_kind(9)
    type(basetree_t) :: tree, tree_b
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
    call tree % Insert([10], ccc, ierr=ires)
    call tree % Insert([20], ccc, ierr=ires)
    call tree % Insert([15], ccc, ierr=ires)
    call tree % Insert([50], ccc, ierr=ires)
    call tree % Insert([70], ccc, ierr=ires)
    call tree % Insert([65], ccc, ierr=ires)
    print *, 'ires =', ires, tree%Nodesf()

    call tree % Firstnode(ires)
    do
      if (ires /= TREE_ERR_OK) exit
      print *, tree % Printcurrentnode()
      call tree % Nextnode(ires)
      if (ires == TREE_ERR_NONEXT) &
          print *, 'next node says: this was the last node'
    enddo

    ! now empty tree
    call tree_b % Nextnode(ires)
    print *, 'empty tree nextnode', ires
    call tree_b % Firstnode(ires)
    print *, 'empty tree Firstnode', ires
    call tree_b % Nextnode(ires)
    print *, 'empty tree nextnode', ires
    call tree_b % Insert([50], ccc, ierr=ires)
    call tree_b % Nextnode(ires)
    print *, 'one element tree nextnode', ires
    call tree_b % Firstnode(ires)
    print *, 'one element tree firstnode', ires
    print *, tree_b % Printcurrentnode()
    call tree_b % Nextnode(ires)
    print *, 'one element tree nextnode', ires
    print *,  tree_b % Printcurrentnode()

    print *, 'Verify tree...'
    print *, 'Valid = ', tree % Isvalid_BST(ccc)
    print *, 'Valid = ', tree_b % Isvalid_BST(ccc)

  print *, "Put some tests in here!"

    rbtree = rbtr_t()
    call rbtree % Insert([10], ccc, ierr=ires)
    print *, 'ires 1 =', ires, rbtree%Nodesf()
    call rbtree % Insert([20], ccc, ierr=ires)
    print *, 'ires 1 =', ires, rbtree%Nodesf()

    call rbtree % Firstnode(ires)
    do
      if (ires /= TREE_ERR_OK) exit
      print *, rbtree % Printcurrentnode()
      call rbtree % Nextnode(ires)
      if (ires == TREE_ERR_NONEXT) &
          print *, 'next node says: this was the last node'
    enddo

    print *, 'Verify tree...'
    print *, 'Valid = ', rbtree % Isvalid_BST(ccc)
    print *, 'Verify RB...'
    print *, 'Valid RB = ', rbtree % Isvalid_rbtree()

  end subroutine test1



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
