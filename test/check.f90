  program check
    implicit none
    integer :: n

    write(*,'(a)',advance='no') 'Items = ? '
    read(*,*) n
    print *
  ! call test1()
  ! call test2()
    call test3(n)
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
    call tree % Insert([10], ccc)
    call tree % Insert([20], ccc)
    call tree % Insert([15], ccc)
    call tree % Insert([50], ccc)
    call tree % Insert([70], ccc)
    call tree % Insert([65], ccc)
    print *, 'aize =', tree%Size()

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
    call tree_b % Insert([50], ccc)
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
    call rbtree % Insert([10], ccc)
    print *, 'size 1 =', rbtree%Size()
    call rbtree % Insert([20], ccc)
    print *, 'size 1 =', rbtree%Size()

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



  subroutine test2
    use tree_tests_m
    use rbtr_m
    use basetree_m
    implicit none


    type(rbtr_t)     :: aobj, cobj, dobj
    type(basetree_t) :: bobj
    integer :: i

    interface
      pure function ccc(a,b) result(ires)
        use kinds_m, only : I4B
        integer :: ires
        integer(I4B), intent(in) :: a(:), b(:)
      end function
    end interface


    print *
    print *, 'In test 2 '
    call Insert_nodes(aobj, [10, 7, 5, 2, 6, 3], ccc)
    call Print_nodes(aobj)
    print *, 'Height of tree is: ', aobj % Height_range(), &
    & aobj % Isvalid_rbtree()
    print *

    call Print_nodes(bobj)
    call Insert_nodes(bobj, [10, 20, 30, 40, 50, 60], ccc)
    call Insert_nodes(bobj, [21], ccc)
    call Print_nodes(bobj)
    print *, 'Height of tree is: ', bobj % Height_range()

    call Insert_nodes(cobj, [10, 5, 7, 8, 9, 11, 12, 13], ccc)
    print *, 'Is valid BST =',  cobj % Isvalid_BST(ccc)

    !print *, 'Calling rotation'
    !call Rotate_left(cobj, cobj % root)
    !call Rotate_right(cobj, cobj % root)
    print *, 'Is valid =',  cobj % Isvalid_BST(ccc), cobj % Isvalid_rbtree()

    call Print_nodes(cobj)
    print *, 'Height of tree is: ', cobj % Height_range()


    do i = 100, 1, -1
      call dobj % Insert([i], ccc)
    enddo
    call Print_nodes(dobj)
    print *, 'Is valid =', dobj%Isvalid_BST(ccc), dobj%Isvalid_rbtree(), &
    & dobj % Height_range()

    !do i=1, 100
    do i=100, 1, -3
      if(dobj%Exists([i], ccc)) call dobj%Delete()
      print *, 'Deleted :', dobj%Isvalid_rbtree()
    enddo
    call Print_nodes(dobj)
 return

    ! test Exists
    print *, 'Exist 10? ', aobj%Exists([10], ccc)
    print *, 'Exist 7? ', aobj%Exists([7], ccc)
    print *, 'Exist 2? ', aobj%Exists([2], ccc)
    print *, 'Exist 13? ', aobj%Exists([13], ccc)
100 continue
    call Delete_nodes(aobj, [10, 7 ], ccc)
    !call Delete_nodes(aobj, [10, 7, 5, 2, 6, 3], ccc)
    call Print_nodes(aobj)
    print *, 'Height of tree is: ', aobj % Height_range(), &
    & aobj % Isvalid_rbtree()
    print *
    call Delete_nodes(aobj, [5, 2, 6, 3], ccc)

  end subroutine test2



  subroutine test3(nsize)
    use tree_tests_m
    use rbtr_m, only : rbtr_t
    use basetree_m, only : basetree_t
    !use tree_common_m
    logical, parameter :: IS_VALIDATED = .false.

    integer, intent(in) :: nsize
    type(rbtr_t) :: aobj
    integer, allocatable :: y0(:), ys(:), yinside(:), yremoved(:)
    integer :: i, ipart
    logical found

    interface
      pure function ccc(a,b) result(ires)
        use kinds_m, only : I4B
        integer :: ires
        integer(I4B), intent(in) :: a(:), b(:)
      end function
    end interface


    ! Fill by N items
    y0 = Get_array(nsize)

    ys = Shuffle_array(y0)
    call Insert_nodes(aobj, ys, ccc, IS_VALIDATED)

    print '(a,i0,3x,i0,1x,i0)', 'Tree size / height ', &
    & aobj%size(), aobj%height_range()
    print *, 'log_2(size) =', log(real(aobj%size()))/log(2.0)

call Print_nodes2(aobj)
call Print_nodes(aobj)

    ! Test that all items are present
    ys = Shuffle_array(y0)
    do i = 1, size(y0)
       found = aobj % Exists(ys(i:i), ccc)
       if (.not. found) then
          print *, "ERROR TEST - ITEM NOT FOUND"
          exit
       endif
    enddo


    ! Remove part of items
    ipart = nsize/2
    ys = Shuffle_array(y0)
    call Delete_nodes(aobj, ys(1:ipart), ccc, IS_VALIDATED)
    yremoved = ys(1:ipart)
    yinside = ys(ipart+1:nsize)

    ! test, that removed are no longer in the tree
    yremoved = Shuffle_array(yremoved)
    yinside = Shuffle_array(yinside)

    do i=1, size(yremoved)
      found = aobj % Exists(yremoved(i:i), ccc)
      if (found) then
        print *, "ERROR TEST - ITEM FOUND, BUT SHOULD NOT"
        exit
      endif
    enddo

    do i=1, size(yinside)
      found = aobj % Exists(yinside(i:i), ccc)
      if (.not. found) then
        print *, "ERROR TEST - ITEM NOT FOUND, BUT SHOULD "
        exit
      endif
    enddo

    ! remove all remaining
   !call Delete_nodes(aobj, yinside, ccc, IS_VALIDATED)





    !ys = Shuffle_array(y0)
    !call Delete_nodes(aobj, ys, ccc, IS_VALIDATED)
  end subroutine test3





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
