  submodule(tree_m) basetree
    implicit none

  contains

    module function basetree_Printcurrentnode(this) result(str)
!
! Display content of the "current" node
!
      class(basetree_t), intent(in) :: this
      character(len=:), allocatable :: str
      character(len=1000) :: dat
      if (.not. associated(this % current)) then
        str='current not allocated...'
      else
        write(dat,*) this % current % dat
        str='['//trim(adjustl(dat))//']'
      endif
    end function basetree_Printcurrentnode



    module function basetree_Initialize() result(new)
      type(basetree_t) :: new
!
! A dummy constructor doing nothing at the moment
!
    end function basetree_Initialize



    module subroutine basetree_Insert(this, dat, cfun, newnode)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      procedure(cfun_abstract) :: cfun
      class(basenode_t), pointer, optional :: newnode
!
! Insert a new node to the tree.
! If pointer "newnode" is present, the existing node is used,
! otherwise a new "basenode_t" node is allocated.
! The "dat" component of an existing node must be deallocated. 
!
! The "current" pointer is unchanged.
!        
      class(basenode_t), pointer :: new0

      ! Prepare new node to be selected to the tree
      ! Node type other than "basenode_t" must be allocated up-stream
      if (present(newnode)) then
        new0 => newnode
      else
        allocate(basenode_t :: new0)
      endif
      if (.not. associated(new0)) &
      &   error stop 'basetree_Insert: newnode points nowhere'

      ! Fail if neccessary to prevent inadvertent data overwriting
      if (allocated(new0 % dat)) &
      &   error stop 'basetree_Insert: newnode already contains data'

      new0 % dat = dat
      if (.not. associated(this % root)) then
        ! tree is empty
        this % root => new0
        if (this % nodes /= 0) &
        &   error stop 'basetree_Insert: nodes /= 0 for the first node'
      else
        call insert_recurse(this, this % root, new0, cfun)
      endif

      this % nodes = this % nodes + 1
    end subroutine basetree_Insert



    recursive subroutine insert_recurse(a, root, new, cfun)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), intent(inout), pointer :: root, new
      procedure(cfun_abstract) :: cfun

      integer :: ires

      if (.not. associated(root)) then
        error stop "insert_recurse: something got wrong"
      endif

      ires = cfun(new % dat, root % dat)
      select case(ires)
      case(+1) ! "new % key" < "root % key"
        ! Insert node to the left-subtree
        if (associated(root % left)) then
          call insert_recurse(a, root % left, new, cfun)
        else
          root % left => new
          new % parent => root
        endif

      case(-1) ! "new % key" > "root % key"
        ! Insert node to the right-subtree
        if (associated(root % right)) then
          call insert_recurse(a, root % right, new, cfun)
        else
          root % right => new
          new % parent => root
        endif

      case(0)
        ! Duplicit nodes not allowed at the moment
        error stop "insert_recurse: duplicit not ready yet"

      case default
        error stop "insert_recurse: invalid output from cfun"
      end select
    end subroutine insert_recurse



    module function basetree_Exists(this, dat, cfun) result(exists)
      logical :: exists
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      procedure(cfun_abstract) :: cfun
!
! Return TRUE and set current pointer on the node if the node
! with the value "dat" is present in the tree.
! Otherwise return FALSE and current pointer is not touched.
!
      class(basenode_t), pointer :: n
      integer :: ires

      exists = .false.
      n => this % root
      do
        if (.not. associated(n)) exit
        ires = cfun(dat, n % dat)
        select case (ires)
        case(-1) ! "dat % key" > "n % key" 
          n => n % right
        case(1)  ! "dat % key" < "n % key"
          n => n % left
        case(0)  ! node found
          exists = .true.
          this % current => n
          exit
        case default
          error stop "Exists: invalid resutl from cfun"
        end select 
      enddo
    end function basetree_Exists



    module function basetree_Read(this, ierr) result(dat)
      class(basetree_t), intent(in) :: this
      integer, optional, intent(out) :: ierr
      integer(DAT_KIND), allocatable :: dat(:)
!
! Return data of the node that is pointed by "current" pointer.
! If current points nowhere, function fails or flags an error.
!
      integer :: ierr0

      if (.not. associated(this % root)) then
        ierr0 = TREE_ERR_EMPTY
      elseif (.not. associated(this % current)) then
        ierr0 = TREE_ERR_NOCURRENT
      else
        ierr0 = TREE_ERR_OK
        dat = this % current % dat
      endif

      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= TREE_ERR_OK) then
        error stop "basetree_Read: current pointer is null"
      endif
    end function basetree_Read



    module function basetree_ReadNext(this, ierr) result(dat)
      class(basetree_t), intent(inout) :: this
      integer, optional, intent(out) :: ierr
      integer(DAT_KIND), allocatable :: dat(:)
!
! Move "current" node pointer to a next node and return its data.
! If "current" is not set, data of a first node are returned.
! If "current" points to the last node, function fails or flags an error.
! If tree is empty, function fails or flags an error.
!
      integer :: ierr0
      character(len=:), allocatable :: message
      integer(DAT_KIND), parameter :: empty_data(2) = [-99, -99]

      ierr0 = TREE_ERR_OK
      if (.not. associated(this % root)) then
        ierr0 = TREE_ERR_EMPTY
        dat = empty_data
      elseif (.not. associated(this % current)) then
        this % current => Leftmost(this % root)
        dat = this % current % dat
      else
        this % current => Successor(this % current)
        if (associated(this % current)) then
          dat = this % current % dat
        else
          ierr0 = TREE_ERR_NOCURRENT
          dat = empty_data
        endif
      endif

      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= TREE_ERR_OK) then
        select case (ierr0)
        case (TREE_ERR_EMPTY)
          message = "reading from empty tree"
        case (TREE_ERR_NOCURRENT)
          message = "reading from behind the last node"
        case default
          message = "Just to make compiler happy"
        end select
        error stop "basetree_Readnext: "//message
      endif
    end function basetree_ReadNext



    module function basenode_Parent(n) result(parentf)
      class(basenode_t), pointer :: parentf
      class(basenode_t), intent(in) :: n
!
! Type bound function to allow access to private component of "basenode"
!
      parentf => n % parent
    end function basenode_Parent  



    module function basenode_Grandparent(n) result(grandparent)
      class(basenode_t), pointer :: grandparent
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        grandparent => p % Parentf()
      else
        grandparent => null()
      endif
    end function basenode_Grandparent



    module function basenode_Leftchild(n) result(leftchild)
      class(basenode_t), pointer :: leftchild
      class(basenode_t), intent(in) :: n

      leftchild => n % left
    end function basenode_Leftchild



    module function basenode_Rightchild(n) result(rightchild)
      class(basenode_t), pointer :: rightchild
      class(basenode_t), intent(in) :: n

      rightchild => n % right
    end function basenode_Rightchild



    module function Sibling(n) result(sb)
      class(basenode_t), pointer :: sb
      class(basenode_t), intent(in), pointer :: n
!
! Note: Unlile other "family" functions, this is not a type bound procedure
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          sb => p % right
        elseif (associated(n, p % right)) then
          sb => p % left
        else
          error stop 'basetree::Sibling ERROR. n is uknown to p'
        endif
      else
        sb => null()
      endif
    end function Sibling



    module function basenode_Uncle(n) result(uncle)
      class(basenode_t), pointer :: uncle
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        uncle => Sibling(p)
      else
        uncle => null()
      endif
    end function basenode_Uncle



    module subroutine Rotate_left(a, piv)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), pointer :: piv
!
! Rotate left. Fails if pivot is leaf or if pivot's right child is leaf.
!
      class(basenode_t), pointer :: par, rot
      logical :: is_lc, is_rc

      if (.not. associated(piv)) &
      &   error stop "Rotate_left: pivot is not associated"

      ! "par" is pivot's parent, it will be relinked to rotator.
      ! Rotator "rot" is a right child of pivot. It must exists.
      is_lc = Is_left_child(piv)
      is_rc = Is_right_child(piv)
      par => piv % Parentf()
      rot => piv % right
      if (.not. associated(rot)) &
      &   error stop "Rotate_left: rotator is leaf"

      ! Left child of rotator relinked as right child of pivot,
      ! and pivot will be its new parent (if it is not leaf)
      piv % right => rot % left
      if (associated(piv % right)) piv % right % parent => piv

      ! Pivot will be a left child of rotator, and
      ! rotator is new pivot's parent.
      rot % left => piv
      piv % parent => rot

      ! Relink "par <--> piv" now as "par <--> rot"
      if (.not. associated(par)) then
        ! Pivot was a root node, rotator is a new root.
        rot % parent => null()
        a % root => rot
 ! TODO temporary defensive check
 if (is_lc .or. is_rc) error stop "Rotator_left: Defensive check"

      else
        ! Pivot was a parent's child, rotator takes its place.
        rot % parent => par
        if (is_lc .and. .not. is_rc) then
          par % left => rot
        elseif (is_rc .and. .not. is_lc) then
          par % right => rot
        else
          error stop "Rotator_left: internal error, something very wrong"
        endif
      endif
    end subroutine Rotate_left



    module subroutine Rotate_right(a, piv)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), pointer :: piv
!
! Rotate right. Fails if pivot is leaf or if pivot's left child is leaf.
!
      class(basenode_t), pointer :: par, rot
      logical :: is_lc, is_rc

      if (.not. associated(piv)) &
      &   error stop "Rotate_right: pivot is not associated"

      ! "par" is pivot's parent, it will be relinked to rotator.
      ! Rotator "rot" is a left child of pivot. It must exists.
      is_lc = Is_left_child(piv)
      is_rc = Is_right_child(piv)
      par => piv % Parentf()
      rot => piv % left
      if (.not. associated(rot)) &
      &   error stop "Rotate_right: rotator is a leaf"

      ! Right child of rotator relinked as leff child of pipvot,
      ! and pivot will be its new parent (if it is not leaf)
      piv % left => rot % right
      if (associated(piv % left)) piv % left % parent => piv

      ! Pivot will be a right child of rotator, and rotator is new
      ! pivot's parent
      rot % right => piv
      piv % parent => rot

 !TODO This part duplicates "rotate_left", refactor?
      ! Reconnect rotated part of tree to a up-stream node (par)
      if (.not. associated(par)) then
        ! Pivot was a root node, rotator is a new root
        rot % parent => null()
        a % root => rot
 !TODO Temporary defensive check
 if (is_lc .or. is_rc) error stop "Rotator_right: Defensive check"

      else
        ! Pivot was a parent's child, rotator takes its place
        rot % parent => par
        if (is_lc .and. .not. is_rc) then
          par % left => rot
        elseif (is_rc .and. .not. is_lc) then
          par % right => rot
        else
          error stop "Rotator_right: internal error, something very wrong"
        endif
      endif
    end subroutine Rotate_right


    module function Is_left_child(n)
      logical :: Is_left_child
      class(basenode_t), intent(in), pointer :: n
!
! Returns .false. when called with unassociated pointer.
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          is_left_child = .true.
        elseif (associated(n, p % right)) then
          is_left_child = .false.
        else
          error stop 'basetree::Is_left_child ERROR. n is uknown to p'
        endif
      else
        ! node has no parent
        is_left_child = .false.
      endif
    end function Is_left_child



    module function Is_right_child(n)
      logical :: Is_right_child
      class(basenode_t), intent(in), pointer :: n
!
! Returns .false. when called with unassociated pointer.
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        is_right_child = .not. Is_left_child(n)
      else
        ! node has no parent
        is_right_child = .false.
      endif
    end function Is_right_child



   ! logical function Is_inner_grandparent(n)
   !   class(basenode_t), intent(in), pointer :: n
!
! True if "n" is in the inner sub-tree of his grand-parent
! (to be used by red-black tree insetion algorithm)
!
      !class(basenode_t), pointer :: p

      !is_inner_grandparent = .false.
      !p => n % Parentf()
      !if (.not. associated(p)) return

      !if (Is_left_child(n)) then
      !  if (Is_right_child(p)) is_inner_grandparent = .true.
      !elseif (Is_right_child(n)) then
      !  if (Is_left_child(p)) is_inner_grandparent = .true.
      !endif
    !end function Is_inner_grandparent



    pure module function basetree_Nodes(this)
      integer :: basetree_Nodes
      class(basetree_t), intent(in) :: this
!
! Type-bound function - return number of nodes in the tree
!
      basetree_Nodes = this % nodes
    end function basetree_Nodes



    module subroutine basetree_Firstnode(this, ierr)
      class(basetree_t), intent(inout) :: this
      integer, intent(out), optional :: ierr
!
! Set "current" pointer to the first node in the tree.
! If tree is empty, "current" end as null pointer.
!
      integer :: ierr0

      if (associated(this % root)) then
        this % current => Leftmost(this % root)
        ierr0 = TREE_ERR_OK
      else
        this % current => null()
        ierr0 = TREE_ERR_EMPTY
      endif
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Firstnode



    module subroutine basetree_Resetnode(this, ierr)
      class(basetree_t), intent(inout) :: this
      integer, intent(out), optional :: ierr
!
! Unset the "current" pointer.
! If tree is empty, flag TREE_ERR_EMPTY
!
      integer :: ierr0

      this % current => null()
      ierr0 = TREE_ERR_OK
      if (.not. associated(this % root)) ierr0 = TREE_ERR_EMPTY
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Resetnode



    module subroutine basetree_Nextnode(this, ierr)
      class(basetree_t), intent(inout) :: this
      integer, intent(out), optional :: ierr
!
! Moves "current" pointer to the next node in the tree.
! Ïf "current" is the last node, "current" is nullified.
!
      integer :: ierr0
      class(basenode_t), pointer :: n

      n => this % current
      if (.not. associated(n)) then
        ierr0 = TREE_ERR_NOCURRENT
      else
        n => Successor(n)
        if (associated(n)) then
          ierr0 = TREE_ERR_OK
        else
          ierr0 = TREE_ERR_NONEXT
        endif
        this % current => n
      endif
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Nextnode



    function Successor(n)
      class(basenode_t), pointer :: Successor
      class(basenode_t), pointer, intent(in) :: n
!
! Fails if called with null pointer. Returns a pointer to a successor node
! or a null pointer if the current node is the last node
!
      class(basenode_t), pointer :: oldnode

      if (.not. associated(n)) error stop "Successor called with nil node"

      ! If node has a right subtree, the next element is the leftmost element
      ! in this right subtree.
      if (associated(n % right)) then
        Successor => Leftmost(n % right)
        return
      endif

      ! If node does not have a right child, the next element is a parent of
      ! a first left child that is found when going up.
      ! If no such parent is found, the current element is the last one.

      oldnode => n
      do
        Successor => oldnode % Parentf()

        ! no more parents, null pointer is returned
        if (.not. associated(Successor)) exit

        ! parent of a left child is the next node and it is returned
        if (Is_left_child(oldnode)) exit

        ! defensive check, that current node is a right child
        ! (and must have a parent)
        if (.not. Is_right_child(oldnode)) &
        &   error stop "Successor unexpected error: node is not child"

        ! parent of a right child is not the next node, but its
        ! grandparent can be...
        oldnode => oldnode % Parentf()
      enddo
    end function Successor



    module function Leftmost(n) result(lm)
      class(basenode_t), pointer :: lm
      class(basenode_t), pointer, intent(in) :: n
!
! Fails if called with a null pointer, always returns associated pointer.
!
      if (.not. associated(n)) error stop "Leftmost called with nil node"
      lm => n
      do
        if (.not. associated(lm % left)) exit
        lm => lm % left
      enddo
    end function Leftmost



    module function basetree_Isvalid_BST(this, cfun) result(isvalid)
      logical :: isvalid
      class(basetree_t), intent(in) :: this
      procedure(cfun_abstract) :: cfun
!
! Verify that the tree is a valid BST tree
!
      class(basenode_t), pointer :: minnode, maxnode
      call Verify_BST(this % root, cfun, isvalid, minnode, maxnode)
    end function basetree_Isvalid_BST



    recursive subroutine Verify_BST(tree, cfun, isvalid, minnode, maxnode)
      class(basenode_t), pointer, intent(in) :: tree
      procedure(cfun_abstract) :: cfun
      logical, intent(out) :: isvalid
      class(basenode_t), pointer, intent(out) :: minnode, maxnode
!
! Recursively check all subtrees and assert all are valid BST's
!
      class(basenode_t), pointer :: min_left_subtree, min_right_subtree
      class(basenode_t), pointer :: max_left_subtree, max_right_subtree
      logical :: isvalid_left_subtree, isvalid_right_subtree
      integer :: resmin, resmax


      ! Empty tree is a valid tree
      if (.not. associated(tree)) then
        isvalid = .true.
        minnode => null()
        maxnode => null()
        return
      endif

      ! Validate left and right sub-trees
      call Verify_BST(tree % left, cfun, isvalid_left_subtree, &
      &   min_left_subtree, max_left_subtree)
      call Verify_BST(tree % right, cfun, isvalid_right_subtree, &
      &   min_right_subtree, max_right_subtree)

      ! Single node tree is always valid
      if (.not. associated(tree % left) .and. &
      &   .not. associated(tree % right)) then
        isvalid = .true.
        minnode => tree
        maxnode => tree
        return
      endif


      ! Combine maxnode / minnode from both subtrees
      if (associated(tree % left) .and. .not. associated(tree % right)) then
        ! Left child only
        minnode => min_left_subtree
        maxnode => max_left_subtree
      elseif (.not. associated(tree % left) .and. &
      &       associated(tree % right)) then
        ! Right child only
        minnode => min_right_subtree
        maxnode => max_right_subtree
      elseif (associated(tree % left) .and. associated(tree % right)) then
        ! Both childrens
        resmin = cfun(min_left_subtree % dat, min_right_subtree % dat)
        resmax = cfun(max_left_subtree % dat, max_right_subtree % dat)

        select case(resmin)
        case(1)  ! "A" < "B"
           minnode => min_left_subtree
        case(-1) ! "A" > "B"
           minnode => min_right_subtree
        case(0)  ! "A" == "B"
          error stop "Verify_BST: resmin duplicit nodes?"
        case default
          error stop "Verify_BST: resmin invalid result from cfun"
        end select

        select case(resmax)
        case(1)  ! "A" < "B"
           maxnode => max_right_subtree
        case(-1) ! "A" > "B"
           maxnode => max_left_subtree
        case(0)  ! "A" == "B"
          error stop "Verify_BST: resmax duplicit nodes?"
        case default
          error stop "Verify_BST: resmax invalid result from cfun"
        end select

      else
        error stop "Verify_BST: impossible branch"
      endif


      ! Tree is a valid BST if both sub-trees are also valid
      isvalid = isvalid_left_subtree .and. isvalid_right_subtree

      ! Also verify that:
      !   (1)  all lc%key < root%key,
      !   (2)  all rc%key > root%key.
      ! Invalidate "isvalid" if any condition not met
      if (associated(max_left_subtree)) then
        resmax = cfun(max_left_subtree % dat, tree % dat)
      else
        ! With empty left tree, condition (1) must be valid
        resmax = 1
      endif
      if (resmax /= 1) isvalid = .false.

      if (associated(min_right_subtree)) then
        resmin = cfun(tree % dat, min_right_subtree % dat)
      else
        ! With empty right tree, condition (2) must be valid
        resmin = 1
      endif
      if (resmin /= 1) isvalid = .false.

    end subroutine Verify_BST



    module function basetree_Height_range(this) result(ht)
       integer :: ht(2)
       class(basetree_t), intent(in) :: this
       call recurse_HR(this % root, ht(1), ht(2))
    end function basetree_Height_range



    recursive subroutine recurse_HR(n, hmin, hmax)
      class(basenode_t), pointer, intent(in) :: n
      integer, intent(out) :: hmin, hmax

      integer :: hmin_left, hmin_right, hmax_left, hmax_right

      if (.not. associated(n)) then
        hmin = -1
        hmax = -1
        return
      endif
      call recurse_HR(n % Leftchild(), hmin_left, hmax_left)
      call recurse_HR(n % Rightchild(), hmin_right, hmax_right)
      hmin = min(hmin_left, hmin_right)
      hmax = max(hmax_left, hmax_right)

      hmin = hmin + 1
      hmax = hmax + 1
    end subroutine recurse_HR



    !Finalizer for basenode_t objects is not needed at the moment
    !
    !subroutine basenode_Destructor(n)
    !  type(basenode_t), intent(inout) :: n
    !  if (allocated(n % dat)) deallocate(n % dat)
    !end subroutine basenode_Destructor



    module subroutine basetree_Destructor(this)
      type(basetree_t), intent(inout) :: this
!
! Finalize the "basetree_t" object
!
      integer :: counter

      counter = 0
print *, 'In basetree_Destructor...'
      if (associated(this % root)) call delete_recurse(this % root, counter)
print *, '...deleted nodes = ', counter

      if (this % nodes /= counter) &
      &   error stop "basetree_Delete: counter mismatch"

      ! reset all components (just in case the routine called directly)
      this % nodes = 0
      this % current => null()
    end subroutine basetree_Destructor



    recursive subroutine delete_recurse(n, counter)
      class(basenode_t), pointer, intent(inout) :: n
      integer, intent(out) :: counter
 !
 ! Fails if "n" is not associated.
 ! Deallocate recursively all nodes in left/right subtrees and then
 ! the node itself.
 !
      integer :: ileft, iright

      if (.not. associated(n)) error stop "delete_recurse: null pointer"

      ileft = 0
      iright = 0

      if (associated(n % left)) call delete_recurse(n % left, ileft)
      if (associated(n % right)) call delete_recurse(n % right, iright)

      ! defensive check that nodes were deallocated
      if (associated(n % left) .or. associated(n % right)) &
      &   error stop "delete_recurse: nodes left allocated"

      deallocate(n)
      counter = ileft + iright + 1
    end subroutine delete_recurse



    subroutine basetree_Display(this)
      class(basetree_t), intent(in) :: this

      integer :: ch, gap, line, n, level, maxlevel
      integer :: i
      class(basenode_t), pointer :: root
      type ptr_t
        class(basenode_t), pointer :: p
      end type ptr_t
      type(ptr_t), allocatable :: queue(:)
      type(ptr_t) :: item, lc, rc


      ch = 2    ! item width (TODO)
      line = 100 ! line length
      maxlevel = 5
      level = -1

      allocate(queue(0))
      item % p => this % root
      queue = enqueue(queue, item)

      print '(a)', '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
      do
        level = level + 1
        n = 2**(level)
        gap = max((line-n*ch)/(n), 0)
        do i=1,n
          queue = dequeue(queue, item)
          if (i==1) then
            call print_node(null(), gap/2)
          else
            call print_node(null(), gap)
          endif
          call print_node(item % p, ch)

          if (associated(item % p)) then
            lc % p => item % p % Leftchild()
            rc % p => item % p % Rightchild()
          else
            lc % p => null()
            rc % p => null()
          endif
          queue = enqueue(queue, lc)
          queue = enqueue(queue, rc)
        enddo
        write(*,*)
        if (level >= maxlevel) exit
      enddo
      print '(a)', '1234567890         01234567890         01234567890         012345678901234567890'

    contains

      function enqueue(pin, new) result(pout)
        type(ptr_t), intent(in) :: pin(:)
        type(ptr_t), intent(in) :: new
        type(ptr_t), allocatable :: pout(:)
        integer :: n
        n = size(pin)
        allocate(pout(n+1))
        pout(1:n) = pin
        pout(n+1) = new
      end function enqueue

      function dequeue(pin, removed) result(pout)
        type(ptr_t), intent(in) :: pin(:)
        type(ptr_t), intent(out) :: removed
        type(ptr_t), allocatable :: pout(:)
        integer :: n
        n = size(pin)
        if (n==0) error stop 'dequeue empty'
        allocate(pout(n-1))
        removed = pin(1)
        pout = pin(2:n)
      end function dequeue
    end subroutine basetree_Display



    subroutine print_node(node, ch)
      class(basenode_t), intent(in), pointer :: node
      integer, intent(in) :: ch
!
! print node on screen, width is "ch" characters
! node can be empty pointer
!
      integer, parameter :: MAXBUF = 200
      character(len=MAXBUF) :: prepare
      character(len=:), allocatable :: buffer
      integer :: i

        ! temporary - works for integers
      write(prepare,*) ch
      buffer = '(i'//trim(adjustl(prepare))//')'
      do i=1,MAXBUF
        prepare(i:i) = ' '
      enddo

      if (associated(node)) then
        ! temporary - must take actual value
        write(prepare,fmt=buffer) node % dat(1)
      endif
      write(*,'(a)',advance='no') prepare(1:ch)
    end subroutine print_node




  end submodule basetree
