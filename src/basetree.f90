!
! Classes for binary search tree (BST) container
!
! grofz@vscht.cz
! March 2022
!
! Version: Work in progress...
!
  module basetree_m
    use tree_common_m
    implicit none
    private

    public Rotate_left, Rotate_right, Is_right_child, Is_left_child

    type, public :: basenode_t
      private
      class(basenode_t), pointer :: parent => null()
      class(basenode_t), pointer :: left => null()
      class(basenode_t), pointer :: right => null()
      integer(DAT_KIND), allocatable :: dat(:)
    contains
      procedure :: Parentf
      procedure :: Leftchild, Rightchild
      procedure :: Grandparent
      procedure :: Uncle
      final :: basenode_Delete
    end type basenode_t



    type, public :: basetree_t
      private
      class(basenode_t), pointer, public :: root => null()
      class(basenode_t), pointer, public :: current => null()
      integer :: nodes = 0
      ! note: some components must be public for extended types to work
      ! this inconvenience could be corrected later...
    contains
      procedure :: Insert => basetree_Insert
      procedure :: Nodesf => basetree_Nodes
      procedure :: Firstnode, Nextnode
      procedure :: Printcurrentnode
      final :: basetree_Delete
      procedure :: Isvalid_BST => basetree_Isvalid_BST
      procedure :: Height_range => basetree_Height_range
    end type basetree_t

    interface basetree_t
      module procedure basetree_Initialize
    end interface basetree_t



  contains

function Printcurrentnode(this) result(str)
! TODO temporary for testing
  class(basetree_t), intent(in) :: this
  character(len=:), allocatable :: str
  character(len=1000) :: dat
  if (.not. associated(this % current)) then
    str='current not allocated...'
  else
    write(dat,*) this % current % dat
    str='['//trim(adjustl(dat))//']'
  endif

end function Printcurrentnode


    function basetree_Initialize() result(new)
      type(basetree_t) :: new
!     allocate(basenode_t :: new % typet)
! TODO just dummy constructot
    end function basetree_Initialize



    subroutine basetree_Insert(this, dat, cfun, newnode, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      procedure(cfun_abstract) :: cfun
      class(basenode_t), pointer, optional :: newnode
      integer, intent(out), optional :: ierr
!
! Insert new node to the tree.
!        
      class(basenode_t), pointer :: new0
      integer :: ierr0

      ! Prepare new node to be selected to the tree
      ! Node that other than "basenode_t" must be allocated up-stream
      if (present(newnode)) then
        new0 => newnode
      else
        allocate(basenode_t :: new0)
      endif
      if (.not. associated(new0)) &
      &   error stop 'basetree_Insert: newnode points nowhere'

      new0 % dat = dat ! TODO budu davat data tady nebo up-stream?

      if (.not. associated(this % root)) then
        ! This is the first node
        this % root => new0
        if (this % nodes /= 0) &
        &   error stop 'basetree_Insert: nodes /= 0 for the first node'
      else
        ! Insert new node into the existing tree
        call insert_recurse(this, this % root, new0, cfun)
      endif

      this % current => new0
      this % nodes = this % nodes + 1
      ierr0 = TREE_ERR_OK
      if (present(ierr)) ierr = ierr0
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
        ! Duplicit nodes
        error stop "insert_recurse: duplicit not ready yet"

      case default
        error stop "insert_recurse: invalid result from cfun"
      end select
    end subroutine insert_recurse



    function Parentf(n)
      class(basenode_t), pointer :: parentf
      class(basenode_t), intent(in) :: n
!
! Type bound function to allow access to private component of "basenode"
!
      parentf => n % parent
    end function Parentf  



    function Grandparent(n)
      class(basenode_t), pointer :: grandparent
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        grandparent => p % Parentf()
      else
        grandparent => null()
      endif
    end function Grandparent



    function Leftchild(n)
      class(basenode_t), pointer :: leftchild
      class(basenode_t), intent(in) :: n

      leftchild => n % left
    end function Leftchild



    function Rightchild(n)
      class(basenode_t), pointer :: rightchild
      class(basenode_t), intent(in) :: n

      rightchild => n % right
    end function Rightchild



    function Sibling(n)
      class(basenode_t), pointer :: sibling
      class(basenode_t), intent(in), pointer :: n
!
! Note: Unlile other "family" functions, this is not a type bound procedure
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          sibling => p % right
        elseif (associated(n, p % right)) then
          sibling => p % left
        else
          error stop 'basetree::Sibling ERROR. n is uknown to p'
        endif
      else
        sibling => null()
      endif
    end function Sibling



    function Uncle(n)
      class(basenode_t), pointer :: uncle
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        uncle => Sibling(p)
      else
        uncle => null()
      endif
    end function Uncle



    subroutine Rotate_left(a, piv)
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



    subroutine Rotate_right(a, piv)
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


    logical function Is_left_child(n)
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



    logical function Is_right_child(n)
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



    logical function Is_inner_grandparent(n)
      class(basenode_t), intent(in), pointer :: n
!
! True if "n" is in the inner sub-tree of his grand-parent
! (to be used by red-black tree insetion algorithm)
!
      class(basenode_t), pointer :: p

      is_inner_grandparent = .false.
      p => n % Parentf()
      if (.not. associated(p)) return

      if (Is_left_child(n)) then
        if (Is_right_child(p)) is_inner_grandparent = .true.
      elseif (Is_right_child(n)) then
        if (Is_left_child(p)) is_inner_grandparent = .true.
      endif
    end function Is_inner_grandparent



    pure integer function basetree_Nodes(this)
      class(basetree_t), intent(in) :: this
!
! Type-bound function - return number of nodes in the tree
!
      basetree_Nodes = this % nodes
    end function basetree_Nodes



    subroutine Firstnode(this, ierr)
      class(basetree_t), intent(inout) :: this
      integer, intent(out), optional :: ierr
!
! Set "current" pointer to the first node in the tree.
! If tree is empty, "current" is nullified.
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
    end subroutine Firstnode



    subroutine Nextnode(this, ierr)
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
    end subroutine Nextnode



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
        Successor => Parentf(oldnode)

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
        oldnode => Parentf(oldnode)
      enddo
    end function Successor



    function Leftmost(n)
      class(basenode_t), pointer :: Leftmost
      class(basenode_t), pointer, intent(in) :: n
!
! Fails if called with a null pointer, always returns associated pointer.
!
      if (.not. associated(n)) error stop "Leftmost called with nil node"
      Leftmost => n
      do
        if (.not. associated(Leftmost % left)) exit
        Leftmost => Leftmost % left
      enddo
    end function Leftmost



    function basetree_Isvalid_BST(this, cfun) result(isvalid)
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



    function basetree_Height_range(this) result(ht)
       integer :: ht(2)
       class(basetree_t), intent(in) :: this
       call recurse_HR(this % root, ht(1), ht(2))
    end function basetree_Height_range



    recursive subroutine recurse_HR(n, hmin, hmax)
      class(basenode_t), pointer, intent(in) :: n
      integer, intent(out) :: hmin, hmax

      integer :: hmin_left, hmin_right, hmax_left, hmax_right

      if (.not. associated(n)) then
        hmin = 0
        hmax = 0
        return
      endif
      call recurse_HR(n % Leftchild(), hmin_left, hmax_left)
      call recurse_HR(n % Rightchild(), hmin_right, hmax_right)
      hmin = min(hmin_left, hmin_right)
      hmax = max(hmax_left, hmax_right)

      hmin = hmin + 1
      hmax = hmax + 1
    end subroutine recurse_HR




    subroutine basenode_Delete(n)
      type(basenode_t), intent(inout) :: n
      if (allocated(n % dat)) deallocate(n % dat)
      ! TODO asi neni potreba
    end subroutine basenode_Delete



    subroutine basetree_Delete(this)
      type(basetree_t), intent(inout) :: this
!
! Finalize the "basetree_t" object
!
      integer :: counter

      counter = 0
print *, 'In basetree_Delete...'
      if (associated(this % root)) call delete_recurse(this % root, counter)
print *, '...deleted nodes = ', counter

      if (this % nodes /= counter) &
      &   error stop "basetree_Delete: counter mismatch"

      ! reset all components (just in case the routine called directly)
      this % nodes = 0
      this % current => null()
    end subroutine basetree_Delete



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
    


  end module basetree_m
