! Classes for a binary search tree (BST) container.
!
! grofz@vscht.cz
! March 2022
!
! Version: Work in progress... Submoduled
!
  module tree_m
    use kinds_m, only : I4B, I1B
    use abstract_container
    implicit none
    private
    public compare_fun, basetree_t, basenode_t, rbtr_t, rbnode_t
    public tree_mold

    ! Kind of integer array containing the data
    !!integer, parameter, public :: DAT_KIND = I4B
    public DAT_KIND

    ! Variable used as "mold" in transfer function
    integer(DAT_KIND), allocatable :: tree_mold(:)

    ! Error codes of tree processing routines
    integer, parameter, public :: TREE_ERR_OK = 0,         &
    &                             TREE_ERR_NOCURRENT = -1, &
    &                             TREE_ERR_EMPTY = -2,     &
    &                             TREE_ERR_NONEXT = 1

    ! Function to compare two tree nodes
    !abstract interface
    !  integer function cfun_abstract(a, b)
    !    import DAT_KIND
    !    integer(DAT_KIND), intent(in) :: a(:), b(:)
    !  end function
    !end interface



    ! base tree class
    type :: basenode_t
      private
      class(basenode_t), pointer :: parent => null()
      class(basenode_t), pointer :: left => null()
      class(basenode_t), pointer :: right => null()
      integer(DAT_KIND), allocatable :: dat(:)
    contains
      procedure :: Parentf => basenode_Parent
      procedure :: Leftchild => basenode_Leftchild
      procedure :: Rightchild => basenode_Rightchild
      procedure :: Grandparent => basenode_Grandparent
      procedure :: Uncle => basenode_Uncle
      !final :: basenode_Destructor
    end type basenode_t



    type :: basetree_t
      private
      class(basenode_t), pointer :: root => null()
      class(basenode_t), pointer :: current => null()
      integer :: nodes = 0
      procedure(compare_fun), pointer, nopass, public :: cfun => null()
    contains
      !procedure :: Initialize => basetree_Initialize
      procedure :: Add => basetree_Add
      procedure :: Add2 => basetree_Add2
      procedure :: Isin => basetree_Isin
      procedure :: Read => basetree_Read
      procedure :: ReadNext => basetree_ReadNext
      procedure :: Resetnode => basetree_Resetnode
      procedure :: Firstnode => basetree_Firstnode
      procedure :: Nextnode => basetree_Nextnode
      procedure :: Printcurrentnode => basetree_Printcurrentnode
      final :: basetree_Destructor
      procedure :: Size => basetree_Nodes
      procedure :: Isvalid_BST => basetree_Isvalid_BST
      procedure :: Height_range => basetree_Height_range
      procedure :: Display => basetree_Display
    end type basetree_t

    interface basetree_t
      module function basetree_Initialize(cfun) result(new)
        type(basetree_t) :: new
        procedure(compare_fun) :: cfun
      end function basetree_Initialize
    end interface basetree_t



    interface
      module function basetree_Printcurrentnode(this) result(str)
        class(basetree_t), intent(in) :: this
        character(len=:), allocatable :: str
      end function basetree_Printcurrentnode

      module subroutine basetree_Add(this, dat, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Add

      module subroutine basetree_Add2(this, dat, newnode, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        class(basenode_t), pointer, optional :: newnode
        integer, intent(out), optional :: ierr
      end subroutine basetree_Add2

      module function basetree_Isin(this, dat) result(exists)
        logical :: exists
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
      end function basetree_Isin

      module function search_node(this, dat) result(foundnode)
        class(basenode_t), pointer :: foundnode
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
      end function

      module function basetree_Read(this, ierr) result(dat)
        class(basetree_t), intent(in) :: this
        integer, optional, intent(out) :: ierr
        integer(DAT_KIND), allocatable :: dat(:)
      end function basetree_Read

      module function basetree_ReadNext(this, ierr) result(dat)
        class(basetree_t), intent(inout) :: this
        integer, optional, intent(out) :: ierr
        integer(DAT_KIND), allocatable :: dat(:)
      end function basetree_ReadNext

      pure module function basetree_Nodes(this)
        integer :: basetree_Nodes
        class(basetree_t), intent(in) :: this
      end function basetree_Nodes

      module subroutine basetree_Firstnode(this, ierr)
        class(basetree_t), intent(inout) :: this
        integer, intent(out), optional :: ierr
      end subroutine basetree_Firstnode

      module subroutine basetree_Resetnode(this, ierr)
        class(basetree_t), intent(inout) :: this
        integer, intent(out), optional :: ierr
      end subroutine basetree_Resetnode

      module subroutine basetree_Nextnode(this, ierr)
        class(basetree_t), intent(inout) :: this
        integer, intent(out), optional :: ierr
      end subroutine basetree_Nextnode

      module function basetree_Isvalid_BST(this) result(isvalid)
        logical :: isvalid
        class(basetree_t), intent(in) :: this
      end function basetree_Isvalid_BST

      module function basetree_Height_range(this) result(ht)
        integer :: ht(2)
        class(basetree_t), intent(in) :: this
      end function basetree_Height_range

      module subroutine basetree_Destructor(this)
        type(basetree_t), intent(inout) :: this
      end subroutine basetree_Destructor

      module subroutine basetree_Display(this)
        class(basetree_t), intent(in) :: this
      end subroutine basetree_Display
    end interface



    interface
      module function basenode_Parent(n) result(parentf)
        class(basenode_t), pointer :: parentf
        class(basenode_t), intent(in) :: n
      end function basenode_Parent

      module function basenode_Grandparent(n) result(grandparent)
        class(basenode_t), intent(in) :: n
        class(basenode_t), pointer :: grandparent
      end function basenode_Grandparent

      module function basenode_Leftchild(n) result(leftchild)
        class(basenode_t), pointer :: leftchild
        class(basenode_t), intent(in) :: n
      end function basenode_Leftchild

      module function basenode_Rightchild(n) result(rightchild)
        class(basenode_t), pointer :: rightchild
        class(basenode_t), intent(in) :: n
      end function basenode_Rightchild

      module function Sibling(n) result(sb)
        class(basenode_t), pointer :: sb
        class(basenode_t), intent(in), pointer :: n
      end function Sibling

      module function basenode_Uncle(n) result(uncle)
        class(basenode_t), pointer :: uncle
        class(basenode_t), intent(in) :: n
      end function basenode_Uncle

      module subroutine Rotate_Left(a, piv)
        class(basetree_t), intent(inout) :: a
        class(basenode_t), pointer :: piv
      end subroutine Rotate_Left

      module subroutine Rotate_Right(a, piv)
        class(basetree_t), intent(inout) :: a
        class(basenode_t), pointer :: piv
      end subroutine Rotate_Right

      module function Is_left_child(n)
        logical :: Is_left_child
        class(basenode_t), intent(in), pointer :: n
      end function Is_left_child

      module function Is_right_child(n)
        logical :: Is_right_child
        class(basenode_t), intent(in), pointer :: n
      end function Is_right_child

      module function Leftmost(n) result(lm)
        class(basenode_t), pointer :: lm
        class(basenode_t), pointer, intent(in) :: n
      end function Leftmost
    end interface



    ! extended class for red-black trees
    integer(I1B), parameter :: RED_NODE = 1, BLACK_NODE = 0

    type, extends(basenode_t) :: rbnode_t
      private
      integer(I1B) :: color = RED_NODE
    end type rbnode_t



    type, extends(basetree_t) :: rbtr_t
      private
    contains
      procedure :: Add => rbtr_Add
      procedure :: Add2 => rbtr_Add2
      procedure :: Remove => rbtr_Delete
      procedure :: Isvalid_rbtree => rbtr_Isvalid_rbtree
      procedure :: Printcurrentnode => rbtr_Printcurrentnode
    end type rbtr_t

    interface rbtr_t
      module function rbtr_Initialize(cfun) result(new)
        type(rbtr_t) :: new
        procedure(compare_fun) :: cfun
      end function rbtr_Initialize
    end interface rbtr_t



    interface
      module function rbtr_Printcurrentnode(this) result(str)
        character(len=:), allocatable :: str
        class(rbtr_t), intent(in) :: this
      end function rbtr_Printcurrentnode

      module subroutine rbtr_Add(this, dat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, intent(out), optional :: ierr
      end subroutine rbtr_Add

      module subroutine rbtr_Add2(this, dat, newnode, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        class(basenode_t), pointer, optional :: newnode
        integer, intent(out), optional :: ierr
      end subroutine rbtr_Add2

      module function rbtr_Isvalid_rbtree(this) result(isvalid)
        logical :: isvalid
        class(rbtr_t), intent(in) :: this
      end function rbtr_Isvalid_rbtree

      module subroutine rbtr_Delete(this, dat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, optional, intent(out) :: ierr
      end subroutine rbtr_Delete
    end interface

  end module tree_m
