! coding: utf-8


program main

    use util_module
    implicit none

    integer, parameter :: n = 10
    integer            :: data(n) = 0
    real(8)            :: pi

    write(*, *) data(1)
    pi = calc_pi(data, n)
    write(*, *) pi

!     contains
!     function calc_pi(op, n) result(pi)
!
!         integer, intent(in) :: op(:)
!         integer, intent(in) :: n
!         real(8)             :: pi
!         integer             :: i
!
!         pi = 0.0d0
!
!         do i = 1, n
!             if (op(i) .eq. 0) then
!                 pi = pi + 2.0d0
!             else
!                 pi = sqrt(pi)
!             endif
!         enddo
!
!     end function calc_pi

end program main