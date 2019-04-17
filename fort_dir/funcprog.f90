! coding: utf-8


function calc_pi(op, n) result(pi)

    implicit none
    integer, intent(in) :: op(:)
    integer, intent(in) :: n
    real(8)             :: pi
    integer             :: i

    pi = 0.0d0

    do i = 1, n
        if (op(i) .eq. 0) then
            pi = pi + 2.0d0
        else
            pi = sqrt(pi)
        endif
    enddo

    return
end function calc_pi


function evaluate(chrom, n, nbit) result(f)

    use params
    use util_module
    implicit none
    integer, intent(in) :: chrom(:, :)
    integer, intent(in) :: n, nbit
    real(8)             :: f(n)
    real(8)             :: x
    integer             :: i

    f = 0.0d0

    do i = 1, n
        x = calc_pi(chrom(i, :), nbit)
        f(i) = dabs(1.0d0 / (x - PI))
    enddo

    return
end function evaluate