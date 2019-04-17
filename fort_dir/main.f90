! coding: utf-8


program main

    use gene_algo
    use util_module
    implicit none

    integer, parameter :: n = 100, nbit = 60, M = 4000
    integer            :: i
    integer            :: chrom(n, nbit) = 0
    real(8)            :: f(n), q(n)
    real(8)            :: cp = 0.8d0, mp = 0.03d0

    call init(chrom, n, nbit)
    do i = 1, M
        f = evaluate(chrom, n, nbit)
        call roulette(f, q, n)
        call select_gene(chrom, q, n, nbit)
        call crossover(chrom, cp, n, nbit)
        call mutation(chrom, mp, n, nbit)
    enddo

    do i = 1, n
        write(*, *) calc_pi(chrom(i, :), nbit)
        ! write(*, *) chrom(i, :)
    enddo

end program main