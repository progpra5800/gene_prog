! coding: utf-8


subroutine init(chrom, n, nbit)

    implicit none
    integer, intent(out) :: chrom(:, :)
    integer, intent(in) :: n, nbit
    integer             :: i, j
    integer             :: SEED = 1
    real(8)             :: r

    ! call random_seed(SEED)
    do i = 1, n
        do j = 1, nbit
            call random_number(r)
            if (r .le. 0.5d0) then
                chrom(i, j) = 0
            else
                chrom(i, j) = 1
            endif
        enddo
    enddo

end subroutine init


subroutine roulette(val, q, n)

    implicit none
    real(8), intent(in)  :: val(:)
    real(8), intent(out) :: q(:)
    integer, intent(in)  :: n
    integer              :: i
    real(8)              :: sums

    sums = sum(val)
    q(1) = val(1) / sums
    do i = 2, n
        q(i) = q(i - 1) + val(i) / sums
    enddo

end subroutine roulette


subroutine select_gene(chrom, q, n, nbit)

    implicit none
    integer, intent(out) :: chrom(:, :)
    real(8), intent(in)  :: q(:)
    integer, intent(in)  :: n, nbit
    integer              :: i, j
    real(8)              :: r
    integer, allocatable :: select_idx(:)
    integer, allocatable :: new_chrom(:, :)

    allocate(select_idx(n), new_chrom(n, nbit))

    do i = 1, n
        call random_number(r)
        do j = 1, n
            if (r .le. q(j)) then
                select_idx(i) = j
                exit
            endif
        enddo
    enddo

    do i = 1, n
        new_chrom(i, :) = chrom(select_idx(i), :)
    enddo

    do i = 1, n
        chrom(i, :) = new_chrom(i, :)
    enddo

    deallocate(select_idx, new_chrom)

end subroutine select_gene


subroutine crossover(chrom, cp, n, nbit)

    implicit none
    integer, intent(out) :: chrom(:, :)
    integer, intent(in)  :: n, nbit
    real(8), intent(in)  :: cp
    integer              :: i, crossNum, split
    integer, allocatable :: ns(:), sc(:)
    real(8)              :: r

    allocate(ns(n), sc(nbit))
    crossNum = 0
    ns = 0

    do i = 1, n
        call random_number(r)
        if (r .le. cp) then
            crossNum = crossNum + 1
            ns(crossNum) = i
        endif
    enddo

    if (mod(crossNum, 2) .ne. 0) then
        ns(crossNum) = 0
        crossNum = crossNum - 1
    endif

    i = 1
    do while ((ns(i) .ne. 0) .and. i .le. n)
        call random_number(r)
        split = int(r * nbit) + 1
        sc(split:) = chrom(ns(i), split:)
        chrom(ns(i), split:) = chrom(ns(i + 1), split:)
        chrom(ns(i + 1), split:) = sc(split:)
        i = i + 2
    enddo

    deallocate(ns, sc)

end subroutine crossover


subroutine mutation(chrom, mp, n, nbit)

    implicit none
    integer, intent(out) :: chrom(:, :)
    integer, intent(in)  :: n, nbit
    real(8), intent(in)  :: mp
    integer              :: i, j
    real(8)              :: r

    do i = 1, n
        do j = 1, nbit
            call random_number(r)
            if (r .le. mp) then
                if (chrom(i, j) .eq. 0) then
                    chrom(i, j) = 1
                else
                    chrom(i, j) = 0
                endif
            endif
        enddo
    enddo

end subroutine mutation