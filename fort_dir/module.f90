! coding: utf-8


module params

    real(8), parameter :: PI = 4.0d0 * atan(1.0d0)

end module


module util_module
    interface

        function calc_pi(op, n) result(pi)

            integer, intent(in) :: n
            integer, intent(in) :: op(:)
            real(8)             :: pi

        end function calc_pi

    end interface
end module util_module


module gene_algo
    interface

        subroutine init(chrom, n, nbit)

            integer, intent(out) :: chrom(:, :)
            integer, intent(in) :: n, nbit

        end subroutine init

        function evaluate(chrom, n, nbit) result(f)

            integer, intent(in) :: chrom(:, :)
            integer, intent(in) :: n, nbit
            real(8)             :: f(n)

        end function evaluate

        subroutine roulette(val, q, n)

            real(8), intent(in)  :: val(:)
            real(8), intent(out) :: q(:)
            integer, intent(in)  :: n

        end subroutine roulette

        subroutine select_gene(chrom, q, n, nbit)

            integer, intent(out) :: chrom(:, :)
            real(8), intent(in)  :: q(:)
            integer, intent(in)  :: n, nbit

        end subroutine select_gene

        subroutine crossover(chrom, cp, n, nbit)

            integer, intent(out) :: chrom(:, :)
            integer, intent(in)  :: n, nbit
            real(8), intent(in)  :: cp

        end subroutine crossover

        subroutine mutation(chrom, mp, n, nbit)

            integer, intent(out) :: chrom(:, :)
            integer, intent(in)  :: n, nbit
            real(8), intent(in)  :: mp

        end subroutine mutation

    end interface
end module gene_algo