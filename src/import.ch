import

"lib/complex.ch"

def

mandelbrot(c pair) :
    mandeler(0, (0.0 :: 0.0), c)
given :
    mandeler = func(i int, z, c pair) :
        i > 50 : true
        z[0] * z[0] + z[1] * z[1] > 4.0 : false
        else : this(i + 1, z squared + c , c)

