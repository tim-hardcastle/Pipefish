import

"lib/complex.ch" :: ""

def

mandelbrot(c pair) :
    mandeler(0, (0.0 :: 0.0))
given :
    mandeler = func(i int, z) :
        i > 50 : true
        z[0] * z[0] + z[1] * z[1] > 4.0 : false
        else : this(i + 1, z squared + c , c)

