import

gofunc "math"

def

abs(x float64) : gofunc {
    return math.Abs(x)
}

acos(x float64) : gofunc {
    return math.Acos(x)
}

acosh(x float64) : gofunc {
    return math.Acosh(x)
}

asin(x float64) : gofunc {
    return math.Asin(x)
}

asinh(x float64) : gofunc {
    return math.Asinh(x)
}

atan(x float64) : gofunc {
    return math.Atan(x)
}

atan2(y, x float64) : gofunc {
    return math.Atan2(y, x)
}

atanh(x float64) : gofunc {
    return math.Atanh(x)
}

cbrt(x float64) : gofunc {
    return math.Cbrt(x)
}

ceil(x float64) : gofunc {
    return math.Ceil(x)
}

copysign(f, sign float64) : gofunc {
    return math.Copysign(f, sign)
}

cos(x float64) : gofunc {
    return math.Cos(x)
}

cosh(x float64) : gofunc {
    return math.Cosh(x)
}

dim(x, y float64) : gofunc {
    return math.Dim(x, y)
}

erf(x float64) : gofunc {
    return math.Erf(x)
}

erfc(x float64) : gofunc {
    return math.Erfc(x)
}

erfcinv(x float64) : gofunc {
    return math.Erfcinv(x)
}

erfinv(x float64) : gofunc {
    return math.Erfinv(x)
}

exp(x float64) : gofunc {
    return math.Exp(x)
}

exp2(x float64) : gofunc {
    return math.Exp2(x)
}

expm1(x float64) : gofunc {
    return math.Expm1(x)
}

fMA(x float64, y float64, z float64) : gofunc {
    return math.FMA(x, y, z)
}

floor(x float64) : gofunc {
    return math.Floor(x)
}

frexp(f float64) : gofunc {
    return math.Frexp(f)
}

gamma(x float64) : gofunc {
    return math.Gamma(x)
}

hypot(p, q float64) : gofunc {
    return math.Hypot(p, q)
}

inf(sign int) : gofunc {
    return math.Inf(sign)
}

isInf(f float64, sign int) : gofunc {
    return math.IsInf(f, sign)
}

isNaN(f float64) : gofunc {
    return math.IsNaN(f)
}

j0(x float64) : gofunc {
    return math.J0(x)
}

j1(x float64) : gofunc {
    return math.J1(x)
}

jn(n int, x float64) : gofunc {
    return math.Jn(n, x)
}

// ldexp(frac float64, ex int) : gofunc {
//     return math.Ldexp(frac, ex)
// }

lgamma(x float64) : gofunc {
    return math.Lgamma(x)
}

log(x float64) : gofunc {
    return math.Log(x)
}

log10(x float64) : gofunc {
    return math.Log10(x)
}

log1p(x float64) : gofunc {
    return math.Log1p(x)
}

log2(x float64) : gofunc {
    return math.Log2(x)
}

logb(x float64) : gofunc {
    return math.Logb(x)
}

max(x, y float64) : gofunc {
    return math.Max(x, y)
}

min(x, y float64) : gofunc {
    return math.Min(x, y)
}

mod(x, y float64) : gofunc {
    return math.Mod(x, y)
}

modf(x float64) : gofunc {
    return math.Modf(x)
}

nextafter(x, y float64) : gofunc {
    return math.Nextafter(x, y)
}

pow(x, y float64) : gofunc {
    return math.Pow(x, y)
}

pow10(n int) : gofunc {
    return math.Pow10(n)
}

remainder(x, y float64) : gofunc {
    return math.Remainder(x, y)
}

round(x float64) : gofunc {
    return math.Round(x)
}

roundToEven(x float64) : gofunc {
    return math.RoundToEven(x)
}

signbit(x float64) : gofunc {
    return math.Signbit(x)
}

sin(x float64) : gofunc {
    return math.Sin(x)
}

sincos(x float64) : gofunc {
    return math.Sincos(x)
}

sinh(x float64) : gofunc {
    return math.Sinh(x)
}

sqrt(x float64) : gofunc {
    return math.Sqrt(x)
}

tan(x float64) : gofunc {
    return math.Tan(x)
}

tanh(x float64) : gofunc {
    return math.Tanh(x)
}

trunc(x float64) : gofunc {
    return math.Trunc(x)
}

y0(x float64) : gofunc {
    return math.Y0(x)
}

y1(x float64) : gofunc {
    return math.Y1(x)
}

yn(n int, x float64) : gofunc {
    return math.Yn(n, x)
}