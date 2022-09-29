import

golang "math"

def

abs(x float) : golang {
    return math.Abs(x)
}

acos(x float) : golang {
    return math.Acos(x)
}

acosh(x float) : golang {
    return math.Acosh(x)
}

asin(x float) : golang {
    return math.Asin(x)
}

asinh(x float) : golang {
    return math.Asinh(x)
}

atan(x float) : golang {
    return math.Atan(x)
}

atan2(y, x float) : golang {
    return math.Atan2(y, x)
}

atanh(x float) : golang {
    return math.Atanh(x)
}

cbrt(x float) : golang {
    return math.Cbrt(x)
}

ceil(x float) : golang {
    return math.Ceil(x)
}

copysign(f, sign float) : golang {
    return math.Copysign(f, sign)
}

cos(x float) : golang {
    return math.Cos(x)
}

cosh(x float) : golang {
    return math.Cosh(x)
}

dim(x, y float) : golang {
    return math.Dim(x, y)
}

erf(x float) : golang {
    return math.Erf(x)
}

erfc(x float) : golang {
    return math.Erfc(x)
}

erfcinv(x float) : golang {
    return math.Erfcinv(x)
}

erfinv(x float) : golang {
    return math.Erfinv(x)
}

exp(x float) : golang {
    return math.Exp(x)
}

exp2(x float) : golang {
    return math.Exp2(x)
}

expm1(x float) : golang {
    return math.Expm1(x)
}

fMA(x float, y float, z float) : golang {
    return math.FMA(x, y, z)
}

floor(x float) : golang {
    return math.Floor(x)
}

frexp(f float) : golang {
    return math.Frexp(f)
}

gamma(x float) : golang {
    return math.Gamma(x)
}

hypot(p, q float) : golang {
    return math.Hypot(p, q)
}

inf(sign int) : golang {
    return math.Inf(sign)
}

isInf(f float, sign int) : golang {
    return math.IsInf(f, sign)
}

isNaN(f float) : golang {
    return math.IsNaN(f)
}

j0(x float) : golang {
    return math.J0(x)
}

j1(x float) : golang {
    return math.J1(x)
}

jn(n int, x float) : golang {
    return math.Jn(n, x)
}

ldexp(frac float, ex int) : golang {
    return math.Ldexp(frac, ex)
}

lgamma(x float) : golang {
    return math.Lgamma(x)
}

log(x float) : golang {
    return math.Log(x)
}

log10(x float) : golang {
    return math.Log10(x)
}

log1p(x float) : golang {
    return math.Log1p(x)
}

log2(x float) : golang {
    return math.Log2(x)
}

logb(x float) : golang {
    return math.Logb(x)
}

max(x, y float) : golang {
    return math.Max(x, y)
}

min(x, y float) : golang {
    return math.Min(x, y)
}

mod(x, y float) : golang {
    return math.Mod(x, y)
}

modf(x float) : golang {
    return math.Modf(x)
}

nextafter(x, y float) : golang {
    return math.Nextafter(x, y)
}

pow(x, y float) : golang {
    return math.Pow(x, y)
}

pow10(n int) : golang {
    return math.Pow10(n)
}

remainder(x, y float) : golang {
    return math.Remainder(x, y)
}

round(x float) : golang {
    return math.Round(x)
}

roundToEven(x float) : golang {
    return math.RoundToEven(x)
}

signbit(x float) : golang {
    return math.Signbit(x)
}

sin(x float) : golang {
    return math.Sin(x)
}

sincos(x float) : golang {
    return math.Sincos(x)
}

sinh(x float) : golang {
    return math.Sinh(x)
}

sqrt(x float) : golang {
    return math.Sqrt(x)
}

tan(x float) : golang {
    return math.Tan(x)
}

tanh(x float) : golang {
    return math.Tanh(x)
}

trunc(x float) : golang {
    return math.Trunc(x)
}

y0(x float) : golang {
    return math.Y0(x)
}

y1(x float) : golang {
    return math.Y1(x)
}

yn(n int, x float) : golang {
    return math.Yn(n, x)
}